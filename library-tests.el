;;; library-tests.el --- Tests for library.el        -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Paul D. Nelson

;; Author: Paul D. Nelson <ultrono@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for library.el.

;;; Code:

(require 'cl-lib)
(require 'ert)

(unless (featurep 'czm-tex-util)
  (provide 'czm-tex-util)
  (defun czm-tex-util-remove-braces-accents (s) s))

(load-file (expand-file-name
            "library.el"
            (file-name-directory (or load-file-name buffer-file-name))))

(defun library-tests--make-source-archive ()
  "Create a small gzipped tar archive and return its filename."
  (let ((tar (or (executable-find "tar")
                 (error "Cannot find tar executable")))
        (source-dir (make-temp-file "library-source-input-" t))
        (archive (make-temp-file "library-source-archive-" nil ".tar.gz")))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "paper.tex" source-dir)
            (insert "\\documentclass{article}\n\\begin{document}\nHi.\n\\end{document}\n"))
          (unless (zerop (call-process tar nil nil nil
                                       "-czf" archive "-C" source-dir "."))
            (error "Unable to create test source archive"))
          archive)
      (delete-directory source-dir t))))

(defun library-tests--make-single-source-archive ()
  "Create a gzip-compressed single source file and return its filename."
  (let ((gzip (or (executable-find "gzip")
                  (error "Cannot find gzip executable")))
        (source (make-temp-file "library-single-source-" nil ".tex")))
    (with-temp-file source
      (insert "\\documentclass{article}\nSingle source.\n"))
    (unless (zerop (call-process gzip nil nil nil "-f" source))
      (error "Unable to compress test source"))
    (concat source ".gz")))

(ert-deftest library--bounded-pdf-basename-respects-byte-limit ()
  (let ((library-filename-max-bytes 80)
        (library-filename-max-authors 3))
    (let* ((year "2026")
           (authors '("alpha" "beta" "gamma" "delta" "epsilon" "zeta"))
           (title (make-string 200 ?a))
           (base (library--bounded-pdf-basename year authors title))
           (full (file-name-with-extension base "pdf")))
      (should (<= (library--file-name-bytes full) library-filename-max-bytes)))))

(ert-deftest library--bounded-pdf-basename-uses-et-al-when-needed ()
  (let ((library-filename-max-bytes 120)
        (library-filename-max-authors 2))
    (let* ((year "2026")
           (authors '("alpha" "beta" "gamma" "delta"))
           (title (make-string 200 ?b))
           (base (library--bounded-pdf-basename year authors title)))
      (should (string-match-p "_alpha_beta_et-al--" base)))))

(ert-deftest library--truncate-to-bytes-with-suffix-respects-max-bytes ()
  (let ((file-name-coding-system 'utf-8-unix))
    (should (equal (library--truncate-to-bytes-with-suffix "abcdef" 2 "-etc")
                   "-e"))
    (should (= (library--file-name-bytes
                (library--truncate-to-bytes-with-suffix "abcdef" 2 "-etc"))
               2))))

(ert-deftest library--bounded-pdf-basename-errors-on-impossibly-small-max-bytes ()
  (let ((library-filename-max-bytes 4)
        (library-filename-max-authors 3))
    (should-error
     (library--bounded-pdf-basename "2026" '("alpha") "title")
     :type 'user-error)))

(ert-deftest library--filename-from-bibtex-string-generates-pdf-style-slug ()
  (let ((bibtex "
@article{sample,
  title = {Sample Title},
  author = {Jane Doe and John Roe},
  year = {2026}
}"))
    (should (equal (library--filename-from-bibtex-string bibtex)
                   "2026_jane-doe_john-roe--Sample-Title"))))

(ert-deftest library--bibtex-from-arxiv-id-allows-missing-summary ()
  (let ((xml-response
         "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<feed xmlns=\"http://www.w3.org/2005/Atom\">
  <entry>
    <id>https://arxiv.org/abs/2402.00354v1</id>
    <published>2024-02-01T00:00:00Z</published>
    <title> Sample Title </title>
    <author><name>Jane Doe</name></author>
    <author><name>John Roe</name></author>
  </entry>
</feed>"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (_url &rest _args)
                 (with-current-buffer (generate-new-buffer " *library-arxiv-test*")
                   (insert "HTTP/1.1 200 OK\r\n\r\n")
                   (setq url-http-end-of-headers (point))
                   (insert xml-response)
                   (current-buffer)))))
      (let ((bibtex (library--bibtex-from-arxiv-id "2402.00354")))
        (should (string-match-p "title[[:space:]]*=[[:space:]]*{Sample Title}" bibtex))
        (should (string-match-p "author[[:space:]]*=[[:space:]]*{Jane Doe and John Roe}" bibtex))
        (should (string-match-p "abstract[[:space:]]*=[[:space:]]*{}" bibtex))))))

(ert-deftest library--bibtex-from-arxiv-id-errors-without-entry ()
  (let ((xml-response
         "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<feed xmlns=\"http://www.w3.org/2005/Atom\">
</feed>"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (_url &rest _args)
                 (with-current-buffer (generate-new-buffer " *library-arxiv-test*")
                   (insert "HTTP/1.1 200 OK\r\n\r\n")
                   (setq url-http-end-of-headers (point))
                   (insert xml-response)
                   (current-buffer))))
              ((symbol-function 'library--bibtex-from-arxiv-id-nasa-ads)
               (lambda (_arxiv-id) nil)))
      (should-error
       (library--bibtex-from-arxiv-id "2402.00354")
       :type 'user-error))))

(ert-deftest library--bibtex-from-arxiv-id-falls-back-on-rate-limit ()
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (_url &rest _args)
               (with-current-buffer (generate-new-buffer " *library-arxiv-test*")
                 (insert "HTTP/1.1 200 OK\r\n\r\n")
                 (setq url-http-end-of-headers (point))
                 (insert "Rate exceeded.")
                 (current-buffer))))
            ((symbol-function 'library--bibtex-from-arxiv-id-nasa-ads)
             (lambda (_arxiv-id) "@ARTICLE{fallback}")))
    (should (equal (library--bibtex-from-arxiv-id "2402.00354")
                   "@ARTICLE{fallback}"))))

(ert-deftest library--normalize-arxiv-id-handles-urls-and-prefixes ()
  (should (equal (library--normalize-arxiv-id "arXiv:2402.00354v1")
                 "2402.00354v1"))
  (should (equal (library--normalize-arxiv-id
                  "https://arxiv.org/pdf/2402.00354.pdf")
                 "2402.00354"))
  (should (equal (library--normalize-arxiv-id
                  "arxiv.org/pdf/2402.00354.pdf")
                 "2402.00354"))
  (should (equal (library--normalize-arxiv-id
                  "https://arxiv.org/abs/2402.00354?context=math")
                 "2402.00354"))
  (should (equal (library--normalize-arxiv-id
                  "https://arxiv.org/src/math/0309136v2")
                 "math/0309136v2"))
  (should (equal (library--normalize-arxiv-id "math_0309136v2")
                 "math/0309136v2"))
  (should (equal (library--normalize-arxiv-id
                  "https://arxiv.org/abs/2402.00354/")
                 "2402.00354")))

(ert-deftest library--arxiv-source-url-adds-math-prefix-for-bare-legacy-id ()
  (should (equal (library--arxiv-source-url "0309136")
                 "https://arxiv.org/src/math/0309136"))
  (should (equal (library--arxiv-source-url "hep-th/9901001")
                 "https://arxiv.org/src/hep-th/9901001")))

(ert-deftest library--arxiv-id-file-basename-replaces-legacy-slash ()
  (should (equal (library--arxiv-id-file-basename "math/0309136")
                 "math_0309136")))

(ert-deftest library--arxiv-source-name-uses-metadata-then-id-fallback ()
  (cl-letf (((symbol-function 'library--arxiv-source-name-from-metadata)
             (lambda (_id) "2026_doe--Sample-Title")))
    (should (equal (library--arxiv-source-name "2402.00354")
                   "2026_doe--Sample-Title")))
  (cl-letf (((symbol-function 'library--arxiv-source-name-from-metadata)
             (lambda (_id) nil)))
    (should (equal (library--arxiv-source-name "0309136")
                   "math_0309136"))))

(ert-deftest library-download-arxiv-source-extracts-src-archive ()
  (skip-unless (executable-find "tar"))
  (let ((library-arxiv-source-directory (make-temp-file "library-source-" t))
        (library-arxiv-url-timeout 7)
        (archive (library-tests--make-source-archive))
        called-url
        called-timeout)
    (unwind-protect
        (cl-letf (((symbol-function 'url-retrieve-synchronously)
                   (lambda (url &optional _silent _inhibit-cookies timeout)
                     (setq called-url url)
                     (setq called-timeout timeout)
                     (with-current-buffer
                         (generate-new-buffer " *library-arxiv-source-test*")
                       (set-buffer-multibyte nil)
                       (insert "HTTP/1.1 200 OK\r\n\r\n")
                       (setq url-http-response-status 200)
                       (setq url-http-end-of-headers (copy-marker (1- (point))))
                       (insert-file-contents-literally archive)
                       (current-buffer))))
                  ((symbol-function 'library--arxiv-source-name-from-metadata)
                   (lambda (_id) "2026_doe--Sample-Title")))
          (let ((outdir (library-download-arxiv-source "0309136")))
            (should (equal called-url "https://arxiv.org/src/math/0309136"))
            (should (= called-timeout library-arxiv-url-timeout))
            (should (string-suffix-p "2026_doe--Sample-Title" outdir))
            (should (file-directory-p outdir))
            (should-not (file-exists-p (concat outdir ".tar.gz")))
            (should (equal (with-temp-buffer
                             (insert-file-contents
                              (expand-file-name "paper.tex" outdir))
                             (buffer-string))
                           "\\documentclass{article}\n\\begin{document}\nHi.\n\\end{document}\n"))))
      (delete-file archive)
      (delete-directory library-arxiv-source-directory t))))

(ert-deftest library--extract-tar-gz-handles-single-gzipped-source ()
  (skip-unless (and (executable-find "tar") (executable-find "gzip")))
  (let ((archive (library-tests--make-single-source-archive))
        (parent (make-temp-file "library-single-source-output-" t)))
    (unwind-protect
        (let ((outdir (expand-file-name "paper" parent)))
          (library--extract-tar-gz archive outdir)
          (should (equal (with-temp-buffer
                           (insert-file-contents
                            (expand-file-name "source.tex" outdir))
                           (buffer-string))
                         "\\documentclass{article}\nSingle source.\n")))
      (delete-file archive)
      (delete-directory parent t))))

(ert-deftest library--download-url-to-file-errors-on-html-response ()
  (let ((library-arxiv-url-timeout 7)
        (outfile (make-temp-file "library-download-")))
    (unwind-protect
        (cl-letf (((symbol-function 'url-retrieve-synchronously)
                   (lambda (_url &rest _args)
                     (with-current-buffer
                         (generate-new-buffer " *library-arxiv-html-test*")
                       (insert "HTTP/1.1 200 OK\r\n\r\n")
                       (setq url-http-response-status 200)
                       (setq url-http-end-of-headers (point))
                       (insert "<!doctype html>")
                       (current-buffer)))))
          (should-error
           (library--download-url-to-file "https://arxiv.org/src/0309136"
                                          outfile)
           :type 'user-error))
      (when (file-exists-p outfile)
        (delete-file outfile)))))

(ert-deftest library--download-url-to-file-errors-on-http-error ()
  (let ((library-arxiv-url-timeout 7)
        (outfile (make-temp-file "library-download-")))
    (unwind-protect
        (cl-letf (((symbol-function 'url-retrieve-synchronously)
                   (lambda (_url &rest _args)
                     (with-current-buffer
                         (generate-new-buffer " *library-arxiv-http-error-test*")
                       (insert "HTTP/1.1 403 Forbidden\r\n\r\n")
                       (setq url-http-response-status 403)
                       (setq url-http-end-of-headers (point))
                       (insert "Forbidden")
                       (current-buffer)))))
          (should-error
           (library--download-url-to-file "https://arxiv.org/src/0309136"
                                          outfile)
           :type 'user-error))
      (when (file-exists-p outfile)
        (delete-file outfile)))))

(ert-deftest library--download-url-to-file-errors-on-incomplete-headers ()
  (let ((library-arxiv-url-timeout 7)
        (outfile (make-temp-file "library-download-")))
    (unwind-protect
        (cl-letf (((symbol-function 'url-retrieve-synchronously)
                   (lambda (_url &rest _args)
                     (with-current-buffer
                         (generate-new-buffer " *library-arxiv-no-headers-test*")
                       (setq url-http-response-status 200)
                       (setq url-http-end-of-headers nil)
                       (insert "source archive")
                       (current-buffer)))))
          (should-error
           (library--download-url-to-file "https://arxiv.org/src/0309136"
                                          outfile)
           :type 'user-error))
      (when (file-exists-p outfile)
        (delete-file outfile)))))

(ert-deftest library-download-arxiv-keeps-pdf-flow-when-source-fails ()
  (let* ((library-download-directory (make-temp-file "library-download-" t))
         (processed-file (expand-file-name "processed.pdf"
                                           library-download-directory))
         visited-file)
    (unwind-protect
        (cl-letf (((symbol-function 'library--download-url-to-file)
                   (lambda (_url file)
                     (with-temp-file file
                       (insert "pdf"))
                     file))
                  ((symbol-function 'library-process-arxiv)
                   (lambda (_file _arxiv-id)
                     processed-file))
                  ((symbol-function 'library--download-arxiv-source)
                   (lambda (&rest _args)
                     (error "source unavailable")))
                  ((symbol-function 'find-file)
                   (lambda (file)
                     (setq visited-file file))))
          (library-download-arxiv "2402.00354" t)
          (should (equal visited-file processed-file)))
      (delete-directory library-download-directory t))))

(ert-deftest library-download-arxiv-downloads-source-when-processing-returns-nil ()
  (let* ((library-download-directory (make-temp-file "library-download-" t))
         source-id
         source-name)
    (unwind-protect
        (cl-letf (((symbol-function 'library--download-url-to-file)
                   (lambda (_url file)
                     (with-temp-file file
                       (insert "pdf"))
                     file))
                  ((symbol-function 'library-process-arxiv)
                   (lambda (_file _arxiv-id)
                     nil))
                  ((symbol-function 'library--download-arxiv-source-noerror)
                   (lambda (id &optional name)
                     (setq source-id id)
                     (setq source-name name))))
          (library-download-arxiv "2402.00354" t)
          (should (equal source-id "2402.00354"))
          (should (null source-name)))
      (delete-directory library-download-directory t))))

(provide 'library-tests)
;;; library-tests.el ends here
