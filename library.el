;;; library.el --- Helper functions for managing downloaded papers -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.0
;; URL: https://github.com/ultronozm/library.el
;; Package-Requires: ((emacs "29.1") (czm-tex-util "0.0"))
;; Keywords: tex, convenience

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

;; Helper functions for managing downloaded PDFs, e.g.:
;; 
;; `library-find-newest-downloaded-pdf' finds the newest PDF file in
;; the downloads directory.
;;
;; `library-process-clipboard' processes the current PDF file using
;; BibTeX stored in the clipboard -- moves it to a dedicated folder,
;; names it appropriately, and records it in your BibTeX file and
;; journal.
;;
;; `library-process-arxiv' processes the current PDF file using BibTeX
;; retrieved via the arXiv API.

;;; Code:

(require 'bibtex)
(require 'czm-tex-util)
(require 'dired)
(require 'org-capture)
(require 'url)
(require 'xml)

(defcustom library-pdf-directory "~/Dropbox/math documents/unsorted/"
  "Directory where PDFs are stored."
  :type 'string
  :group 'library)

(defcustom library-local-pdf-link-name "Dropbox link"
  "Name of link to local PDF file in Org journal file."
  :type 'string
  :group 'library)

(defcustom library-bibtex-file "~/doit/refs.bib"
  "BibTeX file where references are stored."
  :type 'string
  :group 'library)

(defcustom library-download-directory "~/Downloads/"
  "Directory where PDFs are downloaded."
  :type 'string
  :group 'library)

(defcustom library-org-capture-template-key "j"
  "Key to use for journal capture template.
If nil, then no journal entry is created."
  :type 'string
  :group 'library)

;;;###autoload
(defun library-find-newest-downloaded-pdf ()
  "Find newest PDF file in the download directory."
  (interactive)
  (let* ((downloads-dir (expand-file-name library-download-directory))
         (pdf-files (directory-files downloads-dir t "\\.pdf$"))
         (pdf-files-sorted (sort pdf-files
                                 (lambda (a b)
                                   (time-less-p (nth 5 (file-attributes b))
                                                (nth 5 (file-attributes a)))))))
    (if pdf-files
        (find-file (car pdf-files-sorted))
      (message "No PDF files found in download directory."))))

(defcustom library-generate-filename-function 'library--generate-filename
  "Function used to generate filename from bibtex entry.
The function should take four arguments: the bibtex entry, year,
author, and title.  It should return a filename as a string.  The
default is the built-in function `library--generate-filename'."
  :type 'function
  :group 'library)

(defun library--generate-filename (_entry year author title)
  "Generate filename from bibtex ENTRY, YEAR, AUTHOR, and TITLE."
  (let* (
         ;; this concatenates first and last names when using arxiv
         ;; API bibtex entries.  could optimize it to just use last
         ;; names, but this is fine for now.
         (lastnames
          (mapconcat
           'identity
           (mapcar
            (lambda (x)
		            (downcase
               (replace-regexp-in-string
                " " "-"
		              (replace-regexp-in-string "[^a-zA-Z ]" ""
					                                     (car (split-string x ", "))))))
            (split-string author (rx (seq (zero-or-more space)
                                          word-boundary (group (or "and" ","))
                                          word-boundary (zero-or-more space)))))
           "_"))
         (clean-title
          (replace-regexp-in-string
           "-[-]+" "-"
           (replace-regexp-in-string
            "[^a-zA-Z0-9-]" ""
            (replace-regexp-in-string
             " +" "-"
             title)))))
    (concat year "_" lastnames "--" clean-title)))

(defun library--filename-from-bibtex ()
  "Generate filename from current bibtex entry.
Uses publication year, author last names, and title."
  (bibtex-beginning-of-entry)
  (when-let* ((entry (bibtex-parse-entry))
	             (year (bibtex-text-in-field "year" entry))
	             (author (czm-tex-util-remove-braces-accents
                       (or (bibtex-text-in-field "author" entry)
                           (bibtex-text-in-field "editor" entry))))
	             (title (czm-tex-util-remove-braces-accents (bibtex-text-in-field "title" entry))))
    (funcall library-generate-filename-function entry year author title)))

(defun library--deposit-bibtex-return-filename (bibtex)
  "Deposit BIBTEX into references file, return suitable filename."
  (save-window-excursion
    (let ((name))
      (with-temp-buffer
	       (insert bibtex)
        (goto-char (point-min))
	       (search-forward-regexp "{\\([^,]+\\)")
	       (setq name (match-string 1))
	       (if (not name)
	           (error "Invalid bib entry")))
      (find-file library-bibtex-file)
      (goto-char (point-min))
      (unless (search-forward name (point-max) t)
        (goto-char (point-max))
	       (newline-and-indent)
	       (insert bibtex)
	       (save-buffer))
      (library--filename-from-bibtex))))

(defun library--path-of-name (name)
  "Return full path of file NAME in PDF directory.
NAME is the filename without extension."
	 (expand-file-name (file-name-with-extension name "pdf")
                    library-pdf-directory))

(defun library--process-pdf-bibtex (file bibtex)
  "Process pdf FILE with associated BIBTEX.
First, deposit BIBTEX into references file.  Then, move PDF file
to PDF directory, and rename it according to the BIBTEX entry."
  (interactive)
  (when-let* ((name (library--deposit-bibtex-return-filename bibtex))
	             (path (library--path-of-name name)))
    (dired-rename-file file path t)
    (message "PDF file moved and renamed successfully.")
    path))

(defun library--generate-log-entry (arxiv-id bibtex file)
  "Generate log entry for pdf FILE with associated BIBTEX.
Optionally, make use of specified ARXIV-ID."
  (with-temp-buffer
    (let* ((entry
	           (progn
	             (insert bibtex)
	             (bibtex-beginning-of-entry)
	             (bibtex-parse-entry)))
	          (author (czm-tex-util-remove-braces-accents (bibtex-text-in-field "author" entry)))
	          (title
	           (string-clean-whitespace
	            (czm-tex-util-remove-braces-accents (bibtex-text-in-field "title" entry))))
	          (year (bibtex-text-in-field "year" entry)))
      (concat
       title
       (when arxiv-id
	        (format " (arXiv:%s)  :arxiv" arxiv-id))
       "\n"
       ":PROPERTIES:\n"
       (when arxiv-id
	        (format ":ARXIV_ID: %s\n" arxiv-id))
       (format ":YEAR: %s\n" year)
       (format ":AUTHORS: %s\n" author)
       (format ":TITLE: %s\n" title)
       ":END:\n"
       "\n"
       (format (concat "[[%s][" library-local-pdf-link-name "]]\n\n") file)
       (when arxiv-id
	        (format "[[https://arxiv.org/abs/%s][arXiv link]]\n\n" arxiv-id))))))

(defun library--process-pdf-bibtex-with-log (file bibtex &optional arxiv-id)
  "Process pdf FILE with associated BIBTEX and optional ARXIV-ID.
First, deposit BIBTEX into references file.  Then, move PDF file
to PDF directory, and rename it according to the BIBTEX entry.
Finally, create a journal entry.  Returns the new path of the PDF."
  (when-let ((newpath
		            (library--process-pdf-bibtex file bibtex)))
    (library--capture-journal-entry (library--generate-log-entry arxiv-id bibtex newpath))
    newpath))

;;;###autoload
(defun library-process-clipboard ()
  "Process PDF file using BibTeX stored in clipboard.
First, move PDF file to the PDF directory, renamed according to
the BibTeX.  Next, add BibTeX to the references file, if not
already there.  Finally, create a journal entry concerning this
reference."
  (interactive)
  (let* ((file-name (if (derived-mode-p 'dired-mode)
                        (dired-get-filename)
                      (buffer-file-name)))
	        (bibtex-entry
	         (with-temp-buffer
	           (clipboard-yank)
	           (buffer-substring-no-properties (point-min) (point-max)))))
    (library--process-pdf-bibtex-with-log file-name bibtex-entry)))

;;;###autoload
(defun library-process-clipboard-newest-download ()
  "Process newest downloaded PDF file using BibTeX stored in clipboard."
  (interactive)
  (library-find-newest-downloaded-pdf)
  (library-process-clipboard))

(defun library--bibtex-from-arxiv-id (arxiv-id)
  "Retrieve bibtex entry for ARXIV-ID using arxiv API."
  (interactive "sarxiv id: ")

  (unless (string-match "\\.\\|/" arxiv-id)
    (setq arxiv-id (concat "math/" arxiv-id)))

  (defvar url-http-end-of-headers) ; to silence byte-compiler
  (let* ((url-request-method "GET")
         (url-request-extra-headers nil)
         (url-mime-accept-string "application/atom+xml")
         (url (format "http://export.arxiv.org/api/query?id_list=%s" arxiv-id))
         (buffer (url-retrieve-synchronously url))
         (xml (with-current-buffer buffer (xml-parse-region url-http-end-of-headers (point-max)))))
    (kill-buffer buffer)

    (let* ((entry (car (xml-get-children (car xml) 'entry)))
           (id (car (xml-node-children (assoc 'id entry))))
           (published (car (xml-node-children (assoc 'published entry))))
           (year (substring published 0 4))
           (month (substring published 5 7))
           (title (replace-regexp-in-string
                   "\\s-+" " "
                   (car (xml-node-children (assoc 'title entry)))))
           (author
            (mapconcat
             (lambda (a) (car (xml-node-children (assoc 'name a))))
             (xml-get-children entry 'author)
             " and "))
           (summary (replace-regexp-in-string
                     "\\s-+" " "
                     (car (xml-node-children (assoc 'summary entry))))))
      (format "
@article{%sarXiv%s,
  title                    = {%s},
  author                   = {%s},
  year                     = {%s},
  month                    = {%s},
  url                      = {%s},
  note                     = {arXiv:%s},
  eprinttype               = {arXiv},
  abstract                 = {%s}
}" year arxiv-id title author year month id arxiv-id (substring summary 0 (- (length summary) 1))))))

(defun library--bibtex-from-arxiv-id-nasa-ads (arxiv-id)
  "Retrieve bibtex entry for ARXIV-ID using NASA ADS."
  (let* ((url
	         ;;  if arxiv-id contains a dot:
	         (if (string-match "\\." arxiv-id)
	             (format "https://ui.adsabs.harvard.edu/abs/arXiv:%s/exportcitation" arxiv-id)
	           (format "https://ui.adsabs.harvard.edu/abs/arXiv:math%s/exportcitation" (concat "%2F" arxiv-id))))
         (response-buffer (url-retrieve-synchronously url)))
    (with-current-buffer response-buffer
      (if (not (re-search-forward "<textarea class=\"export-textarea form-control\"" nil t))
          nil
	       (let ((start (match-beginning 0)))
	         (if (not (re-search-forward "</textarea>" nil t))
	             nil
	           (let* ((end (match-end 0)))
	             (caddr
	              (libxml-parse-xml-region start end)))))))))

(defun library--capture-journal-entry (text)
  "Capture TEXT as a journal entry.
Uses `library-org-capture-template-key' to determine which
template to use."
  (when library-org-capture-template-key
    (let ((org-capture-link-is-already-stored t)) ;; rmail issue
      (org-capture nil library-org-capture-template-key)
      (insert text)
      (org-capture-finalize))))

;;;###autoload
(defun library-process-arxiv (&optional filename)
  "Process arxiv pdf FILE or current buffer/Dired file.
If FILENAME is not specified, then use current buffer or Dired
file.  First, retrieve bibtex entry from arxiv API.  Then, deposit
BIBTEX into references file.  Then, move PDF file to PDF
directory, and rename it according to the BIBTEX entry.  Finally,
create a journal entry.  Returns the new path of the PDF file."
  (interactive)
  (unless filename
    (if (derived-mode-p 'dired-mode)
	       (setq filename (dired-get-filename))
      (setq filename (buffer-file-name))))
  (when-let* ((arxiv-id (file-name-base filename))
	             (bibtex-entry (library--bibtex-from-arxiv-id arxiv-id)))
    (library--process-pdf-bibtex-with-log filename bibtex-entry arxiv-id)))

;;;###autoload
(defun library-process-arxiv-manual-id ()
  "Process arxiv file with manual arxiv id."
  (interactive)
  (let ((filename
	        (if (derived-mode-p 'dired-mode)
	            (dired-get-filename)
	          (buffer-file-name)))
	       (arxiv-id (read-string "arxiv id: ")))
    (when-let ((bibtex-entry (library--bibtex-from-arxiv-id arxiv-id)))
      (library--process-pdf-bibtex-with-log filename bibtex-entry arxiv-id))))

;;;###autoload
(defun library-process-arxiv-dired ()
  "Process arxiv files marked in Dired."
  (interactive)
  ;; if no marked files, then deal with current file
  (if-let ((marked-files (dired-get-marked-files)))
      (dolist (file marked-files)
	       (library-process-arxiv file))
    (library-process-arxiv)))

;;;###autoload
(defun library-clipboard-to-refs ()
  "Add current clipboard contents to references file."
  (interactive)
  (save-window-excursion
    (let ((name))
      (with-temp-buffer
	       (clipboard-yank)
	       (goto-char (point-min))
	       (search-forward-regexp "{\\([^,]+\\)")
	       (setq name (match-string 1))
	       (if (not name)
	           (error "Invalid bib entry in clipboard")))
      (find-file library-bibtex-file)
      (goto-char (point-min))
      (if (search-forward name (point-max) t)
	         (error "Bib entry already exists"))
      (goto-char (point-max))
      (newline-and-indent)
      (clipboard-yank)
      (save-buffer))))

;;;###autoload
(defun library-move-pdf ()
  "Move current PDF file to PDF directory, with a new name."
  (interactive)
  (when (string= (file-name-extension (buffer-file-name)) "pdf")
    (let* ((name (read-string "Enter new name (without .pdf): "))
           (path (library--path-of-name name)))
      (rename-file (buffer-file-name) path)
      (find-alternate-file path)
      (message "PDF file moved and renamed successfully."))))

;;;###autoload
(defun library-download-arxiv (id)
  "Download, process and visit PDF with given arXiv ID.
When called interactively, defaults to URL at point if present."
  (interactive
   (list (read-string "arXiv ID: " (thing-at-point 'url))))
  (save-excursion
    (when (string-match ".*/" id)
      (setq id (substring id (match-end 0))))
    (when (string-match "\\.pdf$" id)
      (setq id (substring id 0 -4)))
    (let* ((url (format "https://arxiv.org/pdf/%s.pdf" id))
           (outfile (expand-file-name
                     (format "%s.pdf" id) library-download-directory)))
      (url-copy-file url outfile t)
      (if (file-exists-p outfile)
          (when-let (newfile (library-process-arxiv outfile))
            (find-file newfile))
        (message "File not found.")))))

(provide 'library)
;;; library.el ends here
