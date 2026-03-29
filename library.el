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

(defcustom library-filename-max-bytes 255
  "Maximum number of bytes for the final PDF filename, including extension.
Most filesystems limit a single path component to 255 bytes.  When a
generated filename would exceed this limit, `library--generate-filename'
will shorten it (preferring to shorten the author list first, then the
title)."
  :type 'integer
  :group 'library)

(defcustom library-filename-max-authors 4
  "Maximum number of authors to include in the filename before using \"et-al\".
This only takes effect when the full generated filename would exceed
`library-filename-max-bytes'."
  :type 'integer
  :group 'library)

(defun library--file-name-bytes (s)
  "Return the number of bytes S will take as a filename.
This approximates the filesystem byte limit by encoding using
`file-name-coding-system' (or UTF-8 when nil)."
  (length (encode-coding-string (or s "")
                                (or file-name-coding-system 'utf-8)
                                t)))

(defun library--truncate-to-bytes (s max-bytes)
  "Return the longest prefix of S whose encoded size is <= MAX-BYTES."
  (setq s (or s ""))
  (cond
   ((<= max-bytes 0) "")
   ((<= (library--file-name-bytes s) max-bytes) s)
   (t
    ;; Binary search over character length; we measure bytes after encoding.
    (let ((lo 0)
          (hi (length s)))
      (while (< lo hi)
        (let* ((mid (/ (+ lo hi 1) 2))
               (sub (substring s 0 mid)))
          (if (<= (library--file-name-bytes sub) max-bytes)
              (setq lo mid)
            (setq hi (1- mid)))))
      (substring s 0 lo)))))

(defun library--truncate-to-bytes-with-suffix (s max-bytes suffix)
  "Truncate S to fit within MAX-BYTES, appending SUFFIX when truncation occurs."
  (setq s (or s ""))
  (setq suffix (or suffix ""))
  (if (<= (library--file-name-bytes s) max-bytes)
      s
    ;; Ensure the returned string never exceeds MAX-BYTES by truncating
    ;; SUFFIX first, then truncating S to the remaining budget.
    (let* ((suffix (library--truncate-to-bytes suffix max-bytes))
           (suffix-bytes (library--file-name-bytes suffix))
           (avail (max 0 (- max-bytes suffix-bytes))))
      (concat (library--truncate-to-bytes s avail) suffix))))

(defun library--normalize-author (author)
  "Normalize AUTHOR string by removing newlines and extra whitespace."
  (replace-regexp-in-string "\\s-+" " "
                            (replace-regexp-in-string "\n" " " author)))

(defvar library--char-accent-map
  '((?á . ?a) (?à . ?a) (?â . ?a) (?ä . ?a) (?ã . ?a) (?å . ?a) (?ā . ?a)
    (?é . ?e) (?è . ?e) (?ê . ?e) (?ë . ?e) (?ē . ?e) (?ě . ?e)
    (?í . ?i) (?ì . ?i) (?î . ?i) (?ï . ?i) (?ī . ?i)
    (?ó . ?o) (?ò . ?o) (?ô . ?o) (?ö . ?o) (?õ . ?o) (?ø . ?o) (?ō . ?o)
    (?ú . ?u) (?ù . ?u) (?û . ?u) (?ü . ?u) (?ū . ?u)
    (?ý . ?y) (?ÿ . ?y) (?ȳ . ?y)
    (?ç . ?c) (?ć . ?c) (?č . ?c)
    (?ñ . ?n) (?ń . ?n) (?ņ . ?n) (?ň . ?n)
    (?ş . ?s) (?ś . ?s) (?š . ?s)
    (?ž . ?z) (?ź . ?z) (?ż . ?z)
    (?Á . ?A) (?À . ?A) (?Â . ?A) (?Ä . ?A) (?Ã . ?A) (?Å . ?A) (?Ā . ?A)
    (?É . ?E) (?È . ?E) (?Ê . ?E) (?Ë . ?E) (?Ē . ?E) (?Ě . ?E)
    (?Í . ?I) (?Ì . ?I) (?Î . ?I) (?Ï . ?I) (?Ī . ?I)
    (?Ó . ?O) (?Ò . ?O) (?Ô . ?O) (?Ö . ?O) (?Õ . ?O) (?Ø . ?O) (?Ō . ?O)
    (?Ú . ?U) (?Ù . ?U) (?Û . ?U) (?Ü . ?U) (?Ū . ?U)
    (?Ý . ?Y) (?Ÿ . ?Y) (?Ȳ . ?Y)
    (?Ç . ?C) (?Ć . ?C) (?Č . ?C)
    (?Ñ . ?N) (?Ń . ?N) (?Ņ . ?N) (?Ň . ?N)
    (?Ş . ?S) (?Ś . ?S) (?Š . ?S)
    (?Ž . ?Z) (?Ź . ?Z) (?Ż . ?Z))
  "Mapping of accented characters to their unaccented equivalents.")

(defun library--unaccent-string (str)
  "Replace accented characters in STR with their unaccented versions."
  (let ((result ""))
    (dotimes (i (length str))
      (let* ((char (aref str i))
             (unaccented (or (cdr (assoc char library--char-accent-map)) char)))
        (setq result (concat result (char-to-string unaccented)))))
    result))

(defun library--process-authors (normalized-author)
  "Convert NORMALIZED-AUTHOR string to a list of processed author last names."
  (let ((author-list
         (split-string normalized-author
                       (rx (seq (zero-or-more space)
                                word-boundary (group (or "and" ","))
                                word-boundary (zero-or-more space))))))
    (mapcar (lambda (author-name)
              (let ((first-part (car (split-string author-name ", "))))
                (downcase
                 (replace-regexp-in-string
                  " " "-"
                  (replace-regexp-in-string
                   "[^a-zA-Z ]" ""
                   (library--unaccent-string first-part))))))
            author-list)))

(defun library--make-author-string (author)
  "Convert AUTHOR string to a normalized author string for filenames."
  (let* ((normalized-author (library--normalize-author author))
         (processed-authors (library--process-authors normalized-author)))
    (mapconcat #'identity processed-authors "_")))

(defun library--clean-title (title)
  "Clean TITLE string for use in filenames."
  (let* ((normalized-title (replace-regexp-in-string "\\s-+" " " title))
         (hyphenated-title (replace-regexp-in-string " +" "-" normalized-title))
         (clean-title (replace-regexp-in-string
                       "-[-]+" "-"
                       (replace-regexp-in-string
                        "[^a-zA-Z0-9-]" ""
                        hyphenated-title))))
    clean-title))

(defun library--bounded-pdf-basename (year processed-authors clean-title)
  "Build a PDF base filename, shortening it if needed to respect byte limits.

YEAR is a string.  PROCESSED-AUTHORS is a list of author last names as
strings (already cleaned for filenames).  CLEAN-TITLE is a cleaned title
string (already cleaned for filenames)."
  (let* ((ext-bytes (library--file-name-bytes ".pdf"))
         (min-bytes (+ ext-bytes 1)))
    (when (< library-filename-max-bytes min-bytes)
      (user-error "library-filename-max-bytes (%d) must be >= %d"
                  library-filename-max-bytes min-bytes))
    (let* ((max-base-bytes (- library-filename-max-bytes ext-bytes))
         (authors processed-authors)
         (author-string (mapconcat #'identity authors "_"))
         (title clean-title)
         (build (lambda ()
                  (if (and title (not (string-empty-p title)))
                      (concat year "_" author-string "--" title)
                    (concat year "_" author-string)))))
      (let ((name (funcall build)))
        (when (> (library--file-name-bytes name) max-base-bytes)
          ;; First try to shorten the author list to N + et-al.
          (when (and (integerp library-filename-max-authors)
                     (> library-filename-max-authors 0)
                     (> (length authors) library-filename-max-authors))
            (setq authors (append (seq-take authors library-filename-max-authors)
                                  (list "et-al")))
            (setq author-string (mapconcat #'identity authors "_"))
            (setq name (funcall build)))
          ;; If still too long, truncate the title to fit, with a suffix.
          (when (> (library--file-name-bytes name) max-base-bytes)
            (let* ((prefix (concat year "_" author-string "--"))
                   (avail (- max-base-bytes (library--file-name-bytes prefix))))
              (setq title (if (<= avail 0)
                              ""
                            (library--truncate-to-bytes-with-suffix
                             title avail "-etc")))
              (when (string-empty-p title)
                (setq title nil))
              (setq name (funcall build))))
          ;; Last resort: truncate the author string itself (keeping YEAR).
          (when (> (library--file-name-bytes name) max-base-bytes)
            (let* ((prefix (concat year "_"))
                   (title-part (if (and title (not (string-empty-p title)))
                                   (concat "--" title)
                                 ""))
                   (avail (- max-base-bytes (library--file-name-bytes
                                             (concat prefix title-part)))))
              (when (< avail 1)
                (setq title nil)
                (setq title-part "")
                (setq avail (- max-base-bytes (library--file-name-bytes prefix))))
              (setq author-string
                    (library--truncate-to-bytes-with-suffix author-string avail "_etc"))
              (setq name (funcall build))))
          ;; Absolute last resort: hard truncate the whole base name.
          (when (> (library--file-name-bytes name) max-base-bytes)
            (setq name (library--truncate-to-bytes name max-base-bytes))))
        name))))

(defun library--generate-filename (_entry year author title)
  "Generate filename from bibtex ENTRY, YEAR, AUTHOR, and TITLE."
  (let* ((author (library--ensure-utf8-encoding author))
         (title (library--ensure-utf8-encoding title))
         (normalized-author (library--normalize-author author))
         (processed-authors (library--process-authors normalized-author))
         (clean-title (library--clean-title title)))
    (library--bounded-pdf-basename year processed-authors clean-title)))

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
           (author (library--ensure-utf8-encoding
                    (czm-tex-util-remove-braces-accents
                     (bibtex-text-in-field "author" entry))))
           (title
            (library--ensure-utf8-encoding
             (string-clean-whitespace
              (czm-tex-util-remove-braces-accents
               (bibtex-text-in-field "title" entry)))))
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
  (when-let* ((newpath
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

(defun library--ensure-utf8-encoding (text)
  "Ensure TEXT is properly encoded as UTF-8.
Returns the properly decoded string."
  (if (or (null text) (string-empty-p text))
      text
    (decode-coding-string
     (encode-coding-string text 'utf-8-auto)
     'utf-8-auto)))

(defun library--xml-child-text (node child)
  "Return CHILD text from XML NODE, or nil when CHILD is absent."
  (when-let* ((child-node (car (xml-get-children node child))))
    (mapconcat (lambda (part)
                 (if (stringp part) part ""))
               (xml-node-children child-node)
               "")))

(defun library--url-response-body (buffer)
  "Return response body from URL BUFFER as a string."
  (with-current-buffer buffer
    (buffer-substring-no-properties url-http-end-of-headers (point-max))))

(defun library--arxiv-id-with-legacy-prefix (arxiv-id)
  "Return ARXIV-ID, adding the legacy \"math/\" prefix when needed."
  (if (string-match-p "\\.\\|/" arxiv-id)
      arxiv-id
    (concat "math/" arxiv-id)))

(defun library--arxiv-query-url (arxiv-id)
  "Return arXiv API query URL for ARXIV-ID."
  (format "http://export.arxiv.org/api/query?id_list=%s"
          (library--arxiv-id-with-legacy-prefix arxiv-id)))

(defun library--arxiv-response-entry (buffer)
  "Return the Atom entry parsed from arXiv API response BUFFER, or nil."
  (let ((body (string-trim (library--url-response-body buffer))))
    (unless (string-prefix-p "Rate exceeded" body)
      (car (xml-get-children
            (car (with-current-buffer buffer
                   (xml-parse-region url-http-end-of-headers (point-max))))
            'entry)))))

(defun library--arxiv-response-rate-limited-p (buffer)
  "Return non-nil when arXiv API response BUFFER indicates rate limiting."
  (string-prefix-p "Rate exceeded"
                   (string-trim (library--url-response-body buffer))))

(defun library--clean-arxiv-field (text)
  "Normalize whitespace and encoding for arXiv metadata TEXT."
  (library--ensure-utf8-encoding
   (string-clean-whitespace (or text ""))))

(defun library--arxiv-entry-bibtex (entry arxiv-id)
  "Build a BibTeX entry from arXiv Atom ENTRY for ARXIV-ID."
  (let* ((id (or (library--xml-child-text entry 'id)
                 (format "https://arxiv.org/abs/%s" arxiv-id)))
         (published (or (library--xml-child-text entry 'published) ""))
         (year (if (>= (length published) 4)
                   (substring published 0 4)
                 ""))
         (month (if (>= (length published) 7)
                    (substring published 5 7)
                  ""))
         (title (library--clean-arxiv-field
                 (library--xml-child-text entry 'title)))
         (author (library--ensure-utf8-encoding
                  (mapconcat
                   (lambda (a)
                     (or (library--xml-child-text a 'name) ""))
                   (xml-get-children entry 'author)
                   " and ")))
         (summary (library--clean-arxiv-field
                   (library--xml-child-text entry 'summary))))
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
}" year arxiv-id title author year month id arxiv-id
       (string-trim-right summary))))

(defun library--signal-arxiv-fetch-error (arxiv-id rate-limited-p)
  "Signal an appropriate metadata lookup error for ARXIV-ID.
RATE-LIMITED-P should be non-nil when the arXiv API reported throttling."
  (if rate-limited-p
      (user-error "arXiv API rate limit exceeded for %s" arxiv-id)
    (user-error "No arXiv entry found for %s" arxiv-id)))

(defun library--bibtex-from-arxiv-id (arxiv-id)
  "Retrieve bibtex entry for ARXIV-ID using arxiv API."
  (interactive "sarxiv id: ")

  (defvar url-http-end-of-headers) ; to silence byte-compiler
  (let* ((url-request-method "GET")
         (url-request-extra-headers nil)
         (url-mime-accept-string "application/atom+xml")
         (url (library--arxiv-query-url arxiv-id))
         (buffer (url-retrieve-synchronously url)))
    (unless buffer
      (user-error "Unable to retrieve arXiv metadata for %s" arxiv-id))
    (unwind-protect
        (let ((entry (library--arxiv-response-entry buffer))
              (rate-limited-p (library--arxiv-response-rate-limited-p buffer)))
          (if-let* ((entry entry))
              (library--arxiv-entry-bibtex entry arxiv-id)
            (or (library--bibtex-from-arxiv-id-nasa-ads arxiv-id)
                (library--signal-arxiv-fetch-error arxiv-id rate-limited-p))))
      (kill-buffer buffer))))

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
    (when-let* ((bibtex-entry (library--bibtex-from-arxiv-id arxiv-id)))
      (library--process-pdf-bibtex-with-log filename bibtex-entry arxiv-id))))

;;;###autoload
(defun library-process-arxiv-dired ()
  "Process arxiv files marked in Dired."
  (interactive)
  ;; if no marked files, then deal with current file
  (if-let* ((marked-files (dired-get-marked-files)))
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
    (let* ((id (library--ensure-utf8-encoding id))
           (url (format "https://arxiv.org/pdf/%s.pdf" id))
           (outfile (expand-file-name
                     (format "%s.pdf" id) library-download-directory)))
      (url-copy-file url outfile t)
      (if (file-exists-p outfile)
          (when-let* ((newfile (library-process-arxiv outfile)))
            (find-file newfile))
        (message "File not found.")))))

(provide 'library)
;;; library.el ends here
