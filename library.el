;;; library.el --- Helper functions for managing PDFs and BibTeX entries  -*- lexical-binding: t; -*-

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

;; Helper functions for managing PDFs and BibTeX entries.
;; This is a work in progress.

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

(defun library-move-pdf ()
  "Move current PDF file to PDF directory, with a new name."
  (interactive)
  (when (string= (file-name-extension (buffer-file-name)) "pdf")
    (let* ((new-name (read-string "Enter new name (without .pdf): "))
           (new-dir library-pdf-directory)
           (new-path (concat new-dir new-name ".pdf")))
      (rename-file (buffer-file-name) new-path)
      (find-alternate-file new-path)
      (message "PDF file moved and renamed successfully."))))

(defun library-filename-from-bibtex ()
  "Transform the current bibtex entry into a filename.
The filename is derived from the publication date, authors' last
names, and the title of the paper."
  (bibtex-beginning-of-entry)
  (when-let* ((entry (bibtex-parse-entry))
	      (year (bibtex-text-in-field "year" entry))
	      (author (czm-tex-util-remove-braces-accents (bibtex-text-in-field "author" entry)))
	      (title (czm-tex-util-remove-braces-accents (bibtex-text-in-field "title" entry))))
    (let* (
           ; this concatenates first and last names when using arxiv
           ; API bibtex entries.  could optimize it to just use last
           ; names, but this is fine for now.
	   (lastnames
	    (mapconcat
	     'identity
	     (mapcar
	      (lambda (x)
		(downcase
		 (replace-regexp-in-string "[^a-zA-Z]" ""
					   (car (split-string x ", ")))))
	      (split-string author "\\band\\b"))
	     "_"))
	   (clean-title
	    (replace-regexp-in-string
	     "-[-]+" "-"
	     (replace-regexp-in-string
	      "[^a-zA-Z0-9-]" ""
	      (replace-regexp-in-string
	       " +" "-"
	       title)))))
      (concat year "_" lastnames "--" clean-title))))

(defun library-manage-pdf-via-clipboard-bibtex ()
  (interactive)
  (let* ((file-name (if (derived-mode-p 'dired-mode)
                        (dired-get-filename)
                      (buffer-file-name)))
         (new-name (save-window-excursion
                     (let ((name))
                       (with-temp-buffer
                         (clipboard-yank)
                         (goto-char (point-min))
                         (search-forward-regexp "{\\([^,]+\\)")
                         (setq name (match-string 1))
                         (if (not name)
                             (error "Invalid bib entry in clipboard")))
                       (find-file "~/doit/refs.bib")
                       (goto-char (point-min))
                       (unless (search-forward name (point-max) t)
                         (goto-char (point-max))
                         (newline-and-indent)
                         (clipboard-yank)
                         (save-buffer))
                       (library-filename-from-bibtex))))
         (new-dir "~/Dropbox/math documents/unsorted/")
         (new-path (concat new-dir new-name ".pdf")))
    (if (derived-mode-p 'dired-mode)
        (dired-rename-file file-name new-path nil)
      (rename-file file-name new-path)
      (find-alternate-file new-path))
    (message "PDF file moved and renamed successfully.")))

(defun library--process-pdf-bibtex-with-log (file-name bibtex-entry &optional arxiv-id)
  (when-let ((newpath
		(library--manage-pdf-via-bibtex file-name bibtex-entry)))
      (library--capture-journal-entry (library-generate-arxiv-log-entry arxiv-id bibtex-entry newpath))))

;;;###autoload
(defun library-manage-pdf-via-clipboard-bibtex ()
  (interactive)
  (let* ((file-name (if (derived-mode-p 'dired-mode)
                        (dired-get-filename)
                      (buffer-file-name)))
	 (bibtex-entry
	  (with-temp-buffer
	    (clipboard-yank)
	    (buffer-substring-no-properties (point-min) (point-max)))))
    (library--process-pdf-bibtex-with-log file-name bibtex-entry)))

(defun library--deposit-bibtex-return-filename (bibtex-entry)
  (save-window-excursion
			  (let ((name))
			    (with-temp-buffer
			      (insert bibtex-entry)
                              (goto-char (point-min))
			      (search-forward-regexp "{\\([^,]+\\)")
			      (setq name (match-string 1))
			      (if (not name)
				  (error "Invalid bib entry")))
			    (find-file "~/doit/refs.bib")
                            (goto-char (point-min))
			    (unless (search-forward name (point-max) t)
                              (goto-char (point-max))
			      (newline-and-indent)
			      (insert bibtex-entry)
			      (save-buffer))
			    (library-filename-from-bibtex))))

(defun library--manage-pdf-via-bibtex (filename bibtex-entry)
  (interactive)
  (when-let* ((new-name (save-window-excursion
			  (let ((name))
			    (with-temp-buffer
			      (insert bibtex-entry)
			      (goto-char (point-min))
			      (search-forward-regexp "{\\([^,]+\\)")
			      (setq name (match-string 1))
			      (if (not name)
				  (error "Invalid bib entry")))
			    (find-file "~/doit/refs.bib")
                            (goto-char (point-min))
			    (unless (search-forward name (point-max) t)
                              (goto-char (point-max))
			      (newline-and-indent)
			      (insert bibtex-entry)
			      (save-buffer))
			    (library-filename-from-bibtex))))
	      (new-dir "~/Dropbox/math documents/unsorted/")
	      (new-path (concat new-dir new-name ".pdf")))
    (dired-rename-file filename new-path t)
    (message "PDF file moved and renamed successfully.")
    new-path))

(defun library--manage-pdf-via-bibtex (filename bibtex-entry)
  (interactive)
  (when-let* ((new-name (library--deposit-bibtex-return-filename bibtex-entry))
	      (new-dir "~/Dropbox/math documents/unsorted/")
	      (new-path (concat new-dir new-name ".pdf")))
    (dired-rename-file filename new-path t)
    (message "PDF file moved and renamed successfully.")
    new-path))


(defun library--bibtex-from-arxiv-id (arxiv-id)
  "Retrieve bibtex entry from arxiv id using arxiv API."
  (interactive "sarxiv id: ")

  (unless (string-match "\\.\\|/" arxiv-id)
    (setq arxiv-id (concat "math/" arxiv-id)))

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
           (authors
            (mapconcat
             (lambda (a) (car (xml-node-children (assoc 'name a))))
             (xml-get-children entry 'author)
             ", "))
           (categories
            (mapconcat
             (lambda (c) (cdr (assoc 'term (xml-node-children c))))
             (xml-get-children entry 'category)
             ", "))
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
  categories               = {%s},
  eprinttype               = {arXiv},
  abstract                 = {%s}
}" year arxiv-id title authors year month id arxiv-id categories (substring summary 0 (- (length summary) 1))))))

(defun library--bibtex-from-arxiv-id-nasa-ads (arxiv-id)
  "Retrieve bibtex entry from arxiv id using NASA ADS."
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
  (org-capture nil "j")
  (insert text)
  (org-capture-finalize))

;;;###autoload
(defun library-deal-with-arxiv-file (&optional filename)
  (interactive)
  (unless filename
    (if (derived-mode-p 'dired-mode)
	(setq filename (dired-get-filename))
      (setq filename (buffer-file-name))))
  (when-let* ((arxiv-id (file-name-base filename))
	      (bibtex-entry (library--bibtex-from-arxiv-id arxiv-id)))
    (library--process-pdf-bibtex-with-log filename bibtex-entry arxiv-id)))

(defun library-deal-with-arxiv-file-manual-id ()
  (interactive)
  (let ((filename
	 (if (derived-mode-p 'dired-mode)
	     (dired-get-filename)
	   (buffer-file-name)))
	(arxiv-id (read-string "arxiv id: ")))
    (when-let ((bibtex-entry (library--bibtex-from-arxiv-id arxiv-id)))
      (library--process-pdf-bibtex-with-log filename bibtex-entry arxiv-id))))


(defun library-deal-with-arxiv-files-dired ()
  (interactive)
  ;; if no marked files, then deal with current file
  (if-let ((marked-files (dired-get-marked-files)))
      (dolist (file marked-files)
	(library-deal-with-arxiv-file file))
    (library-deal-with-arxiv-file)))

(defun library-generate-arxiv-log-entry (arxiv-id bibtex-entry newpath)
  (with-temp-buffer
    (let* ((entry
	    (progn
	      (insert bibtex-entry)
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
       (format "[[%s][Dropbox link]]\n\n" newpath)
       (when arxiv-id
	 (format "[[https://arxiv.org/abs/%s][arXiv link]]\n\n" arxiv-id))))))

;;;###autoload
(defun library-clipboard-to-refs ()
  (interactive)
  (save-window-excursion
    (let ((name))
      (with-temp-buffer
	(clipboard-yank)
	(beginning-of-buffer)
	(search-forward-regexp "{\\([^,]+\\)")
	(setq name (match-string 1))
	(if (not name)
	    (error "invalid bib entry in clipboard")))
      (find-file "~/doit/refs.bib")
      (beginning-of-buffer)
      (if (search-forward name (point-max) t)
	  (error "bib entry already exists"))
      (end-of-buffer)
      (newline-and-indent)
      (clipboard-yank)
      (save-buffer))))


(provide 'library)
;;; library.el ends here
