;;; library.el --- Helper functions for managing PDFs and BibTeX entries  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
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

(defun library-move-pdf ()
  "Prompt the user for a new name for the current PDF file, and then move it to the unsorted math documents folder."
  (interactive)
  (when (string= (file-name-extension (buffer-file-name)) "pdf")
    (let* ((new-name (read-string "Enter new name (without .pdf): "))
           (new-dir "~/Dropbox/math documents/unsorted/")
           (new-path (concat new-dir new-name ".pdf")))
      (rename-file (buffer-file-name) new-path)
      (find-alternate-file new-path)
      (message "PDF file moved and renamed successfully."))))

(defun library-filename-from-bibtex ()
  "Transform the current bibtex entry into a filename.
The filename is the first two authors' last names separated by a hyphen followed by title of paper converted to alnum no spaces."
  (bibtex-beginning-of-entry)
  (when-let* ((entry (bibtex-parse-entry))
	      (year (bibtex-text-in-field "year" entry))
	      (authors (sultex--remove-braces-accents (bibtex-text-in-field "author" entry)))
	      (title (sultex--remove-braces-accents (bibtex-text-in-field "title" entry))))
    (let* (
	   (lastnames
	    (mapconcat
	     'identity
	     (mapcar
	      (lambda (x)
		(downcase
		 (replace-regexp-in-string "[^a-zA-Z]" ""
					   (car (split-string x ", ")))))
	      (split-string authors " and "))
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
                         (beginning-of-buffer)
                         (search-forward-regexp "{\\([^,]+\\)")
                         (setq name (match-string 1))
                         (if (not name)
                             (error "invalid bib entry in clipboard")))
                       (find-file "~/doit/refs.bib")
                       (beginning-of-buffer)
                       (unless (search-forward name (point-max) t)
                         (end-of-buffer)
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
      (library--capture-journal-entry (library-generate-arxiv-log-entry arxiv-id bibtex-entry newpath)))
  )

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
			      (beginning-of-buffer)
			      (search-forward-regexp "{\\([^,]+\\)")
			      (setq name (match-string 1))
			      (if (not name)
				  (error "invalid bib entry")))
			    (find-file "~/doit/refs.bib")
			    (beginning-of-buffer)
			    (unless (search-forward name (point-max) t)
			      (end-of-buffer)
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
			      (beginning-of-buffer)
			      (search-forward-regexp "{\\([^,]+\\)")
			      (setq name (match-string 1))
			      (if (not name)
				  (error "invalid bib entry")))
			    (find-file "~/doit/refs.bib")
			    (beginning-of-buffer)
			    (unless (search-forward name (point-max) t)
			      (end-of-buffer)
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
  (let* ((url
	  ;;  if arxiv-id contains a dot:
	  (if (string-match "\\." arxiv-id)
	      (format "https://ui.adsabs.harvard.edu/abs/arXiv:%s/exportcitation" arxiv-id)
	    (format "https://ui.adsabs.harvard.edu/abs/arXiv:math%s/exportcitation" (concat "%2F" arxiv-id)))
	  )
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
  ;; Start the capture process
  (org-capture nil "j")
  
  ;; Insert the provided text
  (insert text)
  
  ;; Finalize the capture
  (org-capture-finalize))

(defun library-deal-with-arxiv-file (&optional filename)
  (interactive)
  (unless filename
    (if (derived-mode-p 'dired-mode)
	(setq filename (dired-get-filename))
      (setq filename (buffer-file-name))))
  (when-let* ((arxiv-id (file-name-base filename))
	      (bibtex-entry (library--bibtex-from-arxiv-id arxiv-id)))
    (library--process-pdf-bibtex-with-log filename bibtex-entry arxiv-id)    
    ))

(defun library-deal-with-arxiv-file-manual-id ()
  (interactive)
  (let ((filename
	 (if (derived-mode-p 'dired-mode)
	     (setq filename (dired-get-filename))
	   (setq filename (buffer-file-name))))
	(arxiv-id (read-string "arxiv id: ")))
    (when-let ((bibtex-entry (library--bibtex-from-arxiv-id arxiv-id)))
      (library--process-pdf-bibtex-with-log filename bibtex-entry arxiv-id)    
      )))

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
	   (authors (sultex--remove-braces-accents (bibtex-text-in-field "author" entry)))
	   (title
	    (string-clean-whitespace
	     (sultex--remove-braces-accents (bibtex-text-in-field "title" entry))))
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
       (format ":AUTHORS: %s\n" authors)
       (format ":TITLE: %s\n" title)
       ":END:\n"
       "\n"
       (format "[[%s][Dropbox link]]\n\n" newpath)
       (when arxiv-id
	 (format "[[https://arxiv.org/abs/%s][arXiv link]]\n\n" arxiv-id))))))

(provide 'library)
;;; library.el ends here
