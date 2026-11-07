;;; -*- lexical-binding: t; -*-
;;; library-agent-shell.el --- Retrieve and file papers using agent-shell

;; Copyright (C) 2026  Paul D. Nelson
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Optional agent-shell integration for library.el.  The commands start a
;; new agent session to retrieve papers or file local PDFs using library.el
;; and its configured paths.  Agent-shell is loaded only when a command is
;; used.

;;; Code:

(require 'find-func)
(require 'library)
(require 'subr-x)

(declare-function agent-shell-select-config "agent-shell" (&rest args))
(declare-function agent-shell-start "agent-shell" (&rest args))
(declare-function agent-shell-insert "agent-shell" (&rest args))
(defvar agent-shell-session-strategy)

(defcustom library-agent-shell-config-function #'agent-shell-select-config
  "Function returning the agent configuration for retrieval and filing.
Called with no arguments after loading agent-shell.  By default, use its
usual agent picker and `agent-shell-preferred-agent-config'.  Set this to
a configuration maker to choose a backend or its session options."
  :type 'function
  :group 'library)

(defvar library-agent-shell-download-history nil
  "Minibuffer history for `library-agent-shell-download-paper'.")

(defun library-agent-shell--start (task)
  "Start a new agent for TASK with the configured library paths."
  (require 'agent-shell)
  (let* ((default-directory (file-name-directory
                             (expand-file-name library-bibtex-file)))
         (agent-shell-session-strategy 'new)
         (config (funcall library-agent-shell-config-function))
         (prompt
          (format
           (concat
            "%s\n\n"
            "Read %s and use its methods through emacsclient in the running "
            "Emacs session, respecting its configured paths and live buffers. "
            "The bibliography is %s and the PDF directory is %s.\n\n"
            "Identify each paper and obtain accurate BibTeX from authoritative "
            "sources. Check for an existing entry by title, DOI, and arXiv ID; "
            "reuse its citation key and preserve existing metadata. Retain "
            "published bibliographic details when known.\n\n"
            "Verify each file is the correct PDF. Use library.el to deposit "
            "the bibliography, name and file the PDF, and create the journal "
            "entry (library-import-pdf). Check destination files before "
            "replacing anything; preserve an existing manuscript when "
            "upgrading to a published PDF.\n\n"
            "Complete the filing, then report each local PDF path, citation "
            "key, version, and any unavailable items.")
           task
           (find-library-name "library")
           (expand-file-name library-bibtex-file)
           (expand-file-name library-pdf-directory))))
    (let ((shell-buffer (agent-shell-start :config config)))
      (with-current-buffer shell-buffer
        (agent-shell-insert :text prompt :submit t :shell-buffer shell-buffer))
      shell-buffer)))

;;;###autoload
(defun library-agent-shell-download-paper (description)
  "Ask a new agent shell to retrieve and file the paper DESCRIPTION.
DESCRIPTION may be a title, citation, DOI, URL, arXiv ID, or local PDF path.
Submit the request immediately and return the new shell buffer.
See `library-agent-shell-config-function' to choose the agent."
  (interactive (list (read-string "Paper to download and file: "
                                  nil 'library-agent-shell-download-history)))
  (when (string-empty-p (string-trim description))
    (user-error "Please describe a paper"))
  (library-agent-shell--start
   (format
    (concat
     "Download and register this paper in my Emacs library:\n\n%s\n\n"
     "Prefer the published PDF when accessible; otherwise use arXiv or an "
     "author manuscript and clearly report which version was obtained. "
     "Do not purchase access. For arXiv, also fetch sources when "
     "library-download-arxiv-include-source is enabled. For a supplied whole "
     "book, register the edited volume separately and extract the requested "
     "chapter by verified printed page numbers; keep its incollection entry "
     "independently usable.")
    description)))

;;;###autoload
(defun library-agent-shell-file-documents (files)
  "Ask a new agent shell to file local PDF FILES in the library.
FILES is a list of file or directory paths.  For a directory, file its
PDFs without visiting subdirectories.  The agent may look up metadata,
but is instructed not to download PDFs or source archives.

Interactively, use Dired's marked files, or the file at point if none are
marked.  Otherwise prompt, starting in `library-download-directory'; a
prefix argument always prompts.  A directory can be selected directly.
Submit the request immediately and return the new shell buffer.
See `library-agent-shell-config-function' to choose the agent."
  (interactive
   (list (if (and (derived-mode-p 'dired-mode) (not current-prefix-arg))
             (dired-get-marked-files)
           (list (read-file-name "PDF or directory to file: "
                                 library-download-directory nil t)))))
  (unless files
    (user-error "Please select PDFs or a directory"))
  (setq files (mapcar #'expand-file-name files))
  (dolist (file files)
    (unless (and (file-readable-p file)
                 (or (file-regular-p file) (file-directory-p file)))
      (user-error "Not a readable file or directory: %s" file)))
  (library-agent-shell--start
   (concat
    "File these already-downloaded documents in my Emacs library:\n\n"
    (mapconcat #'prin1-to-string files "\n")
    "\n\nFor supplied directories, inspect PDF files directly inside each "
    "directory; do not recurse into subdirectories. File only the supplied "
    "local PDFs. Do not download PDFs or source archives. Online lookup for "
    "bibliographic metadata is fine. Report unsupported documents and files "
    "that cannot be identified; leave those files unchanged.")))

(provide 'library-agent-shell)
;;; library-agent-shell.el ends here
