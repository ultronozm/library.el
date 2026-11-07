;;; -*- lexical-binding: t; -*-
;;; library-agent-shell.el --- Retrieve papers using agent-shell

;; Copyright (C) 2026  Paul D. Nelson
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Optional agent-shell integration for library.el.  The command starts a
;; new agent session with instructions to retrieve and file a paper using
;; library.el and its configured paths.  Agent-shell is loaded only when
;; the command is used.

;;; Code:

(require 'find-func)
(require 'library)
(require 'subr-x)

(declare-function agent-shell-select-config "agent-shell" (&rest args))
(declare-function agent-shell-start "agent-shell" (&rest args))
(declare-function agent-shell-insert "agent-shell" (&rest args))
(defvar agent-shell-session-strategy)

(defcustom library-agent-shell-config-function #'agent-shell-select-config
  "Function returning the agent configuration for paper retrieval.
Called with no arguments after loading agent-shell.  By default, use its
usual agent picker and `agent-shell-preferred-agent-config'.  Set this to
a configuration maker to choose a backend or its session options."
  :type 'function
  :group 'library)

(defvar library-agent-shell-download-history nil
  "Minibuffer history for `library-agent-shell-download-paper'.")

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
  (require 'agent-shell)
  (let* ((default-directory (file-name-directory
                             (expand-file-name library-bibtex-file)))
         (agent-shell-session-strategy 'new)
         (config (funcall library-agent-shell-config-function))
         (prompt
          (format
           (concat
            "Download and register this paper in my Emacs library:\n\n%s\n\n"
            "Read %s and use its methods through emacsclient in the running "
            "Emacs session, respecting its configured paths and live buffers. "
            "The bibliography is %s and the PDF directory is %s.\n\n"
            "Identify the paper and obtain accurate BibTeX from authoritative "
            "sources. Check for an existing entry by title, DOI, and arXiv ID; "
            "reuse its citation key and preserve existing metadata. Prefer the "
            "published PDF when accessible; otherwise use arXiv or an author "
            "manuscript and clearly report which version was obtained. Retain "
            "published bibliographic details when known. Do not purchase access.\n\n"
            "Verify the downloaded file is the correct PDF. Use library.el to "
            "deposit the bibliography, name and file the PDF, and create the "
            "journal entry (library-import-pdf). For arXiv, "
            "also fetch sources when library-download-arxiv-include-source is "
            "enabled. Check destination files before replacing anything; "
            "preserve an existing manuscript when upgrading to a published PDF. "
            "For a supplied whole book, register the edited volume separately "
            "and extract the requested chapter by verified printed page numbers; "
            "keep its incollection entry independently usable.\n\n"
            "Complete the download and registration, then report the local PDF "
            "path, citation key, version, and any unavailable items.")
           description
           (find-library-name "library")
           (expand-file-name library-bibtex-file)
           (expand-file-name library-pdf-directory))))
    (let ((shell-buffer (agent-shell-start :config config)))
      (with-current-buffer shell-buffer
        (agent-shell-insert :text prompt :submit t :shell-buffer shell-buffer))
      shell-buffer)))

(provide 'library-agent-shell)
;;; library-agent-shell.el ends here
