;;; -*- lexical-binding: t; -*-
;;; library-agent-shell-tests.el --- Tests for paper retrieval requests

;; Copyright (C) 2026  Paul D. Nelson
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ert)

;; The integration does not use czm-tex-util; avoid its unrelated dependencies.
(unless (featurep 'czm-tex-util)
  (provide 'czm-tex-util))
(defvar agent-shell-session-strategy 'prompt)
(require 'library-agent-shell)

(ert-deftest library-agent-shell-download-paper-submits-new-session ()
  (let* ((require-function (symbol-function 'require))
         (library-bibtex-file "~/papers/references.bib")
         (library-pdf-directory "~/papers/PDFs/")
         (library-agent-shell-config-function #'agent-shell-select-config)
         (agent-shell-session-strategy 'prompt)
         (shell-buffer (generate-new-buffer " *library agent test*"))
         (config '((:identifier . test-agent)))
         loaded selected started submitted)
    (unwind-protect
        (cl-letf (((symbol-function 'require)
                   (lambda (feature &optional filename noerror)
                     (if (eq feature 'agent-shell)
                         (progn (setq loaded t) feature)
                       (funcall require-function feature filename noerror))))
                  ((symbol-function 'agent-shell-select-config)
                   (lambda ()
                     (should loaded)
                     (setq selected t)
                     config))
                  ((symbol-function 'agent-shell-start)
                   (lambda (&rest args)
                     (should (eq agent-shell-session-strategy 'new))
                     (should (equal default-directory
                                    (expand-file-name "~/papers/")))
                     (should (equal args (list :config config)))
                     (setq started t)
                     shell-buffer))
                  ((symbol-function 'agent-shell-insert)
                   (lambda (&rest args)
                     (should (eq (current-buffer) shell-buffer))
                     (should (eq (plist-get args :shell-buffer) shell-buffer))
                     (should (plist-get args :submit))
                     (setq submitted (plist-get args :text)))))
          (should (eq (library-agent-shell-download-paper "arXiv:0911.2274")
                      shell-buffer))
          (should selected)
          (should started)
          (dolist (text (list "arXiv:0911.2274"
                              (find-library-name "library")
                              (expand-file-name library-bibtex-file)
                              (expand-file-name library-pdf-directory)
                              "library-import-pdf"
                              "preserve existing metadata"
                              "preserve an existing manuscript"
                              "verified printed page numbers"))
            (should (string-match-p (regexp-quote text) submitted)))
          (should (eq agent-shell-session-strategy 'prompt)))
      (kill-buffer shell-buffer))))

(ert-deftest library-agent-shell-download-paper-refuses-blank-description ()
  (let ((library-agent-shell-config-function
         (lambda () (ert-fail "Should not choose an agent"))))
    (should-error (library-agent-shell-download-paper " \t\n")
                  :type 'user-error)))

(provide 'library-agent-shell-tests)
;;; library-agent-shell-tests.el ends here
