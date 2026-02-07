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

(require 'ert)

(unless (featurep 'czm-tex-util)
  (provide 'czm-tex-util)
  (defun czm-tex-util-remove-braces-accents (s) s))

(require 'library)

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

(provide 'library-tests)
;;; library-tests.el ends here
