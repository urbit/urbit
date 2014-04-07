;;; hoon-mode.el --- Major mode for editing hoon files for urbit

;; Copyright (C) 2001  Free Software Foundation, Inc.

;; Author: Adam Bliss <abliss@gmail.com>
;; Keywords: extensions

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is my first Major Mode, so don't expect much. It's heavily based on
;; SampleMode from the emacs wiki.

;;; Code:

(defvar hoon-mode-hook nil)

(defvar hoon-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for `hoon-mode'.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hoon$" . hoon-mode))

(defvar hoon-mode-syntax-table
  (let ((st (make-syntax-table lisp-mode-syntax-table)))
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?| "." st)
    (modify-syntax-entry ?; "." st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?: ". 12b" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for `hoon-mode'.")

(defvar hoon-font-lock-keywords
  '(
    ("\\+\\+  \\(\\w+\\)" (1 font-lock-function-name-face))
    ("\\(%\\w+\\)" (1 font-lock-keyword-face))
    ("\\(\\w+\\)=" (1 font-lock-variable-name-face))
    ("[=,]\\(\\w+\\|@\\w*\\)" (1 font-lock-type-face))
    )
  "Keyword highlighting specification for `hoon-mode'.")

(defvar hoon-imenu-generic-expression ".*")

(defvar hoon-outline-regexp ":::")

;;;###autoload
(define-derived-mode hoon-mode fundamental-mode "Hoon"
  "A major mode for editing Hoon files."
  :syntax-table hoon-mode-syntax-table
  (set (make-local-variable 'comment-start) "::")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-use-syntax) nil)
  (set (make-local-variable 'comment-start-skip) "\\(::+\\)\\s *")
  (set (make-local-variable 'font-lock-defaults) '(hoon-font-lock-keywords))
  (set (make-local-variable 'indent-tabs-mode) nil) ;; tabs zutiefst verboten
  (set (make-local-variable 'indent-line-function) 'indent-relative)
  (set (make-local-variable 'imenu-generic-expression)
       hoon-imenu-generic-expression)
  (set (make-local-variable 'outline-regexp) hoon-outline-regexp)
  )

;;; Indentation

(defun hoon-indent-line ()
  "Indent current line of Hoon code."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
        (indent (condition-case nil (max (hoon-calculate-indentation) 0)
                  (error 0))))
    (if savep
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun hoon-calculate-indentation ()
  "Return the column to which the current line should be indented."
  0) ;;TODO


(provide 'hoon-mode)
;;; hoon.el ends here
