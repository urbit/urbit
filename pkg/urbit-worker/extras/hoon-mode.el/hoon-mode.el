;;; hoon-mode.el --- Major mode for editing hoon files for urbit

;; Copyright (C) 2014–2015 Urbit

;; Author:
;;    * Adam Bliss     https://github.com/abliss          <abliss@gmail.com>
;; Contributors:
;;    * N Gvrnd        https://github.com/ngvrnd
;;    * TJamesCorcoran https://github.com/TJamesCorcoran <jamescorcoran@gmail.com>
;;    * Rastus Vernon  https://github.com/rastus-vernon  <rastus.vernon@protonmail.ch>
;;
;; Keywords: extensions, hoon, nock, urbit, neoreaction, Mars

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
(add-to-list 'auto-mode-alist '("\\.hook$" . hoon-mode))

(defvar hoon-mode-syntax-table
  (let ((st (make-syntax-table lisp-mode-syntax-table)))
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?| "." st)
    (modify-syntax-entry ?\; "." st)
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



;;----------
;; hack the mode line
;;----------

;  In the urbit webserver a directory is basically a resource fork,
;  and contains a single file, always named "hymn.hook". Emacs'
;  default buffer-naming will, of course, name this hymn.hook.
;
;  But if you are visitng two files, 5/hymn.hook and 6/hymn.hook, they
;  will both appear the same on the mode line.
;
;  This sucks, and we'd rather have them appear as "5/hymn.hook" and "6/hymn.hook".
;
;  Trivial, right? No.

; The mode line is an interesting beast.
;   1) it's defined in two variables:
;        * mode-line-format, which includes in turn the variable...
;        * mode-line-buffer-identification
;   2) both of these include "magic" string components which
;      constitute a micro-DSL (domain specific language), which includes tags like
;      '%b', indicating that the buffer-name should be substituted in
;      (see emacs variable docs for 'mode-line-format')
;   3) this magic DSL is evaluated by lisp funcs that are written in C
;      and thus which can not be monkey-patched
;        https://www.gnu.org/software/emacs/manual/html_node/elisp/Primitive-Function-Type.html
;      translation: "do not sharpen chainsaw while it is running"
;   4) the commands that are executed when the DSL is interpreted are likewise written in C
;
; The upshot is...
;
; WAIT. A better way exists. Instead of hacking the mode-line format,
; just invoke 'rename-buffer, which also lives down in the C
; underbelly. Everything falls out nicely.

(defvar hoon-buffer-string "")
(make-variable-buffer-local 'hoon-buffer-string)

(defun hoon-mode-hack-the-modeline ()
  ;; (setq mode-line-format
  ;; 		'("%e"
  ;; 		  mode-line-front-space
  ;; 		  mode-line-mule-info
  ;; 		  mode-line-client
  ;; 		  mode-line-modified
  ;; 		  mode-line-remote
  ;; 		  mode-line-frame-identification
  ;; 		  hoon-buffer-string
  ;; 		  "   "
  ;; 		  mode-line-position
  ;; 		  (vc-mode vc-mode)
  ;; 		  "  "
  ;; 		  mode-line-modes
  ;; 		  mode-line-misc-info
  ;; 		  mode-line-end-spaces))
  ;; (setq hoon-buffer-string
  ;; 		(concat
  ;; 		 (nth 1 (reverse (split-string (file-name-directory (buffer-file-name)) "/")))
  ;; 		 "/"
  ;; 		 (file-name-nondirectory (buffer-file-name))))

  (rename-buffer
		(concat
		 (nth 1 (reverse (split-string (file-name-directory (buffer-file-name)) "/")))
		 "/"
		 (file-name-nondirectory (buffer-file-name))))
)

(add-hook 'hoon-mode-hook 'hoon-mode-hack-the-modeline)



(provide 'hoon-mode)
;;; hoon.el ends here
