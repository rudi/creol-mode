;;; creol-mode.el -- Emacs mode for the programming language Creol
;;;
;;; Copyright (C) 2007 Marcel Kyas <kyas@ifi.uio.no>
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the implied
;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;; PURPOSE.  See the GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; To do:
;;; - Interpreter / Maude integration
;;; - Indentation
;;; - Make M-; use // instead of /* */

(eval-when-compile
  (require 'regexp-opt))

(require 'custom)

(defgroup creol nil
  "Major mode for editing files in the programming language Creol."
  :group 'languages)

(defcustom creol-mode-hook (list 'imenu-add-menubar-index)
  "Hook for customizing `creol-mode'."
  :type 'hook
  :options (list 'imenu-add-menubar-index)
  :group 'creol)

;;; Making faces
(defface creol-keyword-face '((default (:inherit font-lock-keyword-face)))
  "Face for Creol keywords"
  :group 'creol)
(defvar creol-keyword-face 'creol-keyword-face
  "Face for Creol keywords")

(defface creol-constant-face '((default (:inherit font-lock-constant-face)))
  "Face for Creol constants"
  :group 'creol)
(defvar creol-constant-face 'creol-constant-face
  "Face for Creol constants")

(defface creol-builtin-face '((default (:inherit font-lock-builtin-face)))
  "Face for Creol builtins"
  :group 'creol)
(defvar creol-builtin-face 'creol-builtin-face
  "Face for Creol builtins")

(defface creol-function-name-face '((default (:inherit font-lock-function-name-face)))
  "Face for Creol function-names"
  :group 'creol)
(defvar creol-function-name-face 'creol-function-name-face
  "Face for Creol function-names")

(defface creol-type-face '((default (:inherit font-lock-type-face)))
  "Face for Creol types"
  :group 'creol)
(defvar creol-type-face 'creol-type-face
  "Face for Creol types")

(defface creol-variable-name-face '((default (:inherit font-lock-variable-name-face)))
  "Face for Creol variables"
  :group 'creol)
(defvar creol-variable-name-face 'creol-variable-name-face
  "Face for Creol variables")

;;; Font-lock for Creol.
;;;
(defconst creol-keywords
  (eval-when-compile
    (regexp-opt
     '("begin" "by" "case" "class" "contracts"
       "ctor" "datatype" "do" "else" "end" "ensures" "exception"
       "exists" "extern" "forall" "for" "fun" "if" "implements"
       "inherits" "interface" "inv" "in" "of" "op"
       "out" "requires" "some" "then" "to" "try" "var" "when"
       "while" "with" "as") 'words))
  "List of creol keywords.")

(defconst creol-constants
  (eval-when-compile
    (regexp-opt
     '("true" "false" "null" "nil" "now" "caller" "this" "history")
     'words))
  "List of creol special words")

(defconst creol-builtins
  (eval-when-compile
    (regexp-opt
     '("fst" "snd" "head" "tail" "assert" "await" "release" "new" "not" "skip")
     'words))
  "List of creol builtin functions")

;;; Information taken from Lexer.mll
(defconst creol-cid-regexp "\\_<[[:upper:]]\\(?:\\sw\\|\\s_\\)*\\_>")
(defconst creol-id-regexp
  "\\_<\\(?:[[:lower:]]\\|_\\)\\(?:[[:alnum:]]\\|_\\|'\\)*\\_>")

(defvar creol-font-lock-keywords
    (list
     ;; order is important here; earlier entries override later ones
     (cons creol-keywords 'creol-keyword-face)
     (cons creol-constants 'creol-constant-face)
     (cons creol-builtins 'creol-builtin-face)
     (list (concat "op[ \t]\\(" creol-id-regexp "\\)") 1 'creol-function-name-face)
     (cons (concat "\\(" creol-cid-regexp "\\)") 'creol-type-face)
     (list (concat "\\(" creol-id-regexp "\\)[[:space:]]*(") 1 'creol-function-name-face)
     (cons (concat "\\(" creol-id-regexp "\\)") 'creol-variable-name-face)
     (list "\\<\\(# \w+\\)\\>" 1 'font-lock-warning-face t))
    "Creol keywords")

;;; Creol syntax table
(defvar creol-mode-syntax-table (copy-syntax-table)
  "Syntax table for creol-mode")
(modify-syntax-entry ?_ "_" creol-mode-syntax-table)
(modify-syntax-entry ?' "_" creol-mode-syntax-table)
(modify-syntax-entry ?/ ". 124b" creol-mode-syntax-table)
(modify-syntax-entry ?* ". 23" creol-mode-syntax-table)
(modify-syntax-entry ?\n "> b" creol-mode-syntax-table)
(modify-syntax-entry ?\^m "> b" creol-mode-syntax-table)
(modify-syntax-entry ?\" "\"" creol-mode-syntax-table)

;;; Compiling the current buffer.
;;;
(require 'compile)

;;; Put the regular expression for finding error messages here.
;;;
(defconst creol-error-regexp
  "^[^\0-@]+ \"\\(^\"\n]+\\)\", [^\0-@]+ \\([0-9]+\\)[-,:]"
  "Regular expression matching the error messages produced by creolc.")

(if (boundp 'compilation-error-regexp-alist)
    (or (assoc creol-error-regexp compilation-error-regexp-alist)
        (setq compilation-error-regexp-alist
              (cons (list creol-error-regexp 1 2)
		    compilation-error-regexp-alist))))

(defvar creol-imenu-generic-expression
    '(("Interfaces"
       "^[ \t]*interface[ \t\n]+\\(\\b[[:upper:]]\\(?:\\sw\\|\\s_\\)*\\b\\)" 1)
      ("Classes"
       "^[ \t]*class[ \t\n]+\\(\\b[[:upper:]]\\(?:\\sw\\|\\s_\\)*\\b\\)" 1)
      ("Datatypes"
       "^[ \t]*datatype[ \t\n]+\\(\\b[[:upper:]]\\(?:\\sw\\|\\s_\\)*\\b\\)" 1)
      ;; Removed these until I figure out how to group functions and
      ;; methods under their class name
;;       ("Methods"
;;        "^[ \t]*\\(with[ \t]+[[:upper:]]\\(?:\\sw\\|\\s_\\)*[ \t]+\\)?op[ \t\n]+\\(\\sw\\(?:\\sw\\|\\s_\\)*\\)" 2)
;;       ("Functions"
;;        "^[ \t]*fun[ \t\n]+\\(\\sw\\(?:\\sw\\|\\s_\\)*\\)" 1)
      )
  "Imenu expression for creol-mode.  See `imenu-generic-expression'.")


;;; Putting it all together.

(define-derived-mode creol-mode fundamental-mode "Creol"
  "Major mode for editing Creol files.

  The following keys are set:
  \\{creol-mode-map}"
  :group 'creol
  :syntax-table creol-mode-syntax-table
  (define-key creol-mode-map "\C-c\C-c" 'compile)
  (set (make-local-variable 'comment-start) "/*")
  (set (make-local-variable 'comment-end) "*/")
  (set (make-local-variable 'comment-start-skip) "//+\\s-*")
  (let ((filename (file-name-nondirectory (buffer-file-name))))
    (set (make-local-variable 'compile-command)
	 (format "creolc %s -o %s" filename
		(concat (file-name-sans-extension filename) ".maude"))))
  (set (make-local-variable 'font-lock-defaults) '(creol-font-lock-keywords))
  ;; (set (make-local-variable 'indent-line-function) 'creol-indent-line)
  ;; imenu
  (setq imenu-generic-expression creol-imenu-generic-expression)
  ;; speedbar support
  (when (fboundp 'speedbar-add-supported-extension)
    (speedbar-add-supported-extension ".creol")))

(unless (assoc "\\.creol\\'" auto-mode-alist)
  (add-to-list 'auto-mode-alist '("\\.creol\\'" . creol-mode)))

(provide 'creol-mode)
