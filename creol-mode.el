;;; creol-mode.el -- Emacs mode for the programming language Creol
;;;
;;; Copyright (C) 2007 Marcel Kyas <kyas@ifi.uio.no>
;;; Copyright (C) 2008 Rudi Schlatte <rudi@constantly.at>
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
;;; - Better indentation
;;; - Various movement commands (beginning of defun etc)

(eval-when-compile
  (require 'cl))
(require 'compile)
(require 'regexp-opt)
(require 'rx)
(require 'custom)
(require 'newcomment)                   ; for comment-search-forward
(require 'compile)
;; (require 'maude-mode)                   ; for starting Maude

(defgroup creol nil
  "Major mode for editing files in the programming / modelling language Creol."
  :group 'languages)

(defcustom creol-compiler-command "creolc"
  "Path to the creolc executable.  Use \\[creol-next-action] to
compile the current file or load it into Maude.  Note: if a
makefile is present in the current buffer's directory, make will
be called instead."
  :type 'file
  :group 'creol)

(defcustom creol-indent standard-indent
  "Width of indentation step for creol code."
  :type 'integer
  :group 'creol)

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
     (list (concat "op[ \t]+\\(" creol-id-regexp "\\)") 1 'creol-function-name-face)
     (cons (concat "\\(" creol-cid-regexp "\\)") 'creol-type-face)
     (list (concat "\\(" creol-id-regexp "\\)[[:space:]]*(") 1 'creol-function-name-face)
     (cons (concat "\\(" creol-id-regexp "\\)") 'creol-variable-name-face)
     (list "\\<\\(# \w+\\)\\>" 1 'font-lock-warning-face t))
    "Creol keywords")

;;; Creol syntax table
(defvar creol-mode-syntax-table (copy-syntax-table)
  "Syntax table for creol-mode")
(modify-syntax-entry ?_ "_"      creol-mode-syntax-table)
(modify-syntax-entry ?\\ "\\"    creol-mode-syntax-table)
(modify-syntax-entry ?+  "."     creol-mode-syntax-table)
(modify-syntax-entry ?-  "."     creol-mode-syntax-table)
(modify-syntax-entry ?=  "."     creol-mode-syntax-table)
(modify-syntax-entry ?%  "."     creol-mode-syntax-table)
(modify-syntax-entry ?<  "."     creol-mode-syntax-table)
(modify-syntax-entry ?>  "."     creol-mode-syntax-table)
(modify-syntax-entry ?&  "."     creol-mode-syntax-table)
(modify-syntax-entry ?|  "."     creol-mode-syntax-table)
(modify-syntax-entry ?' "_"      creol-mode-syntax-table)
(modify-syntax-entry ?/ ". 124b" creol-mode-syntax-table)
(modify-syntax-entry ?* ". 23"   creol-mode-syntax-table)
(modify-syntax-entry ?\n "> b"   creol-mode-syntax-table)
(modify-syntax-entry ?\^m "> b"  creol-mode-syntax-table)
(modify-syntax-entry ?\" "\""    creol-mode-syntax-table)

;;; Compiling the current buffer.
;;;
(defvar maude-file nil
  "The Maude file that will be loaded by \\[creol-next-action].
Defaults to the buffer filename with a .maude extension.")
(put 'maude-file 'safe-local-variable 'stringp)

;;; Put the regular expression for finding error messages here.
;;;
(defconst creol-error-regexp
  "^[^\0-@]+ \"\\(^\"\n]+\\)\", [^\0-@]+ \\([0-9]+\\)[-,:]"
  "Regular expression matching the error messages produced by creolc.")

(when (and (boundp 'compilation-error-regexp-alist)
           (not (assoc creol-error-regexp compilation-error-regexp-alist)))
  (setq compilation-error-regexp-alist
        (cons (list creol-error-regexp 1 2)
              compilation-error-regexp-alist)))

(defun creol-file-date (file)
  (nth 5 (file-attributes file)))

(defun creol-file-date-< (d1 d2)
  "Compare two file dates, as returned by `file-attributes'."
  (or (and (= (first d1) (first d2))
           (< (second d1) (second d2)))
      (< (first d1) (first d2))))

(defun creol-compile-command ()
  (let ((filename (file-name-nondirectory (buffer-file-name)))
        (makefilep (file-exists-p "Makefile")))
    (if makefilep
        compile-command
      (format "%s %s -o %s" creol-compiler-command filename maude-file))))

(defun creol-absolute-maude-filename ()
  (if (file-name-absolute-p maude-file)
      maude-file
    (concat (file-name-directory (buffer-file-name)) maude-file)))

(defun creol-next-action ()
  "Compile the buffer or load it into Maude.
`creol-next-action' assumes that the result of compilation is
stored in a file with the same name as the current buffer but
with a `.maude' extension.  Set the variable `maude-file'
to change this behavior."
  (interactive)
  (let* ((abs-maude-file (creol-absolute-maude-filename))
         (creol-modtime (nth 5 (file-attributes (buffer-file-name))))
         (maude-modtime (nth 5 (file-attributes abs-maude-file))))
    (if (or (not maude-modtime)
            (creol-file-date-< maude-modtime creol-modtime)
            (buffer-modified-p))
        (call-interactively 'compile compile-command)
      (run-maude)
      (message "Maude loading %s" abs-maude-file)
      (comint-send-string inferior-maude-buffer
                          (concat "in " abs-maude-file "\n")))))

(defvar creol-imenu-generic-expression
    '(("Interfaces"
       "^[ \t]*interface[ \t\n]+\\(\\b[[:upper:]]\\(?:\\sw\\|\\s_\\)*\\b\\)" 1)
      ("Classes"
       "^[ \t]*class[ \t\n]+\\(\\b[[:upper:]]\\(?:\\sw\\|\\s_\\)*\\b\\)" 1)
      ("Datatypes"
       "^[ \t]*datatype[ \t\n]+\\(\\b[[:upper:]]\\(?:\\sw\\|\\s_\\)*\\b\\)" 1))
  "Imenu expression for creol-mode.  See `imenu-generic-expression'.")

;;; Indentation

(defvar creol-module-begin-re
    (rx (and (or "interface" "class") blank))
  "Regex of beginning of class / interface.")

(defvar creol-op-begin-re
    (rx (and (opt "with" (1+ blank) (1+ (any word)) (1+ blank))
             "op" (1+ blank) (1+ (any word))))
  "Regex of beginning of method.")

(defvar creol-with-begin-re
    (rx (and "with" (1+ blank) (1+ (any word))))
  "Regex of \"with ...\" co-interface declaration.")

(defconst creol-infix-function-re
  (rx
   ;; List of operators extracted from creol.texi.  We try not to
   ;; match /* and //
   (and (or ":=" "&&" "/\\" "||" "\\/" "^" "<=>" "=>" "=" "/=" "<=" "<" ">" ">="
	    "+" "-" "*" "**" "/" "%" "-|" "|-" "|-|" "\\"
            (and word-start "in" word-end)
             ","                        ; multi-line multi-assignment
             )
	(or (any blank) line-end)))
  "Regular expression matching functions that affect indentation
of continued expressions.")

(defconst creol-block-starter-re
  (rx (and word-start (or "then" "begin" "do" "else") word-end))
  "Regular expression matching keywords that start a block without braces.")

(defsubst creol-same-line-p (pos)
  "Return t if POS is on the same line as current point."
  (and (>= pos (point-at-bol))
       (<= pos (point-at-eol))))

(defsubst creol-previous-line-sans-comment ()
  "Moves point to previous line that is not entirely a comment.
Leaves point at end of line or before the start of comment."
  (move-beginning-of-line 1)
  (forward-comment (- (buffer-size))))

(defun creol-move-end-of-line-sans-comment ()
  "Moves point to end of line or to start of comment."
  (move-end-of-line 1)
  (let ((line-end-sans-comment (comment-beginning)))
    (when line-end-sans-comment (goto-char line-end-sans-comment))))

(defsubst creol-inside-string-or-comment-p ()
  (let ((state (save-excursion (syntax-ppss))))
    (or (nth 3 state) (nth 4 state))))

(defun creol-line-continues-expression-p ()
  "Returns non-nil if the current line continues an expression."
  (save-excursion
    (back-to-indentation)
    (or (looking-at creol-infix-function-re)
	(progn
          (creol-previous-line-sans-comment)
          ;; hack: make creol-infix-function-re match (except with
          ;; comment-start, argh)
          (unless (eolp) (forward-char 1))
          (looking-back creol-infix-function-re (point-at-bol))))))

(defun creol-previous-line-continues-expression-p ()
  "Returns non-nil if the previous line (disregarding comments)
continues an expression.  Special-cases operations, since an
operation definition is an expression, yet we still want to
indent its body."
  (save-excursion
    (creol-previous-line-sans-comment)
    (and (creol-line-continues-expression-p)
         (not (progn (creol-previous-line-sans-comment)
                     (creol-line-start-of-operation-p))))))

(defun creol-line-start-of-braceless-block-p (point)
  "Returns non-nil if line containing `point' starts the body of
a control statement without braces."
  (save-excursion
    (goto-char point)
    (creol-move-end-of-line-sans-comment)
    (looking-back creol-block-starter-re)))

(defun creol-line-end-of-braceless-block-p ()
  "Returns non-nil if the current line is the end of a control
statement without braces."
  (save-excursion
    (creol-move-end-of-line-sans-comment)
    (and (looking-back (rx (and (or "else" (and "end" (opt ";"))) (0+ blank))))
         (not (progn (back-to-indentation)
                     (looking-at (rx (and "if" word-end))))))))

(defun creol-line-start-of-operation-p ()
  (save-excursion
    (back-to-indentation)
    (looking-at creol-op-begin-re)))

(defun creol-offset-relative-to (prev-line-point)
  "Calculate amount of indent steps of current line from line
containing `prev-line-point'.  Will only get called if
indentation depth can't be calculated by level of parenthesis
depth between lines."
  (let ((offset 0))
    (when (or (creol-line-start-of-braceless-block-p prev-line-point)
              (and (creol-line-continues-expression-p)
                   (not (creol-previous-line-continues-expression-p))))
      (incf offset))
    (when (or (creol-line-end-of-braceless-block-p)
              (and (not (creol-line-continues-expression-p))
                   (creol-previous-line-continues-expression-p)))
      (decf offset)) 
    offset))

;;; stolen from js2-indent.el
(defun creol-calculate-comment-indentation (parse-status)
  "Indent a multi-line block comment continuation line."
  (let* ((beg (nth 8 parse-status))
         (first-line-p (creol-same-line-p beg))
         (offset (save-excursion
                   (goto-char beg)
                   (if (looking-at "/\\*")
                       (+ 1 (current-column))
                     0))))
    (if first-line-p (current-indentation) offset)))

(defun creol-calculate-bracket-indentation (parse-status)
  (let ((bracket-depth (nth 0 parse-status))
        (bracket-indent (save-excursion
                          (goto-char (nth 1 parse-status))
                          (current-indentation))))
    (save-excursion
      (back-to-indentation)
      (cond ((looking-at (rx (syntax close-parenthesis)))
             (backward-up-list bracket-depth)
             (current-indentation))
            (t (+ bracket-indent (* bracket-depth creol-indent)))))))

(defun creol-calculate-op-indentation ()
  (let ((point (point)))
    ;; If op comes after a with, it's within the with's scope and gets
    ;; another indentation level.
    (if (save-excursion
          (creol-beginning-of-class)
          (back-to-indentation)
          (re-search-forward creol-with-begin-re point t))
        (* 2 creol-indent)
      creol-indent)))

(defun creol-calculate-indentation ()
  (let* ((inhibit-point-motion-hooks t)
	 ;; Remember to use parse-partial-sexp instad if we need #2 or
	 ;; #6 of parse-status!
	 (parse-status (save-excursion (syntax-ppss (point-at-bol))))
         (parse-status-prev-line (save-excursion
                                   (creol-previous-line-sans-comment)
                                   (syntax-ppss (point-at-bol))))
         (want-brace-indent (and (> (nth 0 parse-status) 0)
                                 (/= (nth 0 parse-status)
                                     (nth 0 parse-status-prev-line)))))
    (save-excursion
      (back-to-indentation)
      (cond ((nth 4 parse-status)
             (creol-calculate-comment-indentation parse-status))
            ((looking-at creol-module-begin-re)
             0)
            ((looking-at creol-with-begin-re)
             creol-indent)
            ((creol-line-start-of-operation-p)
             (creol-calculate-op-indentation))
            (want-brace-indent
             (creol-calculate-bracket-indentation parse-status))
            (t 
             (let* ((prev-line (save-excursion
                                 (if (> (nth 0 parse-status-prev-line) 0)
                                     (goto-char (nth 1 parse-status-prev-line))
                                   (creol-previous-line-sans-comment))
                                 (point-at-bol)))
                    (prev-line-indent (save-excursion
                                        (goto-char prev-line)
                                        (current-indentation)))
                    (prev-line-offset (creol-offset-relative-to prev-line)))
               (max 0
                    (+ prev-line-indent
                       (* prev-line-offset creol-indent)))))))))

(defun creol-indent-line ()
  "Indent current line as creol code. Currently only somewhat functional.
Uses the variable `creol-indent'."
  (interactive)
  (let ((save-point-position (> (current-column) (current-indentation)))
	(indentation (creol-calculate-indentation)))
    (if save-point-position
	(save-excursion (indent-line-to indentation))
      (indent-line-to indentation))))

;;; Movement

(defun creol-beginning-of-class ()
  "Move backward to the beginning of the current class or interface."
  (interactive)
  (catch 'found
    (while (re-search-backward creol-module-begin-re nil 'move)
      (unless (creol-inside-string-or-comment-p)
	(throw 'found t))))
  (move-beginning-of-line nil))

(defun creol-end-of-class ()
  "Move forward to the end of the current class or interface."
  (interactive)
  (when (looking-at (rx (or whitespace line-end)))
    (forward-sexp 1))
  (unless (looking-at creol-module-begin-re)
    (creol-beginning-of-class))
  (let ((nest 0)
	(seen-inside nil)
	(reg (rx (or (group (and word-start (or "if" "begin") word-end))
		     (group (and word-start "end" word-end))))))
    (while (and (or (not seen-inside) (/= nest 0))
		(re-search-forward reg nil 'move))
      (cond ((creol-inside-string-or-comment-p)
	     nil)
	    ((match-end 1)
	     (incf nest))
	    ((match-end 2)
	     (setf seen-inside t)
	     (decf nest)))))
  (forward-line 1))

;;; Putting it all together.

(define-derived-mode creol-mode fundamental-mode "Creol"
  "Major mode for editing Creol files.

Use \\[creol-next-action] to compile the current buffer and
evaluate the compiled program in Maude.  Running Maude needs
maude-mode to be loaded as well.  Set the buffer-local variable
`maude-file' if the output of compilation is not the current
buffer name with a .maude extension, e.g. when compiling via
make.

The following keys are set:
\\{creol-mode-map}"
  :group 'creol
  :syntax-table creol-mode-syntax-table
  (define-key creol-mode-map "\C-c\C-c" 'creol-next-action)
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "//+\\s-*")
  (set (make-local-variable 'font-lock-defaults) '(creol-font-lock-keywords))
  (set (make-local-variable 'compile-command) (creol-compile-command))
  ;; set maude-file in the Local Variables: section of the file
  ;; if it's different from the buffer's filename, e.g. when using a
  ;; Makefile
  (set (make-local-variable 'maude-file)
       (concat (file-name-sans-extension (buffer-file-name)) ".maude"))
  ;; Movement
  (set (make-local-variable 'beginning-of-defun-function)
       'creol-beginning-of-class)
  (set (make-local-variable 'end-of-defun-function) 'creol-end-of-class)
  ;; Indentation
  (set (make-local-variable 'indent-line-function) 'creol-indent-line)
  ;; imenu
  (setq imenu-generic-expression creol-imenu-generic-expression)
  ;; speedbar support
  (when (fboundp 'speedbar-add-supported-extension)
    (speedbar-add-supported-extension ".creol")))

(unless (assoc "\\.creol\\'" auto-mode-alist)
  (add-to-list 'auto-mode-alist '("\\.creol\\'" . creol-mode)))

(provide 'creol-mode)
