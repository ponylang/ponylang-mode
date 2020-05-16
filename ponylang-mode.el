;;; ponylang-mode.el --- Major mode for Pony code
;;
;; Authors: Sean T Allen <sean@monkeysnatchbanana.com>
;; Version: 0.0.12
;; URL: https://github.com/seantallen/ponylang-mode
;; Keywords: languages programming
;; Package-Requires: ((dash "2.10.0"))
;;
;; This file is not part of GNU Emacs.
;;
;; Copyright (c) 2015 Austin Bingham
;; Copyright (c) 2016 Sean T. Allen
;;
;;; Commentary:
;;
;; Description:
;;
;; This is a language mode for the Pony actor language
;;
;; For more details, see the project page at
;; https://github.com/seantallen/ponylang-mode
;;
;; Installation:
;;
;; The simple way is to use package.el:
;;
;;   M-x package-install ponylang-mode
;;
;; Or, copy ponylang-mode.el to some location in your emacs load
;; path. Then add "(require 'ponylang-mode)" to your emacs initialization
;; (.emacs, init.el, or something).
;;
;; Example config:
;;
;;   (require 'ponylang-mode)
;;
;;; License:
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

(require 'dash)

(defvar ponylang-mode-hook nil)

;; TODO: I don't like having to mention yas-* here, but that's how
;; e.g. python does it. It seems like there should be more general way
;; to detect "repeated tab presses".
(defcustom ponylang-indent-trigger-commands
  '(indent-for-tab-command yas-expand yas/expand)
  "Commands that might trigger a `ponylang-indent-line' call."
  :type '(repeat symbol)
  :group 'ponylang)

(defconst ponylang-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; fontify " using ponylang-keywords

    ;; / is punctuation, but // is a comment starter
    (modify-syntax-entry ?/ ". 124" table)

    ;; /* */ comments, which can be nested
    (modify-syntax-entry ?* ". 23bn" table)

    ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" table)

    ;; Don't treat underscores as whitespace
    (modify-syntax-entry ?_ "w" table)

    table))

(defvar ponylang-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for Pony major mode")

(defvar ponylang--indent-cycle-direction 'left)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pony\\'" . ponylang-mode))

(defconst ponylang-capabilities
  '("box" "iso" "ref" "tag" "trn" "val")
  "Pony capability markers.")

(defconst ponylang-keywords
  '("__loc"
    "actor" "addressof" "and" "as"
    "be" "break"
    "class" "compile_error" "compile_intrinsic" "consume" "continue"
    "digestof" "do"
    "else" "elseif" "embed" "end" "error"
    "for" "fun"
    "if" "ifdef" "iftype" "in" "interface" "is" "isnt"
    "let"
    "match"
    "new" "not"
    "object" "or"
    "primitive"
    "recover" "repeat" "return"
    "struct"
    "then" "this" "trait" "try" "type"
    "until" "use"
    "var"
    "where" "while" "with"
    "xor")
  "Pony language keywords.")

(defconst ponylang-indent-start-keywords
  '("actor"
    "be"
    "class"
    "else"
    "for" "fun"
    "if" "ifdef" "interface"
    "match"
    "new"
    "primitive"
    "recover" "ref" "repeat"
    "struct"
    "tag" "then" "trait" "try"
    "until"
    "while" "with")
  "Pony keywords which indicate a new indentation level.")

(defconst ponylang-declaration-keywords
  '("use"
    "type"
    "class" "actor" "primitive" "struct" "trait" "interface"
    "fun" "be"
    "let" "var" "embed")
  "Pony declaration keywords.")

(defconst ponylang-careful-keywords
  '("continue" "break" "return"
    "new" "object"
    "consume" "recover"
    "_init" "_final"
    "is" "isnt" "as"
    "error" "compile_error" "compile_intrinsic")
  "Pony language careful keywords.")

(defconst ponylang-common-functions
  '("apply" "update"
    "string" "size" "hash")
  "Pony language common functions.")

(defconst ponylang-operator-functions
  '("and" "op_and" "or" "op_or" "xor" "op_xor"
    ;;
    "add" "sub""mul" "div""rem" "mod"
    "shl" "shr"
    "eq" "ne" "lt" "le" "ge" "gt"
    ;;
    "gt_unsafe" "lt_unsafe" "le_unsafe" "ge_unsafe"
    "add_unsafe" "sub_unsafe"
    "mul_unsafe" "div_unsafe"
    "rem_unsafe" "mod_unsafe"
    "shl_unsafe" "shr_unsafe"
    "eq_unsafe" "ne_unsafe"
    ;;
    "add_partial" "sub_partial"
    "mul_partial" "div_partial"
    "rem_partial" "mod_partial")
  "Pony language operators functions.")

(defconst ponylang-constants
  '("false" "true" "None")
  "Common constants.")

;; create the regex string for each class of keywords
(defconst ponylang-keywords-regexp
  (regexp-opt
   (append ponylang-keywords
	   ponylang-capabilities)
   'words)
  "Regular expression for matching keywords.")

(defconst ponylang-constant-regexp
  (regexp-opt ponylang-constants 'words)
  "Regular expression for matching common constants.")

(defconst ponylang-capabilities-regexp
  (regexp-opt ponylang-capabilities 'words)
  "Regular expression for matching capabilities.")

(defconst ponylang-careful-keywords-regexp
  (regexp-opt ponylang-careful-keywords 'words)
  "Regular expression for matching careful keywords.")

(defconst ponylang-declaration-keywords-regexp
  (regexp-opt ponylang-declaration-keywords 'words)
  "Regular expression for matching declaration keywords.")

(defconst ponylang-functions-regexp
  (regexp-opt
   (append ponylang-operator-functions
	   ponylang-common-functions)
   'words)
  "Regular expression for matching functions.")

;(setq ponylang-event-regexp (regexp-opt ponylang-events 'words))
;(setq ponylang-functions-regexp (regexp-opt ponylang-functions 'words))

(defconst ponylang-font-lock-keywords
  `(
    ;; careful
    (,ponylang-careful-keywords-regexp . font-lock-warning-face)

    ;; declaration
    (,ponylang-declaration-keywords-regexp . font-lock-preprocessor-face)

    ;; functions
    (,ponylang-functions-regexp . font-lock-constant-face)

    ;; capabilities
    (,ponylang-capabilities-regexp . font-lock-preprocessor-face)

    ;; capability constraints
    ("#\\(?:read\\|send\\|share\\|any\\|alias\\)" . 'font-lock-builtin-face)

    ;; actor and class definitions
    ("\\(?:actor\\|class\\)\s+\\(?:\\(?:box\\|iso\\|ref\\|tag\\|trn\\|val\\)\s+\\)?\\($?[A-Z_][A-Za-z0-9_]*\\)"
     1
     'font-lock-function-name-face)

    ;; type and primitive definitions
    ("\\(?:type\\|primitive\\)\s+\\($?[A-Z_][A-Za-z0-9_]*\\)"
     1
     'font-lock-function-name-face)

    ;; constructor, method, and behavior definitions
    ("\\(?:new\\|fun\\|be\\)\s+\\(?:\\(?:box\\|iso\\|ref\\|tag\\|trn\\|val\\)\s+\\)?\\($?[a-z_][A-Za-z0-9_]*\\)"
     1
     'font-lock-function-name-face)

    ;; actor, class, and type references
    ("\\(\s\\|[\[]\\|[\(]\\)\\($?_?[A-Z][A-Za-z0-9_]*\\)" 2 'font-lock-type-face)

    ;; ffi
    ("@[A-Za-z_][A-Z-a-z0-9_]+" . 'font-lock-builtin-face)

    ;; constants
    (,ponylang-constant-regexp . font-lock-constant-face)

    ;;(,ponylang-event-regexp . font-lock-builtin-face)
    ;;(,ponylang-functions-regexp . font-lock-function-name-face)

    ;; keywords
    (,ponylang-keywords-regexp . font-lock-keyword-face)

    ("\'\\\\?.\'" . font-lock-string-face)

    ;; note: order above matters. “ponylang-keywords-regexp” goes last because
    ;; otherwise the keyword “state” in the function “state_entry”
    ;; would be highlighted.
    )
  "An alist mapping regexes to font-lock faces.")

;; Indentation
(defun ponylang--looking-at-indent-start ()
  "Determines if the current position is 'looking at' a keyword
  that starts new indentation."
  (-any? (lambda (k) (looking-at (concat  "^[ \t]*" k))) ponylang-indent-start-keywords))

(defun ponylang-syntactic-indent-line ()
  "Indent current line as pony code based on language syntax and
the current context."
  (beginning-of-line)
  (let ((cur-indent (current-indentation)))
    (cond
     ((bobp)
      (setq cur-indent 0))

     ((looking-at "^[[:space:]]*class\\([[:space:]].*\\)?$")
      (setq cur-indent 0))

     ((looking-at "^[[:space:]]*actor\\([[:space:]].*\\)?$")
      (setq cur-indent 0))

     ((looking-at "^[[:space:]]*interface\\([[:space:]].*\\)?$")
      (setq cur-indent 0))

     ((looking-at "^[[:space:]]*primitive\\([[:space:]].*\\)?$")
      (setq cur-indent 0))

     ((looking-at "^[[:space:]]*struct\\([[:space:]].*\\)?$")
      (setq cur-indent 0))

     ((looking-at "^[[:space:]]*trait\\([[:space:]].*\\)?$")
      (setq cur-indent 0))

     ((looking-at "^[[:space:]]*fun\\([[:space:]].*\\)?$")
      (setq cur-indent tab-width))

     ((looking-at "^[[:space:]]*be\\([[:space:]].*\\)?$")
      (setq cur-indent tab-width))

     ((looking-at "^[[:space:]]*=>\\([[:space:]].*\\)?$")
      (setq cur-indent tab-width))

     ((looking-at "^[[:space:]]*new\\([[:space:]].*\\)?$")
      (setq cur-indent tab-width))

     ((looking-at "^[ \t]*\\(end\\|else\\|elseif\\|do\\|then\\|until\\)$")
      (progn
	(save-excursion
	  (forward-line -1)
	  (setq cur-indent (- (current-indentation) tab-width))

	  (if (< cur-indent 0)
	      (setq cur-indent 0)))))

     (t
      (save-excursion
	(let ((keep-looking t))
	  (while keep-looking
	    (setq keep-looking nil)
	    (forward-line -1)
	    (cond
	     ;; if the previous line ends in =, indent one level
	     ((looking-at ".*=[ \t]*$")
	      (setq cur-indent (+ (current-indentation) tab-width)))

	     ((ponylang--looking-at-indent-start)
	      (setq cur-indent (+ (current-indentation) tab-width)))

	     ;; if the previous line is all empty space, keep the current indentation
	     ((not (looking-at "^[ \t]*$"))
	      (setq cur-indent (current-indentation)))

	     ;; if it's the beginning of the buffer, indent to zero
	     ((bobp)
	      (setq cur-indent 0))

	     (t (setq keep-looking t))))))))

    (indent-line-to cur-indent)))

(defun ponylang-cycle-indentation ()
  (if (eq (current-indentation) 0)
      (setq ponylang--indent-cycle-direction 'right))

  (if (eq ponylang--indent-cycle-direction 'left)
      (indent-line-to (max 0 (- (current-indentation) tab-width)))
    (indent-line-to (+ (current-indentation) tab-width))))

(defun ponylang-indent-line ()
  "Indent the current line based either on syntax or repeated use
  of the TAB key."
  (interactive)
  (let ((repeated-indent (memq last-command ponylang-indent-trigger-commands)))
    (if repeated-indent
	(ponylang-cycle-indentation)
      (progn
	(setq ponylang--indent-cycle-direction 'left)
	(ponylang-syntactic-indent-line)))))

(defalias 'ponylang-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

(defun ponylang-stringify-triple-quote ()
  "Put `syntax-table' property on triple-quoted strings."
  (let* ((string-end-pos (point))
         (string-start-pos (- string-end-pos 3))
         (ppss (prog2
                   (backward-char 3)
                   (syntax-ppss)
                 (forward-char 3))))
    (unless (nth 4 (syntax-ppss)) ;; not inside comment
      (if (nth 8 (syntax-ppss))
          ;; We're in a string, so this must be the closing triple-quote.
          ;; Put | on the last " character.
          (put-text-property (1- string-end-pos) string-end-pos
                             'syntax-table (string-to-syntax "|"))
        ;; We're not in a string, so this is the opening triple-quote.
        ;; Put | on the first " character.
        (put-text-property string-start-pos (1+ string-start-pos)
                           'syntax-table (string-to-syntax "|"))))))

(defconst ponylang-syntax-propertize-function
  (syntax-propertize-rules
   ("\"\"\"" ; A triple quoted string
    (0 (ignore (ponylang-stringify-triple-quote))))))

(defun ponylang-root-p (directory)
  (setq-local files '("corral.json" "lock.json" "Makefile" "Dockerfile"
                       ".editorconfig" ".gitignore" "CMakeLists.txt"))
  (setq-local found nil)
  (while (and files
           (not found))
    (let* ((filename (car files))
            (filepath (concat (file-name-as-directory directory) filename)))
      (setq-local files (cdr files))
      (setq-local found (file-exists-p filepath))))
  found)

(defun ponylang-project-root (&optional path)
  (let* ((bufdir (if buffer-file-name
                   (file-name-directory buffer-file-name) default-directory))
          (curdir (if path (file-name-as-directory path) bufdir))
          (parent (file-name-directory (directory-file-name curdir))))
    (if (or (not parent)
          (string= parent curdir)
          (string= parent "/")
          (ponylang-root-p curdir))
      curdir (ponylang-project-root parent))))

(defun ponylang-project-name ()
  (file-name-base (directory-file-name (ponylang-project-root))))

(defun ponylang-project-file-exists-p (filename)
  (file-exists-p (concat (ponylang-project-root) filename)))

(defun ponylang-run-command (command &optional path)
  (setq default-directory (if path path (ponylang-project-root path)))
  (compile command))

(defun ponylang-project-build ()
  (interactive)
  (if (ponylang-project-file-exists-p "corral.json")
    (ponylang-run-command "corral run -- ponyc --debug")
    (if (ponylang-project-file-exists-p "Makefile")
      (ponylang-run-command "make")
      (ponylang-run-command "ponyc"))))

(defun ponylang-project-run ()
  (interactive)
  (let* ((bin1 (concat (ponylang-project-root) "bin/" (ponylang-project-name)))
          (bin2 (concat (ome-buf-dirpath) "/" (ponylang-project-name))))
    (if (ponylang-project-file-exists-p "Makefile")
      (ponylang-run-command bin1)
      (ponylang-run-command bin2))))

(defun ponylang-corral-fetch ()
  (interactive)
  (if (file-exists-p "corral.json")
  (ponylang-run-command "corral update")))

(defun ponylang-corral-update ()
  (interactive)
  (if (file-exists-p "corral.json")
  (ponylang-run-command "corral fetch")))

(easy-menu-define ponylang-mode-menu ponylang-mode-map
  "Menu for Ponylang mode."
  '("Ponylang"
     ["Build" ponylang-project-build t]
     ["Run" ponylang-project-run t]
     "---"
     ("Corral"
       ["Fetch" ponylang-corral-fetch t]
       ["Update" ponylang-corral-update t])))

(defhydra hydra-ponylang-menu
  (:color blue :hint none)
"
 6,dPPYba,   ,adPPYba,   6,dPPYba,  8b       d8
88P'     8a a8       8a 88P'   ` 8a `8b     d8'
88       d8 8b       d8 88       88  `8b   d8'
88b,   ,a8  8a,    ,a8  88       88   `8b,d8'
88`YbbdP '   ` YbbdP '  88       88     Y88'
88                                      d8'
88                                     d8'

  _b_: Build   _r_: Run
  _f_: Fetch   _u_: Update           _q_: Quit
"
  ("b" ponylang-project-build "Build" :color blue)
  ("r" ponylang-project-run "Run" :color green)
  ("f" ponylang-corral-fetch "Fetch" :color pink)
  ("u" ponylang-corral-update "Update" :color pink)
  ("q" nil "Quit"))

(defun ponylang-menu ()
  (interactive)
  (hydra-ponylang-menu/body))

;;;###autoload
(define-derived-mode ponylang-mode ponylang-parent-mode "Pony"
  "Major mode for editing Pony files."
  :syntax-table ponylang-mode-syntax-table
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "//+")
  (setq-local font-lock-defaults '(ponylang-font-lock-keywords))
  (setq-local indent-line-function 'ponylang-indent-line)
  (setq-local syntax-propertize-function ponylang-syntax-propertize-function)
  (setq-local indent-tabs-mode nil)
  (setq-local 'tab-width 2))

(provide 'ponylang-mode)

;;; ponylang-mode.el ends here
