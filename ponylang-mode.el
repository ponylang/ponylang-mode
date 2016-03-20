;;; ponylang-mode.el --- Major mode for Pony code
;;
;; Authors: Sean T Allen <sean@monkeysnatchbanana.com>
;; Version: 0.0.2
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
  '("actor"
    "be" "break"
    "class" "compiler_intrinsic" "consume" "continue"
    "do"
    "else" "elseif" "end" "error"
    "for" "fun"
    "if" "in" "interface" "is"
    "let"
    "match"
    "new"
    "object"
    "primitive"
    "recover" "repeat" "return"
    "then" "this" "trait" "try" "type"
    "until" "use"
    "var"
    "where" "while" "with")
  "Pony language keywords.")

(defconst ponylang-indent-start-keywords
  '("actor"
    "be"
    "class"
    "else"
    "for" "fun"
    "if"
    "new"
    "recover" "ref" "repeat"
    "tag" "then" "try"
    "until"
    "while")
  "Pony keywords which indicate a new indentation level.")

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

;(setq ponylang-event-regexp (regexp-opt ponylang-events 'words))
;(setq ponylang-functions-regexp (regexp-opt ponylang-functions 'words))

(defconst ponylang-font-lock-keywords
  `(
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

     ((looking-at "^[[:space:]]*fun\\([[:space:]].*\\)?$")
      (setq cur-indent tab-width))

     ((looking-at "^[[:space:]]*be\\([[:space:]].*\\)?$")
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

;;;###autoload
(define-derived-mode ponylang-mode ponylang-parent-mode "Pony"
  "Major mode for editing Pony files."
  :syntax-table ponylang-mode-syntax-table
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-start-skip) "//+")
  (set (make-local-variable 'font-lock-defaults) '(ponylang-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'ponylang-indent-line))

(provide 'ponylang-mode)

;;; ponylang-mode.el ends here
