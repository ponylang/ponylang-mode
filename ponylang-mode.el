;;; ponylang-mode --- Language mode for Pony
;;
;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Version: 1
;; URL: https://github.com/abingham/ponylang-mode.el
;; Keywords: programming
;; Package-Requires: ()
;;
;; This file is not part of GNU Emacs.
;;
;; Copyright (c) 2015 Austin Bingham
;;
;;; Commentary:
;;
;; Description:
;;
;; This is a language mode for the Pony actor language
;;
;; For more details, see the project page at
;; https://github.com/abingham/ponylang-mode.el
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

(defvar ponylang-mode-hook nil)

(defvar ponylang-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for Pony major mode")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pony\\'" . ponylang-mode))

;; define several class of keywords
(setq ponylang-keywords '("repeat" "until" "while" "let" "for" "be" "new" "use" "var" "try" "else" "end" "if" "ref" "then" "fun" "tag"))
(setq ponylang-types '("actor" "U32" "U64" "Env" "Range" "F32" "Array" "File" "Options"))
(setq ponylang-constants '("false" "true" "None"))
;(setq ponylang-events '("at_rot_target" "at_target" "attach"))
;(setq ponylang-functions '("llAbs" "llAcos" "llAddToLandBanList" "llAddToLandPassList"))

;; create the regex string for each class of keywords
(setq ponylang-keywords-regexp (regexp-opt ponylang-keywords 'words))
(setq ponylang-type-regexp (regexp-opt ponylang-types 'words))
(setq ponylang-constant-regexp (regexp-opt ponylang-constants 'words))
;(setq ponylang-event-regexp (regexp-opt ponylang-events 'words))
;(setq ponylang-functions-regexp (regexp-opt ponylang-functions 'words))

;; clear memory
(setq ponylang-keywords nil)
(setq ponylang-types nil)
(setq ponylang-constants nil)
;(setq ponylang-events nil)
;(setq ponylang-functions nil)

;; create the list for font-lock.
;; each class of keyword is given a particular face
(setq ponylang-font-lock-keywords
  `(
    (,ponylang-type-regexp . font-lock-type-face)
    (,ponylang-constant-regexp . font-lock-constant-face)
    ;(,ponylang-event-regexp . font-lock-builtin-face)
    ;(,ponylang-functions-regexp . font-lock-function-name-face)
    (,ponylang-keywords-regexp . font-lock-keyword-face)
    ;; note: order above matters. “ponylang-keywords-regexp” goes last because
    ;; otherwise the keyword “state” in the function “state_entry”
    ;; would be highlighted.
    ))

;; Indentation
(defun ponylang-indent-line ()
  "Indent current line as pony code"
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    
    (let ((not-indented t) cur-indent)
    
      (if (looking-at "^[ \t]*end")
	  (progn 
	    (save-excursion
	      (forward-line -1)
	      (setq cur-indent (- (current-indentation) tab-width))

	      (if (< cur-indent 0)
		  (setq cur-indent 0))))

	(save-excursion 
	  (while not-indented
	    (forward-line -1)
	    (if (looking-at "^[ \t]*end")
		(progn
		  (setq cur-indent (current-indentation))
		  (setq not-indented nil))

	      ;; TODO: Construct the proper regexp from
	      ;; ponylang-keywords for this. Or maybe not...perhaps
	      ;; that's boneheaded.
	      (if (looking-at "^[ \t]*\\(repeat\\|until\\|while\\|let\\|for\\|be\\|new\\|use\\|var\\|try\\|else\\|end\\|if\\|ref\\|then\\|fun\\|tag\\)")
		(progn
		  (setq cur-indent (+ (current-indentation) tab-width))
		  (setq not-indented nil))
		(if (bobp)
		    (setq not-indented nil)))))))

      (if cur-indent
	  (indent-line-to cur-indent)
	(indent-line-to 0)))))

;; TODO: Multi-line strings

; This can replace (defun wpdl-mode ()...
(define-derived-mode ponylang-mode fundamental-mode "ponylang-mode"
  "Major mode for editing Pony files."
  (set (make-local-variable 'font-lock-defaults) '(ponylang-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'ponylang-indent-line))

(provide 'ponylang-mode)

;;; ponylang-mode.el ends here
