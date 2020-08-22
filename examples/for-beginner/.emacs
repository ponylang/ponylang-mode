(require 'package)
(unless package--initialized (package-initialize))
(setq package-check-signature nil)

;;; If you live in China, (setq live-in-china? t)
(setq live-in-china? nil)

(if live-in-china?
  (setq package-archives ;;
    '(("gnu-tuna" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
       ("org-tuna" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
       ("melpa-tuna" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
  (setq package-archives ;;
    '(("gnu" . "https://elpa.gnu.org/packages/")
       ("melpa" . "https://melpa.org/packages/")
       ("org" . "http://orgmode.org/elpa/"))))

;;; `package-download'
(defun package-download (pkg)
  (when (not (package-installed-p pkg))
    (progn (unless package-archive-contents (package-refresh-contents))
      (package-install pkg))))

;;; `y-or-n-p'
(fset 'yes-or-no-p 'y-or-n-p)

;;; `open-file' (don't in new frame)
(setq ns-pop-up-frames nil)

;;; when file modify with other program: auto update buffer
(global-auto-revert-mode 1)
(setq view-read-only t)

;;; replace selected context
(delete-selection-mode t)

;;; set word wrap
(global-visual-line-mode t)

;;; set line space(pixel)
(setq line-spacing 2)

;;; empty line with file
(setq require-final-newline t)

;;; clipboard X clipboard
(setq select-enable-clipboard t)
(setq x-select-enable-clipboard t)

;;; kill process buffer without confirmation?
(setq kill-buffer-query-functions       ;
  (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;;; file time stamp
(setq time-stamp-active t)
(setq time-stamp-warn-inactive t)

;;; max delete history (undo)
(setq kill-ring-max 200)

;;; i don't make backup file
(setq make-backup-files nil)

;;; auto save
(auto-save-mode 0)

;;; don't create #filename# temp file
(setq auto-save-default nil)

;;; delete file to:Recycle Bin (emacs24)
(setq delete-by-moving-to-trash t)


;;; display line numbers
(if (version<= "26.0.50" emacs-version )
  (progn
    (require 'display-line-numbers)
    (global-display-line-numbers-mode 1))
  (progn
    (setq linum-mode t)
    (setq linum-format "%4d")
    (global-linum-mode 1)))

;;; display column number
(setq column-number-mode t)

;;; `whitespace' settings
(require 'whitespace)
(setq whitespace-style ;;
  '(face spaces tabs newline space-mark tab-mark newline-mark trailing))
;; Make whitespace-mode and whitespace-newline-mode
;; use "¶" for end of line char and "▷" for tab.
(setq whitespace-display-mappings
  ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
  ;; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
  '((space-mark 32 [183]
      [46])
     (newline-mark 10 [182 10])
     (tab-mark 9 [9655 9]
       [92 9])))

;;; `editorconfig' settings
(package-download 'editorconfig)
(require 'editorconfig)

;;; `theme' settings
(package-download 'solarized-theme)
;; (load-theme 'solarized-light t)
(load-theme 'solarized-gruvbox-dark t)

;;; `company' settings
(package-download 'company)
(require 'company)
(package-download 'company-ctags)
(require 'company-ctags)
(with-eval-after-load 'company (company-ctags-auto-setup))

;;; `helm' settings
(package-download 'helm)
(package-download 'helm-xref)
(package-download 'helm-company)
(require 'helm)
(helm-mode 1)
(helm-autoresize-mode 1)
(require 'helm-company)
(require 'helm-xref)

;;; `fci' settings
(package-download 'fill-column-indicator)
(require 'fill-column-indicator)
(setq fci-rule-column 80)
(setq fci-handle-truncate-lines nil)
(setq fci-rule-width 1)
(setq fci-rule-color "grey30")

;;; `hl-todo' settings
(package-download 'hl-todo)
(require 'hl-todo)
(setq hl-todo-keyword-faces ;;
  '(("TODO" . "green")
     ("FIXME" . "yellow")
     ("DEBUG" . "DarkCyan")
     ("GOTCHA" . "red")
     ("STUB" . "DarkGreen")))
(define-key hl-todo-mode-map (kbd "C-c p") #'hl-todo-previous)
(define-key hl-todo-mode-map (kbd "C-c n") #'hl-todo-next)
(define-key hl-todo-mode-map (kbd "C-c o") #'hl-todo-occur)

;;; `nyan-mode'
(package-download 'nyan-mode)
(if (display-graphic-p)
  (progn
    (require 'nyan-mode)
    (nyan-start-animation)
    (nyan-mode 1)))

;;; `ponylang' settings
(package-download 'ponylang-mode)
(require 'ponylang-mode)

(setq ponylang-use-ctags t)
(setq ponylang-format-on-save t)

(define-key ponylang-mode-map (kbd "<f6>")  #'ponylang-menu)
(define-key ponylang-mode-map (kbd "C-c C-f") #'ponylang-format-buffer)

(add-hook 'ponylang-mode-hook ;;
  #'(lambda ()
      (whitespace-mode 1)
      (editorconfig-mode 1)
      (hl-todo-mode 1)
      (fci-mode 1)))
