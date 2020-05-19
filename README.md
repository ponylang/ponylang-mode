[![MELPA](http://melpa.org/packages/ponylang-mode-badge.svg)](http://melpa.org/#/ponylang-mode)
[![MELPA Stable](http://stable.melpa.org/packages/ponylang-mode-badge.svg)](http://stable.melpa.org/#/ponylang-mode)

# Pony Mode

An Emacs mode that provides syntax highlighting (font-lock) and
indentation for the [Pony](http://www.ponylang.org/) programming
language.

At the moment, ponylang-mode is fairly new and immature. From the
standpoint of indentation, it should work for about 95% of use cases.
In order to work for the rest, it is going to require a rewrite. If
you are interested in contributing to Emacs tooling for Pony, please
get in touch. Contributions and feedback are welcome.

## Installation

This package can be obtain from
[MELPA](http://melpa.org/#/ponylang-mode) or
[MELPA Stable](http://stable.melpa.org/#/ponylang-mode). The `master`
branch is continuously deployed to MELPA, and released versions are
deployed to MELPA Stable.

<kbd>M-x package-install [RET] ponylang-mode [RET]</kbd>

## Configuration

Right now `ponylang-mode` doesn't take a lot of configuration (i.e.
it's too simple to need any). If you want to make sure your
indentation matches that in the `ponyc` project's examples, you can
use something like this:

```elisp
(require 'ponylang-mode)
(setq ponylang-banner 1) ;; 0: None 1: Word (Default) 2: Horse 3: Knight
(define-key ponylang-mode-map (kbd "M-z") 'ponylang-menu)
(define-key ponylang-mode-map (kbd "<f6>")  'ponylang-menu)
```
If you like, you can customize the banner:
```elisp
(setq ponylang-banner "¯\\_(ツ)_/¯")
(setq ponylang-banner "
             _|\\ _/|_,
           ,((\\\\``-\\\\\\\\_
         ,(())      `))\\
       ,(()))       ,_ \\
      ((())'   |        \\
      )))))     >.__     \\
      ((('     /    `-. .c|
              /        `-`'
")
```

If you're using
[`use-package`](https://github.com/jwiegley/use-package) to manage
your configuration, you can configure `ponylang-mode` like so:

```elisp
(use-package ponylang-mode
  :ensure t
  :init
  (setq ponylang-banner 1)
  :config
  :bind-keymap
  ("M-z" . ponylang-menu)
  ("<f6>" . ponylang-menu))
```

## Testing

The tests require
[ERT](https://www.gnu.org/software/emacs/manual/html_node/ert/). To
run them, eval `ponylang-test.el` and run <kbd>M-x ert [RET] t
[RET]</kbd>

## Attribution

Big thanks to Austin Bingham who did the first version of ponylang-mode.
We wouldn't be where we are now without your initial work Austin!
