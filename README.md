[![MELPA](http://melpa.org/packages/ponylang-mode-badge.svg)](http://melpa.org/#/ponylang-mode)
[![MELPA Stable](http://stable.melpa.org/packages/ponylang-mode-badge.svg)](http://stable.melpa.org/#/ponylang-mode)
[![GitHub contributors](https://img.shields.io/github/contributors/ponylang/ponylang-mode)](https://github.com/ponylang/ponylang-mode/graphs/contributors)
[![PR's Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=flat)](https://github.com/ponylang/ponylang-mode/pull/new)
[![Zulip](https://img.shields.io/badge/chat-on%20zulip-52c2af?logo=zulip&logoColor=52c2af&.svg)](https://ponylang.zulipchat.com/#narrow/stream/190367-tooling/topic/Emacs.3A.20ponylang-mode)

# Pony Mode

An Emacs mode that provides `syntax highlighting` (font-lock), `indentation` 
and `code jump` for the [Pony](http://www.ponylang.org/) programming
language.It also provides [Ponyc](https://github.com/ponylang/ponyc),
[Corral](https://github.com/ponylang/corral)
and [Playground](https://playground.ponylang.io) integration.

<!-- At the moment, ponylang-mode is fairly new and immature. From the -->
<!-- standpoint of indentation, it should work for about 99% of use cases. -->
<!-- In order to work for the rest, it is going to require a rewrite. -->
At the moment, `code formatting` is missing.
If you are interested in contributing to Emacs tooling for Pony, please
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
              /        `-`'")
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

Big thanks to [Austin Bingham](https://github.com/abingham) who did the first version of ponylang-mode.
We wouldn't be where we are now without your initial work Austin!

And a huge thanks to [Damon Kwok](https://github.com/damon-kwok) who fixed a number of bugs and added a
number of new features to ponylang-mode. You rock Damon!
