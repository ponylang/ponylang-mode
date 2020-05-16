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

```
(require 'ponylang-mode)
(define-key ponylang-mode-map [f6] 'ponylang-menu)
```

If you're using
[`use-package`](https://github.com/jwiegley/use-package) to manage
your configuration, you can configure `ponylang-mode` like so:

```
(use-package ponylang-mode
  :ensure t
  :bind-keymap
  ([f6] . ponylang-menu))
```

## Testing

The tests require
[ERT](https://www.gnu.org/software/emacs/manual/html_node/ert/). To
run them, eval `ponylang-test.el` and run <kbd>M-x ert [RET] t
[RET]</kbd>

## Attribution

Big thanks to Austin Bingham who did the first version of ponylang-mode.
We wouldn't be where we are now without your initial work Austin!
