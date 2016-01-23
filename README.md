# ponylang-mode: An emacs mode for editing Pony code

*"Pony is an object-oriented, actor-model, capabilities-secure, high
performance programming language."*
*- ponylang.org*

`ponylang-mode` is an Emacs mode for editing Pony code.

At the moment, ponylang-mode is fairly new and immature. From the
standpoint of indentation, it should work for about 95% of use cases.
In order to work for the rest, it is going to require a rewrite.
If you are interested in contributing to Emacs tooling for Pony, please
get in touch. I'm happy to accept contributions and feedback.

Big thanks to Austin Bingham who did the first version of ponylang-mode.
We wouldn't be where we are now without your initial work Austin!

## Installation

You can install `ponylang-mode` via melpa. The fundamental way is with
`package-install`:

```
M-x package-install ponylang-mode
```

Then just make sure you `(require ponylang-mode)` at some point in
your initialization.

## Configuration

Right now `ponylang-mode` doesn't take a lot of configuration
(i.e. it's too simple to need any). If you want to make sure your
indentation matches that in the `ponyc` project's examples, you can
use something like this:

```
(add-hook
  'ponylang-mode-hook
  (lambda ()
    (set-variable 'indent-tabs-mode nil)
    (set-variable 'tab-width 2)))
```

### Using `use-package`

If you're using the excellent
[`use-package`](https://github.com/jwiegley/use-package) to manage
your configuration, you can use something like the following to
install and configure `ponylang-mode`:

```
(use-package ponylang-mode
  :ensure t
  :config
  (progn
    (add-hook
     'ponylang-mode-hook
     (lambda ()
       (set-variable 'indent-tabs-mode nil)
       (set-variable 'tab-width 2)))))
```

## Tests

`ponylang-mode` has an `ert`-based regression test suite. To run it,
evaluate `ponylang-tests.el` and run `M-x ert RET t RET`.
