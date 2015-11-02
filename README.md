# ponylang-mode: An emacs mode for editing Pony code

*"Pony is an object-oriented, actor-model, capabilities-secure, high
performance programming language."*
*- ponylang.org*

`ponylang-mode` is an Emacs mode for editing Pony code.

Right now the mode is very new and immature. This is my first time
writing an editing mode, so it probably has all sorts of issues. Let
me know, provide patches, etc...I'm very happy to accept
contributions and feedback!

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
