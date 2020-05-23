# Change Log

All notable changes to this project will be documented in this file. This project adheres to [Semantic Versioning](http://semver.org/) and [Keep a CHANGELOG](http://keepachangelog.com/).

## [unreleased] - unreleased

### Fixed

- Added end keyword keep indent ([PR #54](https://github.com/ponylang/ponylang-mode/pull/54))
- Improved variable highlighting ([PR #55](https://github.com/ponylang/ponylang-mode/pull/55))
- Improved type highlighting ([PR #56](https://github.com/ponylang/ponylang-mode/pull/56))

### Added

- Added parameter highlighting ([PR #59](https://github.com/ponylang/ponylang-mode/pull/59))
- Added method references highlighting ([PR #60](https://github.com/ponylang/ponylang-mode/pull/60))

### Changed


## [0.1.1] - 2020-05-22

### Fixed

- Fixed melpa recipe format ([PR #52](https://github.com/ponylang/ponylang-mode/pull/52))

## [0.1.0] - 2020-05-21

### Fixed

- Differentiate docstrings from strings ([PR #45](https://github.com/ponylang/ponylang-mode/pull/45))
- Fix this and reference capabilities highlight color ([PR #47](https://github.com/ponylang/ponylang-mode/pull/47))
- Fixed multi-line method indentation ([PR #36](https://github.com/ponylang/ponylang-mode/pull/46))
- Fixed match clause indentation ([PR #36](https://github.com/ponylang/ponylang-mode/pull/46))
- Fixed multi-line type indentation ([PR #36](https://github.com/ponylang/ponylang-mode/pull/46))

### Added

- Compilation Integration ([PR #41](https://github.com/ponylang/ponylang-mode/pull/41))
- Added Hydra support ([PR #42](https://github.com/ponylang/ponylang-mode/pull/42))
- Added some banners to the Hydra menu ([PR #42](https://github.com/ponylang/ponylang-mode/pull/42))
- Better type and constant highlighting ([PR #48](https://github.com/ponylang/ponylang-mode/pull/48))
- Font lock for variable definitions ([PR #49](https://github.com/ponylang/ponylang-mode/pull/49))

## [0.0.12] - 2020-05-10

### Added

- Add missing keywords ([PR #36](https://github.com/ponylang/ponylang-mode/pull/36))

## [0.0.11] - 2018-08-04

### Added

- Nothing. Attempt to get Melpa stable index to show new version.

## [0.0.10] - 2018-08-03

### Fixed

- Problem with triple quoted string handling

## [0.0.9] - 2017-05-14

### Fixed

- Font lock strings and character constants correctly

### Added

-  Add "with" to indenting keywords

## [0.0.8] - 2016-10-08

### Added

- "addressof", "as", "embed", "isnt", "ifdef", "not", and "struct" added as
  highlighted keywords.

## [0.0.7] - 2016-05-04

### Fixed

- Indenting match statements: correct bad "else/end" indentation
- Multiline method indentation

## [0.0.6] - 2016-05-03

### Fixed

- Indenting primitives: force indentation as 0

## [0.0.5] - 2016-04-03

### Fixed

- Indenting interfaces: force indentation as 0
- Indenting traits: force indentation as 0

## [0.0.4] - 2016-04-01

### Fixed

- Constructor indenting: 'new' at beginning of line indents one level.

### Added

- 'lambda' as a highlighted keyword

