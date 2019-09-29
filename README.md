[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](https://melpa.org/packages/clomacs-badge.svg)](https://melpa.org/#/clomacs)
[![Melpa Stable](https://stable.melpa.org/packages/clomacs-badge.svg)](https://stable.melpa.org/#/clomacs)
[![CircleCI](https://circleci.com/gh/clojure-emacs/clomacs.svg?style=svg)](https://circleci.com/gh/clojure-emacs/clomacs)
[![Coverage Status](https://coveralls.io/repos/github/clojure-emacs/clomacs/badge.svg?branch=master)](https://coveralls.io/github/clojure-emacs/clomacs?branch=master)

# clomacs

<img src="http://4.bp.blogspot.com/-xkvH6ps4Rk8/Unj5u2sKj0I/AAAAAAAAAHQ/lVi6e2EpnmU/s1600/clomacs.png"
 alt="Clomacs logo" align="right" />

> Emacs is a Lisp. Clojure is a Lisp. Can the two be put together to form the
> ultimate dev environment? "Clomacs" perhaps?
> * from Emacs isn't for everyone discussion by Anonymous Cow.

Clomacs simplifies call Clojure code from Emacs lisp and vice versa. The purpose
is to provide a tool for creating mixed Elisp-Clojure Emacs extensions. It
provides a small wrapper under [CIDER](https://github.com/clojure-emacs/cider)
to reduce repetitive code and uses
[simple-httpd](https://github.com/skeeto/emacs-web-server) to call Elisp from
Clojure via http requests.

## Overview

There are some requirements to run mixed Elisp-Clojure code. All the Elisp-side
code should be loaded, nREPL must run with all related Clojure-side code and
its dependencies.

So, the user of the mixed Elisp-Clojure Emacs extension wants to simple run
Elisp code from the extension.

The purpose of the `clomacs-defun` is to wrap Clojure function in a Elisp
function, that will start CIDER if necessary or use an existing CIDER connection
of certain Elisp-Clojure Emacs extension, call this Clojure function and return
it's result.

To run Elisp code from Clojure, http server on Emacs side should be started
first by `clomacs-httpd-start` function. Then you can straightforwardly pass
Elisp code as string to Emacs for eval - `clomacs-eval` or wrap Elisp to Clojure
function via `clomacs-defn`.

## Installation

### Elisp side

Add [MELPA](https://github.com/melpa/melpa#usage) (if not yet) to your
`package-archives` list.

Then you can install clomacs with the following command:

<kbd>M-x package-install [RET] clomacs [RET]</kbd>

### Clojure side

To install Clomacs, add the following dependency to your `project.clj`
file:

[![Clojars Project](https://clojars.org/clomacs/latest-version.svg)](https://clojars.org/clomacs)

## Usage

### Prerequisites

`clomacs` requires Clojure 1.7+.

### Simple example

Call Clojure from Elisp:
```lisp
;; emacs lisp:
(require 'clomacs)
(clomacs-defun get-property System/getProperty)
(message (get-property "java.version"))
```
Call Elisp from Clojure:
```lisp
;; emacs lisp:
(require 'clomacs)
(clomacs-httpd-start)
```
```clojure
;; clojure:
(use 'clomacs)
(clomacs-defn emacs-version emacs-version)
(println (emacs-version))
```

Here `System/getProperty` is a Clojure function and `get-property` is a wrapped
Elisp function. `emacs-version` is Elisp function and after macros evaluation -
is a wrapped Clojure function.

### Full-fledged example

The full source code for the following example is here:
[cm-test](https://github.com/kostafey/cm-test).

**1.** Create new Clojure project in a common way:

```bash
lein new cm-test
```

**2.** Add markdown-clj dependency to the `project.clj` file, add `src/clj`
  folder to the classpath:

```clojure
(defproject cm-test "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths ["src/clj"]                    ;; add clj folder to the classpath
  :dependencies [[org.clojure/clojure "1.9.0"] ;; Use recent version of Clojure
                 [markdown-clj "0.9.28"]       ;; markdown-clj dependency
                 [clomacs "0.0.3-SNAPSHOT"]])  ;; Most recent version of clomacs
```

**3.** Create `clj` folder in the `src/`.<br/>
**4.** Copy `cm_test` to `src/clj/` folder.<br/>
**5.** Add some code, using markdown lib in the `src/clj/cm_test/core.clj` file:

```Clojure
(ns cm-test.core
  (:use markdown.core))

(defn my-md-to-html-string
  "Call some function from the dependency."
  [x]
  (md-to-html-string x))
```

**6.** Create `elisp` folder in the `src/`.<br/>
**7.** Create `cm-test.el` file in this foder.<br/>
**8.** Add to the `cm-test.el` the following content:

```lisp
(require 'clomacs)
(clomacs-defun cm-test-md-to-html-wrapper
               cm-test.core/my-md-to-html-string
               :lib-name "cm-test"
               :namespace cm-test.core
               :doc "Convert markdown to html via Clojure lib.")

(defun cm-test-mdarkdown-to-html (beg end)
  "Add to the selected markdown text it's html representation."
  (interactive "r")
  (save-excursion
    (if (< (point) (mark))
        (exchange-point-and-mark))
    (insert
     (concat "\n" (cm-test-md-to-html-wrapper
                   (buffer-substring beg end))))))

(provide 'cm-test)
```

Here is the `cm-test/src` path tree vizualization:
* `src`
   * `clj`
      * `cm_test`
         * `core.clj`
   * `elisp`
      * `cm-test.el`

**9.** So, it can be used in your `.emacs` via:

```lisp
(add-to-list 'load-path "~/.emacs.d/cm-test/src/elisp/")
(require 'cm-test)
```

**10.** Then mark (select) this text in one of your buffers: <br>
`# This is a test`<br>
and run `M-x cm-test-mdarkdown-to-html`.

`<h1>This is a test</h1>` should occurs in the buffer under the original text.

## Projects uses clomacs:

* [cm-test](https://github.com/kostafey/cm-test) - Clomacs usage example.
* [ejc-sql](https://github.com/kostafey/ejc-sql) - Emacs SQL client uses
Clojure JDBC.
* [flower](https://github.com/PositiveTechnologies/flower) - Integration with
Github, Gitlab, Atlassian Jira, Microsoft TFS, Microsoft Exchange and Slack.

## Requirements:

* [GNU Emacs](http://www.gnu.org/software/emacs/emacs.html) 26.3+.
* [CIDER](https://github.com/clojure-emacs/cider)
* [s.el](https://github.com/magnars/s.el)
* [simple-httpd](https://github.com/skeeto/emacs-web-server)

## License

Copyright © 2013-2019 Kostafey <kostafey@gmail.com> and
[contributors](https://github.com/clojure-emacs/clomacs/graphs/contributors)

Distributed under the General Public License, version 3.
