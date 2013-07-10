# clomacs

Simplifies call clojure code from emacs lisp. The purpose is to provide a tool
for crafting mixed elisp-clojure emacs extensions.

## Overview
There are some requirements to run mixed elisp-clojure code.

### Obviously, all the elisp-side code should be loaded at this point:

1. clomacs elisp code should be loaded:<br/>
   `(require 'clomacs)`
2. custom elisp code should be loaded:<br/>
   `(require '<custom>)`

### The clojure-side code requires the following:

1. nrepl must run.
2. clomacs clojure-side code should be loaded.
3. user's java/clojure custom *.jar libraries should be added to the
   CLASSPATH.
4. user's custom clojure code should be added to the CLASSPATH.
5. user's custom clojure code should be loaded.

So, the user of the mixed elisp-clojure lib wants to simple run elisp code
from the <custom> lib.  But at this point we have the unknown state: probably
no one of this terms are supplied, or supplied some of them (or even all of
them).

The purpose of the `clomacs-defun` is to wrap clojure function to elisp
function, that will do all of this verification and loading (if necessary), but
just once per library, call this clojure function and return it's result.


## Usage

### Simple example

```lisp
;; emacs lisp:
(clomacs-defun get-property System/getProperty)
(message (get-property "java.version"))
```

Here `System/getProperty` is a clojure function and `get-property` is a wrapped
elisp function.

### Full-fledged example

**1.** Create new clojure project in a common way:

```bash
lein new cm-test
```

**2.** Add markdown-clj dependency to the `project.clj` file:

```clojure
(defproject cm-test "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [markdown-clj "0.9.28"]]) ; markdown-clj dependency
```

**3.** Copy create `clj` folder in the `src/`.<br/>
**4.** Copy `cm_test` to `src/clj/` folder.<br/>
**5.** Add some code, using markdown lib in the `src/clj/cm_test/core.clj` file:

```clojure
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
(clomacs-defun cm-test-md-to-html-wrapper
               cm-test.core/my-md-to-html-string
               :lib-name "cm-test"
               :clojure-side-file "cm_test/core.clj"
               :doc "Convert markdown to html via clojure lib.")
               
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

**9.** So, it can be used in your `.emacs` via:

```lisp
(add-to-list 'load-path "~/.emacs.d/cm-test/src/elisp/")
(require 'cm-test)
```

**10.** Then mark (select) this in one of your buffers: <br>
`# This is a test`<br>
and run `M-x cm-test-mdarkdown-to-html`.

`<h1>This is a test</h1>` chould occurs in the buffer under the original text.
<br> Even if nrepl is not running or run with unknown CLASSPATH everything
should works.

## License

Copyright © 2013 Kostafey <kostafey@gmail.com>

Distributed under the General Public License, version 3.
