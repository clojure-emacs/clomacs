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

1. nrepl must run:<br/>
   `(clomacs-launch-nrepl)` / `(clomacs-ensure-nrepl-runnig)`
2. clomacs clojure-side code should be loaded:<br/>
   `(clomacs-init)`
3. user's java/clojure custom *.jar libraries should be added to the
   CLASSPATH:<br/>
   `(clomacs-load-project-dependences <custom>)`
4. user's custom clojure code should be added to the CLASSPATH:<br/>
   `(clomacs-load <custom> <clojure-side-file>)`

So, the user of the mixed elisp-clojure lib wants to simple run elisp code
from the <custom> lib.  But at this point we have the unknown state: probably
no one of this terms are supplied, or supplied some of them (or even all of
them).

The purpose of the `clomacs-defun` is to do all of this verification and
loading (if necessary), but just once per library.


## Usage

FIXME

## License

Copyright © 2013 Kostafey <kostafey@gmail.com>

Distributed under the General Public License, version 3.
