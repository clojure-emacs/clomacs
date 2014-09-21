(ns clomacs.cm-test
  (:use markdown.core))

(defn my-md-to-html-string
  "Call some function from the dependency."
  [x]
  (md-to-html-string x))

