(ns clomacs.cm-test
  (:use markdown.core))

(defn my-md-to-html-string
  "Call some function from the dependency."
  [x]
  (md-to-html-string x))

(defn text-with-newlines []
  (str "Some text in the first line \n"
       "and text in the new line. "
       "Text with backslash and \\no new line."))
