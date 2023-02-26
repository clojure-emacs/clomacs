(defproject clomacs "0.0.5-SNAPSHOT"
  :description "Simplifies emacs lisp interaction with clojure."
  :url "https://github.com/clojure-emacs/clomacs"
  :license {:name "General Public License 3"
            :url "http://www.gnu.org/licenses/gpl-3.0.html"}
  :test-paths ["test/clj"]
  :source-paths ["src/clj"]
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [clj-http "3.7.0"]
                 ;; Used for clomacs self-testing.
                 [markdown-clj "0.9.47"]])
