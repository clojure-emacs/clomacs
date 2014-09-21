(defproject clomacs "0.0.2-SNAPSHOT"
  :description "Simplifies emacs lisp interaction with clojure."
  :url "https://github.com/kostafey/clomacs"
  :license {:name "General Public License 3"
            :url "http://www.gnu.org/licenses/gpl-3.0.html"}
  :test-paths ["test/clj"]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 ;; Used for clomacs self-testing.
                 [markdown-clj "0.9.47"]])
