(ns clomacs.core-test
  (:use clomacs)
  (:require [clojure.test :refer :all]))

(deftest emacs-connection-test
  (testing "clomacs-httpd-start & clomacs-defn fn test."
    (clomacs-defn emacs-version emacs-version)
    (is
     (let [ev (emacs-version)]
       (and (.contains ev "Emacs")
            (.contains ev "Version"))))))
