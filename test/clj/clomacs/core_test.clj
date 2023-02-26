(ns clomacs.core-test
  (:use clomacs)
  (:require [clojure.test :refer :all]))

(deftest emacs-connection-test
  (testing "clomacs-httpd-start & clomacs-defn fn test."
    (clomacs-defn emacs-version emacs-version)
    (is
     (let [ev (emacs-version)]
       (.contains ev "Emacs")))))

(deftest type-params-handler-test
  (testing "param-handler multimethods test."
    (clomacs-defn el-identity identity)
    (clomacs-defn emacs-major-version clomacs-get-emacs-major-version)
    (is (= (el-identity 1) "1"))
    (is (= (el-identity true) "t"))
    (is (= (el-identity false) ""))
    (is (= (el-identity 3.14) "3.14"))
    (is (= (el-identity "abc") "abc"))
    (is (= (el-identity 'abc) "abc"))
    (is (= (el-identity {:a 1 :b 2}) "((:a . 1) (:b . 2))"))
    (is (= (el-identity '(1 2 3)) "(1 2 3)"))
    (is (= (el-identity (java.util.LinkedList. [1 2 3])) "(1 2 3)"))
    (is (some #{(el-identity #{1 2 3})}
              '("(1 2 3)" "(1 3 2)" "(2 1 3)" "(2 3 1)" "(3 1 2)" "(3 2 1)")))
    (is (= (el-identity [1 2 3]) "[1 2 3]"))
    (is (= (el-identity (java.util.ArrayList. [1 2 3])) "[1 2 3]"))
    (is (= (el-identity (let [ar (make-array Integer/TYPE 3)]
                          (aset ar 0 1)
                          (aset ar 1 2)
                          (aset ar 2 3)
                          ar)) "[1 2 3]"))
    (is (number? (read-string (emacs-major-version))))
    (is (= (el-identity [:a "b" 'c])
           (if (>= (read-string (emacs-major-version)) 27)
             "[:a b 'c]"
             "[:a b (quote c)]")))))
