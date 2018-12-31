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
    (is (= (el-identity 1) "1"))
    (is (= (el-identity true) "t"))
    (is (= (el-identity false) ""))
    (is (= (el-identity 3.14) "3.14"))
    (is (= (el-identity "abc") "abc"))
    (is (= (el-identity {:a 1 :b 2}) "((:a . 1) (:b . 2))"))
    (is (= (el-identity '(1 2 3)) "(1 2 3)"))
    (is (= (el-identity [1 2 3]) "(1 2 3)"))
    (is (= (el-identity [:a "b" 'c]) "(:a b c)"))))
