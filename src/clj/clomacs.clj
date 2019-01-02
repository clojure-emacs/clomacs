;;; clomacs.clj --- Simplifies call Emacs Lisp from Clojure.

;; Copyright (C) 2017-2019 Kostafey <kostafey@gmail.com>

;; Author: Kostafey <kostafey@gmail.com>
;; URL: https://github.com/clojure-emacs/clomacs
;; Keywords: emacs-lisp, clojure, interaction

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `clomacs-eval` - pass Elisp code as string to Emacs for eval.
;; `clomacs-defn` - core Elisp to Clojure function wrapper.
;;
;; See README.md for detailed description.

;;; Code:

(ns clomacs
  (:require [clj-http.client :as client]))

(def emacs-connection
  "Keep Emacs httpd server connection information (host and port)."
  (atom {}))

(defn set-emacs-connection [host port]
  "Set Emacs httpd server connection information.
Actual data passed when Elisp `clomacs-httpd-start` is called."
  (reset! emacs-connection {:host host :port port}))

(defn get-emacs-connection []
  "Get Emacs httpd server connection information.
Used for check if connection data in Clojure side is ok."
  @emacs-connection)

(defn close-emacs-connection []
  "Clear Emacs httpd server connection information.
Called by Elisp `clomacs-httpd-stop`."
  (reset! emacs-connection {}))

(defn clomacs-eval [fname elisp]
  "Send `elisp` string to eval it in Emacs.
Return evaluation result as string.
If connection data is empty - return nil."
  (when (not (empty? @emacs-connection))
    (try
      (:body
       (client/post
        (format "http://%s:%s/execute"
                (:host @emacs-connection)
                (:port @emacs-connection))
        {:form-params {:elisp elisp
                       :fname fname}}))
      (catch org.apache.http.NoHttpResponseException e nil))))

(defmulti param-handler (fn [acc param] (class param)))

(defmethod param-handler java.lang.String [acc param]
  "Wrap Clojure string with quotes for concatenation."
  (.append acc "\"")
  (.append acc param)
  (.append acc "\""))

(defmethod param-handler java.util.Map [acc param]
  "Convert Clojure map to Elisp alist."
  (.append acc "'(")
  (mapv (fn [[k v]]
          (.append acc "(")
          (param-handler acc k)
          (.append acc " . ")
          (param-handler acc v)
          (.append acc ")"))
        param)
  (.append acc ")"))

(defmethod param-handler java.util.List [acc param]
  "Convert Clojure list or vector to Elisp list."
  (.append acc "'(")
  (mapv (fn [v] (param-handler acc v) (.append acc " ")) param)
  (.append acc ")"))

(defmethod param-handler java.lang.Boolean [acc param]
  "Convert Clojure boolean to Elisp boolean."
  (.append acc (if param "t" "nil")))

(defmethod param-handler nil [acc param]
  "Convert Clojure nil to Elisp nil."
  (.append acc "nil"))

(defmethod param-handler :default [acc param]
  "Use .toString call for param in other cases."
  (.append acc param))

(defmacro clomacs-defn [cl-func-name
                        el-func-name &
                        {:keys [doc
                                result-handler]
                         :or {doc ""
                              result-handler identity}}]
  "Wrap `el-func-name`, evaluated on Emacs side by `cl-func-name`.
`doc` - optional clojure function docstring.
`result-handler` - function called with result of Elisp returned
value as parameter, it returns the result of whapped function."
  `(defn ~cl-func-name [& params#]
     ~doc
     (~result-handler
      (clomacs-eval
       ~cl-func-name
       (format "(%s%s)"
               (str '~el-func-name)
               (loop [rest-params# params#
                      acc# (new StringBuffer "")]
                 (let [param# (first rest-params#)]
                   (if (empty? rest-params#)
                     (str acc#)
                     (recur (next rest-params#)
                            (param-handler (.append acc# " ") param#))))))))))
