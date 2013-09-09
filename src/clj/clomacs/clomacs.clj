;;; clomacs.clj --- Simplifies emacs lisp interaction with clojure.

;; Copyright (C) 2013 Kostafey <kostafey@gmail.com>

;; Author: Kostafey <kostafey@gmail.com>
;; URL: https://github.com/kostafey/clomacs
;; Keywords: clojure, interaction
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

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

(ns clomacs.clomacs
  (:use [clojure.string :only (join split)])
  (:import (java.io StringWriter File)
           (java.net URL URLClassLoader)
           (java.lang.reflect Method)))

(defn parse-artifact [artifact-name]
  "Parse `artifact-name' to list (`group-id' `artifact-id' `version')
Input format, e.g.:
 [org.clojure/clojure \"1.5.1\"]
Ouptut format, e.g.:
 (\"org.clojure\" \"clojure\" \"1.5.1\")"
  (let [group-and-artifact (split (str (first artifact-name)) #"/")
        group-id (first group-and-artifact)
        artifact-id (if (nil? (second group-and-artifact))
                      (first group-and-artifact)
                      (second group-and-artifact))
        version (second artifact-name)]
    (list group-id artifact-id version)))

(defmacro with-artifact [artifact-name & body]
  "Inject `group-id' `artifact-id' `version' local variables to the `body'
scope."
  `(let [artifact# (parse-artifact ~artifact-name)
         ~(symbol "group-id") (nth artifact# 0)
         ~(symbol "artifact-id") (nth artifact# 1)
         ~(symbol "version") (nth artifact# 2)]
     ~@body))

(defn get-path-tail [path]
  (.getName (File. path)))

(defn get-path-parent [path]
  (.getParent (File. path)))

(defn concat-path [& path-list]
  (let [path-cons (fn [& path-list]
                    (loop [acc (File. (first path-list))
                           pl (rest path-list)]
                      (if (empty? pl)
                        acc
                        (recur (File. acc (first pl)) (rest pl)))
                      ))]
    (.getPath (apply path-cons path-list))))

(def is-windows
  "The value is true if it runs under the os Windows."
  (<= 0 (.indexOf (System/getProperty "os.name") "Windows")))

(def is-linux
  "The value is true if it runs under the os Linux."
  (<= 0 (.indexOf (System/getProperty "os.name") "Linux")))

(defn get-m2-path [artifact-name]
  (with-artifact
    artifact-name
    (let [home (if (= (get-path-tail (System/getenv "HOME")) "Application Data")
                 (get-path-parent (System/getenv "HOME"))
                 (System/getenv "HOME"))
          m2 (concat-path home ".m2" "repository")
          sep (if is-windows "\\\\" "/")]
      (concat-path m2
                   (.replaceAll group-id "\\." sep)
                   artifact-id
                   version "/"))))

(defn get-artifact-file-name [artifact-name extension]
  (with-artifact
    artifact-name
    (str artifact-id "-" version "." extension)))

(defn get-jar-location [artifact-name]
  (str (get-m2-path artifact-name)
       (get-artifact-file-name artifact-name "jar")))

(defn add-to-cp "Since add-classpath is deprecated."
  [#^String jarpath] ; path without "file:///..." prefix.
  (let [#^URL url (.. (File. jarpath) toURI toURL)
        url-ldr-cls (. (URLClassLoader. (into-array URL [])) getClass)
        arr-cls (into-array Class [(. url getClass)])
        arr-obj (into-array Object [url])
        #^Method mthd (. url-ldr-cls getDeclaredMethod "addURL" arr-cls)]
    (doto mthd
      (.setAccessible true)
      (.invoke (ClassLoader/getSystemClassLoader) arr-obj))
    (println (format "Added %s to classpath" jarpath))))

(defn print-cp []
  (doseq [url (seq
               (.getURLs (java.lang.ClassLoader/getSystemClassLoader)))]
    (println (.getFile url))))

(defn load-artifact [artifact-name]
  (add-to-cp (get-jar-location artifact-name)))

(do
  ;; (in-ns 'clomacs.clomacs)
  (dorun
   (map load-artifact
        (list
         '[org.clojure/clojure "1.5.1"]
         '[robert/hooke "1.3.0"]
         '[pedantic "0.1.0"]
         '[com.hypirion/io "0.3.1"]
         '[org.clojure/tools.macro "0.1.1"]
         '[useful "0.8.3-alpha8"]
         '[classlojure "0.6.6"]
         '[bultitude "0.2.2"]
         '[leiningen-core "2.3.2"]
         '[org.tcrawley/dynapath "0.2.3"]
         '[org.sonatype.aether/aether-util "1.13.1"]
         '[org.sonatype.aether/aether-spi "1.13.1"]
         '[org.sonatype.aether/aether-impl "1.13.1"]
         '[org.sonatype.sisu/sisu-guice "3.0.3" :classifier "no_aop" :exclusions [[javax.inject] [aopalliance]]]
         '[org.sonatype.sisu/sisu-inject-bean "2.2.3"]
         '[org.sonatype.sisu/sisu-inject-plexus "2.2.3"]
         '[org.codehaus.plexus/plexus-utils "2.0.7"]
         '[org.codehaus.plexus/plexus-classworlds "2.4"]
         '[org.sonatype.aether/aether-connector-wagon "1.13.1"]
         '[org.sonatype.aether/aether-connector-file "1.13.1"]
         '[org.sonatype.aether/aether-api "1.13.1"]
         '[org.codehaus.plexus/plexus-component-annotations "1.5.5" :exclusions [[junit]]]
         '[org.apache.maven/maven-repository-metadata "3.0.4"]
         '[org.apache.maven/maven-model "3.0.4"]
         '[org.codehaus.plexus/plexus-interpolation "1.14"]
         '[org.apache.maven/maven-model-builder "3.0.4"]
         '[org.apache.maven/maven-aether-provider "3.0.4"]
         '[org.apache.maven.wagon/wagon-provider-api "2.2"]
         '[org.jsoup/jsoup "1.6.1"]
         '[commons-logging "1.1.1"]
         '[commons-io "2.0.1"]
         '[org.apache.maven.wagon/wagon-http-shared4 "2.2"]
         '[org.apache.httpcomponents/httpcore "4.1.2"]
         '[commons-codec "1.4"]
         '[org.apache.httpcomponents/httpclient "4.1.2"]
         '[org.apache.maven.wagon/wagon-http "2.2"]
         '[com.cemerick/pomegranate "0.2.0"])))
  (require '[cemerick.pomegranate :as pom])
  (pom/add-dependencies :coordinates '[[leiningen-core "2.3.2"]]
                        :repositories (merge cemerick.pomegranate.aether/maven-central
                                             {"clojars" "http://clojars.org/repo"}))
  (require '[leiningen.core.project :as project]))

(defn load-project-dependences [project-file-path]
  (pom/add-dependencies :coordinates
                        (:dependencies (project/read
                                        project-file-path))
                        :repositories project/default-repositories))

(defn add-source-paths [project-file-path]
  (map add-to-cp
       (:source-paths (project/read
                       project-file-path))))

(comment
  (get-jar-location '[leiningen-core "2.1.3"])
  (load-artifact '[com.cemerick/pomegranate "0.2.0"])

  (print-cp)
  ;; (add-to-cp (.replaceAll (get-jar-location '[org.clojure/clojure-contrib "1.2.0"]) "\\\\" "/"))

  )
