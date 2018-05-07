(ns clojars-download.core
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]))

(def feed "feed.clj")

(defn good-project-clj? [{:keys [scm] :as m}]
  (when-let [url (not-empty (:url scm))]
    (when-let [tag (not-empty (:tag scm))]
      (let [pclj-url (str url "/raw/" tag "/project.clj")]
        (try
          (with-open [pclj (io/reader pclj-url)]
            true)
          (catch java.io.FileNotFoundException _
            false))))))

(defn download []
  (with-open [f (io/reader feed)]
    (let [rs (map read-string (line-seq f))
          projects (->> rs
                        #_(take 10)
                        (filter (fn [{:keys [^String artifact-id ^String description scm versions] :as m}]
                             (and scm
                                  (string? description)
                                  (not= description "FIXME: write description")
                                  (not (.contains (.toLowerCase description) "deprecated"))
                                  (not (.contains (.toLowerCase description) "clojurescript"))
                                  (not
                                    (and (or (.contains (.toLowerCase description) "leiningen")
                                             (.contains (.toLowerCase description) "lein")
                                             (.contains (.toLowerCase description) "boot"))
                                         (.contains (.toLowerCase description) "plugin")))
                                  (not (.contains (.toLowerCase artifact-id) "lein-"))
                                  (not (.contains (.toLowerCase artifact-id) "-lein"))
                                  (not (.contains (.toLowerCase artifact-id) "boot-"))
                                  (not (.contains (.toLowerCase artifact-id) "-boot"))
                                  (< 1 (count versions))
                                  (let [^String l (nth versions 0)]
                                    (and (not (.endsWith l "SNAPSHOT"))
                                         (not (.endsWith l "deprecated"))))
                                  #_(some (fn [^String s]
                                          (not (.endsWith s "SNAPSHOT")))
                                        versions)
                                  (not= "" (:url scm))
                                  (not= "HEAD" (:tag scm))
                                  #_(good-project-clj? m)))))
          projects (vec projects)]
      #_(vec (take 10 projects))
      #_(prn (count projects) "projects")
      (spit "new-feeds.clj"
            (with-out-str
              (mapv prn projects)))
      projects
      )))

(comment
        (def fst (first (download)))
        (good-project-clj? fst)
        )
