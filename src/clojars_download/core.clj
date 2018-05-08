(ns clojars-download.core
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]))

(def feed-file "feed.clj")
(def new-feed-file "new-feeds.clj")
(def clones-file "clones.txt")

(defn good-project-clj? [{:keys [scm] :as m}]
  (when-let [url (not-empty (:url scm))]
    (when-let [tag (not-empty (:tag scm))]
      (let [pclj-url (str url "/raw/" tag "/project.clj")]
        (try
          (with-open [pclj (io/reader pclj-url)]
            true)
          (catch java.io.FileNotFoundException _
            false))))))

(defn new-feeds []
  (with-open [f (io/reader feed-file)]
    (let [rs (map read-string (line-seq f))
          projects (->> rs
                        #_(take 10)
                        (filter (fn [{:keys [group-id ^String artifact-id ^String description scm versions] :as m}]
                             (and scm
                                  (string? description)
                                  (not= description "FIXME: write description")
                                  (not (.contains (.toLowerCase description) "deprecated"))
                                  (not (.contains (.toLowerCase description) "clojurescript"))
                                  (not (.contains (.toLowerCase description) "clojure[script]"))
                                  (not (.contains (.toLowerCase description) "cljs"))
                                  (not
                                    (and (or (.contains (.toLowerCase description) "leiningen")
                                             (.contains (.toLowerCase description) "lein")
                                             (.contains (.toLowerCase description) "boot"))
                                         (.contains (.toLowerCase description) "plugin")))
                                  (not= "cljsjs" group-id)
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
                                  (string? (:url scm))
                                  (not= "" (:url scm))
                                  (not= "https://github.com/cljsjs/packages" (:url scm))
                                  (not= "HEAD" (:tag scm))
                                  #_(good-project-clj? m)))))
          projects (vec projects)]
      #_(vec (take 10 projects))
      #_(prn (count projects) "projects")
      (spit new-feed-file
            (with-out-str
              (mapv prn projects)))
      projects
      )))

(defn clone-projects [feeds]
  (let [clone-cmds (->> feeds
                        (keep (fn [{:keys [scm]}]
                                (let [^String url (:url scm)]
                                  (when-let [tag (not-empty (:tag scm))]
                                    (when (string? url)
                                      (let [[match? group artifact] (re-matches #"https://github.com/(\w+)/(\w+)" url)]
                                        (when match?
                                          (let [folder (str group "::" artifact)]
                                            (str "git_clone " url " " folder " " tag))))))))))]
    (spit clones-file
          (with-out-str
            (mapv println clone-cmds)))))

(comment
        (new-feeds)
        (clone-projects (new-feeds))
        )
