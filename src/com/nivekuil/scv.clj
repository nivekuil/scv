(ns com.nivekuil.scv
  (:require [charred.api :as json]
            [hcl.core :as hcl]
            [clojure.string :as str]
            [toml.core :as toml]
            [meander.epsilon :as mean]))

(defn kw->str [k]
  (if (keyword? k) (name k) k))

(defn single-quote [s]
  (format "'%s'" (str/escape s  {\' "\\'"})))
(defn flags
  {:style/indent 1}
  [prefix & pairs]
  (when (odd? (count pairs))
    (throw (ex-info "Flags must be even" {:pairs pairs})))
  (->> (partition 2 pairs)
       (map (fn [[k v]] (format "%s%s=%s" prefix
                              (kw->str k)
                              (single-quote (str v)))))
       (str/join " ")))

(defn- denormalize-flags [data]
  (cond
    (or (sequential? (val data))
        (set? (val data)))
    (for [v (val data)]
      [(key data) v])
    (map? (val data))
    (for [[k v] (val data)]
      [(str (name (key data)) "=" k) (str v)])
    :else data))

(defn- args->flags [prefix arg-map]
  (->> arg-map (map denormalize-flags) flatten (apply flags prefix)))

(defn conf-unix [m]
  (letfn [(f  [m]
          (mean/search m
            {?k ?v} (cond
                      (map? ?v)
                      (format "[%s]\n%s"(kw->str ?k)
                              (str/join "\n" (f ?v)))
                      (sequential? ?v)
                      (str/join "\n" (map #(conf-unix {?k %}) ?v))
                      :else
                      (format "%s=%s" (kw->str ?k) ?v))))]
    (str/join "\n" (f m))))

(def json-writer (json/write-json-fn {:escape-slash false}))

(defmulti write (fn [x] (-> x meta :scv/format)))
(defmethod write :json [x] (let [w (java.io.StringWriter.)]
                             (json-writer w x)
                             (.toString w)))
(defmethod write :toml [x] (-> x toml/write str/trim))
(defmethod write :hcl [x] (-> x hcl/emit str/trim))
(defmethod write :flags-- [x] (args->flags "--" x))
(defmethod write :flags- [x] (args->flags "-" x))
(defmethod write :conf-unix [x] (conf-unix x))
(defmethod write nil [x] (if (string? x) x (throw (ex-info "not SCV-able" {:input x}))))

(defn write-as [x format]
  (write (with-meta x {:scv/format format})))

(comment (write-as {:a 1 :b [1 2]} :flags--))
