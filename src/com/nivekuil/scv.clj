(ns com.nivekuil.scv
  (:require [charred.api :as json]
            [hcl.core :as hcl]
            [clojure.string :as str]
            [toml.core :as toml]
            [meander.epsilon :as mean]
	    [hyperfiddle.rcf :as rcf]
	    [camel-snake-kebab.core :as csk]))

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

(defn- process-args [prefix data]
  (cond (map? data)
        (args->flags prefix data)
        (sequential? data)
        (str/join " " (mapv (fn [x] (if (string? x) x (args->flags prefix x)))
                            data))))

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

(defn systemd [m top-level?]
  (let [f (fn [v] (if (keyword? v) (csk/->PascalCase (kw->str v)) v))]
    (mean/search
      m
      {?k ?v} (cond
	        (map? ?v)
	        (if top-level?
		  (format "[%s]\n%s" (f ?k) (str/join "\n" (systemd ?v false)))
		  (str/join "\n" (map #(format "%s=%s=%s" (f ?k) (kw->str (key %)) (val %)) ?v)))
	        (or (sequential? ?v) (set? ?v))
	        (str/join "\n" (map #(format "%s=%s" (f ?k) %) ?v))
	        :else
	        (format "%s=%s" (f ?k) ?v)))))

(def json-writer (json/write-json-fn {:escape-slash false}))

(defmulti -write-as (fn [x format] format))
(defmethod -write-as :json [x _] (let [w (java.io.StringWriter.)]
                             (json-writer w x)
                             (.toString w)))
(defmethod -write-as :toml [x _] (-> x toml/write str/trim))
(defmethod -write-as :hcl [x _] (-> x hcl/emit str/trim))
(defmethod -write-as :flags-- [x _] (process-args "--" x))
(defmethod -write-as :flags- [x _] (process-args "-" x))
(defmethod -write-as :args [x _] (process-args "--" x))
(defmethod -write-as :conf-unix [x _] (conf-unix x))
(defmethod -write-as :systemd [x _] (str/join "\n" (systemd x true)))
(defmethod -write-as :babashka [x _ ] (str "#!/usr/bin/env bb\n" (str/join "\n" x)))
(defmethod -write-as nil [x _] (if (string? x) x (throw (ex-info "not SCV-able" {:input x}))))
(defn write-as [x format]
  (if (map? format)
    (reduce-kv (fn [acc path f] (if (seq path)
                                (update-in acc path write-as f)
                                (write-as acc f)))
               x
               (sort-by (comp count key) #(compare %2 %1) format))
    (-write-as x format)))
(defn write [x] (write-as x (:scv/format (meta x))))

#_(defn write-as [x format]
  (write (with-meta x {:scv/format format})))

(rcf/enable!)
(rcf/tests
  (write-as {:a 1} :json) := "{\"a\":1}"
  (write-as {:a 1 :b {:c 1}}  {[] :systemd
                               [:b] :args})
  := "A=1\nB=--c='1'"
  
  (write-as {:container
	     {:environment {:foo "bar"
			    :bar "baz"
                            "ENV" "env"}
	      :volume #{"foo:/bar"}
	      :image "foo"}
	     :unit
	     {:wants "foo.service"}}
	    :systemd)
  :=
  "[Container]\nEnvironment=foo=bar\nEnvironment=bar=baz\nEnvironment=ENV=env\nVolume=foo:/bar\nImage=foo\n[Unit]\nWants=foo.service"

  (write-as {:a 1 :b [1 2]} :flags--)
  := "--a='1' --b='1' --b='2'"

  (write-as [{:v 1} "command" {:a 1 :b [1 2]} "command2"] :flags--)
  := "--v='1' command --a='1' --b='1' --b='2' command2"

  (write-as {:leveldb3 {:enabled true
		        :dir     "/data/filerldb3"}} :toml)
  := "[leveldb3]\nenabled = true\ndir = \"/data/filerldb3\""
  )

(defn conj
  "A permissive conj that coerces non-collections into vectors."
  [x y]
  (if (coll? x)
    (clojure.core/conj x y)
    (if (nil? x)
      (conj [] y)
      (vector x y))))
(rcf/tests
  (conj 1 2) := [1 2]
  (conj [1] 2) := [1 2])

