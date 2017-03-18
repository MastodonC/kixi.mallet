(ns kixi.mallet.pipe
  (:import [cc.mallet.pipe Pipe]
           [cc.mallet.types Instance]
           [java.util Arrays]))

(defn instance->bean [instance]
  (select-keys (bean instance) [:data :target :name :source]))

(defn bean->instance [{:keys [data name source target]}]
  (Instance. data target name source))

(defn transduce-instances
  "Apply the transducer to an iterable of Instances, returning the same."
  [xform]
  (fn [this source]
    (let [f (comp (map instance->bean)
                  xform
                  (map bean->instance))]
      (->> (iterator-seq source)
           (sequence f)
           (into-array Instance)
           (Arrays/asList)
           (.iterator)))))

(defmacro defpipe
  "Defines a pipe compatible with Mallet.
  xform must be a transducer expecting to receive maps with the following keys:
  data, target, name and source."
  [name xform]
  `(do
     (def ~(symbol (str name "-newIteratorFrom"))
       (transduce-instances ~xform))
     (gen-class :name ~(symbol (str (ns-name *ns*) "." name))
                :extends cc.mallet.pipe.Pipe
                :prefix ~(symbol (str name "-")))))
