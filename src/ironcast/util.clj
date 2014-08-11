(ns ironcast.util
  (:import (clojure.lang IDeref)
           (java.util Deque Set HashSet LinkedList)
           (java.io File)
           (com.badlogic.gdx.files FileHandle)
           (com.badlogic.gdx.graphics.g2d SpriteBatch BitmapFont)
           (com.badlogic.gdx.graphics Color))
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clj-tuple :refer [tuple]]))


(defmacro half
  [n]
  `(/ ~n 2))

(defn indmap
  "2d index map"
  [x y w]
  (+ x (* w y)))

(defn rindmap
  "reverse 2d index map"
  [i w]
  (let [y (int (/ i w))
        x (int (- i (* y w)))]
    [x y]))

(defn index-by
  "index a column by the value given by the function f"
  [f coll]
  (into {} (map #(do [(f %) %]) coll)))

(defn assoc-if-val
  [map key val]
  (if val
    (assoc map key val)
    map))

(defn update
  ([m k f & args]
   (update m k #(apply f % args)))
  ([m k f]
   (assoc m k (f (get m k)))))

(defn lines
  [str]
  (str/split str #"(\n|\r\n)"))

(defn words
  [str]
  (str/split str #"\s"))

(defn str-words
  [& str]
  (str/join " " str))

(defn map-values
  [f mapcoll]
  (into {} (map (fn [[k v]] [k (f v)]) mapcoll)))

(defn map-apply
  [fns m]
  (->> m
       (map (fn [[k v]] [k ((get fns k identity) v)]))
       (into {})))

(defn map-distinct
  [f coll]
  (distinct (map f coll)))

(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
  nested structure. keys is a sequence of keys. Any empty maps that result
  will not be present in the new structure."
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))


(defn find-first
  [pred coll]
  (first (filter pred coll)))

(defn group-by-unique
  [f coll]
  (map-values first (group-by f coll)))

(defn index-unique
  [xrel keys]
  (map-values first (set/index xrel keys)))

(defn pprint-str
  [val]
  (with-out-str (clojure.pprint/pprint val)))

(defn default-indexer
  [xrel keys]
  (if (second keys)
    (set/index xrel keys)
    (group-by (first keys) xrel)))

(defmacro indexer
  "Takes unique keys as arguments and
   returns a function that will index a collection returning the right
   index type for each relationship as follows:
   (def index (indexer :foo #{:bar :baz})))
   (index coll :foo) => group-by-unique
   (index coll :bar :baz) => index-unique
   (index coll :bar) => clojure.set/group-by
   (index coll :fred :ethel) => clojure.set/index"
  [& unique-keys]
  (let [xrel (gensym "xrel")
        ks (gensym "ks")
        keyset (gensym)]
    `(fn [~xrel & ~ks]
       (let [~keyset (set ~ks)]
         (cond
           ~@(->> (for [k unique-keys]
                    (if (set? k)
                      [`(= ~k ~keyset) `(index-unique ~xrel ~k)]
                      [`(= #{~k} ~keyset) `(group-by-unique ~k ~xrel)]))
                  (apply concat))
           :else (default-indexer ~xrel ~keyset))))))

(defn derefable
  [f]
  (reify IDeref
    (deref [this]
      (f))))

(defmacro >>
  [& forms]
  `(derefable (fn []
                ~@forms)))

;;geom
(defn int-tuple
  [tuple]
  (mapv int tuple))

(defn move-pt
  ([tuple [x y]]
   (move-pt tuple x y))
  ([tuple x y]
   (assoc tuple
     0 x
     1 y)))

(defn add-pt
  ([tuple [x y]]
   (add-pt tuple x y))
  ([[ox oy :as tuple] x y]
   (move-pt tuple
            (+ ox x)
            (+ oy y))))

(defn sub-pt
  ([tuple [x y]]
   (sub-pt tuple x y))
  ([[ox oy :as tuple] x y]
   (move-pt tuple
            (- ox x)
            (- oy y))))

(defn mult-pt
  ([[ox oy :as tuple] x* y*]
   (move-pt tuple
            (int (* ox x*))
            (int (* oy y*))))
  ([tuple [x* y*]]
   (mult-pt tuple x* y*)))

(defn mult-ptn
  [tuple n]
  (mult-pt tuple n n))

(defn div-pt
  ([tuple x y]
   (mult-pt tuple (/ 1 x) (/ 1 y)))
  ([tuple [x y]]
   (div-pt tuple x y)))

(defn div-ptn
  [tuple n]
  (div-pt tuple n n))

(defn mult-rect
  ([[ox oy ow oh] x y w h]
   (tuple
     (* ox x)
     (* oy y)
     (* ow w)
     (* oh h)))
  ([tuple [x y w h]]
   (mult-rect tuple x y w h)))

(defn div-rect
  ([tuple x y w h]
   (mult-rect tuple
              (/ 1 x)
              (/ 1 y)
              (/ 1 w)
              (/ 1 h)))
  ([tuple [x y w h]]
   (div-rect tuple x y w h)))

(defn div-rectn
  [tuple n]
  (div-rect tuple n n n n))

(defn add-rect
  ([[ox oy ow oh] x y w h]
   (tuple
     (+ ox x)
     (+ oy y)
     (+ ow w)
     (+ oh h)))
  ([rect [x y w h]]
   (add-rect rect x y w h)))

(defn add-rectn
  [rect n]
  (add-rect rect n n n n))

(defn explode-rect
  [rect n]
  (add-rect rect (- n) (- n) (* 2 n) (* 2 n)))

(defn rect-pts
  ([x y w h]
   (for [xn (range 0 w)
         yn (range 0 h)]
     (tuple (+ x xn) (+ y yn))))
  ([[x y w h]]
   (rect-pts x y w h)))

(defn pt-in?
  ([x y width height x2 y2]
   (and (<= x x2 (+ x width))
        (<= y y2 (+ y height))))
  ([x y width height [x2 y2]]
   (pt-in? x y width height x2 y2)))

(defn pt-in-screen?
  ([x y width height [x2 y2]]
   (pt-in-screen? x y width height x2 y2))
  ([x y width height x2 y2]
    (and (<= x x2 (+ x width))
         (>= y y2 (- y height)))))

(def dir->pt
  {:n (tuple 0 -1)
   :ne (tuple 1 -1)
   :e (tuple 1 0)
   :se (tuple 1 1)
   :s (tuple 0 1)
   :sw (tuple -1 1)
   :w (tuple -1 0)
   :nw (tuple -1 -1)})

(def dirs
  (vals dir->pt))

(def cardinal?
  #{:n :e :s :w})

(def cardinal-dirs
  (for [[d vec] dir->pt
        :when (cardinal? d)]
    vec))

(defn adj?
  ([[x y] [x2 y2]]
   (adj? x y x2 y2))
  ([x1 y1 x2 y2]
    (and (<= (Math/abs ^int (- x1 x2)) 1)
         (<= (Math/abs ^int (- y1 y2)) 1))))

(defn adj
  ([x y]
   (map #(add-pt % x y) dirs))
  ([[x y]]
   (adj x y)))

(defn cardinal-adj
  ([x y]
   (map #(add-pt % x y) cardinal-dirs))
  ([[x y]]
   (cardinal-adj x y)))

;;todo efficient functional algorithm here?
(defn- flood*
  [^Deque q ^Set hs pred]
  (if (< 0 (.size q))
    (let [n (.poll q)]
      (if (pred n)
        (lazy-seq
          (doseq [a (cardinal-adj n)
                  :when (.add hs a)]
            (.addFirst q a))
          (cons n (flood* q hs pred)))
        (lazy-seq
          (flood* q hs pred))))))

(defn flood
  [p pred]
  (let [hs (HashSet.)
        q (LinkedList.)]
    (.add hs p)
    (.add q p)
    (flood* q hs pred)))

(defmacro xor
  ([] false)
  ([a] true)
  ([a b]
   `(if ~a
      (if ~b false true)
      (if ~b true false)))
  ([a b & more]
   `(xor  (xor ~a ~b) (xor ~@more))))

(defn diagonal?
  [[x1 y1] [x2 y2]]
  (let [a (zero? (- x1 x2))
        b (zero? (- y1 y2))]
    (not (xor a b))))

(defn magnitude
  ([[x y]]
   (magnitude x y))
  ([x y]
   (Math/sqrt
     (+ (* x x)
        (* y y)))))

(defn normalize
  ([[x y]]
   (normalize x y))
  ([x y]
    (let [m (magnitude x y)]
      (tuple (/ x m)
             (/ y m)))))

(defn direction
  [a b]
  (normalize (sub-pt b a)))

(defn manhattan-dist
  ([[x0 y0] [x1 y1]]
   (manhattan-dist x0 y0 x1 y1))
  ([x0 y0 x1 y1]
   (+ (Math/abs ^int (- x1 x0)) (Math/abs ^int (- y1 y0)))))

(defn precise-dist
  [^double x0 ^double y0 ^double x1 ^double y1]
  (let [dx (- x1 x0)
        dy (- y1 y0)
        dx (* dx dx)
        dy (* dy dy)]
    (Math/sqrt (+ dx dy))))

(defn cost
  [a b]
  (manhattan-dist a b))

(defn cost-path
  "Cost up a path
   returning triples [pt cost total-cost]"
  ([start total path]
   (lazy-seq
     (if-let [head (first path)]
       (let [cost (cost start head)
             total (+ total cost)]
         (cons (tuple head
                      cost
                      total)
               (cost-path head total (rest path)))))))
  ([start path]
   (cost-path start 0 path)))

(def colors
  {:white (Color. 1 1 1 1)
   :off-white (Color. 0.8 0.8 0.8 1)
   :black (Color. 0 0 0 1)
   :transparent-black (Color. 0 0 0 0.5)
   :grey (Color. 0.5 0.5 0.5 1)
   :green (Color. 0 1 0 1)
   :light-green (Color. 0.5 1 0.5 1)
   :yellow (Color. 1 1 0 1)
   :light-yellow (Color. 1 1 0.5 1)
   :dark-yellow (Color. 0.5 0.5 0 1)
   :blue (Color. 0 0 1 1)
   :light-blue (Color. 0.8 0.8 1 1)
   :red (Color. 1 0 0 1)})

(defn file-handle
  [file]
  (FileHandle. (File. ^String file)))

(defmacro getter
  "Generate a first class getter"
  [type getter]
  (let [sym (gensym)]
    `(fn [~(with-meta sym {:tag type})]
       (~getter ~sym))))

(defmacro setter
  "Generate a first class setter"
  [type setter]
  (let [sym (gensym)]
    `(fn [~(with-meta sym {:tag type}) val#]
       (~setter ~sym val#))))

(defmacro mapify**
  [type mac & forms]
  `(hash-map
     ~@(->> (for [[key g] (partition 2 forms)]
              `[~key (~mac ~type ~g)])
            (apply concat))))

(defmacro getter-map
  "Create a map of functions wrapping java getters with the correct type hints
  e.g
  (getter-map MyClass
    :foo .getFoo
    :bar .getbar)}"
  [type & getters]
  `(mapify** ~type getter ~@getters))

(defmacro setter-map
  "Create a map of functions wrapping java setters with the correct type hints
  e.g
  (setter-map MyClass
    :foo .setFoo
    :bar .setBar)}"
  [type & setters]
  `(mapify** ~type setter ~@setters))

(defn run-setters
  "For each key/value pair, looks up the setter in the setter-map provided
   and executes it, passing the source instance, and the value.
   e.g. (run-setters obj {:foo .setFoo} {:foo \"bar\") =>
         obj with foo set to bar"
  ([source setter-map opts]
   (doseq [[key val] opts
           :let [setter (setter-map key)]
           :when setter]
     (setter source val))
   source)
  ([source setter-map key val & opts]
   (run-setters source setter-map (conj (partition 2 opts) [key val]))))

(defn setter-ctor
  "Takes a constructor function and a setter-map
   e.g. (def create-some-obj (setter-ctor #(SomeObj.) {:foo .setFoo}))
        (create-some-obj :foo 1)
        =>  an instance of SomeObj with the foo property set to 1."
  [ctor setter-map]
  (fn [& opts] (apply run-setters (ctor) setter-map opts)))

(defn pre-setter
  "Takes a setter fn and a set of transforms
   returns a new setter fn that will apply the transforms (as a normal composed fn)
   to the value before it is passed to the setter."
  ([setter transform & transforms]
   (pre-setter setter (apply comp transform transforms)))
  ([setter transform]
   (fn [inst val]
     (setter inst (transform val)))))


(defmacro field-setter
  [type field]
  (let [sym (gensym)]
    `(fn [~(with-meta sym {:tag type}) val#]
       (set! (~field ~sym) val#))))

(defmacro field-setter-map
  [type & setters]
  `(mapify** ~type field-setter ~@setters))

(defn line*
  [x1 y1 x2 y2 dx dy sx sy err]
  (lazy-seq
    (if (and (= x1 x2)
             (= y1 y2))
      (cons (tuple x2 y2) nil)
      (cons (tuple x1 y1)
            (let [e2 (* 2 err)
                  err (if (> e2 (- dy))
                        (- err dy)
                        err)
                  err (if (< e2 dx)
                        (+ err dx)
                        err)
                  x1 (if (> e2 (- dy))
                       (+ x1 sx)
                       x1)
                  y1 (if (< e2 dx)
                       (+ y1 sy)
                       y1)]
              (line*
                x1 y1 x2 y2 dx dy sx sy err))))))

(defn line
  ([[x1 y1] [x2 y2]]
   (line x1 y1 x2 y2))
  ([x1 y1 x2 y2]
   (let [dx (Math/abs ^int (- x2 x1))
          dy (Math/abs ^int (- y2 y1))
          sx (if (< x1 x2) 1 -1)
          sy (if (< y1 y2) 1 -1)
          err (- dx dy)]
      (line* x1 y1 x2 y2 dx dy sx sy err))))

(defn circle
  [^double x ^double y ^double radius]
  (loop [res (list
               (tuple x (+ y radius))
               (tuple x (- y radius))
               (tuple (+ x radius) y)
               (tuple (- x radius) y))
         f (- 1 radius)
         ddf-x 1
         ddf-y (* -2 radius)
         xx 0
         yy radius]
    (if (< xx yy)
      (let [yy (if (>= f 0) (dec yy) yy)
            ddf-y (if (>= f 0) (+ ddf-y 2) ddf-y)
            f (if (>= f 0) (+ f ddf-y) f)
            xx (inc xx)
            ddf-x (+ ddf-x 2)
            f (+ f ddf-x)]
        (recur
          (conj res
                (tuple (+ x xx) (+ y yy))
                (tuple (- x xx) (+ y yy))
                (tuple (+ x xx) (- y yy))
                (tuple (- x xx) (- y yy))

                (tuple (+ x yy) (+ y xx))
                (tuple (- x yy) (+ y xx))
                (tuple (+ x yy) (- y xx))
                (tuple (- x yy) (- y xx)))
          f
          ddf-x
          ddf-y
          xx
          yy))
      (set res))))

(defn filled-circle
  [x y radius]
  (for [xx (range (- x radius) (+ x radius 1))
        yy (range (- y radius) (+ y radius 1))
        :when (<= (manhattan-dist x y xx yy) radius)]
    (tuple xx yy)))

(defn square
  [x y width]
  (rect-pts (- x (int (/ width 2))) (- y (int (/ width 2))) width width))


(defn set-conj
  ([maybe-set v]
   (if (set? maybe-set)
     (conj maybe-set v)
     #{v}))
  ([maybe-set v & vs]
   (if (set? maybe-set)
     (apply conj maybe-set v vs)
     (set (cons v vs)))))

(defprotocol Comp
  (start [this])
  (stop [this]))

(defmacro try-state
  [[sym state] & body]
  (let [nsym (gensym)
        [b & rest] body]
    `(let [os# ~state
           ~sym ~state]
       (let [[~nsym success?#] ~b]
         (if success?#
           ~(if (seq rest)
              `(try-state [~sym ~nsym] ~@rest)
              `(tuple ~nsym true))
           (tuple os# false))))))

(defmacro lift-try
  [body]
  `(tuple ~body true))

(defn success
  [st]
  (tuple st true))

(defn fail
  [st]
  (tuple st false))