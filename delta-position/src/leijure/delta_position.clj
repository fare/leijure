(ns leijure.delta-position)

;; If you want to know what line and column you're at while reading a stream,
;; you can just use clojure.lang.LineNumberingPushbackReader...
;; unless you care about tab width, that said class doesn't handle
;; (inheriting this behavior or lack thereof from java.io.LineNumberingReader).
;; If you care about tab width, use this delta-position library instead.
;;
;; A delta-position is a map representing how a given string affects the position.
;; Importantly, combining two delta-position is associative
;; — it's not merely a position that you have to compute from the beginning
;; of the file, but a delta that you can compute between arbitrary points.
;;
;; Thus, if you want fast random access to the line and column
;; information from a random byte or character position in your file,
;; you can divide your file in chunks of strings,
;; compute delta-position for each chunk in parallel,
;; and build a tree index that makes it easy to
;; compute the position of a random character or byte.
;; Of course, if you're using a variable length encoding,
;; such as the default, UTF-8, be sure to properly handle
;; the codepoints that may be broken at your chunk boundary, and e.g.
;; attach each partial codepoint to the previous chunk.
;;
;; This (version of this) library makes no attempt to recognize
;; double-width characters. This shouldn't be too hard to implement, though.
;;
;; Note that clojure.lang.LineNumberingPushbackReader.
;; counts lines and columns from 1, while we count from 0.
;; You may have to add one before you report them to users.

(def ^:dynamic *tab-length* 8)

(defn %ceiling [n m] (+ n (- m (mod n m))))
(defn %floor [n m] (- n (mod n m)))
(defn next-ceiling [n m] (+ m (%floor n m)))

(defn %charset-encoder [name]
  (-> (java.nio.charset.Charset/forName name)
      (.newEncoder)))

(def utf8-encoder (%charset-encoder "UTF-8"))

(defn charset-encoder [name]
  (case name
    "UTF-8" utf8-encoder
    (%charset-encoder name)))

(defn encoder-char-length [char encoder]
  (-> encoder
      (.encode (java.nio.CharBuffer/wrap (char-array [char])))
      (.limit)))

;; A delta-position is a difference in position from a block of text.
(def null-delta-position
  {:byte 0 :char 0 :line 0 :column 0 :tab-lead nil
   :starts-with-newline? false :ends-with-return? false })

(defn %normal-char-delta-position [length]
  {:byte length :char 1 :line 0 :column 1 :tab-lead nil
   :starts-with-newline? false :ends-with-return? false })

(def %normal-char-delta-position-vec
  (into [] (for [i (range 1 7)] (%normal-char-delta-position i))))

(def %normal-char-delta-position-1
  (%normal-char-delta-position-vec 0))

(defn normal-char-delta-position
  ([l] (%normal-char-delta-position-vec (dec l)))
  ([] %normal-char-delta-position-1))

(def return-delta-position
  {:byte 1 :char 1 :line 1 :column 0 :tab-lead nil
   :starts-with-newline? false :ends-with-return? true})

(def newline-delta-position
  {:byte 1 :char 1 :line 1 :column 0 :tab-lead nil
   :starts-with-newline? true :ends-with-return? false})

(def formfeed-delta-position
  ;; hack: formfeed has zero width.
  ;; Maybe add state so it resets the column?
  {:byte 1 :char 1 :line 0 :column 0 :tab-lead nil
   :starts-with-newline? true :ends-with-return? false})

(def tab-delta-position
  {:byte 1 :char 1 :line 0 :column 0 :tab-lead 0
   :starts-with-newline? false :ends-with-return? false})

(defn combine-delta-position
  ([] null-delta-position)
  ([x] x)
  ([x y] (let [{xb :byte xc :char xln :line xco :column xt :tab-lead
                xn :starts-with-newline? xr :ends-with-return?} x
               {yb :byte yc :char yln :line yco :column yt :tab-lead
                yn :starts-with-newline? yr :ends-with-return?} y
               [tab-lead column]
               (cond
                (not (zero? yln)) [yt yco]
                xt [xt (+ (if yt (next-ceiling (+ xco yt) *tab-length*) xco)
                          yco)]
                yt [(mod (+ xco yt) *tab-length*)
                    (+ (next-ceiling (+ xco yt) *tab-length*) yco)]
                :else [nil (+ xco yco)])]
           {:byte (+ xb yb)
            :char (+ xc yc)
            :line (+ xln yln (if (and xr yn) -1 0))
            :tab-lead tab-lead
            :column column
            :starts-with-newline? (if (zero? xc) yn xn)
            :ends-with-return? (if (zero? yc) xr yr)}))
  ([x y z & t] (reduce combine-delta-position (list* x y z t))))

(defn char-delta-position
  ([char encoder]
     (case char
       \return return-delta-position
       \newline newline-delta-position
       \formfeed formfeed-delta-position
       \tab tab-delta-position
       nil null-delta-position
       (if (and (= encoder utf8-encoder) (< (int char) 128))
         %normal-char-delta-position-1
         (normal-char-delta-position char encoder))))
  ([char]
     (char-delta-position char utf8-encoder)))

(defn inc-delta-position
  ([delta-position char encoder]
     (combine-delta-position delta-position (char-delta-position char encoder)))
  ([delta-position char]
     (inc-delta-position delta-position char utf8-encoder)))

(defn seq-delta-position
  ([s encoder]
     (reduce #(inc-delta-position % %2 encoder) null-delta-position s))
  ([s]
     (seq-delta-position s utf8-encoder)))

(defn to-char [c] (if (= c -1) nil (char c)))
(defn read-char [stream] (to-char (.read stream)))

(defn ensure-reader [s]
  (cond
   (instance? java.io.Reader s) s
   (instance? String s) (java.io.StringReader. s)
   :else (throw (Exception. (format "can't turn %s into a Reader" s)))))

(defn- %positioned-stream [reader delta-position encoder line-offset column-offset]
  (lazy-seq
   (if-let [char (read-char reader)]
     (cons [char (+ line-offset (:line delta-position)) (+ column-offset (:column delta-position))]
           (%positioned-stream
            reader (inc-delta-position delta-position char encoder) encoder line-offset column-offset)))))

(defn positioned-stream
  ([input]
     (positioned-stream input {}))
  ([input options]
     (%positioned-stream
      (ensure-reader input)
      (or (:delta-position options) null-delta-position)
      (or (:encoder options) utf8-encoder)
      (or (:line-offset options) 0)
      (or (:column-offset options) 0))))
