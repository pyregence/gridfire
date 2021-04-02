(ns gridfire.binary-output
  (:import (java.io DataInputStream DataOutputStream)
           (java.nio ByteBuffer))
  (:require [clojure.java.io     :as io]
            [clojure.core.matrix :as m]
            [taoensso.tufte      :as tufte]))

;; We assume that matrix[0,0] is the upper left corner.
(defn non-zero-data [matrix]
  (let [row-count (m/row-count matrix)]
    (transduce
     (comp (map-indexed (fn [row cols]
                          (when (pos? (count cols))
                            (let [y (- row-count row)]
                              {:x (mapv inc cols) ; 1 -> m left to right
                               :y (into [] (repeat (count cols) y)) ; n -> 1 top to bottom
                               :v (mapv (fn [col] (m/mget matrix row col)) cols)}))))
           (remove nil?))
     (completing
      (fn [m1 m2]
        {:x (into (:x m1) (:x m2))
         :y (into (:y m1) (:y m2))
         :v (into (:v m1) (:v m2))}))
     {:x []
      :y []
      :v []}
     (m/non-zero-indices matrix))))

;;FIXME max-row max-col is not correct. Need dimensions in binary file.
(defn indices-to-matrix
  ([indices]
   (indices-to-matrix indices nil))

  ([indices ttype]
   (let [rows    (indices :y)
         cols    (indices :x)
         values  (indices :v)
         max-row (apply max rows)
         max-col (apply max cols)
         matrix  (if (= ttype :int)
                   (make-array Integer/TYPE max-row max-col)
                   (make-array Float/TYPE max-row max-col))]
     (dotimes [i (count values)]
       (let [true-row (- max-row (aget rows i))
             true-col (dec (aget cols i))]
         (aset matrix true-row true-col (aget values i))))
     (m/matrix matrix))))

;;FIXME optimize
(defn write-matrix-as-binary [matrix file-name]
  (let [num-burned-cells (m/non-zero-count matrix)
        burned-data      (non-zero-data matrix)]
    (with-open [out (DataOutputStream. (io/output-stream file-name))]
      (.writeInt out (int num-burned-cells))                          ; Int32
      (doseq [x (burned-data :x)] (.writeInt out (int x)))            ; Int32
      (doseq [y (burned-data :y)] (.writeInt out (int y)))            ; Int32
      (doseq [v (burned-data :v)] (.writeFloat out (float v)))))) ; Float32

(defn ints-to-bytes [int-coll]
  (let [num-ints (tufte/p :count (count int-coll))
        buf      (ByteBuffer/allocate (* 4 num-ints))]
    (tufte/p :doseq (doseq [i int-coll] (.putInt buf (int i))))
    (.array buf)))

(defn floats-to-bytes [float-coll]
  (let [num-floats (tufte/p :count (count float-coll))
        buf        (ByteBuffer/allocate (* 4 num-floats))]
    (tufte/p :doseq (doseq [i float-coll] (.putFloat buf (float i))))
    (.array buf)))

(defn write-matrices-as-binary
  [file-name types matrices]
  (tufte/p
   :write-matrices-as-binary
   (let [num-burned-cells (m/non-zero-count (first matrices))
         data             (tufte/p :non-zero-data (mapv non-zero-data matrices))]
     (with-open [out (DataOutputStream. (io/output-stream file-name))]
       (.writeInt out (int num-burned-cells))                   ; Int32
       (let [xs (:x (first data))
             ys (:y (first data))]
         (.write out (ints-to-bytes xs) 0 (* 4 (count xs)))  ; Int32
         (.write out (ints-to-bytes ys) 0 (* 4 (count ys)))) ; Int32
       (doseq [[t d] (map vector types data)]
         (when-let [nums-to-bytes (case t
                                    :int   ints-to-bytes
                                    :float floats-to-bytes
                                    nil)]
           (let [vs (:v d)]
             (.write out (nums-to-bytes vs) 0 (* 4 (count vs))))))))))

;;FIXME optimize
(defn read-matrix-as-binary [file-name]
  (with-open [in (DataInputStream. (io/input-stream file-name))]
    (let [num-burned-cells (.readInt in)]                                ; Int32
      (indices-to-matrix
       {:x (into-array Integer/TYPE (repeatedly num-burned-cells #(.readInt in))) ; Int32
        :y (into-array Integer/TYPE (repeatedly num-burned-cells #(.readInt in))) ; Int32
        :v (into-array Float/TYPE (repeatedly num-burned-cells #(.readFloat in)))})))) ; Float32

(defn read-matrices-as-binary [file-name ttypes]
  (with-open [in (DataInputStream. (io/input-stream file-name))]
    (let [num-burned-cells (.readInt in)
          xs-bytes         (byte-array (* 4 num-burned-cells))
          ys-bytes         (byte-array (* 4 num-burned-cells))
          _                (.read in xs-bytes) ; Int32
          _                (.read in ys-bytes)
          xs               (int-array num-burned-cells)
          ys               (int-array num-burned-cells)
          _               (.get (.asIntBuffer (ByteBuffer/wrap xs-bytes)) xs)
          _               (.get (.asIntBuffer (ByteBuffer/wrap ys-bytes)) ys)] ; Int32
      (mapv (fn [ttype]
              (indices-to-matrix
               {:x xs
                :y ys
                :v (let [vs-bytes (byte-array (* 4 num-burned-cells))
                         _        (.read in vs-bytes)]
                     (case ttype
                       :int   (let [vs (int-array num-burned-cells)]
                                (.get (.asIntBuffer (ByteBuffer/wrap vs-bytes)) vs)
                                vs)
                       :float (let [vs (float-array num-burned-cells)]
                                (.get (.asFloatBuffer (ByteBuffer/wrap vs-bytes)) vs)
                                vs)
                       nil))} ; Float32/Int32
               ttype))
            ttypes))))

;; toa_test.bin was created with:
;; (write-matrix-as-binary [[0 1 2] [3 0 4] [5 6 0]] "toa_test.bin")
