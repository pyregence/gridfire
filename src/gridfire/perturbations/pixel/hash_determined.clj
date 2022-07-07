(ns gridfire.perturbations.pixel.hash-determined
  (:import (org.apache.commons.codec.digest MurmurHash3)))


(defn- is-power-of-2?
  [^long n]
  (if-not (pos-int? n)
    false
    (cond
      (= n 1) true
      (odd? n) false
      :else (recur (quot n 2)))))

(comment

  (def n 2r1000)
  (is-power-of-2? 2r1000)
  (is-power-of-2? 2r1010)

  *e)

(defn gen-hash->perturbation
  [n-buckets gen-perturbation]
  {:pre [(is-power-of-2? n-buckets)]}
  (into-array
    Double/TYPE
    (map
      gen-perturbation
      (range n-buckets))))

(defn resolve-perturbation-for-coords
  "WARNING: Assumes the given array's length is a power of 2."
  (^double [^doubles h->perturb ^long i ^long j]
   (let [tuple-hash (->
                      (->> (int 36791)
                           (MurmurHash3/hash32 i j))
                      (bit-and
                        (dec (alength h->perturb))))]
     (aget h->perturb tuple-hash)))
  (^double [^doubles h->perturb ^long b ^long i ^long j]
   (let [tuple-hash (->
                      (->> (int 6053696)                    ;; randomly chosen seed.
                           (MurmurHash3/hash32 i j)
                           (MurmurHash3/hash32 b))
                      (bit-and                              ;; clever modulo op.
                        ;; NOTE there is no significant efficiency gain to replacing this expr with a hardcoded int like 2r111111111.
                        (dec (alength h->perturb))))]
     (aget h->perturb tuple-hash))))
