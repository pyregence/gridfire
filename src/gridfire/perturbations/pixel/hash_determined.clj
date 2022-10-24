;; [[file:../../../../org/GridFire.org::hdpp-implementation][hdpp-implementation]]
(ns gridfire.perturbations.pixel.hash-determined
  "Implements 'Hash-Determined Pixel Perturbations' (a name chosen by the GridFire team),
  an algorithm for efficiently emulating a 'white noise' random process on a large discrete grid,
  without having to generate a random perturbation for each cell of the entire grid,
  nor even for each requested cell."
  (:import (org.apache.commons.codec.digest MurmurHash3)))

(defn- is-power-of-2?
  [^long n]
  (if-not (pos-int? n)
    false
    (cond
      (= n 1)  true
      (odd? n) false
      :else    (recur (quot n 2)))))

(comment

  (def n 2r1000)
  (is-power-of-2? 2r1000)
  (is-power-of-2? 2r1010)

  *e)

(defn gen-hash->perturbation
  "Randomly populates an array mapping hash buckets to perturbations.

  Given:
  - n-buckets: a power of 2, the number of hash buckets,
  - gen-perturbation: a 1-arg function, accepting a hash bucket h (an integer < n-buckets)
    and returning a double, presumably sampled from the desired distribution of perturbations
    (Typically, the argument h will not be used, but it might serve to implement a form of reproducible
    pseudo-randomness.),

  returns an array of length n-buckets."
  ^doubles
  [n-buckets gen-perturbation]
  {:pre [(is-power-of-2? n-buckets)]}
  (into-array
   Double/TYPE
   (map gen-perturbation
        (range n-buckets))))

(defn resolve-perturbation-for-coords
  "Resolves the random perturbation for the grid cell ('pixel') of the supplied coordinates,
  using the h->perturb returned by #'gen-hash->perturbation.

  This function is deterministic: all the randomness happened when h->perturb was created."
  (^double [^doubles h->perturb ^long i ^long j]
   (let [n-buckets   (alength h->perturb)
         coords-hash (as-> (int 36791) h
                       (MurmurHash3/hash32 i j h)
                       (bit-and h (dec n-buckets)))]
     (aget h->perturb coords-hash)))
  (^double [^doubles h->perturb ^long b ^long i ^long j]
   (let [n-buckets   (alength h->perturb)
         coords-hash (as-> (int 6053696) h
                       ;; Compared to clojure.core's (hash) or (hash-combine) functions, MurmurHash3 buys us 2 things:
                       ;; 1. a guarantee of stable behaviour - the hashing functions will still return
                       ;; the same results after upgrading. That's not guaranteed with (hash).
                       ;; 2. Better performance, since those methods have primitive signatures.
                       ;; (this performance gain was empirically observed).
                       (MurmurHash3/hash32 i j h)
                       (MurmurHash3/hash32 b h)
                       (bit-and h
                                ;; fast modulo op - that's why we require n-buckets to be a power of 2.
                                ;; NOTE there is no significant efficiency gain to replacing this expr with a hardcoded int like 2r111111111.
                                (dec n-buckets)))]
     (aget h->perturb coords-hash))))
;; hdpp-implementation ends here
