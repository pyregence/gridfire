;; FIXME LP coverage
(ns gridfire.elliptical-test
  (:require [clojure.test                  :refer [deftest is testing]]
            [gridfire.elliptical           :refer [fireline-normal-spread-rate-scalar]]
            [gridfire.surface-fire-optimal :refer [compute-spread-rate]]
            [gridfire.conversion           :as convert]))

(defn almost-=?
  [^double d1 ^double d2]
  (< (Math/abs (- d1 d2))
     1e-5))

(deftest ^:unit fireline-normal-spread-rate-scalar--examples
  (testing "With non-zero eccentricity,"
    (let [eccentricity 0.9]
      (testing "for the heading fire (max-spread and fireline-normal directions are aligned),"
        (let [cos-ang 1.0]
          (is (almost-=? 1.0
                         (fireline-normal-spread-rate-scalar eccentricity cos-ang))
              "the fireline-normal spread rate achieves the max spread rate.")))
      (testing "for the backing fire (max-spread and fireline-normal directions are opposed),"
        (let [cos-ang         -1.0
              min-spread-rate (-> 1.0 (* (- 1.0 eccentricity)) (/ (+ 1.0 eccentricity)))]
          (is (almost-=? min-spread-rate
                         (fireline-normal-spread-rate-scalar eccentricity cos-ang))
              "the fireline-normal spread rate achieves the minimum spread rate.")))
      (testing "for the flanking fire (max-spread and fireline-normal directions are orthogonal),"
        (let [cos-ang              0.0
              LoW                  (Math/pow (- 1.0 (Math/pow eccentricity 2))
                                             -0.5)
              ;; ellipse half-length (A) and half-width (B)
              A                    (/ 1.0 (+ 1.0 eccentricity))
              B                    (/ A LoW)
              flanking-spread-rate B]
          (is (almost-=? flanking-spread-rate
                         (fireline-normal-spread-rate-scalar eccentricity cos-ang))
              "the fireline-normal spread rate is the flanking spread rate (ellipse half-width).")))
      (let [;; arbitrary values
            max-spread-rate  12.0
            spread-direction 0.0]
        (testing "the fireline-normal and burn-vector spread rates,"
          (testing "when the max-spread and fireline-normal directions are not aligned,"
            (doseq [cos-ang [-0.8 -0.2 0.0 0.3 0.5 0.7]]
              (let [burn-vec-spread-rate (compute-spread-rate max-spread-rate
                                                              (-> cos-ang (double) (Math/acos) (convert/rad->deg))
                                                              eccentricity
                                                              spread-direction)]
                (is (< burn-vec-spread-rate
                       (* max-spread-rate (fireline-normal-spread-rate-scalar eccentricity cos-ang)))
                    "are not equal (and yes, that is expected, because some non-normal directions achieve faster normal-projected spread.)"))))
          (testing "when the max-spread and fireline-normal directions are aligned,"
            (doseq [cos-ang [-1.0 1.0]]
              (let [burn-vec-spread-rate (compute-spread-rate max-spread-rate
                                                              (-> cos-ang (double) (Math/acos) (convert/rad->deg))
                                                              eccentricity
                                                              spread-direction)]
                (is (almost-=? burn-vec-spread-rate
                               (* max-spread-rate (fireline-normal-spread-rate-scalar eccentricity cos-ang)))
                    "are equal (and only then.)"))))))
      (testing "the fire-normal spread rate increases as the angle gets reduced"
        (is (apply <
                   (concat [0.0]
                           (->> (range -1.0 1.0 0.1)
                                (map (fn [^double cos-ang]
                                       (fireline-normal-spread-rate-scalar eccentricity cos-ang))))
                           [1.0])))))))
