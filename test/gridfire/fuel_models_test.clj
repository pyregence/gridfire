(ns gridfire.fuel-models-test
  (:require [clojure.test :refer :all]
            [gridfire.fuel-models :refer :all]
            [gridfire.behaveplus-results :refer :all]))

(deftest moisturize-test
  (doseq [num sb40-fuel-models]
    (let [gridfire-fuel-model-dry   (build-fuel-model num)
          gridfire-fuel-model-wet   (moisturize gridfire-fuel-model-dry (test-fuel-moisture :dry))
          gridfire-M_x-live         (* 100 (-> gridfire-fuel-model-wet :M_x :live :herbaceous))
          gridfire-fraction-cured   (if (pos? (-> gridfire-fuel-model-dry :w_o :live :herbaceous))
                                      (* 100 (/ (-> gridfire-fuel-model-wet :w_o :dead :herbaceous)
                                                (-> gridfire-fuel-model-dry :w_o :live :herbaceous)))
                                      0)
          behaveplus-outputs        (-> (:name gridfire-fuel-model-dry)
                                        (behaveplus5-surface-fire-values-dry-no-wind-no-slope))
          behaveplus-M_x-live       (behaveplus-outputs 4)
          behaveplus-fraction-cured (behaveplus-outputs 6)]
      (is (within gridfire-M_x-live behaveplus-M_x-live 6)) ;; all are within 1% except TU2 at -6%
      (is (within gridfire-fraction-cured behaveplus-fraction-cured 0.1)))))

;; TODO: Expand moisturize-test into three tests for the :dry, :mid, and :wet test-fuel-moisture values.
