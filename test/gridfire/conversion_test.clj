(ns gridfire.conversion-test
  (:require [clojure.test :refer [deftest is testing]]
            [gridfire.conversion :as convert]
            [gridfire.magellan-bridge :refer [geotiff-raster-to-matrix]]
            [gridfire.utils.test :as utils]
            [clojure.core.matrix :as m]))

(def resources-path "test/gridfire/resources/weather-test")

(deftest canopy-height-test
  (let [layer-before  (geotiff-raster-to-matrix (utils/in-file-path resources-path "ch.tif"))
        unit-fn       convert/m->ft
        multiplier-fn #(* 100 %)]

    (testing "convert metric"
      (let [layer-after (convert/to-imperial layer-before {:units :metric} :canopy-height)]

        (is (= (:matrix layer-after) (m/emap unit-fn (:matrix layer-before))))))

    (testing "scale with multiplier"
      (let [layer-after (convert/to-imperial layer-before {:multiplier 100} :canopy-height)]

        (is (= (:matrix layer-after) (m/emap multiplier-fn (:matrix layer-before))))))

    (testing "convert metric and scale with multiplier"
      (let [layer-after (convert/to-imperial layer-before {:units :metric :multiplier 100} :canopy-height)]

        (is (= (:matrix layer-after) (m/emap (comp unit-fn multiplier-fn) (:matrix layer-before))))))

    (testing "no conversion, already in imperial"
      (let [layer-after (convert/to-imperial layer-before {:units :imperial} :canopy-height)]

        (is (= (:matrix layer-after) (:matrix layer-before)))))))


(deftest elevation-test
  (let [layer-before  (geotiff-raster-to-matrix (utils/in-file-path resources-path "dem.tif"))
        unit-fn       convert/m->ft
        multiplier-fn #(* 100 %)]

    (testing "convert metric"
      (let [layer-after (convert/to-imperial layer-before {:units :metric} :elevation)]

        (is (= (:matrix layer-after) (m/emap unit-fn (:matrix layer-before))))))

    (testing "scale with multiplier"
      (let [layer-after (convert/to-imperial layer-before {:multiplier 100} :elevation)]

        (is (= (:matrix layer-after) (m/emap multiplier-fn (:matrix layer-before))))))

    (testing "convert metric and scale with multiplier"
      (let [layer-after (convert/to-imperial layer-before {:units :metric :multiplier 100} :elevation)]

        (is (= (:matrix layer-after) (m/emap (comp unit-fn multiplier-fn) (:matrix layer-before))))))

    (testing "no conversion, already in imperial"
      (let [layer-after (convert/to-imperial layer-before {:units :imperial} :elevation)]

        (is (= (:matrix layer-after) (:matrix layer-before)))))))


(deftest canopy-base-height-test
  (let [layer-before  (geotiff-raster-to-matrix (utils/in-file-path resources-path "cbh.tif"))
        unit-fn       convert/m->ft
        multiplier-fn #(* 100 %)]

    (testing "convert metric"
      (let [layer-after (convert/to-imperial layer-before {:units :metric} :canopy-base-height)]

        (is (= (:matrix layer-after) (m/emap unit-fn (:matrix layer-before))))))

    (testing "scale with multiplier"
      (let [layer-after (convert/to-imperial layer-before {:multiplier 100} :canopy-base-height)]

        (is (= (:matrix layer-after) (m/emap multiplier-fn (:matrix layer-before))))))

    (testing "convert metric and scale with multiplier"
      (let [layer-after (convert/to-imperial layer-before {:units :metric :multiplier 100} :canopy-base-height)]

        (is (= (:matrix layer-after) (m/emap (comp unit-fn multiplier-fn) (:matrix layer-before))))))

    (testing "no conversion, already in imperial"
      (let [layer-after (convert/to-imperial layer-before {:units :imperial} :canopy-base-height)]

        (is (= (:matrix layer-after) (:matrix layer-before)))))))



(deftest crown-bulk-density-test
  (let [layer-before  (geotiff-raster-to-matrix (utils/in-file-path resources-path "cbd.tif"))
        unit-fn       #(* 0.0624 %)
        multiplier-fn #(* 100 %)]

    (testing "convert metric"
      (let [layer-after (convert/to-imperial layer-before {:units :metric} :crown-bulk-density)]

        (is (= (:matrix layer-after) (m/emap unit-fn (:matrix layer-before))))))

    (testing "scale with multiplier"
      (let [layer-after (convert/to-imperial layer-before {:multiplier 100} :crown-bulk-density)]

        (is (= (:matrix layer-after) (m/emap multiplier-fn (:matrix layer-before))))))

    (testing "convert metric and scale with multiplier"
      (let [layer-after (convert/to-imperial layer-before {:units :metric :multiplier 100} :crown-bulk-density)]

        (is (= (:matrix layer-after) (m/emap (comp unit-fn multiplier-fn) (:matrix layer-before))))))

    (testing "no conversion, already in imperial"
      (let [layer-after (convert/to-imperial layer-before {:units :imperial} :crown-bulk-density)]

        (is (= (:matrix layer-after) (:matrix layer-before)))))))


(deftest wind-speed-20ft-test
  (let [layer-before  (geotiff-raster-to-matrix (utils/in-file-path resources-path "ws_to_sample.tif"))
        unit-fn       #(* % 2.237)
        multiplier-fn #(* 100 %)]

    (testing "convert metric"
      (let [layer-after (convert/to-imperial layer-before {:units :metric} :wind-speed-20ft)]

        (is (= (:matrix layer-after) (m/emap unit-fn (:matrix layer-before))))))

    (testing "scale with multiplier"
      (let [layer-after (convert/to-imperial layer-before {:multiplier 100} :wind-speed-20ft)]

        (is (= (:matrix layer-after) (m/emap multiplier-fn (:matrix layer-before))))))

    (testing "convert metric and scale with multiplier"
      (let [layer-after (convert/to-imperial layer-before {:units :metric :multiplier 100} :wind-speed-20ft)]

        (is (= (:matrix layer-after) (m/emap (comp unit-fn multiplier-fn) (:matrix layer-before))))))

    (testing "no conversion, already in imperial"
      (let [layer-after (convert/to-imperial layer-before {:units :imperial} :wind-speed-20ft)]

        (is (= (:matrix layer-after) (:matrix layer-before)))))))


(deftest temperature-test
  (let [layer-before  (geotiff-raster-to-matrix (utils/in-file-path resources-path "tmpf_to_sample.tif"))
        multiplier-fn #(* 100 %)]

    (testing "convert metric"
      (let [layer-after (convert/to-imperial layer-before {:units :metric} :temperature)
            unit-fn     convert/C->F]

        (is (= (:matrix layer-after) (m/emap unit-fn (:matrix layer-before))))))

    (testing "convert absolute"
      (let [layer-after (convert/to-imperial layer-before {:units :absolute} :temperature)
            unit-fn     convert/K->F]

        (is (= (:matrix layer-after) (m/emap unit-fn (:matrix layer-before))))))

    (testing "scale with multiplier"
      (let [layer-after (convert/to-imperial layer-before {:multiplier 100} :temperature)]

        (is (= (:matrix layer-after) (m/emap multiplier-fn (:matrix layer-before))))))

    (testing "convert metric and scale with multiplier"
      (let [layer-after (convert/to-imperial layer-before {:units :metric :multiplier 100} :temperature)
            unit-fn     convert/C->F]

        (is (= (:matrix layer-after) (m/emap (comp unit-fn multiplier-fn) (:matrix layer-before))))))

    (testing "no conversion, already in imperial"
      (let [layer-after (convert/to-imperial layer-before {:units :imperial} :temperature)]

        (is (= (:matrix layer-after) (:matrix layer-before)))))))
