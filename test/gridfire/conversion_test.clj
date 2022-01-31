(ns gridfire.conversion-test
  (:require [clojure.test             :refer [deftest is testing]]
            [gridfire.conversion      :as convert]
            [gridfire.magellan-bridge :refer [geotiff-raster-to-matrix]]
            [gridfire.utils.test      :as utils]
            [tech.v3.datatype :as d]
            [tech.v3.datatype.functional :as dfn]))

(def resources-path "test/gridfire/resources/conversion_test")

(deftest ^:unit canopy-height-test
  (let [layer         (geotiff-raster-to-matrix (utils/in-file-path resources-path "single-band.tif"))
        unit-fn       convert/m->ft
        multiplier-fn #(* 100 %)]

    (testing "convert metric"
      (let [layer-before (assoc layer :matrix (d/clone (:matrix layer)))
            layer-after  (convert/to-imperial! layer-before {:units :metric} :canopy-height)]
        (is (dfn/equals (:matrix layer-after) (d/clone (d/emap unit-fn nil (:matrix layer)))))))

    (testing "scale with multiplier"
      (let [layer-before (assoc layer :matrix (d/clone (:matrix layer)))
            layer-after  (convert/to-imperial! layer-before {:multiplier 100} :canopy-height)]

        (is (dfn/equals (:matrix layer-after) (d/clone (d/emap multiplier-fn nil (:matrix layer)))))))

    (testing "convert metric and scale with multiplier"
        (let [layer-before (assoc layer :matrix (d/clone (:matrix layer)))
              layer-after  (convert/to-imperial! layer-before {:units :metric :multiplier 100} :canopy-height)]

          (is (dfn/equals (:matrix layer-after) (d/clone (d/emap (comp unit-fn multiplier-fn) nil (:matrix layer)))))))

    (testing "no conversion, already in imperial"
        (let [layer-before (assoc layer :matrix (d/clone (:matrix layer)))
              layer-after  (convert/to-imperial! layer-before {:units :imperial} :canopy-height)]

          (is (dfn/equals (:matrix layer-after) (:matrix layer)))))))

(deftest ^:unit elevation-test
  (let [layer         (geotiff-raster-to-matrix (utils/in-file-path resources-path "single-band.tif"))
        unit-fn       convert/m->ft
        multiplier-fn #(* 100 %)]

    (testing "convert metric"
      (let [layer-before (assoc layer :matrix (d/clone (:matrix layer)))
            layer-after  (convert/to-imperial! layer-before {:units :metric} :elevation)]

        (is (dfn/equals (:matrix layer-after) (d/clone (d/emap unit-fn nil (:matrix layer)))))))

    (testing "scale with multiplier"
      (let [layer-before (assoc layer :matrix (d/clone (:matrix layer)))
            layer-after  (convert/to-imperial! layer-before {:multiplier 100} :elevation)]

        (is (dfn/equals (:matrix layer-after) (d/clone (d/emap multiplier-fn nil (:matrix layer)))))))

    (testing "convert metric and scale with multiplier"
      (let [layer-before (assoc layer :matrix (d/clone (:matrix layer)))
            layer-after  (convert/to-imperial! layer-before {:units :metric :multiplier 100} :elevation)]

        (is (dfn/equals (:matrix layer-after) (d/clone (d/emap (comp unit-fn multiplier-fn) nil (:matrix layer)))))))

    (testing "no conversion, already in imperial"
      (let [layer-before (assoc layer :matrix (d/clone (:matrix layer)))
            layer-after  (convert/to-imperial! layer-before {:units :imperial} :elevation)]

        (is (dfn/equals (:matrix layer-after) (:matrix layer)))))))

(deftest ^:unit canopy-base-height-test
  (let [layer         (geotiff-raster-to-matrix (utils/in-file-path resources-path "single-band.tif"))
        unit-fn       convert/m->ft
        multiplier-fn #(* 100 %)]

    (testing "convert metric"
      (let [layer-before (assoc layer :matrix (d/clone (:matrix layer)))
            layer-after  (convert/to-imperial! layer-before {:units :metric} :canopy-base-height)]

        (is (dfn/equals (:matrix layer-after) (d/clone (d/emap unit-fn nil (:matrix layer)))))))

    (testing "scale with multiplier"
      (let [layer-before (assoc layer :matrix (d/clone (:matrix layer)))
            layer-after  (convert/to-imperial! layer-before {:multiplier 100} :canopy-base-height)]

        (is (dfn/equals (:matrix layer-after) (d/clone (d/emap multiplier-fn nil (:matrix layer)))))))

    (testing "convert metric and scale with multiplier"
      (let [layer-before (assoc layer :matrix (d/clone (:matrix layer)))
            layer-after  (convert/to-imperial! layer-before {:units :metric :multiplier 100} :canopy-base-height)]

        (is (dfn/equals (:matrix layer-after) (d/clone (d/emap (comp unit-fn multiplier-fn) nil (:matrix layer)))))))

    (testing "no conversion, already in imperial"
      (let [layer-before (assoc layer :matrix (d/clone (:matrix layer)))
            layer-after  (convert/to-imperial! layer-before {:units :imperial} :canopy-base-height)]

        (is (dfn/equals (:matrix layer-after) (:matrix layer)))))))

(deftest ^:unit crown-bulk-density-test
  (let [layer           (geotiff-raster-to-matrix (utils/in-file-path resources-path "single-band.tif"))
        unit-fn         #(* 0.0624 %)
        multiplier-fn   #(* 100 %)]

    (testing "convert metric"
      (let [layer-before (assoc layer :matrix (d/clone (:matrix layer)))
            layer-after  (convert/to-imperial! layer-before {:units :metric} :crown-bulk-density)]

        (is (dfn/equals (:matrix layer-after) (d/clone (d/emap unit-fn nil (:matrix layer)))))))

    (testing "scale with multiplier"
      (let [layer-before (assoc layer :matrix (d/clone (:matrix layer)))
            layer-after  (convert/to-imperial! layer-before {:multiplier 100} :crown-bulk-density)]

        (is (dfn/equals (:matrix layer-after) (d/clone (d/emap multiplier-fn nil (:matrix layer)))))))

    (testing "convert metric and scale with multiplier"
      (let [layer-before (assoc layer :matrix (d/clone (:matrix layer)))
            layer-after  (convert/to-imperial! layer-before {:units :metric :multiplier 100} :crown-bulk-density)]

        (is (dfn/equals (:matrix layer-after) (d/clone (d/emap (comp unit-fn multiplier-fn) nil (:matrix layer)))))))

    (testing "no conversion, already in imperial"
      (let [layer-before (assoc layer :matrix (d/clone (:matrix layer)))
            layer-after  (convert/to-imperial! layer-before {:units :imperial} :crown-bulk-density)]

        (is (dfn/equals (:matrix layer-after) (:matrix layer)))))))

(deftest ^:unit wind-speed-20ft-test
  (let [layer           (geotiff-raster-to-matrix (utils/in-file-path resources-path "single-band.tif"))
        unit-fn         convert/mps->mph
        multiplier-fn   #(* 100 %)]

    (testing "convert metric"
      (let [layer-before (assoc layer :matrix (d/clone (:matrix layer)))
            layer-after  (convert/to-imperial! layer-before {:units :metric} :wind-speed-20ft)]

        (is (dfn/equals (:matrix layer-after) (d/clone (d/emap unit-fn nil (:matrix layer)))))))

    (testing "scale with multiplier"
      (let [layer-before (assoc layer :matrix (d/clone (:matrix layer)))
            layer-after  (convert/to-imperial! layer-before {:multiplier 100} :wind-speed-20ft)]

        (is (dfn/equals (:matrix layer-after) (d/clone (d/emap multiplier-fn nil (:matrix layer)))))))

    (testing "convert metric and scale with multiplier"
      (let [layer-before (assoc layer :matrix (d/clone (:matrix layer)))
            layer-after  (convert/to-imperial! layer-before {:units :metric :multiplier 100} :wind-speed-20ft)]

        (is (dfn/equals (:matrix layer-after) (d/clone (d/emap (comp unit-fn multiplier-fn) nil (:matrix layer)))))))

    (testing "no conversion, already in imperial"
      (let [layer-before (assoc layer :matrix (d/clone (:matrix layer)))
            layer-after  (convert/to-imperial! layer-before {:units :imperial} :wind-speed-20ft)]

        (is (dfn/equals (:matrix layer-after) (:matrix layer)))))))

(deftest ^:unit temperature-test
  (let [layer         (geotiff-raster-to-matrix (utils/in-file-path resources-path "single-band.tif"))
        multiplier-fn #(* 100 %)]

    (testing "convert metric"
      (let [layer-before (assoc layer :matrix (d/clone (:matrix layer)))
            layer-after  (convert/to-imperial! layer-before {:units :metric} :temperature)
            unit-fn      convert/C->F]

        (is (dfn/equals (:matrix layer-after) (d/clone (d/emap unit-fn nil (:matrix layer)))))))

    (testing "convert absolute"
      (let [layer-before (assoc layer :matrix (d/clone (:matrix layer)))
            layer-after  (convert/to-imperial! layer-before {:units :absolute} :temperature)
            unit-fn      convert/K->F]

        (is (dfn/equals (:matrix layer-after) (d/clone (d/emap unit-fn nil (:matrix layer)))))))

    (testing "scale with multiplier"
      (let [layer-before (assoc layer :matrix (d/clone (:matrix layer)))
            layer-after  (convert/to-imperial! layer-before {:multiplier 100} :temperature)]

        (is (dfn/equals (:matrix layer-after) (d/clone (d/emap multiplier-fn nil (:matrix layer)))))))

    (testing "convert metric and scale with multiplier"
      (let [layer-before (assoc layer :matrix (d/clone (:matrix layer)))
            layer-after  (convert/to-imperial! layer-before {:units :metric :multiplier 100} :temperature)
            unit-fn      convert/C->F]

        (is (dfn/equals (:matrix layer-after) (d/clone (d/emap (comp unit-fn multiplier-fn) nil (:matrix layer)))))))

    (testing "no conversion, already in imperial"
      (let [layer-before (assoc layer :matrix (d/clone (:matrix layer)))
            layer-after  (convert/to-imperial! layer-before {:units :imperial} :temperature)]

        (is (dfn/equals (:matrix layer-after) (:matrix layer)))))))
