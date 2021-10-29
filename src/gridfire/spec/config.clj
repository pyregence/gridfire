(ns gridfire.spec.config
  (:require [clojure.spec.alpha            :as s]
            [gridfire.spec.common          :as common]
            [gridfire.spec.fuel-moisture   :as fuel-moisture]
            [gridfire.spec.ignition        :as ignition]
            [gridfire.spec.optimization    :as optimization]
            [gridfire.spec.output          :as output]
            [gridfire.spec.perturbations   :as perturbations]
            [gridfire.spec.random-ignition :as random-ignition]
            [gridfire.spec.spotting        :as spotting]))

;;=============================================================================
;; Required Keys
;;=============================================================================

(s/def ::max-runtime     number?)
(s/def ::simulations     integer?)
(s/def ::srid            string?)
(s/def ::cell-size       number?)
(s/def ::foliar-moisture number?)

;; Weather

(s/def ::weather
  (s/or :string string?
        :scalar number?
        :list   (s/coll-of number? :kind list?)
        :vector (s/coll-of number? :kind vector? :count 2)
        :map    ::common/postgis-or-geotiff))

(s/def ::temperature         ::weather)
(s/def ::relative-humidity   ::weather)
(s/def ::wind-speed-20ft     ::weather)
(s/def ::wind-from-direction ::weather)

;; LANDFIRE

(s/def ::path-or-map (s/or :path ::common/path
                           :map  ::common/postgis-or-geotiff))

(s/def ::aspect             ::path-or-map)
(s/def ::canopy-base-height ::path-or-map)
(s/def ::canopy-cover       ::path-or-map)
(s/def ::canopy-height      ::path-or-map)
(s/def ::crown-bulk-density ::path-or-map)
(s/def ::elevation          ::path-or-map)
(s/def ::fuel-model         ::path-or-map)
(s/def ::slope              ::path-or-map)

(s/def ::landfire-layers
  (s/keys
   :req-un [::aspect
            ::canopy-base-height
            ::canopy-cover
            ::canopy-height
            ::crown-bulk-density
            ::elevation
            ::fuel-model
            ::slope]))

;;=============================================================================
;; Optional Keys
;;=============================================================================

(s/def ::random-seed                     integer?)
(s/def ::ellipse-adjustment-factor       number?)
(s/def ::fractional-distance-combination #{:sum}) ; FIXME Is this the only option?

;; DB Specification

(s/def ::classname   string?)
(s/def ::subprotocol string?)
(s/def ::subname     string?)
(s/def ::user        string?)
(s/def ::password    string?)

(s/def ::db-spec
  (s/keys
   :req-un [::classname
            ::subprotocol
            ::subname
            ::user
            ::password]))

;; Ignitions

(s/def ::sample-value
  (s/or :scalar integer?
        :list   (s/coll-of integer? :kind list?)
        :vector (s/coll-of integer? :kind vector? :count 2)))

(s/def ::ignition-row ::sample-value)
(s/def ::ignition-col ::sample-value)

;; Outputs

(s/def ::outfile-suffix          string?)
(s/def ::output-landfire-inputs? boolean?)
(s/def ::output-geotiffs?        boolean?)
(s/def ::output-pngs?            boolean?)
(s/def ::output-csvs?            boolean?)

;;=============================================================================
;; Config Map
;;=============================================================================

(s/def ::config
  (s/keys
   :req-un [::max-runtime
            ::simulations
            ::srid
            ::cell-size
            ::foliar-moisture
            ::temperature
            ::relative-humidity
            ::wind-speed-20ft
            ::wind-from-direction
            ::landfire-layers]
   :opt-un [::random-seed
            ::ellipse-adjustment-factor
            ::fractional-distance-combination
            ::db-spec
            ::ignition-row
            ::ignition-col
            ::ignition/ignition-layer
            ::outfile-suffix
            ::output-landfire-inputs?
            ::output-geotiffs?
            ::output-pngs?
            ::output-csvs?
            ::output/output-binary?
            ::output/output-layers
            ::output/output-burn-probability
            ::perturbations/perturbations
            ::random-ignition/random-ignition
            ::fuel-moisture/fuel-moisture-layers
            ::optimization/parallel-strategy
            ::spotting/spotting]))
