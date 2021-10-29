(ns gridfire.spec.config
  (:require [clojure.spec.alpha            :as s]
            [gridfire.spec.common          :as common]
            [gridfire.spec.optimization    :as optimization]
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
  (s/or :matrix ::common/layer-coords
        :global ::common/number-sample))

(s/def ::temperature         ::weather)
(s/def ::relative-humidity   ::weather)
(s/def ::wind-speed-20ft     ::weather)
(s/def ::wind-from-direction ::weather)

;; LANDFIRE

(s/def ::aspect             ::common/layer-coords)
(s/def ::canopy-base-height ::common/layer-coords)
(s/def ::canopy-cover       ::common/layer-coords)
(s/def ::canopy-height      ::common/layer-coords)
(s/def ::crown-bulk-density ::common/layer-coords)
(s/def ::elevation          ::common/layer-coords)
(s/def ::fuel-model         ::common/layer-coords)
(s/def ::slope              ::common/layer-coords)

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

(s/def ::burned   float?)
(s/def ::unburned float?)

(s/def ::burn-values
  (s/keys :req-un [::burned ::unburned]))

(s/def ::ignition-row ::common/integer-sample)
(s/def ::ignition-col ::common/integer-sample)

(s/def ::ignition-layer
  (s/and
   ::common/postgis-or-geotiff
   (s/keys :opt-un [::burn-values])))

;; Fuel Moisture

(s/def ::1hr        ::common/layer-coords)
(s/def ::10hr       ::common/layer-coords)
(s/def ::100hr      ::common/layer-coords)
(s/def ::woody      ::common/layer-coords)
(s/def ::herbaceous ::common/layer-coords)

(s/def ::dead
  (s/keys :req-un [::1hr ::10hr ::100hr]))

(s/def ::live
  (s/keys :req-un [::woody ::herbaceous]))

(s/def ::fuel-moisture-layers
  (s/keys :req-un [::dead ::live]))

;; Outputs

(s/def ::output-directory        ::common/file-path)
(s/def ::outfile-suffix          string?)
(s/def ::output-landfire-inputs? boolean?)
(s/def ::output-geotiffs?        boolean?)
(s/def ::output-pngs?            boolean?)
(s/def ::output-csvs?            boolean?)
(s/def ::output-binary?          boolean?)

(s/def ::burn-probability-type
  (s/or :scalar number?
        :key    #{:final}))

(s/def ::output-burn-probability ::burn-probability-type) ; FIXME: Why isn't this in :output-layers?
(s/def ::fire-spread             ::burn-probability-type)
(s/def ::flame-length            ::burn-probability-type)
(s/def ::fire-line-intensity     ::burn-probability-type)
(s/def ::spread-rate             ::burn-probability-type)
(s/def ::burn-history            ::burn-probability-type)

(s/def ::output-layers
  (common/one-or-more-keys
   [::fire-spread
    ::flame-length
    ::fire-line-intensity
    ::spread-rate
    ::burn-history]))

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
            ::ignition-layer
            ::fuel-moisture-layers
            ::output-directory
            ::outfile-suffix
            ::output-landfire-inputs?
            ::output-geotiffs?
            ::output-pngs?
            ::output-csvs?
            ::output-binary?
            ::output-burn-probability
            ::output-layers
            ::perturbations/perturbations
            ::random-ignition/random-ignition
            ::optimization/parallel-strategy
            ::spotting/spotting]))
