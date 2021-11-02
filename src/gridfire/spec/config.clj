(ns gridfire.spec.config
  (:require [clojure.spec.alpha   :as s]
            [gridfire.spec.common :as common]))

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
(s/def ::fractional-distance-combination #{:sum}) ; FIXME This is currently unused.
(s/def ::parallel-strategy               #{:within-fires :between-fires})

;; DB Connection

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

(s/def ::ignition-row ::common/integer-sample)
(s/def ::ignition-col ::common/integer-sample)

(s/def ::burned   float?)
(s/def ::unburned float?)

(s/def ::burn-values
  (s/keys :req-un [::burned ::unburned]))

(s/def ::ignition-layer
  (s/and
   ::common/postgis-or-geotiff
   (s/keys :opt-un [::burn-values])))

(s/def ::ignition-csv ::common/readable-file)

(s/def ::ignition-mask ::common/postgis-or-geotiff)

(s/def ::edge-buffer number?)

(s/def ::random-ignition
  (s/or
   :boolean boolean?
   :map     (s/keys :req-un [::ignition-mask ::edge-buffer])))

;; Fuel Moisture

(s/def ::1hr        ::common/layer-coords)
(s/def ::10hr       ::common/layer-coords)
(s/def ::100hr      ::common/layer-coords)
(s/def ::woody      ::common/number-or-layer-coords)
(s/def ::herbaceous ::common/number-or-layer-coords)

(s/def ::dead
  (s/keys :req-un [::1hr ::10hr ::100hr]))

(s/def ::live
  (s/keys :req-un [::woody ::herbaceous]))

(s/def ::fuel-moisture-layers
  (s/keys :req-un [::dead ::live]))

(s/def ::rh-or-fuel-moisture
  (fn [{:keys [relative-humidity fuel-moisture-layers]}]
    (or relative-humidity fuel-moisture-layers)))

;; Spotting

(s/def ::num-firebrands               ::common/number-or-range-map)
(s/def ::mean-distance                ::common/number-or-range-map)
(s/def ::flin-exp                     ::common/number-or-range-map)
(s/def ::ws-exp                       ::common/number-or-range-map)
(s/def ::normalized-distance-variance ::common/number-or-range-map)
(s/def ::crown-fire-spotting-percent  ::common/percent-or-range)

(s/def ::valid-fuel-range             (fn [[lo hi]] (< 0 lo hi 205)))
(s/def ::fuel-number-range            (s/and ::common/integer-range ::valid-fuel-range))
(s/def ::fuel-percent-pair            (s/tuple ::fuel-number-range ::common/float-or-range))
(s/def ::spotting-percent             (s/coll-of ::fuel-percent-pair :kind vector?))
(s/def ::critical-fire-line-intensity number?)

(s/def ::surface-fire-spotting
  (s/keys :req-un [::spotting-percent
                   ::critical-fire-line-intensity]))

(s/def ::spotting
  (s/keys :req-un [::num-firebrands
                   ::mean-distance
                   ::flin-exp
                   ::ws-exp
                   ::normalized-distance-variance
                   ::crown-fire-spotting-percent]
          :opt-un [::surface-fire-spotting]))

;; Outputs

(s/def ::output-directory        ::common/file-path)
(s/def ::outfile-suffix          string?)
(s/def ::output-landfire-inputs? boolean?)
(s/def ::output-geotiffs?        boolean?)
(s/def ::output-pngs?            boolean?)
(s/def ::output-csvs?            boolean?)
(s/def ::output-binary?          boolean?)

(s/def ::burn-probability-type
  (s/or :number number?
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
  (s/and
   (s/keys
    :req-un [::max-runtime
             ::simulations
             ::srid
             ::cell-size
             ::foliar-moisture
             ::temperature
             ::wind-speed-20ft
             ::wind-from-direction
             ::landfire-layers]
    :opt-un [::random-seed
             ::ellipse-adjustment-factor
             ::fractional-distance-combination
             ::parallel-strategy
             ::db-spec
             ::ignition-row
             ::ignition-col
             ::ignition-layer
             ::ignition-csv
             ::random-ignition
             ::relative-humidity
             ::fuel-moisture-layers
             ::spotting
             ::output-directory
             ::outfile-suffix
             ::output-landfire-inputs?
             ::output-geotiffs?
             ::output-pngs?
             ::output-csvs?
             ::output-binary?
             ::output-burn-probability
             ::output-layers])
   ::rh-or-fuel-moisture))
