(ns gridfire.spec.config
  (:require [clojure.spec.alpha          :as s]
            [gridfire.spec.burn-period   :as burn-period]
            [gridfire.spec.common        :as common]
            [gridfire.spec.perturbations :as perturbations]
            [gridfire.spec.suppression   :as suppression]))

;;=============================================================================
;; Required Keys
;;=============================================================================

(s/def ::max-runtime     ::common/number-sample)
(s/def ::simulations     integer?)
(s/def ::srid            string?)
(s/def ::cell-size       number?)
(s/def ::foliar-moisture ::common/number-sample)

;; Weather

(s/def ::weather-start-timestamp inst?)

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
(s/def ::ellipse-adjustment-factor       ::common/number-sample)
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

(s/def ::ignition-start-timestamp inst?)
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
   :map     (common/one-or-more-keys [::ignition-mask ::edge-buffer])))

(s/def ::simulations-or-ignition-csv
  (fn [{:keys [simulations ignition-csv]}]
    (or simulations ignition-csv)))

(s/def ::max-runtime-or-ignition-csv
  (fn [{:keys [max-runtime ignition-csv]}]
    (or max-runtime ignition-csv)))

(s/def ::ignition-layer-or-ignition-csv
  (fn [{:keys [ignition-layer ignition-csv]}]
    (not (and ignition-layer ignition-csv))))

;; Fuel Moisture

(s/def ::1hr        ::common/ratio-or-layer-coords)
(s/def ::10hr       ::common/ratio-or-layer-coords)
(s/def ::100hr      ::common/ratio-or-layer-coords)
(s/def ::woody      ::common/ratio-or-layer-coords)
(s/def ::herbaceous ::common/ratio-or-layer-coords)

(s/def ::dead
  (s/keys :req-un [::1hr ::10hr ::100hr]))

(s/def ::live
  (s/keys :req-un [::woody ::herbaceous]))

(s/def ::fuel-moisture
  (s/keys :req-un [::dead ::live]))

(s/def ::rh-or-fuel-moisture
  (fn [{:keys [relative-humidity fuel-moisture]}]
    (or relative-humidity fuel-moisture)))

;; Spotting

(s/def ::num-firebrands               ::common/number-or-range-map)
(s/def ::mean-distance                ::common/number-or-range-map)
(s/def ::flin-exp                     ::common/number-or-range-map)
(s/def ::ws-exp                       ::common/number-or-range-map)
(s/def ::normalized-distance-variance ::common/number-or-range-map)
(s/def ::crown-fire-spotting-percent  ::common/ratio-or-range)

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

;; Suppression

(s/def ::suppression
  (s/and
   (s/keys :req-un [::suppression/suppression-dt]
           :opt-un [::suppression/suppression-curve-sharpness
                    ::suppression/sdi-layer
                    ::suppression/sdi-sensitivity-to-difficulty
                    ::suppression/sdi-containment-overwhelming-area-growth-rate
                    ::suppression/sdi-reference-suppression-speed])
   ::suppression/mutually-exclusive-keys))

;; Perturbations

(s/def ::perturbations
  (common/one-or-more-keys
   [::perturbations/canopy-base-height
    ::perturbations/canopy-cover
    ::perturbations/canopy-height
    ::perturbations/crown-bulk-density
    ::perturbations/temperature
    ::perturbations/relative-humidity
    ::perturbations/wind-speed-20ft
    ::perturbations/wind-direction
    ::perturbations/fuel-moisture-dead-1hr
    ::perturbations/fuel-moisture-dead-10hr
    ::perturbations/fuel-moisture-dead-100hr
    ::perturbations/fuel-moisture-live-herbaceous
    ::perturbations/fuel-moisture-live-woody
    ::perturbations/foliar-moisture]))

;; Outputs

(s/def ::output-directory        ::common/writable-directory)
(s/def ::outfile-suffix          string?)
(s/def ::output-landfire-inputs? boolean?)
(s/def ::output-geotiffs?        boolean?)
(s/def ::output-pngs?            boolean?)
(s/def ::output-csvs?            boolean?)
(s/def ::output-binary?          boolean?)

(s/def ::output-frequency
  (s/or :number number?
        :key    #{:final}))

(s/def ::fire-spread         ::output-frequency)
(s/def ::flame-length        ::output-frequency)
(s/def ::fire-line-intensity ::output-frequency)
(s/def ::spread-rate         ::output-frequency)
(s/def ::burn-history        ::output-frequency)
(s/def ::fire-type           ::output-frequency)

(s/def ::output-layers
  (common/one-or-more-keys
   [::fire-spread
    ::flame-length
    ::fire-line-intensity
    ::spread-rate
    ::burn-history
    ::fire-type]))

(s/def ::output-burn-probability  ::output-frequency) ; FIXME: Why isn't this also just boolean?
(s/def ::output-burn-count?       boolean?)
(s/def ::output-spot-count?       boolean?)
(s/def ::output-flame-length-max  #{:max :directional})
(s/def ::output-flame-length-sum  #{:max :directional})


;;=============================================================================
;; Spread Rate Adjustment
;;=============================================================================

(defn long? [x]
  (instance? Long x))

(s/def ::fuel-number->spread-rate-adjustment
  (s/map-of long? double?))

(s/def ::fuel-number->spread-rate-adjustment-samples
  (s/coll-of ::fuel-number->spread-rate-adjustment :kind vector?))

;;=============================================================================
;; Burn Period
;;=============================================================================

(s/def ::burn-period-required-keys
  (fn [{:keys [burn-period weather-start-timestamp]}]
    (or (nil? burn-period)
        weather-start-timestamp)))

;;=============================================================================
;; Timestamps
;;=============================================================================

(defn- not-after? [t1 t2]
  (<= (inst-ms t1) (inst-ms t2)))

(s/def ::valid-timestamps
  (fn [{:keys [ignition-start-timestamp weather-start-timestamp]}]
    (or (nil? ignition-start-timestamp)
        (and weather-start-timestamp
             ignition-start-timestamp
             (not-after? weather-start-timestamp ignition-start-timestamp)))))

;;=============================================================================
;; Mutually exclusive-keys
;;=============================================================================

(s/def ::mutually-exclusive-keys
  (fn [{:keys [ignition-start-timestamp ignition-csv]}]
    (or (and ignition-start-timestamp (nil? ignition-csv))
        (and (nil? ignition-start-timestamp) ignition-csv)
        (every? nil? [ignition-start-timestamp ignition-csv]))))

;;=============================================================================
;; Config Map
;;=============================================================================

(s/def ::config
  (s/and
   (s/keys
    :req-un [::srid
             ::cell-size
             ::foliar-moisture
             ::temperature
             ::wind-speed-20ft
             ::wind-from-direction
             ::landfire-layers]
    :opt-un [::max-runtime
             ::burn-period/burn-period
             ::simulations
             ::random-seed
             ::ellipse-adjustment-factor
             ::fractional-distance-combination
             ::fuel-number->spread-rate-adjustment-samples
             ::parallel-strategy
             ::db-spec
             ::ignition-row
             ::ignition-col
             ::ignition-layer
             ::ignition-csv
             ::ignition-start-timestamp
             ::weather-start-timestamp
             ::random-ignition
             ::relative-humidity
             ::fuel-moisture
             ::spotting
             ::suppression
             ::perturbations
             ::output-directory
             ::outfile-suffix
             ::output-landfire-inputs?
             ::output-geotiffs?
             ::output-pngs?
             ::output-csvs?
             ::output-binary?
             ::output-layers
             ::output-burn-probability
             ::output-burn-count?
             ::output-spot-count?
             ::output-flame-length-max
             ::output-flame-length-sum
             ::suppression/suppression-dt-samples
             ::suppression/suppression-curve-sharpnes-samples
             ::suppression/sdi-sensitivity-to-difficult-samples
             ::suppression/sdi-containment-overwhelming-area-growth-rate-samples
             ::suppression/sdi-reference-suppression-speed-samples])
   ::ignition-layer-or-ignition-csv
   ::max-runtime-or-ignition-csv
   ::simulations-or-ignition-csv
   ::rh-or-fuel-moisture
   ::burn-period-required-keys
   ::valid-timestamps
   ::mutually-exclusive-keys))
