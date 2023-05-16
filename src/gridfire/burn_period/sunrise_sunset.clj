;; [[file:../../../org/GridFire.org::gridfire.burn-period.sunrise-sunset][gridfire.burn-period.sunrise-sunset]]
(ns gridfire.burn-period.sunrise-sunset
  (:require [clojure.test :as test])
  (:import  (java.time Instant ZonedDateTime ZoneOffset)))

;; WARNING running this script in polar circles is at your own risk!

;;=============================================================================
;; Sunrise/sunset computations.
;;=============================================================================

;; Original Fortran code from ElmFire:
;!This gives the sunrise and sunset time in local time: (Chris Lautenberger)
;
;!**
;SUBROUTINE SUNRISE_SUNSET_CALCS(LON_DEG,LAT_DEG,UTC_OFFSET_HOURS,YEAR,HOUR_OF_YEAR)
;!**
;
;REAL, INTENT(IN) :: LON_DEG, LAT_DEG
;INTEGER, INTENT(IN) :: UTC_OFFSET_HOURS, YEAR, HOUR_OF_YEAR
;LOGICAL :: LEAPYEAR
;INTEGER :: DAY_OF_YEAR, HOUR_OF_DAY
;REAL :: DAYS_PER_YEAR, GAMMA, EQTIME, COSPIMTHETA, DECL, HA, HA_SUNRISE, LAT_RAD, LON_RAD, PHI, THETA, TIME_OFFSET, TST, &
;        SUNRISE_MIN_UTC, SUNRISE_H_LOCAL, SUNRISE_H_UTC, SUNSET_MIN_UTC, HA_SUNSET, SUNSET_H_UTC, SUNSET_H_LOCAL
;
;LON_RAD = LON_DEG * PI / 180.
;LAT_RAD = LAT_DEG * PI / 180.
;
;LEAPYEAR = .FALSE.
;IF (MOD(YEAR,4) .EQ. 0) LEAPYEAR = .TRUE.
;DAYS_PER_YEAR = 365.
;IF (LEAPYEAR) DAYS_PER_YEAR = 366.
;
;DAY_OF_YEAR = 1 + FLOOR(REAL(HOUR_OF_YEAR) / 24.)
;HOUR_OF_DAY = HOUR_OF_YEAR - (DAY_OF_YEAR - 1) * 24
;GAMMA = 2.0 * (PI/DAYS_PER_YEAR) * (DAY_OF_YEAR - 1)
;
;EQTIME = 229.18 * ( 0.000075 + 0.001868COS(GAMMA) - 0.032077SIN(GAMMA) - 0.014615COS(2.GAMMA) - 0.040849SIN(2.GAMMA) )
;DECL   = 0.006918 - 0.399912COS(GAMMA) + 0.070257SIN(GAMMA) - 0.006758COS(2.GAMMA) + 0.000907SIN(2.GAMMA) - 0.002697COS(3.GAMMA) + 0.00148SIN(3.GAMMA)
;
;! Begin part not needed for sunrise / sunset calcs:
;TIME_OFFSET = EQTIME + 4. * LON_DEG - 60. * UTC_OFFSET_HOURS
;TST = REAL(HOUR_OF_DAY) * 60. + TIME_OFFSET
;HA = 0.25 * TST - 180.
;PHI = ACOS(SIN(LAT_RAD)SIN(DECL)+COS(LAT_RAD)COS(DECL)COS(HAPI/180))
;COSPIMTHETA = (SIN(LAT_RAD)COS(PHI) - SIN(DECL)) / (COS(LAT_RAD)SIN(PHI))
;THETA = PI - ACOS(COSPIMTHETA)
;! End part not needed for sunrise / sunset calcs
;
;HA_SUNRISE = ACOS( COS(90.833PI/180) / (COS(LAT_RAD)COS(DECL)) -TAN(LAT_RAD)TAN(DECL) )
;SUNRISE_MIN_UTC = 720. - 4.(LON_DEG + HA_SUNRISE*180./PI) - EQTIME
;SUNRISE_H_UTC = SUNRISE_MIN_UTC / 60.
;SUNRISE_H_LOCAL = SUNRISE_H_UTC + UTC_OFFSET_HOURS
;
;HA_SUNSET = -HA_SUNRISE
;SUNSET_MIN_UTC = 720. - 4.(LON_DEG + HA_SUNSET180./PI) - EQTIME
;SUNSET_H_UTC = SUNSET_MIN_UTC / 60.
;SUNSET_H_LOCAL = SUNSET_H_UTC + UTC_OFFSET_HOURS
;
;SUNRISE_HOUR = SUNRISE_H_LOCAL ! Scope is global
;SUNSET_HOUR  = SUNSET_H_LOCAL ! Scope is global
;
;! *
;END SUBROUTINE SUNRISE_SUNSET_CALCS
;!**
;
;!Once sunrise / sunset have been determined, the burn period is controlled by two parameters - BURN_PERIOD_CENTER_FRACTION and BURN_PERIOD_LENGTH. This is the code:
;
;IF (USE_DIURNAL_ADJUSTMENT_FACTOR) THEN
;  BURN_PERIOD_CENTER_HOUR = SUNRISE_HOUR + BURN_PERIOD_CENTER_FRAC * (SUNSET_HOUR - SUNRISE_HOUR)
;  BURN_PERIOD_START_HOUR  = BURN_PERIOD_CENTER_HOUR - 0.5 * BURN_PERIOD_LENGTH
;  BURN_PERIOD_STOP_HOUR   = BURN_PERIOD_CENTER_HOUR + 0.5 * BURN_PERIOD_LENGTH
;
;  HOUR_OF_DAY = FORECAST_START_HOUR + T / 3600.0
;  HOUR_OF_DAY = MODULO(HOUR_OF_DAY, 24.)
;  IF (HOUR_OF_DAY .GT. BURN_PERIOD_START_HOUR .AND. HOUR_OF_DAY .LT. BURN_PERIOD_STOP_HOUR) THEN
;     DIURNAL_ADJUSTMENT_FACTOR = 1.0
;  ELSE
;     DIURNAL_ADJUSTMENT_FACTOR = OVERNIGHT_ADJUSTMENT_FACTOR
;  ENDIF
;ENDIF

(defn fourier-series-approximation
  "Computes an approximation of a real-valued angular-arg function by a (truncated) Fourier Series.
  Given:
  - cos+sin-coeffs: A sequence of [cos-coeff-k sin-coeff-k] pairs of doubles,
  corresponding to the terms of frequency k/(2*PI) rotations/rad,
  (sin-coeff-0 need not be specified)
  - gamma: an angle in [0, 2*PI]

  Returns the Fourier series evaluated at gamma,
  in the same physical unit as the coefficients."
  ^double
  [cos+sin-coeffs ^double gamma]
  (->> cos+sin-coeffs
       (map-indexed
        (fn [^long k [cos-coeff sin-coeff]]
          (let [sin-coeff (or sin-coeff 0.0)]
            (+
             (* (double cos-coeff) (Math/cos (* k gamma)))
             (* (double sin-coeff) (Math/sin (* k gamma)))))))
       (reduce + 0.0)))

(defn eqtime-at
  "[min] the correcting offset from the sunrise-sunset midpoint to UTC-clock noon,
  based on the time-of-year angle gamma."
  ^double [^double gamma]
  (*
   229.18
   (fourier-series-approximation
    [[0.000075]
     [0.001868 -0.032077]
     [-0.014615 -0.040849]]
    gamma)))

(defn declivity-at
  "[rad] the angle between the Earth's equatorial plane and its orbital plane,
  based on the time-of-year angle gamma."
  ^double [^double gamma]
  (fourier-series-approximation
   [[0.006918]
    [-0.399912 0.070257]
    [-0.006758 0.000907]
    [-0.002697 0.00148]]
   gamma))

(defn daylight-half-angle
  "[rad] (Approximately) half of the angle exposed to sunlight in a given iso-latitude circle.

  Arguments:
  - lat: latitude [rad],
  - decl: declivity [rad]"
  ^double [^double lat ^double decl]
  (Math/acos
   (+
    ;; Approximate cosine of daylight-half-angle, see e.g:
    ;; https://vvvvalvalval.github.io/posts/2019-12-03-inferring-earth-tilt-from-day-lengths.html#Physical-Model
    (* -1.0 (Math/tan lat) (Math/tan decl))
    ;; Small correction
    (/
     (Math/cos (* Math/PI (/ 90.833 180.0)))
     (* (Math/cos lat) (Math/cos decl))))))

(defn longitudinal-angle->duration
  "[min] converts an angular offset (in rad) to a time offset."
  ^double [^double lng-angle]
  (*
   ;; APPROXIMATE - the Earth actually rotates
   ;; a little more than 360° between 2 solar noons,
   ;; due to orbital movement
   ;; (about (360/365) ≈ 0.98° more, it depends on the time of the year,
   ;; but the error is something like 4 minutes per rotation).
   4.0 ;; [min/deg] (/ (* 24 60) 360)
   (/ 180.0 Math/PI) ;; [deg/rad]
   lng-angle))

(def ^:const min-per-hour 60.0)

(defn min->hr
  ^double [^double t-min]
  (/ t-min min-per-hour))

(defn deg->rad
  ^double [^double theta-deg]
  (-> theta-deg (* Math/PI) (/ 180.0)))

(defn sunrise-hour
  "[hr] UTC hour of the day at which the sun rises."
  ^double [^double lat ^double lng ^double gamma]
  (let [eqtime (eqtime-at gamma)
        decl   (declivity-at gamma)
        ha     (daylight-half-angle lat decl)]
    (+
     (- 12.0 (min->hr eqtime))
     (-> (- lng) (- ha)
         (longitudinal-angle->duration)
         (min->hr)))))

(defn sunset-hour
  "[hr] UTC hour of the day at which the sun sets."
  ^double [^double lat ^double lng ^double gamma]
  (let [eqtime (eqtime-at gamma)
        decl   (declivity-at gamma)
        ha     (daylight-half-angle lat decl)]
    (+
     (- 12.0 (min->hr eqtime))
     (-> (- lng) (+ ha)
         (longitudinal-angle->duration)
         (min->hr)))))

(test/deftest sunrise-sunset-reality-checks
  (test/is
   (<
    (sunrise-hour 0.3 0.0 Math/PI)
    (sunrise-hour 0.1 0.0 Math/PI))
   "In Northern summer, the sun rises earlier at higher latitudes.")
  (test/is
   (>
    (sunset-hour 0.3 0.0 Math/PI)
    (sunset-hour 0.1 0.0 Math/PI))
   "... and sets later.")
  (test/is
   (>
    (sunrise-hour -0.3 0.0 Math/PI)
    (sunrise-hour -0.1 0.0 Math/PI))
   "The situation is reversed in Southern Winter.")
  (test/is
   (>
    (sunrise-hour 0.3 0.0 0.0)
    (sunrise-hour 0.1 0.0 0.0))
   "... and Northern Winter.")
  (test/is
   (every? true?
           (for [lat   [-0.4 -0.2 0.0 0.1 0.3]
                 gamma [0.0 1.0 2.0 3.0 5.0 6.0]]
             (<
              (sunrise-hour lat 0.5 gamma)
              (sunrise-hour lat -0.5 gamma))))
   "The sun rises in the East before in the West.")
  (test/is
   (every? true?
           (for [lat   [-0.4 -0.2 0.0 0.1 0.3]
                 lng   [-3.0 -2.0 -1.0 0.0 1.0 2.0 3.0]
                 gamma [0.0 1.0 2.0 3.0 5.0 6.0]]
             (<
              (sunrise-hour lat lng gamma)
              (sunset-hour lat lng gamma))))
   "Sunrise is always before sunset.")
  (test/is
   (=
    (->> (range 12)
         (map (fn [mnth]
                (-> mnth (/ 12.0) (* 2.0 Math/PI))))
         (mapv (fn [gamma]
                 (-
                  (sunset-hour 0.0 0.0 gamma)
                  (sunrise-hour 0.0 0.0 gamma)))))
    [12.120711690930627
     12.116455880448317
     12.111944801470845
     12.111445309127586
     12.115166383631657
     12.119873606728143
     12.120728143894391
     12.116778690773202
     12.11228595353197
     12.11123549802302
     12.11458941491778
     12.119607952649702])
   "At the Equator, pretty much 12 hours of sunlight per day, all year long.")
  (test/is
   (=
    (->> (range 12)
         (map (fn [mnth]
                (-> mnth (/ 12.0) (* 2.0 Math/PI))))
         (mapv (fn [gamma]
                 (-
                  (sunset-hour 0.8 0.5 gamma)
                  (sunrise-hour 0.8 0.5 gamma)))))
    [8.725544159704336
     9.650457702575103
     11.16798773916028
     12.811517288872603
     14.359379610955962
     15.485521075449928
     15.66341255886112
     14.783619423056567
     13.3366494848639
     11.72573857583448
     10.149741212187232
     8.947654067059547])
   "The situation is clearly more varied at high latitudes."))

;;=============================================================================
;; Gridfire config transformation
;;=============================================================================

(defn instant-ms
  ^long [t]
  (comment "At the time of writing," clojure.core/inst-ms "is not implemented in Babashka :/")
  (cond
    (instance? Instant t)        (* 1000 (.getEpochSecond ^Instant t))
    (instance? java.util.Date t) (.getTime ^java.util.Date t)))

;; NOTE this function is approximate,
;; and also slightly inconsistent with the original Fortran code,
;; (which is also quite approximate,)
;; and that's fine.
;; The sensitivity on gamma is quite low,
;; so we can totally afford this sort of inaccuracy;
;; in fact, we can afford several days (degrees) of inaccuracy on gamma,
;; given how this script is used.
(defn gamma-at-instant
  "[rad] Converts an instant to a time-of-year angle."
  ^double [t]
  {:pre [(inst? t)]}
  (let [dt            (-> (instant-ms t)
                          (Instant/ofEpochMilli)
                          (.atZone ZoneOffset/UTC))
        y-floor       (->
                        (ZonedDateTime/of
                         (.getYear dt)
                         (int 1) (int 1)                   ; January 1st
                         (int 0) (int 0) (int 0)
                         (int 0)
                         ZoneOffset/UTC)
                        (.toInstant))
        y-ceil       (->
                       (ZonedDateTime/of
                        (-> (.getYear dt) (inc) (int))
                        (int 1) (int 1)                   ; January 1st
                        (int 0) (int 0) (int 0)
                        (int 0)
                        ZoneOffset/UTC)
                       (.toInstant))
        year-duration (-
                       (instant-ms y-ceil)
                       (instant-ms y-floor))]
    (->
      (instant-ms t)
      (double)
      (- (instant-ms y-floor))
      (/ year-duration)
      (* 2.0 Math/PI))))

(test/deftest gamma-at-instant-examples
  (test/is
   (= 0.0
      (/ (gamma-at-instant #inst "2022-01-01") (* 2.0 Math/PI))))
  (test/is
   (= 0.21643835616438353
      (/ (gamma-at-instant #inst "2022-03-21") (* 2.0 Math/PI))))
  (test/is
   (= 0.49589041095890407
      (/ (gamma-at-instant #inst "2022-07-01") (* 2.0 Math/PI))))
  (test/is
   (= 0.7753424657534247
      (/ (gamma-at-instant #inst "2022-10-11") (* 2.0 Math/PI)))))

(defn format-fractional-hour
  [^double h]
  (format "%02d:%02d"
          (-> h (Math/floor) (long) (mod 24))
          (-> h (rem 1.0) (* 60.0) (double) (Math/round) (long))))

(test/deftest format-fractional-hour-examples
  (test/is (= "09:12" (format-fractional-hour 9.2)))
  (test/is (= "19:45" (format-fractional-hour 19.75)))
  (test/is (= "00:00" (format-fractional-hour 0.0)))
  (test/is (= "23:59" (format-fractional-hour 23.99)))
  (test/is (= "02:30" (format-fractional-hour 26.5))))

(defn infer-burn-period
  "Resolves the :burn-period map,
  given a `bp-info` map which may contain the same keys
  as a regular GridFire :burn-period,
  and some more information specific to this script."
  [{ign-t     :ignition-start-timestamp
    bp-length :burn-period-length
    bp-frac   :burn-period-frac
    lat-deg   ::lat-deg
    lng-deg   ::lng-deg
    :or       {bp-frac 0.5}
    :as       _bp-info}]
  (let [gamma           (gamma-at-instant ign-t)
        lat             (deg->rad lat-deg)
        lng             (deg->rad lng-deg)
        h-sunrise       (sunrise-hour lat lng gamma)
        h-sunset        (sunset-hour lat lng gamma)
        [h-start h-end] (if (nil? bp-length)
                          [h-sunrise h-sunset]
                          (let [bp-half-length (* bp-length 0.5)
                                h-center       (+
                                                (* (- 1.0 bp-frac) h-sunrise)
                                                (* bp-frac h-sunset))]
                            [(-> h-center (- bp-half-length))
                             (-> h-center (+ bp-half-length))]))]
    {:burn-period-start (format-fractional-hour h-start)
     :burn-period-end   (format-fractional-hour h-end)}))

(test/deftest infer-burn-period-example
  (test/is
   (=
    ;; Almost correct, Météo France says 6:15 / 21:14.
    {:burn-period-start "04:15",
     :burn-period-end   "19:13"}
    (infer-burn-period
     {:ignition-start-timestamp #inst"2022-07-19T16:54:09.073-00:00"
      ::lat-deg                43.17
      ::lng-deg                5.60}))
   "correct sunrise/sunset hours for La Ciotat, France on 2022-07-19.")

  (test/is
   (=
    {:burn-period-start "08:14",
     :burn-period-end   "18:14"}
    (infer-burn-period
     {:ignition-start-timestamp #inst"2022-07-19T16:54:09.073-00:00"
      ::lat-deg                43.17
      ::lng-deg                5.60
      :burn-period-length      10.0
      ;; Centering on 15:14 :
      :burn-period-frac        (/ (- 15.0 6.0) (- 21.0 6.0))}))
   (str "if provided, correctly uses " (pr-str :burn-period-length) " and " (pr-str :burn-period-frac) ".")))

#_(test/run-tests)
;; gridfire.burn-period.sunrise-sunset ends here
