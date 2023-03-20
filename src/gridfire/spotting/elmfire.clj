;; [[file:../../../org/GridFire.org::resolve-spotting-lognormal-elmfire][resolve-spotting-lognormal-elmfire]]
(ns gridfire.spotting.elmfire
  "Resolution of spotting parameters as done in ELMFIRE.")

(defn resolve-exp-delta-x
  "Computes the expected value E[ΔX] of the downwind spotting distance ΔX (in meters), as a function of:
  - `spotting-config`: a map of spotting parameters,
  - `fire-line-intensity` (kW/m)
  - `wind-speed-20ft` (m/s)"
  ^double [spotting-config ^double fire-line-intensity ^double wind-speed-20ft]
  (let [a        (-> spotting-config :mean-distance (double))
        flin-exp (-> spotting-config :flin-exp (double))
        ws-exp   (-> spotting-config :ws-exp (double))]
    (* a
       (* (Math/pow fire-line-intensity flin-exp)
          (Math/pow wind-speed-20ft ws-exp)))))

(defn resolve-var-delta-x
  "Computes the variance Var[ΔX] (in m^2) of the downwind spotting distance ΔX, as a function of:
  - `spotting-config`: a map of spotting parameters,
  - `exp-delta-x`: E[ΔX] (in m)"
  ^double [spotting-config ^double exp-delta-x]
  (* (-> spotting-config :normalized-distance-variance (double))
     exp-delta-x))

(defn lognormal-mu-from-moments
  ^double [^double mean ^double variance]
  (let [m2 (Math/pow mean 2)]
    (Math/log (/ m2
                 (Math/sqrt (+ m2 variance))))))

(defn lognormal-sigma-from-moments
  ^double [^double mean ^double variance]
  (Math/sqrt (Math/log (+ 1.0
                          (/ variance
                             (Math/pow mean 2))))))

(defn resolve-lognormal-params
  [spotting-config ^double fire-line-intensity ^double wind-speed-20ft]
  (let [exp-delta-x (resolve-exp-delta-x spotting-config fire-line-intensity wind-speed-20ft)
        var-delta-x (resolve-var-delta-x spotting-config exp-delta-x)]
    {:prob.lognormal/mu    (lognormal-mu-from-moments exp-delta-x var-delta-x)
     :prob.lognormal/sigma (lognormal-sigma-from-moments exp-delta-x var-delta-x)}))
;; resolve-spotting-lognormal-elmfire ends here