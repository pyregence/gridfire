;; [[file:../../../org/GridFire.org::resolve-spotting-lognormal-sardoy][resolve-spotting-lognormal-sardoy]]
(ns gridfire.spotting.sardoy)

;; IMPROVEMENT allow for this model to be configured instead of the Elmfire one.
(defn sardoy-resolve-mu-x
  ^double [spotting-config ^double fire-line-intensity ^double wind-speed-20ft]
  (+ (* (double (:delta-x-ln-mu-a spotting-config))
        (double
         (* (Math/pow (-> fire-line-intensity
                          ;; kW/m -> MW/m
                          (* 1e-3))
                      (double (:delta-x-ln-mu-p spotting-config)))
            (Math/pow wind-speed-20ft                       ; m/s
                      (double (:delta-x-ln-mu-q spotting-config))))))
     (double (:delta-x-ln-mu-b spotting-config))))

(defn sardoy-resolve-sigma-x
  ^double [spotting-config ^double fire-line-intensity ^double wind-speed-20ft]
  (+ (* (double (:delta-x-ln-sigma-a spotting-config))
        (* (Math/pow (-> fire-line-intensity
                         ;; kW/m -> MW/m
                         (* 1e-3))
                     (double (:delta-x-ln-sigma-p spotting-config)))
           (Math/pow wind-speed-20ft                        ; m/s
                     (double (:delta-x-ln-sigma-q spotting-config)))))
     (double (:delta-x-ln-sigma-b spotting-config))))
;; resolve-spotting-lognormal-sardoy ends here
