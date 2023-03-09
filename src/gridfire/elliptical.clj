;; [[file:../../org/GridFire.org::gridfire.elliptical][gridfire.elliptical]]
(ns gridfire.elliptical
  "Computations based on the Elliptical Wavelets model.")

(defn fireline-normal-spread-rate-scalar
  "Computes the ratio of fireline-normal spread rate to maximal (heading) spread rate,
  in our idealized elliptical-wavelet model
  (which assumes wavelets shaped as ellipses growing around their focus point).

  Given:
  - eccentricity (dimensionless, in [0.0, 1.0)): the eccentricity of the elliptical fire-spread wavelet;
  - cos-ang (dimensionless, in [-1.0 1.0]): the cosine of the angle
  from the unit vector orthogonal to the fireline (pointing towards the unburned area)
  and the unit vector in the direction of maximum spread (in flat terrain, the wind direction)
  -equivalently, cos-ang is the dot-product between those unit vectors-
  e.g. cos-ang = 1.0 for heading fire, cos-ang = -1.0 for backing fire,
  and cos-ang = 0 for flanking fire;

  returns a number by which to downscale max-spread-rate to get
  the fireline-normal spread rate,
  i.e. the speed at which this segment of the fire line advances
  orthogonally to itself."
  ^double
  [^double eccentricity
   ^double cos-ang]
  (let [E         eccentricity
        E*cos-ang (* E cos-ang)]
    ;; NOTE for a mathematical derivation of this formula, see files:
    ;; org/elliptical/elliptical-wavelet-spread-rate-math.jpg
    ;; org/elliptical/elliptical-wavelet-spread-rate-math2.jpg
    (/ (+ E*cos-ang
          (Math/sqrt (+ (- 1.0 (* E E))
                        (* E*cos-ang E*cos-ang))))
       (+ 1.0 E))))
;; gridfire.elliptical ends here
