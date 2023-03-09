;; [[file:../../../org/GridFire.org::gridfire.structs.rfwo][gridfire.structs.rfwo]]
(ns gridfire.structs.rfwo
  "Custom data structure for memoizing rothermel-fast-wrapper-optimal,
  isolated in this namespace to wrap JVM interop."
  (:import (clojure.lang IHashEq)
           (org.apache.commons.codec.digest MurmurHash3)))

;; NOTE it's good practice in Clojure to isolate custom JVM datatypes in their own small namespace;
;; doing so tends to prevent subtle issues when reloading the namespace during interactive development.
;; What matters is for the namespace's file to not be edited often.
(deftype RothFWOArgs
  ;; Custom datatype representing the argument to rothermel-fast-wrapper-optimal,
  ;; yielding much more efficient memoization.
  [^double fuel-model-number
   ^double fuel-moisture-dead-1hr
   ^double fuel-moisture-dead-10hr
   ^double fuel-moisture-dead-100hr
   ^double fuel-moisture-dead-herbaceous
   ^double fuel-moisture-live-herbaceous
   ^double fuel-moisture-live-woody
   ^boolean grass-suppression?]
  Object
  (equals [_this rothFWOArgs2]
    (let [^RothFWOArgs rothFWOArgs2 rothFWOArgs2]
      (and (= fuel-model-number             (.-fuel-model-number rothFWOArgs2))
           (= fuel-moisture-dead-1hr        (.-fuel-moisture-dead-1hr rothFWOArgs2))
           (= fuel-moisture-dead-10hr       (.-fuel-moisture-dead-10hr rothFWOArgs2))
           (= fuel-moisture-dead-100hr      (.-fuel-moisture-dead-100hr rothFWOArgs2))
           (= fuel-moisture-dead-herbaceous (.-fuel-moisture-dead-herbaceous rothFWOArgs2))
           (= fuel-moisture-live-herbaceous (.-fuel-moisture-live-herbaceous rothFWOArgs2))
           (= fuel-moisture-live-woody      (.-fuel-moisture-live-woody rothFWOArgs2))
           (= grass-suppression?            (.-grass-suppression? rothFWOArgs2)))))
  (hashCode [this] (.hasheq ^IHashEq this))
  IHashEq
  (hasheq [_this]
    (-> (MurmurHash3/hash32 (Double/doubleToRawLongBits fuel-model-number))
        (long) (MurmurHash3/hash32 (Double/doubleToRawLongBits fuel-moisture-dead-1hr))
        (long) (MurmurHash3/hash32 (Double/doubleToRawLongBits fuel-moisture-dead-10hr))
        (long) (MurmurHash3/hash32 (Double/doubleToRawLongBits fuel-moisture-dead-100hr))
        (long) (MurmurHash3/hash32 (Double/doubleToRawLongBits fuel-moisture-dead-herbaceous))
        (long) (MurmurHash3/hash32 (Double/doubleToRawLongBits fuel-moisture-live-herbaceous))
        (long) (MurmurHash3/hash32 (Double/doubleToRawLongBits fuel-moisture-live-woody))
        (long) (MurmurHash3/hash32 (long (if grass-suppression? 1 0))))))

;;-----------------------------------------------------------------------------
;; JVM interop wrappers.
;;-----------------------------------------------------------------------------

(defmacro make-RothFWOArgs
  "Fast constructor for RothFWOArgs, wrapping JVM interop."
  [& args]
  `(RothFWOArgs. ~@args))

;; Fast getters:

(defn get-fuel-model-number
  ^double [^RothFWOArgs rothFWOArgs]
  (.-fuel-model-number rothFWOArgs))

(defn get-fuel-moisture-vec
  [^RothFWOArgs rothFWOArgs]
  [(.-fuel-moisture-dead-1hr rothFWOArgs)
   (.-fuel-moisture-dead-10hr rothFWOArgs)
   (.-fuel-moisture-dead-100hr rothFWOArgs)
   (.-fuel-moisture-dead-herbaceous rothFWOArgs)
   (.-fuel-moisture-live-herbaceous rothFWOArgs)
   (.-fuel-moisture-live-woody rothFWOArgs)])

(defn get-grass-suppression?
  [^RothFWOArgs rothFWOArgs]
  (.-grass-suppression? rothFWOArgs))
;; gridfire.structs.rfwo ends here
