;; [[file:../../../org/GridFire.org::gridfire.structs.tensor-meta][gridfire.structs.tensor-meta]]
(ns gridfire.structs.tensor-meta)

(defprotocol IDoubleGetter
  (double-getter [this]))

;; A special-purpose record type meant to be used as a fast-reads metadata map on Tensor objects.
(defrecord GettersMeta
  [grid-lookup-double-getter]
  IDoubleGetter
  (double-getter [_this] grid-lookup-double-getter))
;; gridfire.structs.tensor-meta ends here
