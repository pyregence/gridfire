(ns gridfire.outputs)

(defn write-landfire-layers!
  [{:keys [output-landfire-inputs? outfile-suffix landfire-rasters envelope]}]
  (when output-landfire-inputs?
    (doseq [[layer matrix] landfire-rasters]
      (-> (matrix-to-raster (name layer) matrix envelope)
          (write-raster (str (name layer) outfile-suffix ".tif"))))))

(defn write-burn-probability-layer!
  [{:keys [output-burn-probability simulations envelope output-pngs?] :as inputs} {:keys [burn-count-matrix]}]
  (when-let [timestep output-burn-probability]
    (let [output-name "burn_probability"]
      (if (int? timestep)
        (doseq [[band matrix] (map-indexed vector burn-count-matrix)]
          (let [output-time        (* band timestep)
                probability-matrix (m/emap #(/ % simulations) matrix)]
            (output-geotiff inputs probability-matrix output-name envelope nil output-time)
            (output-png inputs probability-matrix output-name envelope nil output-time)))
        (let [probability-matrix (m/emap #(/ % simulations) burn-count-matrix)]
          (output-geotiff inputs probability-matrix output-name envelope)
          (when output-pngs?
            (output-png inputs probability-matrix output-name envelope)))))))

(defn write-flame-length-sum-layer!
  [{:keys [envelope output-flame-length-sum?] :as inputs}
   {:keys [flame-length-sum-matrix]}]
  (when output-flame-length-sum?
    (output-geotiff inputs flame-length-sum-matrix "flame_length_sum" envelope)))

(defn write-flame-length-max-layer!
  [{:keys [envelope output-flame-length-max?] :as inputs}
   {:keys [flame-length-max-matrix]}]
  (when output-flame-length-max?
    (output-geotiff inputs flame-length-max-matrix "flame_length_max" envelope)))

(defn write-burn-count-layer!
  [{:keys [envelope output-burn-count?] :as inputs}
   {:keys [burn-count-matrix]}]
  (when output-burn-count?
    (output-geotiff inputs burn-count-matrix "burn_count" envelope)))

(defn write-spot-count-layer!
  [{:keys [envelope output-spot-count?] :as inputs}
   {:keys [spot-count-matrix]}]
  (when output-spot-count?
    (output-geotiff inputs spot-count-matrix "spot_count" envelope)))

(defn write-aggregate-layers!
  [inputs outputs]
  (write-burn-probability-layer! inputs outputs)
  (write-flame-length-sum-layer! inputs outputs)
  (write-flame-length-max-layer! inputs outputs)
  (write-burn-count-layer! inputs outputs)
  (write-spot-count-layer! inputs outputs))

(defn write-csv-outputs!
  [{:keys [output-csvs? output-directory outfile-suffix]} {:keys [summary-stats]}]
  (when output-csvs?
    (let [output-filename (str "summary_stats" outfile-suffix ".csv")]
      (with-open [out-file (io/writer (if output-directory
                                        (str/join "/" [output-directory output-filename])
                                        output-filename))]
        (->> summary-stats
             (sort-by #(vector (:ignition-row %) (:ignition-col %)))
             (mapv (fn [{:keys [ignition-row ignition-col max-runtime temperature relative-humidity
                                wind-speed-20ft wind-from-direction foliar-moisture ellipse-adjustment-factor
                                fire-size flame-length-mean flame-length-stddev fire-line-intensity-mean
                                fire-line-intensity-stddev simulation crown-fire-size spot-count]}]
                     [simulation
                      ignition-row
                      ignition-col
                      max-runtime
                      temperature
                      relative-humidity
                      wind-speed-20ft
                      wind-from-direction
                      foliar-moisture
                      ellipse-adjustment-factor
                      fire-size
                      flame-length-mean
                      flame-length-stddev
                      fire-line-intensity-mean
                      fire-line-intensity-stddev
                      crown-fire-size
                      spot-count]))
             (cons ["simulation" "ignition-row" "ignition-col" "max-runtime" "temperature" "relative-humidity"
                    "wind-speed-20ft" "wind-from-direction" "foliar-moisture" "ellipse-adjustment-factor"
                    "fire-size" "flame-length-mean" "flame-length-stddev" "fire-line-intensity-mean"
                    "fire-line-intensity-stddev" "crown-fire-size" "spot-count"])
             (csv/write-csv out-file))))))
