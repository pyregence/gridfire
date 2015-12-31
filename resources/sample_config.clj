{:max-runtime                 60.0   ;; double (minutes)
 :cell-size                   98.425 ;; double (feet)
 :wind-speed-20ft             20.0   ;; double (miles/hour)
 :wind-from-direction         90.0   ;; double (degrees clockwise from north)
 :fuel-moisture               {:dead {:1hr 0.06
                                      :10hr 0.07
                                      :100hr 0.08}
                               :live {:herbaceous 0.60
                                      :woody 0.90}} ;; doubles (%)
 :foliar-moisture             0.9 ;; double (%)
 :ellipse-adjustment-factor   1.0 ;; (< 1.0 = more circular, > 1.0 = more elliptical)
 :ignition-site               [25 40] ;; point represented as [row col]
 :db-spec                     {:classname   "org.postgresql.Driver"
                               :subprotocol "postgresql"
                               :subname     "//localhost:5432/gridfire"
                               :user        "gjohnson"}
 :landfire-layers             {:elevation          "landfire.dem WHERE rid=100"
                               :slope              "landfire.slp WHERE rid=100"
                               :aspect             "landfire.asp WHERE rid=100"
                               :fuel-model         "landfire.fbfm40 WHERE rid=100"
                               :canopy-height      "landfire.ch WHERE rid=100"
                               :canopy-base-height "landfire.cbh WHERE rid=100"
                               :crown-bulk-density "landfire.cbd WHERE rid=100"
                               :canopy-cover       "landfire.cc WHERE rid=100"}
 :srid                        "CALFIRE:900914"
 :fire-spread-outfile         "fire_spread.tif"
 :flame-length-outfile        "flame_length.tif"
 :fire-line-intensity-outfile "fire_line_intensity.tif"}
