#!/bin/bash

NP=`grep processor /proc/cpuinfo | wc -l`
MODEL=gridfire
FIRENAME=`echo $(pwd) | rev | cut -d'/' -f2 | rev | cut -d_ -f1`
START_DATE=`echo $(pwd) | rev | cut -d'/' -f2 | rev | cut -d_ -f2`
START_TIME=`echo $(pwd) | rev | cut -d'/' -f2 | rev | cut -d_ -f3`
START_HOUR=${START_TIME:0:2}
START_MIN=${START_TIME:2:2}
start_min=$((10#$START_MIN))

rm -f -r geoserver
mkdir geoserver
mkdir geoserver/$FIRENAME
mkdir geoserver/$FIRENAME/${START_DATE}_${START_TIME}
mkdir geoserver/$FIRENAME/${START_DATE}_${START_TIME}/gridfire/
mkdir geoserver/$FIRENAME/${START_DATE}_${START_TIME}/gridfire/landfire/

for BP in 10 30 50 70 90; do
    mkdir ./geoserver/$FIRENAME/${START_DATE}_$START_TIME/gridfire/landfire/$BP
    cp ./imagemosaic_properties.zip ./geoserver/$FIRENAME/${START_DATE}_$START_TIME/gridfire/landfire/$BP/
    #   gdal_contour -i 12 time-of-arrival_$BP.tif ./geoserver/$FIRENAME/${START_DATE}_$START_TIME/gridfire/landfire/$BP/isochrones.shp &
done

rename () {
    local f=$1
    local BP=`basename $f | cut -d_ -f2`
    local FRAME=`basename $f | cut -d_ -f3 | cut -d. -f1`
    local frame=$((10#$FRAME))
    local MINUTES
    if [ "$frame" = "1" ]; then
        local TIMESTAMP=`date -u -d "$START_DATE $START_HOUR:$START_MIN UTC" +"%Y%m%d_%H%M00"`
    else
        let "MINUTES = (frame - 1) * 60 - start_min"
        local TIMESTAMP=`date -u -d "$START_DATE $START_HOUR:$START_MIN UTC + $MINUTES minutes" +"%Y%m%d_%H%M00"`
    fi
    local RASTER=`basename $f | cut -d_ -f1`
    mv $f ./geoserver/$FIRENAME/${START_DATE}_${START_TIME}/gridfire/landfire/$BP/${RASTER}_$TIMESTAMP.tif
}

(
    for f in `ls crown-fire*.tif flame-length*.tif hours-since-burned*.tif spread-rate*.tif 2> /dev/null`; do
        ((i=i%NP)); ((i++==0)) && wait
        rename "$f" &
    done
)
wait

for BP in 10 30 50 70 90; do
    mv time-of-arrival_$BP.tif ./geoserver/$FIRENAME/${START_DATE}_$START_TIME/gridfire/landfire/$BP/time-of-arrival.tif
done

cd geoserver
echo "Creating tarball for GeoServer"
tar -cvf $MODEL-$FIRENAME-${START_DATE}_${START_TIME}.tar * >& /dev/null

exit 0
