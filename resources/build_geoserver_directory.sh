#!/usr/bin/env bash

NP=`grep processor /proc/cpuinfo | wc -l`
MODEL=gridfire
FIRENAME=`echo $(pwd) | rev | cut -d'/' -f2 | rev | cut -d_ -f1`
START_DATE=`echo $(pwd) | rev | cut -d'/' -f2 | rev | cut -d_ -f2`
START_TIME=`echo $(pwd) | rev | cut -d'/' -f2 | rev | cut -d_ -f3`
START_HOUR=${START_TIME:0:2}
START_MIN=${START_TIME:2:2}
start_min=$((10#$START_MIN))

rename () {
    local f=$1
    local PERC=`basename $f | cut -d_ -f2`
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
    cp $f     ./geoserver/$FIRENAME/${START_DATE}_${START_TIME}/gridfire/landfire/$PERC/${RASTER}_$TIMESTAMP.tif
    mv $f ./geoserver_new/$FIRENAME/${START_DATE}_${START_TIME}/gridfire/landfire/$PERC/$RASTER/${RASTER}_$TIMESTAMP.tif
}

echo "Preparing tifs for GeoServer"

for BASEDIR in geoserver geoserver_new; do
    rm -f -r $BASEDIR
    mkdir $BASEDIR
    mkdir $BASEDIR/$FIRENAME
    mkdir $BASEDIR/$FIRENAME/${START_DATE}_${START_TIME}
    mkdir $BASEDIR/$FIRENAME/${START_DATE}_${START_TIME}/gridfire
    mkdir $BASEDIR/$FIRENAME/${START_DATE}_${START_TIME}/gridfire/landfire
done

for PERC in 10 30 50 70 90; do
    mkdir     geoserver/$FIRENAME/${START_DATE}_${START_TIME}/gridfire/landfire/$PERC
    mkdir geoserver_new/$FIRENAME/${START_DATE}_${START_TIME}/gridfire/landfire/$PERC

    # for RASTER in crown-fire flame-length hours-since-burned spread-rate; do
    for RASTER in hours-since-burned; do
        mkdir geoserver_new/$FIRENAME/${START_DATE}_${START_TIME}/gridfire/landfire/$PERC/$RASTER
        cp -f imagemosaic_properties/*.properties geoserver_new/$FIRENAME/${START_DATE}_${START_TIME}/gridfire/landfire/$PERC/$RASTER/
    done

    # gdal_contour -i 12 time-of-arrival_$BP.tif ./geoserver_new/$FIRENAME/${START_DATE}_${START_TIME}/gridfire/landfire/$BP/isochrones.shp &

done

(
    # for f in `ls crown-fire*.tif flame-length*.tif hours-since-burned*.tif spread-rate*.tif 2> /dev/null`; do
    for f in `ls hours-since-burned*.tif 2> /dev/null`; do
        ((i=i%NP)); ((i++==0)) && wait
        rename "$f" &
    done
)
wait

# Process original geoserver directory
cd geoserver
echo "Creating tarball for GeoServer (Prod)"
tar -cvf $FIRENAME-${START_DATE}_$START_TIME.tar * >& /dev/null

exit 0
