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

    gdal_contour -i 12 time-of-arrival_$PERC.tif ./geoserver_new/$FIRENAME/${START_DATE}_${START_TIME}/gridfire/landfire/$PERC/isochrones_${FIRENAME}_${START_DATE}_${START_TIME}_$PERC.shp &
    gdal_contour -i 12 time-of-arrival_$PERC.tif ./geoserver/$FIRENAME/${START_DATE}_${START_TIME}/gridfire/landfire/$PERC/isochrones_${FIRENAME}_${START_DATE}_${START_TIME}_$PERC.shp &

done

(
    # for f in `ls crown-fire*.tif flame-length*.tif hours-since-burned*.tif spread-rate*.tif 2> /dev/null`; do
    for f in `ls hours-since-burned*.tif 2> /dev/null`; do
        ((i=i%NP)); ((i++==0)) && wait
        rename "$f" &
    done
)
wait


for PERC in 10 20 30 40 50 60 70 80 90; do
   cp time-of-arrival_$PERC.tif ./geoserver/$FIRENAME/${START_DATE}_$START_TIME/gridfire/landfire/$PERC/time-of-arrival.tif

   rm -f ./scratch/intermediate*
   gdal_calc.py -A time-of-arrival_$PERC.tif --type=Byte --calc="(A>0)*$PERC" --outfile=./scratch/intermediate_multi.tif
   gdal_polygonize.py ./scratch/intermediate_multi.tif ./scratch/intermediate_multi.shp
   ogr2ogr -progress -skipfailures ./scratch/intermediate.shp ./scratch/intermediate_multi.shp -dialect sqlite -sql "SELECT ST_Union(geometry) as geometry FROM intermediate_multi"
   set_percentile ./scratch/intermediate.shp $PERC

   if [ "$PERC" = "10" ]; then
      ogr2ogr ./scratch/out.shp ./scratch/intermediate.shp
   else
      ogr2ogr -update -append ./scratch/out.shp ./scratch/intermediate.shp -nln out
   fi

   mv time-of-arrival_$PERC.tif ./geoserver_new/$FIRENAME/${START_DATE}_$START_TIME/gridfire/landfire/$PERC/time-of-arrival.tif
   for RASTER in crown-fire flame-length spread-rate; do
      rm -f ./geoserver/$FIRENAME/${START_DATE}_$START_TIME/gridfire/landfire/$PERC/$RASTER*.tif
   done
   rmdir ./geoserver/$FIRENAME/${START_DATE}_$START_TIME/gridfire/landfire/$PERC/* 2> /dev/null
done

if [ -e ./scratch/burning.shp ]; then
   ogr2ogr ./scratch/burning_onefeature.shp ./scratch/burning.shp -dialect sqlite -sql "SELECT ST_Union(geometry) as geometry FROM burning"
   set_percentile ./scratch/burning_onefeature.shp -1
   ogr2ogr -update -append ./scratch/out.shp ./scratch/burning_onefeature.shp -nln out
fi

if [ -e ./scratch/already_burned.shp ]; then
   ogr2ogr ./scratch/already_burned_onefeature.shp ./scratch/already_burned.shp -dialect sqlite -sql "SELECT ST_Union(geometry) as geometry FROM already_burned"
   set_percentile ./scratch/already_burned_onefeature.shp -2
   ogr2ogr -update -append ./scratch/out.shp ./scratch/already_burned_onefeature.shp -nln out
fi

ogr2ogr ./scratch/${FIRENAME}_${START_DATE}_${START_TIME}_gridfire.shp ./scratch/out.shp &

wait

# Convert to MDT and create individual shapefiles
HH=${START_TIME:0:2}
MM=${START_TIME:2:2}
MDT=`date -u -d "$START_DATE $HH:$MM UTC - 6 hours" +%Y%m%d_%H%M`
ogr2ogr -fid 0 ./scratch/${FIRENAME}_${MDT}_gridfire_10.shp ./scratch/${FIRENAME}_${START_DATE}_${START_TIME}_gridfire.shp &
ogr2ogr -fid 1 ./scratch/${FIRENAME}_${MDT}_gridfire_20.shp ./scratch/${FIRENAME}_${START_DATE}_${START_TIME}_gridfire.shp &
ogr2ogr -fid 2 ./scratch/${FIRENAME}_${MDT}_gridfire_30.shp ./scratch/${FIRENAME}_${START_DATE}_${START_TIME}_gridfire.shp &
ogr2ogr -fid 3 ./scratch/${FIRENAME}_${MDT}_gridfire_40.shp ./scratch/${FIRENAME}_${START_DATE}_${START_TIME}_gridfire.shp &
ogr2ogr -fid 4 ./scratch/${FIRENAME}_${MDT}_gridfire_50.shp ./scratch/${FIRENAME}_${START_DATE}_${START_TIME}_gridfire.shp &
ogr2ogr -fid 5 ./scratch/${FIRENAME}_${MDT}_gridfire_60.shp ./scratch/${FIRENAME}_${START_DATE}_${START_TIME}_gridfire.shp &
ogr2ogr -fid 6 ./scratch/${FIRENAME}_${MDT}_gridfire_70.shp ./scratch/${FIRENAME}_${START_DATE}_${START_TIME}_gridfire.shp &
ogr2ogr -fid 7 ./scratch/${FIRENAME}_${MDT}_gridfire_80.shp ./scratch/${FIRENAME}_${START_DATE}_${START_TIME}_gridfire.shp &
ogr2ogr -fid 8 ./scratch/${FIRENAME}_${MDT}_gridfire_90.shp ./scratch/${FIRENAME}_${START_DATE}_${START_TIME}_gridfire.shp &
wait
zip -9 -j /home/gridfire/gridfire/runs/fire_forecasts/rsync/${FIRENAME}_${MDT}_gridfire.zip ./scratch/${FIRENAME}_${MDT}_gridfire*

for GEOSERVER_DIR in geoserver geoserver_new; do
   for PERC in 20 40 60 80; do
      rm -f -r ./$GEOSERVER_DIR/$FIRENAME/${START_DATE}_${START_TIME}/gridfire/landfire/$PERC
   done
done


# Process original geoserver directory
cd geoserver
echo "Creating tarball for GeoServer (Prod)"
tar -cvf $FIRENAME-${START_DATE}_$START_TIME.tar * >& /dev/null

exit 0
