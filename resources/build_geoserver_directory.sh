#!/bin/bash

NP=`grep processor /proc/cpuinfo | wc -l`
MODEL=gridfire
FIRENAME=`echo $(pwd) | rev | cut -d'/' -f2 | rev | cut -d_ -f1`
START_DATE=`echo $(pwd) | rev | cut -d'/' -f2 | rev | cut -d_ -f2`
START_TIME=`echo $(pwd) | rev | cut -d'/' -f2 | rev | cut -d_ -f3`
START_HOUR=${START_TIME:0:2}

rm -f -r geoserver
mkdir geoserver
mkdir geoserver/$FIRENAME
mkdir geoserver/$FIRENAME/${START_DATE}_${START_TIME}
mkdir geoserver/$FIRENAME/${START_DATE}_${START_TIME}/gridfire/
mkdir geoserver/$FIRENAME/${START_DATE}_${START_TIME}/gridfire/landfire/
for BP in 10 20 30 40 50 60 70 80 90; do
   mkdir ./geoserver/$FIRENAME/${START_DATE}_${START_TIME}/gridfire/landfire/$BP
done

rename () {
   local f=$1
   local BP=`basename $f | cut -d_ -f2`
   local HOUR=`basename $f | cut -d_ -f3 | cut -d. -f1`
   local TIMESTAMP=`date -u -d "$START_DATE $START_HOUR:00 UTC + $HOUR hours" +"%Y%m%d_%H0000"`
   local RASTER=`basename $f | cut -d_ -f1`
   mv $f ./geoserver/$FIRENAME/${START_DATE}_${START_TIME}/gridfire/landfire/$BP/${RASTER}_$TIMESTAMP.tif
}

(
for f in *.tif; do
   ((i=i%NP)); ((i++==0)) && wait
   rename "$f" &
done
)
wait
sleep 0.5
echo "Geoserver directory Prepared"

cd geoserver
tar -cvf $MODEL-$FIRENAME-${START_DATE}_${START_TIME}.tar * >& /dev/null

echo "Created tarball for GeoServer"

exit 0
