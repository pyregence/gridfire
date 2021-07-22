#!/bin/bash

MODEL=gridfire
FIRENAME=`echo $(pwd) | rev | cut -d'/' -f2 | rev | cut -d_ -f1`
START_DATE=`echo $(pwd) | rev | cut -d'/' -f2 | rev | cut -d_ -f2`
START_TIME=`echo $(pwd) | rev | cut -d'/' -f2 | rev | cut -d_ -f3`

echo "Uploaded tarball to GeoServer"
cd geoserver
scp  $MODEL-$FIRENAME-${START_DATE}_${START_TIME}.tar gridfire@data.pyregence.org:/incoming/match_drop/
mv *.tar ..

echo "Extracting tarball on data.pyregence.org"
ssh gridfire@data.pyregence.org \
    tar -xf /incoming/match_drop/$MODEL-$FIRENAME-${START_DATE}_${START_TIME}.tar \
    -C /var/www/html/fire_spread_forecast --no-overwrite-dir

exit 0
