#!/bin/bash

cd geoserver
scp  $MODEL-$FIRENAME-${START_DATE}_${START_TIME}.tar gridfire@data.pyregence.org:/incoming/
mv *.tar ..
ssh gridfire@data.pyregence.org \
    tar -xf /incoming/$MODEL-$FIRENAME-${START_DATE}_${START_TIME}.tar \
    -C /var/www/html/fire_spread_forecast

echo "Uploaded tarball to GeoServer"

exit 0
