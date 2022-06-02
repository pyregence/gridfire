#!/usr/bin/env bash

FIRENAME=`echo $(pwd) | rev | cut -d'/' -f2 | rev | cut -d_ -f1`
START_DATE=`echo $(pwd) | rev | cut -d'/' -f2 | rev | cut -d_ -f2`
START_TIME=`echo $(pwd) | rev | cut -d'/' -f2 | rev | cut -d_ -f3`
FIRE_SPREAD_FORECAST_PROD="/var/www/html/fire_spread_forecast"
FIRE_SPREAD_FORECAST_DEV="/var/www/html/fire_spread_forecast_dev"

echo "Uploading $FIRENAME-${START_DATE}_$START_TIME.tar to data.pyregence.org"
cd geoserver
scp $FIRENAME-${START_DATE}_$START_TIME.tar gridfire@data.pyregence.org:/home/gridfire/incoming/tar/gridfire-$FIRENAME-${START_DATE}_$START_TIME.tar

echo "Extracting tarball to ${FIRE_SPREAD_FORECAST_PROD} on data.pyregence.org"
ssh gridfire@data.pyregence.org "cd /home/gridfire/incoming/tar; tar -xf gridfire-$FIRENAME-${START_DATE}_$START_TIME.tar -C /var/www/html/fire_spread_forecast/ --no-overwrite-dir; rm -f gridfire-$FIRENAME-${START_DATE}_$START_TIME.tar; chmod g+rw ${FIRE_SPREAD_FORECAST_PROD_DIR}/${FIRENAME}/${START_DATE}_${START_TIME}"
mv *.tar ..

# Process new geoserver directory
cd ../geoserver_new
echo "Creating tarball for GeoServer (Dev)"
tar -cvf $FIRENAME-${START_DATE}_$START_TIME.tar * >& /dev/null

echo "Uploading $FIRENAME-${START_DATE}_$START_TIME.tar to data.pyregence.org"
scp $FIRENAME-${START_DATE}_$START_TIME.tar gridfire@data.pyregence.org:/incoming/gridfire-$FIRENAME-${START_DATE}_$START_TIME.tar

echo "Extracting tarball to ${FIRE_SPREAD_FORECAST_DEV} on data.pyregence.org"
ssh gridfire@data.pyregence.org "cd /incoming; tar -xf gridfire-$FIRENAME-${START_DATE}_$START_TIME.tar -C /var/www/html/fire_spread_forecast_dev/ --no-overwrite-dir; rm gridfire-$FIRENAME-${START_DATE}_$START_TIME.tar; chmod g+rw ${FIRE_SPREAD_FORECAST_DEV}/${FIRENAME}/${START_DATE}_${START_TIME}"
#mv *.tar ..

exit 0
