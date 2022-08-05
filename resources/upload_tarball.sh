#!/usr/bin/env bash

# TODO Make into args
DEST_HOST=trinity
DEST_DIRECTORY_PROD=/home/gridfire/incoming/prod
DEST_DIRECTORY_DEV=/home/gridfire/incoming/dev
FIRE_SPREAD_FORECAST_PROD="/srv/gis/fire_spread_forecast"
FIRE_SPREAD_FORECAST_DEV="/srv/gis/fire_spread_forecast_dev"

FIRENAME=`echo $(pwd) | rev | cut -d'/' -f2 | rev | cut -d_ -f1`
START_DATE=`echo $(pwd) | rev | cut -d'/' -f2 | rev | cut -d_ -f2`
START_TIME=`echo $(pwd) | rev | cut -d'/' -f2 | rev | cut -d_ -f3`
FIRE=${FIRENAME}-${START_DATE}_${START_TIME}
echo "Uploading ${FIRENAME}-${START_DATE}_${START_TIME}.tar to gridfire@${DEST_HOST}"
cd geoserver
scp ${FIRE}.tar ${DEST_HOST}:${DEST_DIRECTORY_PROD}/gridfire-${FIRE}.tar

echo "Extracting tarball to ${FIRE_SPREAD_FORECAST_PROD} on gridfire@${DEST_HOST}"
ssh gridfire@${DEST_HOST} "rm -rf ${FIRE_SPREAD_FORECAST_PROD}/${FIRENAME}/${START_DATE}_${START_TIME}/gridfire; cd ${DEST_DIRECTORY_PROD}; tar -xf gridfire-${FIRE}.tar -C ${FIRE_SPREAD_FORECAST_PROD} --no-overwrite-dir; rm -f gridfire-${FIRE}.tar; chmod -R g+rw ${FIRE_SPREAD_FORECAST_PROD}/${FIRENAME}; chmod -R g+rw ${FIRE_SPREAD_FORECAST_PROD}/${FIRENAME}/${START_DATE}_${START_TIME}/gridfire"
mv *.tar ..

# Process new geoserver directory
cd ../geoserver_new
echo "Creating tarball for GeoServer (Dev)"
tar -cvf ${FIRE}.tar * >& /dev/null

echo "Uploading ${FIRENAME}-${START_DATE}_${START_TIME}.tar to gridfire@${DEST_HOST}"
scp ${FIRE}.tar gridfire@${DEST_HOST}:${DEST_DIRECTORY_DEV}/gridfire-${FIRE}.tar

echo "Extracting tarball to ${FIRE_SPREAD_FORECAST_DEV} on gridfire@${DEST_HOST}"
ssh gridfire@${DEST_HOST} "rm -rf ${FIRE_SPREAD_FORECAST_DEV}/${FIRENAME}/${START_DATE}_${START_TIME}/gridfire; cd ${DEST_DIRECTORY_DEV}; tar -xf gridfire-${FIRE}.tar -C ${FIRE_SPREAD_FORECAST_DEV} --no-overwrite-dir; rm gridfire-${FIRE}.tar; chmod -R g+rw ${FIRE_SPREAD_FORECAST_DEV}/${FIRENAME}; chmod -R g+rw ${FIRE_SPREAD_FORECAST_DEV}/${FIRENAME}/${START_DATE}_${START_TIME}/gridfire"
#mv *.tar ..

exit 0
