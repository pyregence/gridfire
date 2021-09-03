#!/bin/bash

FIRENAME=`echo $(pwd) | rev | cut -d'/' -f2 | rev | cut -d_ -f1`
START_DATE=`echo $(pwd) | rev | cut -d'/' -f2 | rev | cut -d_ -f2`
START_TIME=`echo $(pwd) | rev | cut -d'/' -f2 | rev | cut -d_ -f3`

echo "Uploading tarball to data.pyregence.org"
cd geoserver
scp $FIRENAME-${START_DATE}_$START_TIME.tar gridfire@data.pyregence.org:/home/gridfire/incoming/tar/gridfire-$FIRENAME-${START_DATE}_$START_TIME.tar

echo "Extracting tarball on data.pyregence.org"
ssh gridfire@data.pyregence.org "cd /incoming; tar -xvf gridfire-$FIRENAME-${START_DATE}_$START_TIME.tar -C /var/www/html/fire_spread_forecast/ --no-overwrite-dir; rm -f gridfire-$FIRENAME-${START_DATE}_$START_TIME.tar"
mv *.tar ..

# Process new geoserver directory
cd ../geoserver_new
echo "Creating tarball for GeoServer"
tar -cvf $FIRENAME-${START_DATE}_$START_TIME.tar * >& /dev/null

echo "Uploading tarball to data.pyregence.org"
scp $FIRENAME-${START_DATE}_$START_TIME.tar gridfire@data.pyregence.org:/incoming/gridfire-$FIRENAME-${START_DATE}_$START_TIME.tar

echo "Extracting tarball on data.pyregence.org"
ssh gridfire@data.pyregence.org "cd /incoming; tar -xvf gridfire-$FIRENAME-${START_DATE}_$START_TIME.tar -C /var/www/html/fire_spread_forecast_dev/ --no-overwrite-dir; rm gridfire-$FIRENAME-${START_DATE}_$START_TIME.tar"
#mv *.tar ..

exit 0
