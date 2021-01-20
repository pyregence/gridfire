# [[file:../org/GridFire.org::raster2pgsql-import-weather-rasters][raster2pgsql-import-weather-rasters]]
#!/bin/sh

USERNAME=$1
SCHEMA=$2
SRID=$3

for LAYER in tmpf wd ws rh
do
    raster2pgsql -I -C -t auto -s $SRID ${LAYER}_to_sample.tif $SCHEMA.$LAYER | psql -h localhost -U $USERNAME
done
# raster2pgsql-import-weather-rasters ends here
