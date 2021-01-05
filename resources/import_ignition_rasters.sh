# [[file:../org/GridFire.org::raster2pgsql-import-ignition-raster][raster2pgsql-import-ignition-raster]]
#!/bin/sh
USERNAME=$1
SCHEMA=$2

for LAYER in ign
do
    raster2pgsql -I -C $LAYER.tif $SCHEMA.$LAYER | psql -h localhost -U $USERNAME
done
# raster2pgsql-import-ignition-raster ends here
