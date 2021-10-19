# [[file:../org/GridFire.org::raster2pgsql-import-ignition-raster][raster2pgsql-import-ignition-raster]]
#!/usr/bin/env bash

USERNAME=$1
SCHEMA=$2
SRID=$3

LAYER="ign"
raster2pgsql -I -C -t auto -s $SRID $LAYER.tif $SCHEMA.$LAYER | psql -h localhost -U $USERNAME
# raster2pgsql-import-ignition-raster ends here
