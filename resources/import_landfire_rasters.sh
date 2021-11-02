# [[file:../org/GridFire.org::raster2pgsql-import-example-all][raster2pgsql-import-example-all]]
#!/usr/bin/env bash

USERNAME=$1
SCHEMA=$2
SRID=$3

for LAYER in asp cbd cbh cc ch dem fbfm13 fbfm40 slp
do
    raster2pgsql -t auto -I -C -s $SRID $LAYER.tif $SCHEMA.$LAYER | psql -h localhost -U $USERNAME
done
# raster2pgsql-import-example-all ends here
