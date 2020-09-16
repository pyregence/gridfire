#!/bin/sh

DATABASE=gridfire_test
SCHEMA=landfire

for LAYER in asp cbd cbh cc ch dem fbfm13 fbfm40 slp
do
    raster2pgsql -t 100x100 -I -C $LAYER.tif $SCHEMA.$LAYER | psql -U postgres $DATABASE &
done
