#!/bin/sh

DATABASE=gridfire_test
SCHEMA=landfire

for LAYER in asp cbd cbh cc ch dem fbfm13 fbfm40 slp
do
    if test -f $LAYER.tif; then
        raster2pgsql -t 100x100 -I -C $LAYER.tif $SCHEMA.$LAYER | psql -U postgres $DATABASE &
    fi
done
