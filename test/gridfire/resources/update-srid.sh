#!/bin/sh
for LAYER in asp cbd cbh cc ch dem fbfm13 fbfm40 slp
do psql -d gridfire_test -c \
        "SELECT
UpdateRasterSRID('landfire'::name,'$LAYER'::name,'rast'::name,900914);" &
done
