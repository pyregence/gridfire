#!/bin/bash

SRS=`gdalsrsinfo ../fuels_and_topography/asp.tif  | grep "PROJ.4" | cut -d':' -f2 | xargs`

if [ -z "$GDALROOT" ]; then
    GDALROOT=/usr/bin
fi
GDAL_TRANSLATE=$GDALROOT/gdal_translate

compress () {
    local f=$1
    IS_CROWN_FIRE=`echo $f | grep crown | wc -l`
    IS_HOURS=`echo $f | grep hours | wc -l`
    if [ "$IS_CROWN_FIRE" = "1" ] || [ "$IS_HOURS" = "1" ]; then
        OT=Byte
    else
        OT=Float32
    fi
    local STUB=`echo $f | cut -d. -f1`
    $GDAL_TRANSLATE -a_srs "$SRS" -a_nodata 0 -ot $OT -co "TILED=yes" -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" -co "NUM_THREADS=2" ${STUB}.bil ${STUB}.tif
    if [ $? -eq 0 ]; then
        rm -f ${STUB}.bil ${STUB}.hdr
    fi
}

N=`grep processor /proc/cpuinfo | wc -l`
(
    for f in *.bil; do
        ((i=i%N)); ((i++==0)) && wait
        compress "$f" &
    done
)

wait
sleep 0.5

exit 0
