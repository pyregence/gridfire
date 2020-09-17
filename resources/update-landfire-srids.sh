# [[file:../org/GridFire.org::*Importing Rasters into the Database][Importing Rasters into the Database:5]]
#!/bin/sh

SCHEMA=landfire
SRID=900914

for LAYER in asp cbd cbh cc ch dem fbfm13 fbfm40 slp
do
    psql -d gridfire -c \
         "SELECT UpdateRasterSRID('$SCHEMA'::name,'$LAYER'::name,'rast'::name,$SRID);" &
done
# Importing Rasters into the Database:5 ends here
