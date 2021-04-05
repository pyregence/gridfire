#!/bin/bash

rm -f *.bil *.hdr *.tif

if [ -z "$ELMFIRE_VER" ]; then
   ELMFIRE_VER=0.6548
fi
ELMFIRE_POST=elmfire_post_$ELMFIRE_VER
MPIRUN=/usr/bin/mpirun
HOSTS=`printf "$(hostname),%.0s" {1..64}`
NP=`grep processor /proc/cpuinfo | wc -l`
CELLSIZE=`cat ../elmfire.data | grep COMPUTATIONAL_DOMAIN_CELLSIZE | cut -d= -f2 | xargs`
XLLCORNER=`cat ../elmfire.data | grep COMPUTATIONAL_DOMAIN_XLLCORNER | cut -d= -f2 | xargs`
YLLCORNER=`cat ../elmfire.data | grep COMPUTATIONAL_DOMAIN_YLLCORNER | cut -d= -f2 | xargs`
NCASES=`tail -n +2 summary_stats.csv | wc -l`
DT=3600.
SIMULATION_TSTOP=`cat ../elmfire.data | grep SIMULATION_TSTOP | cut -d= -f2 | xargs`
NUM_TIMESTEPS=`echo "$SIMULATION_TSTOP / $DT" | bc -l | cut -d. -f1`
NX=`gdalinfo ../fuels_and_topography/slp.tif | grep "Size is" | cut -ds -f2 | cut -d, -f1 | xargs`
NY=`gdalinfo ../fuels_and_topography/slp.tif | grep "Size is" | cut -ds -f2 | cut -d, -f2 | xargs`

echo '&ELMFIRE_POST_INPUTS'                            > elmfire_post.data
echo "NX = $NX"                                       >> elmfire_post.data
echo "NY = $NY"                                       >> elmfire_post.data
echo "NCASES = $NCASES"                               >> elmfire_post.data
echo "XLLCORNER = $XLLCORNER"                         >> elmfire_post.data
echo "YLLCORNER = $YLLCORNER"                         >> elmfire_post.data
echo "CELLSIZE = $CELLSIZE"                           >> elmfire_post.data
echo "OUTPUTS_DIRECTORY = './'"                       >> elmfire_post.data
echo "DT = $DT"                                       >> elmfire_post.data
echo "NUM_TIMESTEPS = $NUM_TIMESTEPS"                 >> elmfire_post.data
echo "POSTPROCESS_TYPE = 1"                           >> elmfire_post.data
echo "BINARY_FILE_TYPE = 2"                           >> elmfire_post.data
echo "FIRE_SIZE_STATS_FILENAME = 'summary_stats.csv'" >> elmfire_post.data
echo "/"                                              >> elmfire_post.data

OMP_PROC_BIND=true $MPIRUN --mca btl tcp,self,vader --map-by core --bind-to core -np $NP -host $HOSTS $ELMFIRE_POST elmfire_post.data 1> /dev/null

echo "Built .bil & .hdr files"
