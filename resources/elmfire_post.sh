#!/bin/bash

rm -f *.bil *.hdr *.tif


if [ -z "$ELMFIRE_VER" ]; then
    ELMFIRE_VER=0.6552
fi
ELMFIRE_POST=elmfire_post_$ELMFIRE_VER
MPIRUN=/usr/bin/mpirun
HOSTS=`printf "$(hostname),%.0s" {1..64}`
NP=`cat /proc/cpuinfo | grep "cpu cores" | cut -d: -f2 | tail -n 1 | xargs`

CELLSIZE=`cat ../elmfire.data | grep COMPUTATIONAL_DOMAIN_CELLSIZE | cut -d= -f2 | xargs`
XLLCORNER=`cat ../elmfire.data | grep COMPUTATIONAL_DOMAIN_XLLCORNER | cut -d= -f2 | xargs`
YLLCORNER=`cat ../elmfire.data | grep COMPUTATIONAL_DOMAIN_YLLCORNER | cut -d= -f2 | xargs`
NCASES=`tail -n +2 summary_stats.csv | wc -l`
DT=3600.
SIMULATION_TSTOP=`cat ../elmfire.data | grep SIMULATION_TSTOP | cut -d= -f2 | xargs`
NX=`gdalinfo ../fuels_and_topography/slp.tif | grep "Size is" | cut -ds -f2 | cut -d, -f1 | xargs`
NY=`gdalinfo ../fuels_and_topography/slp.tif | grep "Size is" | cut -ds -f2 | cut -d, -f2 | xargs`

NUM_TIMESTEPS=`echo "$SIMULATION_TSTOP / $DT" | bc -l | cut -d. -f1`
if [ "$start_min" != "0" ]; then
    let "NUM_TIMESTEPS = NUM_TIMESTEPS + 1"
fi

READ_ALREADY_BURNED=no
if [ -e ../fuels_and_topography/already_burned.tif ]; then
    rm -f ./already_burned_float.tif
    gdal_translate -ot Float32 ../fuels_and_topography/already_burned.tif  ./already_burned_float.tif
    READ_ALREADY_BURNED=yes
fi

READ_PHI=no
PHIMIN=`gdalinfo -stats ../fuels_and_topography/phi.tif  | grep STATISTICS_MINIMUM | cut -d= -f2 | xargs`
ISLT0=`echo "$PHIMIN < 0" | bc`
if [ "$ISLT0" = "1" ]; then
    cp -f ../fuels_and_topography/phi.tif ./
    READ_PHI=yes
fi

echo '&ELMFIRE_POST_INPUTS'                           > elmfire_post.data
echo "NX = $NX"                                      >> elmfire_post.data
echo "NY = $NY"                                      >> elmfire_post.data
echo "NCASES = $NCASES"                              >> elmfire_post.data
echo "N_PERCENTILES = 5"                             >> elmfire_post.data
echo "PERCENTILES(:) = 10.0, 30.0, 50.0, 70.0, 90.0" >> elmfire_post.data
echo "XLLCORNER = $XLLCORNER"                        >> elmfire_post.data
echo "YLLCORNER = $YLLCORNER"                        >> elmfire_post.data
echo "CELLSIZE = $CELLSIZE"                          >> elmfire_post.data
echo "OUTPUTS_DIRECTORY = '"$(pwd)"/'"               >> elmfire_post.data
echo "DT = $DT"                                      >> elmfire_post.data
echo "NUM_TIMESTEPS = $NUM_TIMESTEPS"                >> elmfire_post.data
echo "POSTPROCESS_TYPE = 1"                          >> elmfire_post.data
echo "HOURLY_PYREGENCE_OUTPUTS = .TRUE."             >> elmfire_post.data
echo "START_TIME_MINUTES_PAST_HOUR = $start_min"     >> elmfire_post.data
echo "DUMP_FLAME_LENGTH = .FALSE."                   >> elmfire_post.data
echo "DUMP_SPREAD_RATE = .FALSE."                    >> elmfire_post.data
echo "DUMP_CROWN_FIRE = .FALSE."                     >> elmfire_post.data
echo "DUMP_TIME_OF_ARRIVAL = .TRUE."                 >> elmfire_post.data
echo "SCRATCH = './scratch/'"                        >> elmfire_post.data
echo "PATH_TO_GDAL = '/usr/bin/'"                    >> elmfire_post.data
if [ "$READ_ALREADY_BURNED" = "yes" ]; then
   echo "READ_ALREADY_BURNED = .TRUE."                     >> elmfire_post.data
   echo "ALREADY_BURNED_FILENAME = 'already_burned_float'" >> elmfire_post.data
fi
if [ "$READ_PHI" = "yes" ]; then
   echo "READ_PHI = .TRUE."                                >> elmfire_post.data
   echo "PHI_FILENAME = 'phi'"                             >> elmfire_post.data
fi
echo "FIRE_SIZE_STATS_FILENAME = 'summary_stats.csv'" >> elmfire_post.data
echo "BINARY_FILE_TYPE = 2"                           >> elmfire_post.data

echo "/"                                             >> elmfire_post.data

$MPIRUN --mca btl tcp,self,vader --map-by core --bind-to core -np $NP -host $HOSTS $ELMFIRE_POST elmfire_post.data 2> /dev/null
rm -f ./already_burned_float.tif ./phi.tif

echo "Built .bil & .hdr files"
