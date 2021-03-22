#!/bin/bash

rm -f *.bil *.hdr *.tif

if [ -z "$ELMFIRE_VER" ]; then
   ELMFIRE_VER=0.6548
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

OMP_PROC_BIND=true $MPIRUN --mca btl tcp,self,vader --map-by core --bind-to core -np $NP -host $HOSTS $ELMFIRE_POST elmfire_post.data 2> /dev/null

echo "Converting .bil/.hdr files to .tif"
./make_tifs.sh >& /dev/null

echo "Preparing tifs for GeoServer"
FIRENAME=`echo $(pwd) | rev | cut -d'/' -f2 | rev | cut -d_ -f1`
START_DATE=`echo $(pwd) | rev | cut -d'/' -f2 | rev | cut -d_ -f2`
START_TIME=`echo $(pwd) | rev | cut -d'/' -f2 | rev | cut -d_ -f3`
START_HOUR=${START_TIME:0:2}

rm -f -r geoserver
mkdir geoserver
mkdir geoserver/$FIRENAME
mkdir geoserver/$FIRENAME/${START_DATE}_${START_TIME}
mkdir geoserver/$FIRENAME/${START_DATE}_${START_TIME}/gridfire/
mkdir geoserver/$FIRENAME/${START_DATE}_${START_TIME}/gridfire/landfire/
for BP in 10 20 30 40 50 60 70 80 90; do
   mkdir ./geoserver/$FIRENAME/${START_DATE}_${START_TIME}/gridfire/landfire/$BP
done

rename () {
   local f=$1
   local BP=`basename $f | cut -d_ -f2`
   local HOUR=`basename $f | cut -d_ -f3 | cut -d. -f1`
   local TIMESTAMP=`date -u -d "$START_DATE $START_HOUR:00 UTC + $HOUR hours" +"%Y%m%d_%H0000"`
   local RASTER=`basename $f | cut -d_ -f1`
   mv $f ./geoserver/$FIRENAME/${START_DATE}_${START_TIME}/gridfire/landfire/$BP/${RASTER}_$TIMESTAMP.tif
}

(
for f in *.tif; do
   ((i=i%NP)); ((i++==0)) && wait
   rename "$f" &
done
)
wait
sleep 0.5

cd geoserver
echo "Creating tarball for GeoServer"
tar -cvf $FIRENAME-${START_DATE}_${START_TIME}.tar * >& /dev/null

echo "Uploading tarball to GeoServer"
scp  $FIRENAME-${START_DATE}_${START_TIME}.tar gridfire@data.pyregence.org:/incoming/
mv *.tar ..

echo "Compressing binary files and removing originals"
cd ..
tar cf - *.bin | pigz > binary_outputs.tar.gz && rm -f -r *.bin elmfire_post.data diag*.csv geoserver >& /dev/null

exit 0
