#!/usr/bin/env bash

echo "Compressing binary files and removing originals"
tar cf - *.bin | pigz > binary_outputs.tar.gz && rm -f -r *.bin *.tif elmfire_post.data diag*.csv geoserver geoserver_new scratch &

echo "Done compressing and cleaning up"

exit 0
