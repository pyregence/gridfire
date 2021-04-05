#!/bin/bash

tar cf - *.bin | pigz > binary_outputs.tar.gz && rm -f -r *.bin elmfire_post.data diag*.csv geoserver >& /dev/null

echo "Compressed binary files and removed originals"

exit 0
