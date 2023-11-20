#!/bin/bash
# These steps are for removing EQUIVALENCE statements not supported by SPAG
set +e
set +x
python3 ./hollerith_encode.py
find . -name "*.f90" | parallel --progress -j8 python3 ./hidestars.py {}
docker run -it --rm -v /:/mnt -w "/mnt$PWD" --entrypoint /bin/bash camfort -c 'apt update && apt install -f parallel && find . -type f -name "*.f90" | parallel --progress -j8 /camfort equivalence {} --inplace'
python3 showstars.py
find . -name "*.f90" | parallel --progress -j8 python3 ./hollerith_decode.py {} \;
