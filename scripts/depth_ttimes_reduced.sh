#!/usr/bin/env bash
set -ue

/bin/rm -f gmt.conf gmt.history ttlayer.out

if [[ -s STATION0.HYP ]]; then
   echo "Plotting travel times for STATION0.HYP in current directory"
   model=$( tail -1 STATION0.HYP )
elif [[ -s $SEISAN_TOP/DAT/STATION0.HYP ]]; then
   echo "Plotting travel times for STATION0.HYP in \$SEISAN_TOP/DAT directory"
   model=$( tail -1 $SEISAN_TOP/DAT/STATION0.HYP )
else
   echo "ERROR: no STATION0.HYP in search path"
   exit 1
fi

dist_max=80.0
step=1.0

depth=( 0 1 2 5 10 15 20 25 30 )

echo "Model name: $model"

for z in "${depth[@]}"; do

/bin/rm -f ttlayer.out

ttlayer << END
STATION0.HYP
1
$dist_max
$step
$z
 
END

outfile=$( printf "ttlayer%03d.out" $z )

/bin/mv ttlayer.out $outfile

done

gmt set GMT_COMPATIBILITY 6
gmt set PS_PAGE_ORIENTATION landscape

region=$( awk '(NR > 11) {print $1, $4}' ttlayer000.out | gmt info -I10 )
region="-R0/$dist_max/-4/12"

gmt begin ${model}_ttplot pdf

    gmt basemap -JX24.0c/12.0c $region -Bxa20f10+l"distance (km)" -Bya2f1+l"reduced travel time (s)" -BWeSn+t"Model: $model" -Xc -Yc

    for f in ttlayer???.out; do
    zlabel=${f:7:3}
    zy=$( awk '(NR > 11) {print $2; exit; }' $f )
    echo 0 $zy $zlabel
    awk '(NR > 11) {print $1, $2 - $1 / 6.0}' $f | gmt plot -W1,red
    awk '(NR > 11) {print $1, $4 - $1 / 6.0}' $f | gmt plot -W1,green
    done

gmt end show

/bin/rm -f gmt.conf gmt.history out # ttlayer.out
