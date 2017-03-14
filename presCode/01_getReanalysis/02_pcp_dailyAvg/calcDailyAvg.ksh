#!/bin/ksh
set +e -xuva




mkdir out

cd in

cp ../barCalc myBar

for yr in {1981..2016}; do
rm -rf barr


for f in $(find . -type f -name "*${yr}*_0.grib"); do
  ff=${f##./}

  echo 
  echo
  echo $ff
  echo
  echo

  fff=${ff%%_0.grib}
  cp ../barCalc myBar
  sed -i "s/myfile1/${ff}/g" myBar
  sed -i "s/myfile2/${fff}_1200.grib/g" myBar
  sed -i "s/myfile3/${fff}_daily.grib/g" myBar
  cat myBar >> barr

done


mars < barr
done



mv *_daily.grib ../out/

rm -rf myBar
rm -rf barr
set -exuva
cd ../out
pwd
rm -rf eraint_precip_daily.grib
for f in $(find . -type f -name '*_daily.grib' | sort -u); do
  cat $f >> eraint_precip_daily.grib
done


