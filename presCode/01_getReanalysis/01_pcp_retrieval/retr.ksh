#!/bin/ksh
rm -rf barr
for yr in {1981..2016}; do
  echo $yr
  cp barRetr barr
  sed -i "s/yyyy/${yr}/g" barr
  cat barr >> barrr
done
mars < barrr
