# !/bin/ksh

plotMAPS=0

module load nag
# module switch emos/394
module unload emos
module load emos
module unload grib_api
module load eccodes



gfortran -g -fcheck=bounds -ffpe-trap=invalid,overflow -ffree-line-length-none -fno-range-check -ffree-line-length-none -fall-intrinsics -Wsurprising -Wunderflow tools.f90 SPI.f90 -o cal_floody.x $EMOSLIB $NAGLIB $NAGINC $ECCODES_LIB $ECCODES_INCLUDE

./cal_floody.x
