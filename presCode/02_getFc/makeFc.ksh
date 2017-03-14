#!/bin/ksh

set -exuva
mkdir -p numbered
dayCounter=1
for f in $(find . -type f -name '*_daily_m.grib' | sort -u); do
  ff=${f%.grib}
  ff=${ff#./}
  if [[ -n $(echo $ff | grep 0101_daily_m) ]] ; then
    dayCounter=1
  fi
  cp ${ff}.grib numbered/${ff}_${dayCounter}.grib
  dayCounter=$((dayCounter+1))
done



for yr in 201510 201511 201512 201601 201602; do

for f in $(find . -type f -name "*$yr*_daily_m_*.grib" | sort -u); do
  file=${f##./}
  ff=${file#*_}
  ff=${ff#*_}
  date=${ff%%_*}
  dyear=${date:0:4}
  typeset -Z4 dyear
  dmon=${date:4:2}
  typeset -Z2 dmon
  dday=${date:6:2}
  typeset -Z2 dday
  countday=${ff##*_}
  countday=${countday%.grib}
  
  resetyear=0
  mkdir -p "${dyear}-${dmon}-${dday}"
  rm -rf pcp_${dday}
  for day in {1..15}; do
    typeset -Z2 day
    if [[ $resetyear == 1 ]]; then
      if [[ "$ddday" -gt "364" ]]; then
        countday=1
        ddday=1 #$((day+countday-1-lastday))
        dyear=$((dyear+1))
      else
  
        ddday=$((ddday+1))
      fi
    else
      ddday=$((day+countday-1))
    fi
  
    if [[ ("$dyear" -eq "1984" || "$dyear" -eq "1988" || "$dyear" -eq "1992" || "$dyear" -eq "1996" || "$dyear" -eq "2000" || "$dyear" -eq "2004" || "$dyear" -eq "2008" || "$dyear" -eq "2012" || "$dyear" -eq "2016") && "$dmon" -eq "12" && "$dmon" -eq "31" ]]; then
      if [[ "$ddday" -eq "366" ]]; then
        resetyear=1
        lastday=$day
      fi
    else
      if [[ "$ddday" -eq "365" ]]; then
        lastday=$day
        resetyear=1
      fi
    fi

    myFile=$(find . -type f -name "*_${dyear}*_m_${ddday}.grib")
    myFile=${myFile#./}
    cp $myFile ${dyear}-${dmon}-${dday}/pcp_${day}.grib
  done
  
  

done

done
