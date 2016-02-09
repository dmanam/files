#!/bin/bash

red="<fc=#ff0000>"
yellow="<fc=#ffff00>"
green="<fc=#00ff00>"
stopcolor="</fc>"

stats=$(acpi -b | head -n 1)

percent=$(<<<$stats awk '{ print $4 }' | sed -r 's:^(.*)%,?$:\1:')
time=$(<<<$stats awk '{ print $5 }')
bars=$(<<<$percent sed -r 's:([0-9]+):\1/20:' | bc -l | awk '{ printf("%d", $1 + 0.5) }')

if <<<$stats grep Discharging >/dev/null; then
  empty='-----'
  endcolor=$red
  timephrase="dead discharging"
else
  empty='+++++'
  endcolor=$green
  timephrase="full charging"
fi

case "$bars" in
5)
  barcolor=$green;;
[2-4])
  barcolor=$yellow;;
*)
  barcolor=$red;;
esac

echo -n "$percent% "

echo -ne $endcolor'['$stopcolor$barcolor
echo -n $empty | sed -r -e 's:^[+-]*([+-]{'$((5-$bars))'}):|||||\1:' -e 's:^.*(.{5})$:\1:' || echo -n '-----'
echo -ne $stopcolor$endcolor']'$stopcolor' '

if <<<$time grep '.*:.*:.*' >/dev/null; then
  echo -n "$(<<<$timephrase awk '{ print $1 }') in $time"
elif [[ $percent -eq 100 ]]; then
  echo -n 'full'
else
  <<<$timephrase awk '{ print $2 }'
fi
