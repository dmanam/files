#!/bin/bash

cower="$(shuf -n1 -e /usr/bin/cowsay /usr/bin/cowthink)"
cow="$(shuf -n1 -e /usr/share/cowsay-*/cows/*)"

$cower -f $cow $@
