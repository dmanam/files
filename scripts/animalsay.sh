#!/bin/bash

cow="$(shuf -n1 -e /usr/share/cowsay-*/cows/*)"

/usr/bin/cowsay -f $cow $@
