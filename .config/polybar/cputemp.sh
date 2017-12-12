#!/bin/bash


tempGPU=$(sensors | grep temp1 | head -n1 | sed -r 's/.*:\s+[\+-]?(.*C)\s+.*/\1/')
temp=$(sensors | grep -i core0 | head -n1 | sed -r 's/.*:\s+[\+-]?(.*C)\s+.*/\1/')

printf " CPU: $temp / GPU: $tempGPU"

