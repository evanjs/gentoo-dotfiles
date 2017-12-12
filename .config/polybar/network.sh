#!/usr/bin/env bash

online=$(ifconfig | grep "RUNNING,MULTICAST" | cut -d ":" -f1)


if [[ "$online" ]]
then
   echo %{F#81A2BE1}
 else
   echo %{F#E64141}
fi

exit 0