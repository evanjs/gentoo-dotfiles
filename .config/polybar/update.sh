#!/bin/bash

if [[ `checkupdates | wc -l` > "0" ]]; then
    konsole -e "sudo emerge -uv"
fi

exit 0
