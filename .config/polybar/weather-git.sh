#!/bin/bash


### API Information ###
# This script uses the Dark Sky weather API. You will need to register an account to get an API key. The first 1000 API requests you make every day are free of charge.
# Documentation overview: https://darksky.net/dev/docs
# Account overview: https://darksky.net/dev/account

# Define your location. api.darksky.net/forecast/api_key/latitude,longitude?... If you want Fahrenheit, use units=us
address="***REMOVED***"
weather="$(wget -q -O- $address)"

# Look for the current weather conditions
[[ "$weather" =~ \"icon\":\"([^\"]*)\" ]]
condition="${BASH_REMATCH[1]}"

# Look for the current apparent temperature. Use \"temperature\", if you like.
[[ "$weather" =~ \"apparentTemperature\":([^,]*), ]]
temperature="${BASH_REMATCH[1]}"

# Print some spacing, and a cloud icon. If you can't see the icon, install otf-fontawesome from the AUR or find it for your distribution
printf "\040"

# Print a rounded temperature value
LC_ALL=C /usr/bin/printf '%.0f' " $temperature"

# Use either Celsius or Fahrenheit character
if grep -qi 'units=us' <<< $address; then
	printf "°F"
else
	printf "°C"
fi

# Print edgy shit about the weather
if grep -qi 'rain' <<< $condition; then
    printf " Pluviophile dreams"
elif grep -qi 'partly-cloudy' <<< $condition; then
    printf " It's alright"
elif grep -qi 'cloudy' <<< $condition; then
    printf " Grey and dull"
elif grep -qi 'clear-day' <<< $condition; then
    printf " Diffuse sky radiation"
elif grep -qi 'clear-night' <<< $condition; then
	printf " Look up at the stars."
elif grep -qi 'snow' <<< $condition; then
	printf " It's snowing!"
elif grep -qi 'fog' <<< $condition; then
	printf " Spooky."
elif grep -qi 'wind' <<< $condition; then
	printf " Don't fly away"
elif grep -qi 'sleet' <<< $condition; then
	printf " Sleet. Stay inside"
# Next 3 may not be defined yet.
elif grep -qi 'thunderstorm' <<< $condition; then
	printf " The gods are wrathful"
elif grep -qi 'hail' <<< $condition; then
	printf " Hail. Stay inside"
elif grep -qi 'tornado' <<< $condition; then
	printf " Tornado. Stay inside. Or don't. You'll probably never see this text. Basement? Be careful."
else
    printf " Look out the window"
fi




# Powered by Dark Sky. See more: https://darksky.net/poweredby/
