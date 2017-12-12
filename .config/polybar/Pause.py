import os

try :
    val = os.popen('playerctl status').read()
except :
    val = 0
    pass;

if val == "Playing\n" :
    print("%{F#81A2BE}%{F-}")
else :
    print("")
