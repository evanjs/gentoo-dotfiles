#!/bin/sh

B='#00000000'  # blank
C='#ffffff22'  # clear ish
#D='#ff00ffcc'  # default
D='#00ff00cc' # default
#T='#ee00eeee'  # text
T='#00ee00ee' # text
W='#880000bb'  # wrong
#V='#bb00bbbb'  # verifying
V='#00bb00bb' # verifying

i3lock              \
--insidevercolor=$C   \
--ringvercolor=$V     \
\
--insidewrongcolor=$C \
--ringwrongcolor=$W   \
\
--insidecolor=$B      \
--ringcolor=$D        \
--linecolor=$B        \
--separatorcolor=$D   \
\
--textcolor=$T        \
--timecolor=$T        \
--datecolor=$T        \
--keyhlcolor=$W       \
--bshlcolor=$W        \
\
--screen 2            \
--clock               \
--blur 5              \
--indicator           \
--timestr="%H:%M:%S"  \
--datestr="%A, %m %Y" \
--wrongtext="dumbass"

# --blur 5
# --veriftext="Drinking verification can..."
# --wrongtext="Nope!"
# --textsize=20
# --modsize=10
# --timefont=comic-sans
# --datefont=monofur
# etc
