#!/bin/bash

CACHE_PATH="$HOME/.cache"

if [[ ! $(pidof eww) ]]; then
	eww daemon
	sleep 1
fi

monitors=($(hyprctl monitors -j | jq -r '.[] | .id'))
for i in "${!monitors[@]}"; do
	FILE="$CACHE_PATH/eww_launch_$i.xyz"
  if [[ ! -f "$FILE" ]]; then
	  eww open-many bar$i
	  touch "$FILE"
  else
	  eww close-all && killall eww
	  rm "$CACHE_PATH"/eww_launch_*.xyz
  fi
done
