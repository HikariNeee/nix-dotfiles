#!/usr/bin/env fish

set WALLPAPER_DIR (echo $HOME/Pictures)

if test -d $WALLPAPER_DIR
 echo "Folder found! choosing a random wallpaper.."
 swaybg -i (string join "/" $WALLPAPER_DIR (random choice (string split " " (ls $WALLPAPER_DIR))))
 exit
else
 echo "Wallpaper folder not found."
 exit
end
