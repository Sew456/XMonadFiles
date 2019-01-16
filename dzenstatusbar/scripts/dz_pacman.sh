howmany=$(pacman -Qu | wc -l)
updates=$(pacman -Qu)
echo $howmany

