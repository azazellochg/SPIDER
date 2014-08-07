#!/bin/bash

#This simple script will convert relion mrcs particles to spider,
#run multireference alignment (apsh.fed) and convert aligned particles back to mrcs in folder aligned/

source ~/eman21.rc

mkdir aligned

for i in `cat list.txt | sed 's/[^0-9]//g'`;
do

e2proc2d.py ../Particles/Micrographs/img${i}_particles.mrcs particles.spi --threed2twod

`which spider` fed/spi @apsh

/home9/sharov/soft/imsc_em2em_fsc/em2em_linux.sh -t <<EOF > /dev/null
2D
SPIDER
STACKED_IMAGE_FILE
MRC
STACKED_IMAGE_FILE
ali.spi
aligned/img${i}_particles.mrcs
NO
KEEP_DENSITY_VALUES
NAME_OF_IMPORT_FILES
EOF

rm -f ali*.spi ali*.doc

done
