#!/bin/bash

source ~/eman21.rc
source ~/spider.rc

######### user input ###################
DIR_MRCS="$PWD"
vol="3dvol" # SPIDER format 3D volume
########################################

[ -f list.txt ] && rm -f list.txt
[ -f params.spi ] || ( echo "Parameter file not found! Run make-params first!" && exit 1 )
[ -f unaligned_ptcls.spi ] && rm -f unaligned_ptcls.spi
[ -f ${vol}.spi ] || ( echo "SPIDER format 3D volume not found!" && exit 1 )

num=1
tot=`ls ${DIR_MRCS}/*particles.mrcs | wc -l | awk '{print $1}'`

for i in `ls ${DIR_MRCS}/*particles.mrcs`
do
	echo -ne "Creating list of particle files...($num out of $tot)\r"
	numpart=`e2iminfo.py ${i} | grep "MRC format" | awk '{print $11}'`

	echo "${numpart} ${i}" >> list.txt
	echo -ne "Converting mrcs to SPIDER stack...($num out of $tot)\r"

	`which e2proc2d.py` ${i} unaligned_ptcls.spi --threed2twod &> /dev/null
	((num++))
done
[ $? -eq 0 ] && echo -e "\nDone!"
numtot=`e2iminfo.py -c unaligned_ptcls.spi | grep "total" | awk '{print $1}'`
tot_inp=`awk '{ sum+=$1} END {print sum}' list.txt`
[ $numtot -eq $tot_inp ] && echo "Total ${numtot} particles." || echo "Something went wrong! Input is $tot_inp, output is $numtot particles"

spider fed/spi <<EOF
[docfile] = 'params'
MD
	TR OFF		; Decrease results file output
MD
	VB OFF		; Decrease results file output
  
UD 4,[ang_step]
	[docfile]
UD 5,[obj_radius]
	[docfile]
UD E
SYS 
	echo "Creating angular file.."
DE
	ref_images_angles
DE
	ref_images

VO EA [numproj]			; Sets [numproj] to number of reference projections
	[ang_step]		; Theta angular step
	0, 90			; Theta range, 90 is for use with 'Check Mirrored Positions'
	0, 359.9		; Phi range
	ref_images_angles	; Reference angles doc file (output)

SYS
	echo "Projecting 3D input volume.."

PJ 3Q				; Projection operation
	$vol			; 3D volume (input)
	[obj_radius]		; Radius of object
	1-[numproj]		; Ref. angles used    
	ref_images_angles	; Ref. angles doc file (input)
	ref_images@*****	; Template for ref. projections (output) 

EN D
EOF
echo -e "References are in file ref_images.spi\nC'EST LA FIN"
