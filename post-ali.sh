#!/bin/bash

source ~/spider.rc
em2em="/home/igbmc/gsharov/soft/imsc_em2em_fsc_20140519/em2em_linux.sh -t"

DIR_ALIGNED="aligned" # Directory for aligned mrcs particle stacks

mkdir ${DIR_ALIGNED}
[ -f post-ali.log ] && rm -f post-ali.log

spider fed/spi <<EOF
; ----------------- Input files --------------------------------------------

[docfile]	    = 'params'			; Docfile

[unaligned_images]  = 'unaligned_ptcls@'	; Unaligned particles name template

[ref_images]        = 'ref_images@'		; Reference images   

[ref_images_angles] = 'ref_images_angles'	; Reference images angles doc. file

[align_parameters]  = 'alidoc_apsh'		; Alignment parameters doc. file for AP SH

; ----------------- Output files -----------------------------------------------

[align_parameters2] = 'alidoc_apref'		; Alignment parameters doc. file for AP REF (final)

[aligned_images]    = 'aligned_ptcls@'		; Aligned particles (final stack)

; ----------------- END BATCH HEADER -------------------------------------------

MD
	TR OFF		; Decrease results file output
MD
	VB OFF		; Decrease results file output

UD 1,[doalign]
	[docfile]
UD 2,[incore-yn]
	[docfile]
UD 3,[pxsz]
	[docfile]
UD 6,[range]
	[docfile]
UD 8,[range2]
	[docfile]
UD 9,[diam]
	[docfile]
UD 10,[r1]
	[docfile]
UD 11,[r2]
	[docfile]
UD E

IF ([r2] .LE. 0) THEN				; Compute alignment radius from object size  
 
	FI H [winsz]				; Get window size (pixels) from this file 
		[ref_images]			; Unaligned particles name template (input)
		NY				; Number of rows per slice    

	[r2] = INT([diam]/(2.0*[pxsz]))		; Compute object radius (pixels) for last alignment ring
	[ring-sh] = [r2] + [range]		; Compute last ring + translation range
	[maxrad]  = INT([winsz]/2) - 2		; Compute max. radius of object within window

	IF ([ring-sh] .GE. [maxrad]) THEN
		[r2] = [maxrad] - [range] - 1	; Reduce radius of last alignment ring to fit window
	ENDIF
ENDIF

FI H [numpart]			; Get number of particles 
	[unaligned_images]	; Unaligned particles name template (input)
	MAXIM			; Max. image number in stack    

FI H [numproj]			; Get number of particles 
	[ref_images]		; Reference images name template (input)
	MAXIM			; Max. image number in stack 

IF ( [incore-yn] == 0 ) THEN
	; Use on-disk image stack
	[temp_in_images] = '[unaligned_images]'
	[temp_out_images] = 'ali_temp@'

ELSE
	; Load input images into incore image stack
	[temp_in_images] = '_1@'
	[temp_out_images] = '_2@'

	CP				; Load input images into incore stack
		[unaligned_images]	; Image stack (input)
		[temp_in_images]	; Incore stack (output)
		[numpart]		; Stack size
ENDIF

AP REF				; Poorer search than 'AP SH' but 4-5x faster
	[ref_images]****** 	; Reference image name template     (input)
	1-[numproj]
	[range2]		; Shift search range
	[r1],[r2],1		; First, last radial ring, & skip
	[ref_images_angles]	; Reference images angles doc file
	[not-exist]		; Non-existent reference rings file
	[temp_in_images]*****	; Template for normalized aligned AP SH images     (input)
	1-[numpart]
	[align_parameters]	; Alignment parameter doc file   (input)
	0.0,0.0			; Angular search restriction
	Y,Y			; Check mir projections, align first
	[align_parameters2]	; Alignment parameter doc file      (output)

IF ([doalign] .GT. 0) THEN
	; Apply alignment parameters to unaligned images
	SYS
		echo "Applying final alignment parameters.."

	RT SF				; Rotate & shift whole stack operation
		[unaligned_images]	; Unaligned particles stack (input)
		[temp_out_images]	; Aligned particles stack (output)
		[numpart]		; Number of particles
		6,0,7,8			; Reg. numbers for angle, scale,& shifts
		[align_parameters2]	; Alignment angles doc. file (input)

	SYS
		echo "Normalizing images to avoid interpolation errors.."
; Works only in SPIDER v. 20.17+ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	NORM
;		[temp_out_images]	; Aligned particles stack (input)
;		*			; No mask
;		[aligned_images]	; Aligned particles stack normalized (output)
;		N			; No flat-field ramp applied
; For SPIDER v. before 20.17 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CP
		[temp_out_images]	; Aligned particles stack (input)
		[aligned_images]	; Aligned particles stack (output)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	DE				; Remove temp file
		[temp_out_images] 

	DE
		_1@
	DE
		_2@	
	SYS
		echo "DONE! Your aligned particles are in file: [aligned_images]"
ELSE
	DE
		_1@
	DE
		_2@
	SYS
		echo "DONE! Your alignment file is: [align_parameters2]"
ENDIF
EN D 
EOF

k=1
while read line           
do
	ptcls=`echo "$line" | awk '{print $1}'`
	ptcls2=$(($k+$ptcls-1))
	name=`echo "$line" | awk '{print $2}' | sed 's/[^0-9]//g'`

spider fed/spi <<EOF
CP
	aligned_ptcls@******
	$k-$ptcls2
	$DIR_ALIGNED/img$name-particles@******
	1-$ptcls
EN D
EOF
	echo "$ptcls particles converted from aligned_particles.spi: $k-$ptcls2" >> post-ali.log

$em2em <<EOF
2D
SPIDER
STACKED_IMAGE_FILE
MRC
STACKED_IMAGE_FILE
${DIR_ALIGNED}/img${name}-particles.spi
${DIR_ALIGNED}/img${name}_particles.mrcs
NO
KEEP_DENSITY_VALUES
NAME_OF_IMPORT_FILES
EOF

	rm -f ${DIR_ALIGNED}/img${name}-particles.spi
	k=$((ptcls2+1))
done < list.txt

echo "Done! Don't forget to normalise new aligned particle stacks in RELION! They are in folder ${DIR_ALIGNED}"
