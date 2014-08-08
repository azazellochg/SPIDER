; SOURCE: spider/docs/techs/recon/newprogs/apsh.spi 
;          New                                            Nov 2004 ArDean Leith  
;          Added alignment option & stack op              Nov 2006 ArDean Leith
;          Var naming                                     Jan 2010 ArDean Leith
;
; PURPOSE: Multi-reference alignment of an image series. 
;          Reference images are aligned with reference projections via
;          shifts (translations) and rotations. 
;
; ----------------- Input files --------------------------------------------

[docfile]	    = 'params'			; Docfile

[unaligned_images]  = 'unaligned_ptcls@'	; Unaligned particles name template

[ref_images]        = 'ref_images@'		; Reference images   

[ref_images_angles] = 'ref_images_angles'	; Reference images angles doc. file

; ----------------- Output files -------------------------------------------------

[align_parameters] = 'alidoc_apsh'		; Alignment parameters doc. file for AP SH

; ----------------- END BATCH HEADER -------------------------------------------

MD
	TR OFF		; Decrease results file output
MD
	VB OFF		; Decrease results file output
MD
	() OFF		; No () necessary in loops

UD 3,[pxsz]
	[docfile]
UD 6,[range]
	[docfile]
UD 7,[step]
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

DE				; Delete existing alignment parameter output doc. files.
	[align_parameters]

FI H [numpart]			; Get number of particles 
	[unaligned_images]	; Unaligned particles name template (input)
	MAXIM			; Max. image number in stack    

FI H [numproj]			; Get number of particles 
	[ref_images]		; Reference images name template (input)
	MAXIM			; Max. image number in stack    

SYS
	echo "Finding alignment for: {******[numpart]} particles.."

AP SH					; Align sample images using multiple references  
	[ref_images]*****		; References image name template (input)
	1-[numproj]			; Max. image number
	[range],[step]			; Search range, step size  
	[r1],[r2],1			; First and last ring, skip. For large images change 'skip' to 2 or 3 to decrease memory    
	[ref_images_angles]		; Ref. angles doc file (input)
	[unaligned_images]******	; Unaligned particles name template (input)
	1-[numpart]			; Max. image number
	*				; No unaligned images align. doc file  
	0.0,0.0				; No restriction on angular proj. search 
	Y,N				; Check mirrored positions, shift and rotate input
	[align_parameters]		; Alignment angles doc. file (output)

SYS
	echo "Aligment has finished. Output doc file is [align_parameters]"
EN D 
