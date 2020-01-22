# prosogram.praat
# For documentation see http://sites.google.com/site/prosogram/
# Requires Praat 6.0.43 or higher.

# Author: Piet Mertens
# Copyright 2003-2020 Piet Mertens

form Prosogram 2.20a (Jan 10, 2020)
	optionmenu Task 1
		option Prosogram (writes graphics files)
		option Calculate intermediate data files & Prosodic profile (no graphics files)
		option Prosogram, intermediate data files & Prosodic profile
;		option Polytonia
		option Interactive prosogram
		option Recalculate pitch for entire sound file
		option Recalculate intensity after BP filtering for entire sound file
;		option Recalculate loudness for entire sound file
		option Make automatic segmentation into syllables and save
;		option Plot pitch in semitones, with annotation. No stylization.
;		option Draw annotation only. No stylization.
;		option Prosodic boundary detection
		option Validate syllable tier
   comment Input sound files: (leave empty for interactive file selection)
	text Input_files C:/corpus/*.wav
   comment Analysis options:
	real left_Time_range_(s) 0.0
	real right_Time_range_(s) 0.0 (=all)
;	optionmenu Scale_signal_intensity: 1
;		option No scaling
;		option Scale to average intensity of 70dB SPL
;	real Clipping_threshold_(percent) 0 (=no clipping)
	real left_F0_detection_range_(Hz) 0 (=autorange)
	real right_F0_detection_range_(Hz) 450 (=default)
	optionmenu Parameter_calculation: 1
		option Full (saved in file)
		option Partial (not saved in file)
	optionmenu Frame_period_(s): 1
		option 0.005
		option 0.01
	optionmenu Segmentation_method 1
		option Select optimal method for TextGrid (if any) 
		option Automatic: acoustic syllables
		option Nuclei in vowels in tier "phon"
		option Nuclei in rhyme from "syll" and vowels in "phon"
		option Nuclei in syllables in "syll" and vowels in "phon"
		option Nuclei in syllables in "syll" and local peak
;		option Automatic: peak of loudness (obsolete)
;		option Automatic: peak of intensity of BP-filtered signal (obsolete)
		option Using external segmentation in tier "segm"
;		option Automatic: acoustic syllables (using loudness)
	optionmenu Thresholds: 6
		option G=0.16/T^2, DG=20, dmin=0.035
		option G=0.24/T^2, DG=20, dmin=0.035
		option G=0.32/T^2, DG=20, dmin=0.035
		option G=0.32/T^2, DG=30, dmin=0.050
		option G=0.24-0.32/T^2 (adaptive), DG=30, dmin=0.050
		option G=0.16-0.32/T^2 (adaptive), DG=30, dmin=0.050
		option G=0.0/T^2, DG=20, dmin=0.035
		option G=0.0/T^2, DG=40, dmin=0.035
   comment Plotting options:
	optionmenu View: 3
		option Compact, light
		option Compact, rich
		option Wide, light
		option Wide, light, with pitch range
		option Wide, rich
		option Wide, rich, with pitch range
		option Wide, rich, with values pitch targets in semitones
		option Wide, rich, with values pitch targets in Hertz
;		option Large, light
;		option Large, rich
	positive Time_interval_per_strip_(s) 3.0
	sentence Tiers_to_show_(*convert_to_IPA) *1, 2, 3
	optionmenu Font: 1
		option Times 10
		option Times 8
		option Helvetica 10
		option Helvetica 8
	real left_Pitch_range_(ST) 0 (=autorange)
	real right_Pitch_range_(ST) 100
	optionmenu Output_mode: 1
		option Fill page with strips
		option One strip per file
	optionmenu Output_format: 2
		option EPS (Encapsulated Postscript)
		option PNG 300 dpi
		option PNG 600 dpi
		option PDF
		option EMF (Windows Enhanced Metafile)
;		option EMF and EPS
		option EPS and JPG 300 dpi (Windows, Ghostscript must be installed)
		option EPS and JPG 600 dpi (Windows, Ghostscript must be installed)
   comment Output path and filename for graphics files (number and extension added automatically) :
	text Output_filename <input_directory>/<basename>_
endform

batch_mode = 0
include prosomain.praat

@main
exit
