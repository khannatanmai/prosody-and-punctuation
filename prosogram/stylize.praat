# stylize.praat -- Praat include file
# Pitch contour stylization. 
# This file is included (indirectly) by prosogram.praat. It isn't a stand-alone script. Use "prosogram.praat" instead.
# Author: Piet Mertens
# For documentation see http://sites.google.com/site/prosogram/
# Last modification: 2019-09-18

# 2018-10-29	extended prosodic profile
# 2018-10-31	improved handling of octave jumps
# 2018-11-01	improved handling of undefined pitch frames at nucleus boundaries and inside syllabic nucleus
# 2018-11-06	adopt current Praat scripting syntax 
# 2018-11-14	add PropLevel in globalsheet
# 2018-11-15	all pitch movements (intra- and intersyllabic) and trajectory values are now after stylization
# 2018-11-30	added pauses in extended prosodic profile
# 2018-12-08	added segmentation type in extended prosodic profile
# 2019-01-22	handle histogram for data without glissando
# 2019-03-01	deal with non-integer Pitch frame number in "Get frame number from time"
# 2019-03-25	corrected precision error occurring on some hardware
# 2019-03-26	added some rows to reportID
# 2019-04-01	improved decision for octavejump
# 2019-04-26	separate tables for prosodic profile and globalsheet
# 2019-04-28	changed prosodic_profile_new, draw_histogram, and draw_column_as_distribution
# 2019-04-28	improved draw_column_as_distribution
# 2019-05-09	removed variable min_pause_duration, which conflicted with mindur_pause_gap 
# 2019-09-04	corrected bug in pause duration calculation in @initialize_nucldat 
# 2019-09-04	corrected bug for stddev of nucleus duration when there's only one syllable in the speech signal 
# 2019-12-04	improved handling of pauses in @initialize_nucldat


# Procedure hierarchy
#
# create_table_of_nuclei				create TableOfReal with values for each nucleus
#   af									("add field") set column label and store column number in named variable 
# profile_table_create					create TableOfReal for prosodic profile and associated variables
# profile_table_prepare				    add lines for speakers in prosodic profile table 
# profile_table_load					read prosodic profile table from file
# store_features						write spreadsheet/table of syllable features
# safe_nuclei							adjust nucleus boundaries: pitch defined for entire nucleus
#   defined_intersection
#   octavejump
# initialize_nucldat					link TextGrid nucleiID with TableOfReal nucldatID, using pointer_tier in TG
# stylize_nuclei
#   stylize_nucleus
#      turning_point
#      slopeSTs							calculate slope in ST/s and audibility (glissando threshold) of pitch change
#   calc_localrate
#   calc_vowel_duration					calculate vowel duration for spreadsheet
#   calc_rhyme_duration					calculate rhyme duration for spreadsheet
# pitchrange_speakers					calculate statistics of prosodic features per speaker 
#   calc_nPVI
# pitchrange_normalized_pitch
# prosodic_profile						write prosodic profile report text file
#   pitchrange_speakers_report
#   raw_pitchrange_speakers_report
#   pitchprofile_speakers_report
#   duration_profile_speakers_report
#   update_global_report				write TableOfReal of global report for all processed input files
#   prosodic_profile_new
#      draw_table
#      draw_histogram
#      draw_column_as_distribution


spreadsheet_times_reduced_precision = 0		; if true, reduce precision (3 digits) for nucleus t1, t2, and duration, in output spreadsheet (useful for table readability by human)
											; reduced precision was default before version 2.14
mindur_pause_gap = 0.35		; min duration for gap between nuclei for pause. 
							; Also stored in _nucl.TextGrid settings tier.

											
# Flexible handling of tables for syllabic nuclei, prosodic features, etc. 
# A table is created using procedure "create_table_of_nuclei", "create_table_global_report".
# Columns may be added at any time by procedure "af". The column index is stored in a variable specified 
# in the call to "af". The order of columns is determined by the order of the calls to "af". 
# Rows may be added at any time by Praat built-in procedures.
 
											
procedure af: label$, indexname$, comment$
# Add a column to the table (the object id of which is in <af_object>), using <label> as the column label. 
# Return the number of the column in the variable named <indexname>
# label$		label for the column in the table
# indexname$	name of the variable holding the column number  
   selectObject: 'af_object$'
   .nc = Get number of columns
   Insert column (index): .nc+1
   if (not af_object_init)
      Remove column (index): 1
      af_object_init = 1
   endif
   .nc = Get number of columns
   Set column label (index): .nc, label$
   'af_object$'_colname_index$ [.nc] = "'label$'_'.nc'"
   'indexname$' = .nc
endproc


procedure create_table_of_nuclei
   @debug_msg: "create_table_of_nuclei: entry"
   .nrows = nrof_nuclei_analysed
   if (.nrows == 0)
      @error_msg: "No nuclei were found." 
      .nrows = 1		; avoid crash
   endif
   nucldat_ncols = 1	; dummy: at least one column needed at creation of TableOfReal
   nucldatID = Create TableOfReal: "nucl_data", .nrows, nucldat_ncols
   nucldat_available = 1
   af_object$ = "nucldatID"
   af_object_init = 0
   ;    col_label	,indexname			,comment
   @af: "nucl_t1"	,"j_nucl_t1"		,"starttime of nucleus"
   @af: "nucl_t2"	,"j_nucl_t2"		,"endtime of nucleus"
   @af: "f0_min"	,"f0_min"			,"f0 min (Hz) within nucleus            before stylization"
   @af: "f0_max"	,"f0_max"			,"f0 max (Hz) within nucleus            before stylization"
   @af: "f0_median"	,"f0_median"		,"f0 median (Hz) within nucleus         before stylization"
   @af: "f0_mean"	,"j_f0_mean"		,"f0 mean (Hz) within nucleus           before stylization"
   @af: "f0_meanST"	,"j_f0_meanST"		,"f0 mean (ST) within nucleus           before stylization"
   @af: "f0_start"	,"j_f0_start"		,"f0 value (Hz) at start of nucleus     after stylization"
   @af: "f0_end"	,"j_f0_end"			,"f0 value (Hz) at end of nucleus       after stylization"
   @af: "lopitch"	,"lopitch"			,"f0 min (Hz) within nucleus            after stylization"
   @af: "hipitch"	,"hipitch"			,"f0 max (Hz) within nucleus            after stylization"
   @af: "hipitchST"	,"j_hipitchST"		,"f0 max (ST) within nucleus            after stylization"
   @af: "dynamic"	,"j_dynamic"		,"0 = static, 1 = rising, -1 = falling"
   @af: "intrasyllab","j_intrasyl"		,"sum of pitch interval (ST) of tonal segments in nucleus (rises and falls compensate)"
   @af: "intersyllab","j_intersyl"		,"intersyllabic interval (ST) between end of previous nucleus and start of current one"
   @af: "up"		,"j_intrasylup"		,"sum of upward pitch interval (ST) of tonal segments in nucleus, after stylization"
   @af: "down"		,"j_intrasyldown"	,"sum of downward pitch interval (ST) of tonal segments in nucleus, after stylization"
   @af: "trajectory","j_traj"			,"sum of absolute pitch interval (ST) of tonal segments in nucleus (rises and falls add up), after stylization"
   @af: "f0_discont","j_f0_discont"		,"f0 contains discontinuity"
   @af: "prnp_start","j_prnp_start"		,"pitch-range normalized pitch at start of nucleus	after stylization"
   @af: "prnp_end"  ,"j_prnp_end"		,"pitch-range normalized pitch at end of nucleus	after stylization"
   @af: "prnp_intra","j_prnp_intra"		,"pitch-range normalized pitch of intranucleus variation after stylization"
   @af: "nucl_dur"	,"j_nucldur"		,"nucleus duration"
   @af: "syll_dur"	,"j_syllabledur"	,"syllable duration (only for appropriate segmentation methods)"
   @af: "vowel_dur"	,"j_voweldur"		,"vowel duration (only for appropriate segmentation methods)"
   @af: "rhyme_dur"	,"j_rhymedur"		,"rhyme duration (only for appropriate segmentation methods)"
   @af: "gap_left"	,"j_internucldur"	,"time between end of previous nucleus and start of current one"
   @af: "loudness"	,"j_loudness"		,"loudness peak in nucleus (only if parameter available)"
   @af: "int_peak"	,"j_int_peak"		,"peak intensity in nucleus"
if (use_duration_model)
   @af: "en_sylldur","j_en_sylldur"		,"elasticity normalized syllable duration (only for appropriate segmentation methods)"
   @af: "en_rhymedur" ,"j_en_rhymedur"	,"elasticity normalized rhyme duration (only for appropriate segmentation methods)"
endif
if (calc_prominence)
   @af: "promL2R1D_f0_mean" ,"j_promL2R1D_f0_mean"	,"prominence of mean pitch (Hz) wrt dynamic left/right context of 2+1 units"
   ; @af: "promL2R1D_f0_max"  ,"j_promL2R1D_f0_max"	,"prominence of max pitch (Hz) wrt dynamic left/right context of 2+1 units"
   @af: "promL2R1D_hipitch" ,"j_promL2R1D_hipitch"	,"prominence of max pitch (Hz) wrt dynamic left/right context of 2+1 units"
   @af: "promL2R1D_hipitchST" ,"j_promL2R1D_hipitchST"	,"prominence of max pitch (ST) wrt dynamic left/right context of 2+1 units"
   @af: "promL2R1D_f0_meanST" ,"j_promL2R1D_f0_meanST"	,"prominence of mean pitch (ST) wrt dynamic left/right context of 2+1 units"
   @af: "promL2R1D_nucldur" ,"j_promL2R1D_nucldur"	,"prominence of nucleus duration wrt dynamic left/right context of 2+1 units"
   @af: "promL2R1D_sylldur" ,"j_promL2R1D_sylldur"	,"prominence of syllable duration wrt dynamic left/right context of 2+1 units"
   @af: "promL2R1D_rhymedur" ,"j_promL2R1D_rhymedur","prominence of rhyme duration wrt dynamic left/right context of 2+1 units"
   @af: "promL2R1D_int_peak" ,"j_promL2R1D_int_peak","prominence of peak intensity wrt dynamic left/right context of 2+1 units"
   @af: "promL2R1D_loudness" ,"j_promL2R1D_loudness"	,"prominence of loudness wrt dynamic left/right context of 2+1 units"
   @af: "endtime_syll" ,"endtime_syll" ,"endtime of syllable; used by plot_salience (only for appropriate segmentation method)"
endif
if (use_duration_model and calc_prominence)
   @af: "promL2R1D_en_sylldur" ,"j_promL2R1D_en_sylldur", ""
   @af: "promL2R1D_en_rhymedur" ,"j_promL2R1D_en_rhymedur", ""
endif
   @af: "hesitation"	,"j_hesitation"		,"hesitation (required!)"
   @af: "speaker_id"	,"j_speaker_id"		,"speaker ID number, from tier 'speaker' in annotation file"
   @af: "before_pause"	,"j_before_pause"	,"syllable is followed by pause"
   @af: "after_pause"	,"j_after_pause"	,"syllable is preceded by pause"
   @af: "pause_dur"		,"j_pause_dur"		,"duration of pause following current syllable"
if (show_localrate)
   @af: "local_rate_nucl" ,"j_localrate_nucl"	,"local speech rate in complete nuclei per second"
endif
   @af: "iso_dur"	,"j_isodur"		,"inter syllable onset duration, for current onset to next onset"

   @debug_msg: "create_table_of_nuclei: exit"
endproc


procedure profile_table_create
# Create table and variables for prosodic profile (showing prosodic features for each speaker)
   profileID = Create TableOfReal: "prosodic_profile", 1, 1
   af_object$ = "profileID"
   af_object_init = 0

   profileID_empty = 1
   profileID_row_offset = 0
   @af: "SpeakerNr"		,"j_speaker_nr"		,"speaker number within input file"
   @af: "SpeechRate"	,"j_speech_rate"	,"speech rate (= nrofnucl/(TotNuclDur+TotInternuclDur))"
   @af: "NrofNuclei"	,"j_nrofnucl"		,"number of all nuclei, for current speaker"
   @af: "NrofSafe"		,"j_nrofvalid"		,"number of nuclei, without outliers and discontinuities"
   @af: "SpeechTime"	,"j_speech_time"	,"sum of TotNuclDur + TotInternuclDur + TotPauseDur, for current speaker"
   @af: "TotNuclDur"	,"j_tot_nucldur"	,"total nucleus duration, for current speaker"
   @af: "TotInternuclDur","j_tot_internucldur","total internucleus duration, for current speaker"
   @af: "TotPauseDur"	,"j_tot_pausedur"	,"total pause duration, for current speaker"
   @af: "PropPhon"		,"j_propphon"		,"(nucldur+internucldur)/(nucldur+internucldur+pausedur)"
   @af: "PropPause"		,"j_proppause"		,"pausedur/(nucldur+internucldur+pausedur)"
   @af: "F0MedianHz"	,"j_pitch_median_Hz","median in Hz of F0 values in Hz, 2 per nucleus: low and high"
   @af: "F0MedianInST"	,"j_pitch_median_ST","median in ST of F0 values in Hz"
   @af: "F0MeanHz"		,"j_pitch_mean_Hz"	,"mean in Hz of F0 values in Hz, 2 per nucleus: low and high"
   @af: "F0MeanInST"	,"j_pitch_mean_ST"	,"mean in ST of F0 values in Hz"
   @af: "F0StdevHz"		,"j_pitch_stdev_of_Hz","stdev of pitch values in Hz, 2 per nucleus: low and high"
   @af: "PitchMeanST"	,"j_pitch_mean_of_ST","mean in ST of F0 values in ST, 2 per nucleus: low and high"
   @af: "PitchStdevST"	,"j_pitch_stdev_of_ST","stdev of pitch values in ST, 2 per nucleus: low and high"
   @af: "PitchRange"	,"j_pitch_range"	,"pitch range (span), in ST"    
   @af: "PitchTopST"	,"j_pitch_top_ST"	,"top of pitch range, in ST"
   @af: "PitchBottomST"	,"j_pitch_bottom_ST","bottom of pitch range, in ST"
   @af: "PitchTopHz"	,"j_pitch_top_Hz"	,"top of pitch range, in Hz"
   @af: "PitchBottomHz"	,"j_pitch_bottom_Hz","bottom of pitch range, in Hz"
   @af: "RawF0_p02"		,"j_rawf0_p02"		,"2 percentile of raw F0 values in nuclei"
   @af: "RawF0_p25"		,"j_rawf0_p25"		,"25 percentile of raw F0 values in nuclei"
   @af: "RawF0_p50"		,"j_rawf0_p50"		,"50 percentile of raw F0 values in nuclei"
   @af: "RawF0_p75"		,"j_rawf0_p75"		,"75 percentile of raw F0 values in nuclei"
   @af: "RawF0_p98"		,"j_rawf0_p98"		,"98 percentile of raw F0 values in nuclei"
   @af: "RawF0_mean"	,"j_rawf0_mean"		,"mean of raw F0 values in nuclei"
   @af: "PropLevel"		,"j_prop_level"		,"Proportion of nuclei with level pitch (stylized)"
   @af: "Gliss"			,"j_prop_gliss"		,"Proportion of nuclei with abs pitch change >= 4 ST"
   @af: "Rises"			,"j_prop_rises"		,"Proportion of nuclei with pitch change >= 4 ST"
   @af: "Falls"			,"j_prop_falls"		,"Proportion of nuclei with pitch change <= -4 ST"
   @af: "TrajIntra"		,"j_traj_intra"		,"Time-normalized pitch trajectory of intrasyllabic variations"
   @af: "TrajInter"		,"j_traj_inter"		,"Time-normalized pitch trajectory of intrasyllabic variations"
   @af: "TrajPhon"		,"j_traj_phon"		,"Time-normalized pitch trajectory of all pitch variations"
   @af: "TrajIntraZ"	,"j_traj_intra_z"	,"Pitch range normalized TrajIntra"
   @af: "TrajInterZ"	,"j_traj_inter_z"	,"Pitch range normalized TrajInter"
   @af: "TrajPhonZ"		,"j_traj_phon_z"	,"Pitch range normalized TrajPhon"
   @af: "NuclDurMean"	,"j_nucldur_mean"	,"mean nucleus duration"
   @af: "NuclDurStdev"	,"j_nucldur_stdev"	,"stdev of nucleus duration"
   @af: "nPVI_nucldur"	,"j_nPVI_nucldur"	,"nPVI of nucleus duration"
   @af: "nPVI_voweldur"	,"j_nPVI_voweldur"	,"nPVI of vowel duration"
   @af: "nPVI_sylldur"	,"j_nPVI_sylldur"	,"nPVI of syllable duration"
endproc


procedure profile_table_prepare
# Is called once after speakers have been identified for the current input file
# Adds rows for speakers and stores speaker labels in row labels of table
   @debug_msg: "profile_table_prepare: entry"
   if (nrof_nuclei_analysed > 0)
   ; Append 1 row for each speaker
      selectObject: profileID
      for .speaker from 1 to speakers		; nrofspeakers in current file
         .nrows = Get number of rows
         Insert row (index): .nrows+1
         if (profileID_empty)				; for first input file, report contains 1 empty row 
            Remove row (index): 1
            profileID_empty = 0
         endif
         .row = Get number of rows
         .label$ = speaker_label'.speaker'$
         Set row label (index): .row, .label$
         .ncols = Get number of columns
         for .col to .ncols
            Set value: .row, .col, 0
         endfor
         Set value: .row, j_speaker_nr, .speaker
      endfor
   endif
   @debug_msg: "profile_table_prepare: exit"
endproc


procedure profile_table_load
# Read headerless spreadsheet file containing prosodic profile data into TableOfReal 
   profileID = Read TableOfReal from headerless spreadsheet file: profile_file$
   .nrcols = Get number of columns
   for .col to .nrcols						; the following row indices are used by procedures in interactive prosogram
      .label$ = Get column label: .col
      if (.label$ == "PitchBottomST")  
         j_pitch_bottom_ST = .col 
      elsif (.label$ == "PitchTopST")  
         j_pitch_top_ST = .col 
      elsif (.label$ == "F0MedianInST")
         j_pitch_median_ST = .col 
      endif 
   endfor
   profile_available = 1
endproc


procedure store_features: .filecounter, .long, .outfname$
# Stores nuclei data (TableOfReal) to a file in "headerless spreadsheet file" format, possibly adding extra columns ("long" format)
; <.filecounter> counter of input speech files (used to decide when to write table header with column names 
; <.long>	long output format adds columns: syll, rhyme, prom, contour, pitchlevel, corpus
; 			and attemps to provide the appropriate content starting from tiers for phoneme, syllable, 
;			contour, prominence
; <.outfname$> filename of output file
   @debug_msg: "store_features: entry"
   if (.filecounter == 1)
      deleteFile: .outfname$
   endif
   selectObject: nucldatID
   .nrows = Get number of rows
   if (spreadsheet_times_reduced_precision)
      for .row from 1 to .nrows
         .t1 = Get value: .row, j_nucl_t1
         .t2 = Get value: .row, j_nucl_t2
         Set value: .row, j_nucl_t1, number(fixed$(.t1,3))
         Set value: .row, j_nucl_t2, number(fixed$(.t2,3))
         Set value: .row, j_nucldur, number(fixed$(.t2-.t1,3))
      endfor
   endif
   if (not .long)
      Write to headerless spreadsheet file: .outfname$
   elsif (not segfile_available)
      Write to headerless spreadsheet file: .outfname$
   else		; long format
   # Store TableOfReal in a temporary file
      @fname_parts: .outfname$
      .tmpfile$ = result4$ + "_tmp_.txt"
      Write to headerless spreadsheet file: .tmpfile$
   # Find tier number of phoneme, syllable, contour and prominence tiers
      .grid = nucleiID
      @tier_get: .grid, "^phon", "l_phon_tier", "No phon tier", 0
      @tier_get: .grid, "^syll", "l_syll_tier", "No syll tier", 0
      @tier_get: .grid, "^speaker", "l_speaker_tier", "No speaker tier", 0
      @tier_get: .grid, "^contour", "l_contour_tier", "No contour tier", 0
      @tier_get: .grid, "^prom", "l_prom_tier", "No prom tier", 0
   # Read tmp file into a string 
      text$ = readFile$ (.tmpfile$)
      deleteFile: .tmpfile$
   # For each row, add some categorical data not available inside the TableOfReal
      # Handle first line of file, which contains header
         @next_line: "text", "line"
      if (.filecounter = 1)									; we need to construct a header line
         line$ = replace_regex$ (line$, "\n$", "", 1)		; remove end of line from header
         line$ = right$ (line$, length(line$)-index(line$, tab$))	; remove first field 
         if (corpus$ = "cprom")
            line$ = "syll rhyme " + line$ + " speaker prom contour pitchlevel corpus discourse country"
         else
            line$ = "syll rhyme " + line$ + " speaker prom contour pitchlevel corpus"
         endif
         line$ = replace_regex$(line$, " ", "'tab$'", 0)	; tab is field delimiter
         appendFileLine: .outfname$, line$					; write header line
      endif
      # For all lines other than header: add categorical data
      .syl$ = "NA"
      .rhyme$ = "NA"
      .speaker$ = "NA"
      .prom$ = "NA"
      .contour$ = "NA"
      .pitchlevel$ = "NA"
      .country$ = "NA"
      .discourse$ = "NA"
      if (corpus$ = "cprom")
         .discourse$ = replace_regex$(basename$, "\-\w*$", "", 1)
         .country$ = replace_regex$(basename$, "^\w*\-", "", 1)
         .corpus$ = corpus$
      elsif (corpus$ = "")
         .corpus$ = "NA"
      endif
      for .j to .nrows
         @next_line: "text", "line"
         line$ = replace_regex$ (line$, "\n$", "", 1)		; remove end of line
         selectObject: nucldatID
         .t1n = Get value: .j, j_nucl_t1
         .t2n = Get value: .j, j_nucl_t2
         .midt = .t1n+(.t2n-.t1n)/2
         selectObject: .grid
         if (l_syll_tier)		; syllable tier present
            .k = Get interval at time: l_syll_tier, .midt
            .syl$ = Get label of interval: l_syll_tier, .k
            .t1s = Get start time of interval: l_syll_tier, .k
            .t2s = Get end time of interval: l_syll_tier, .k
            if (l_phon_tier)
               .t = .t1s		; start time of syllable
               .rhyme$ = ""		; sequence of sounds in rhyme
               .t2r = .t2s		; end time of rhyme
               .pos = -1		; assume we start in onset of syllable
               repeat			; concatenate the sequence of sounds in the syllable; obtain label of rhyme
                  .k = Get interval at time: l_phon_tier, .t
                  .phon$ = Get label of interval: l_phon_tier, .k
                  @is_syllabic: .phon$
                  if (result)
                     .pos = 0			; in syllable peak
                     .t1r = Get start time of interval: l_phon_tier, .k
                  endif
                  if (.pos >= 0)
                     .rhyme$ = .rhyme$ + .phon$
                  endif
                  .t = Get end time of interval: l_phon_tier, .k
               until (.t >= .t2s)
            endif
         endif
         if (l_speaker_tier)		; speaker tier present
            .k = Get interval at time: speaker_tier, .midt
            .speaker$ = Get label of interval: l_speaker_tier, .k
            .speaker$ = replace_regex$ (.speaker$, "^ +(.*) +$", "\1", 1)	; trim left and right
         endif
         if (l_prom_tier)
            .k = Get interval at time: l_prom_tier, .midt
            .prom$ = Get label of interval: l_prom_tier, .k
         endif
         if (l_contour_tier)
            .k = Get interval at time: l_contour_tier, .midt
            .contour$ = Get label of interval: l_contour_tier, .k
            .pitchlevel$ = .contour$
            .pitchlevel$ = replace_regex$ (.pitchlevel$, "[rRfFSC_]", "", 0) 
            if (length (.pitchlevel$) < 1)
               .pitchlevel$ = "NA"
            endif
            .contour$ = replace_regex$ (.contour$, "[BLMHT]", "", 0) 
            if (index (.contour$, "C"))
               .contour$ = "NA"
            endif
            if (length (.contour$) < 1)
               .contour$ = "_"
            endif
         endif
         ; Finally write line/row
         line$ = right$ (line$, length(line$)-index(line$, tab$))	; remove first field (RowLabel)
         if (corpus$ = "cprom")
            line$ = "'.syl$' '.rhyme$' " + line$ + " '.speaker$' '.prom$' '.contour$' '.pitchlevel$' '.corpus$' '.discourse$' '.country$'"
         else
            line$ = "'.syl$' '.rhyme$' " + line$ + " '.speaker$' '.prom$' '.contour$' '.pitchlevel$' '.corpus$'"
         endif
         line$ = replace_regex$(line$, " ", "'tab$'", 0)
         appendFileLine: .outfname$, line$
      endfor
   endif
   @debug_msg: "store_features: exit"
endproc


procedure safe_nuclei: .t1, .t2
# Adjust nucleus boundaries such that pitch is defined throughout the nucleus, without octave jumps
# Return in <result> the number of valid nuclei in analysis interval
# <.t1>..<.t2>	time range in which procedure is applied
   @debug_msg: "safe_nuclei: entry, t1='.t1:4' t2='.t2:4'"
   mindur_syl = 0.01	; minimum duration for syllable, otherwise skipped
   selectObject: pitchID
   .f0_median_Hz = Get quantile: 0, 0, 0.50, "Hertz"
   @interval_from_time: nucleiID, nucleus_tier, .t1, "first_interval"
   selectObject: nucleiID
   prev_boundary = Get start time of interval: safe_tier, 1
   .x1 = Get start time of interval: nucleus_tier, first_interval
   repeat
      selectObject: nucleiID
      .i = Get interval at time: nucleus_tier, .x1	; nrof intervals may change during process
      .x1 = Get start time of interval: nucleus_tier, .i
      .x2 = Get end time of interval: nucleus_tier, .i
      @is_nucleus: .i
      if (result)
         ; synchronize original boundary times (from TextGrid tier) with pitch frame times
         .st1 = .x1
         .st2 = .x2
         selectObject: pitchID
         .dx = Get time step
         .jf = Get frame number from time: .st1
         .t = Get time from frame number: floor(.jf)
         if (.t < .st1)					
            .st1 = .t + .dx
         else
            .st1 = .t
         endif
         .jf = Get frame number from time: .st2
         .t = Get time from frame number: ceiling(.jf)
         if (.t > .st2)					
            .st2 = .t - .dx
         else
            .st2 = .t
         endif
         @defined_intersection: pitchID, .st1, .st2
         .cx1 = result1
         .cx2 = result2
         ; @debug_msg: "safe_nuclei: defined: cx1='.cx1:6' cx2='.cx2:6'"
         selectObject: nucleiID
         if (result == 0)				; nucleus fully undefined
            Set interval text: nucleus_tier, .i, "undef"
         else
            if (.cx1 > .x1)				; undefined section at start of nucleus
               Insert boundary: nucleus_tier, .cx1
               Set interval text: nucleus_tier, .i, "xL"
               .i += 1
               Set interval text: nucleus_tier, .i, "a"
            endif
            if (.cx2 < .x2)				; undefined section at end of nucleus
               Insert boundary: nucleus_tier, .cx2
               .j = Get interval at time: nucleus_tier, .cx2
               Set interval text: nucleus_tier, .j, "xR"
            endif
            @octavejump: nucleiID, pitchID, .f0_median_Hz, .cx1, .cx2
            .ox1 = result1
            .ox2 = result2
            .dur = .ox2 - .ox1
            ; @debug_msg: "safe_nuclei: ox1='.ox1:6' ox2='.ox2:6' dur='.dur:6'"
            selectObject: nucleiID
            if (result3 == 1)		; discontinuity found
               if (.ox1 > .cx1)			; skipped left part
                  Insert boundary: nucleus_tier, .ox1
                  Set interval text: nucleus_tier, .i, "skip"
                  .i += 1
                  Set interval text: nucleus_tier, .i, "a"
               endif
               if (.ox2 < .cx2)			; skipped right part
                  Insert boundary: nucleus_tier, .ox2
                  Set interval text: nucleus_tier, .i, "a"
                  .i += 1
                  Set interval text: nucleus_tier, .i, "skip"
               endif
            elsif (.ox2-.ox1 < mindur_syl)
               Set interval text: nucleus_tier, .i, "short"
            elsif (.ox2 < .cx2)
               Insert boundary: nucleus_tier, .ox2
               .j = Get interval at time: nucleus_tier, .ox2
               Set interval text: nucleus_tier, .j, "xR"
               Set interval text: nucleus_tier, .j-1, "a"
            endif
            if (.ox2-.ox1 >= mindur_syl)		; safe_tier
               if (.ox1 > prev_boundary)
                  Insert boundary: safe_tier, .ox1
               endif
               Insert boundary: safe_tier, .ox2
               prev_boundary = .ox2
               .j = Get interval at time: safe_tier, .ox1+(.ox2-.ox1)/2
               Set interval text: safe_tier, .j, "a"
            endif
         endif
      endif
      .x1 = .x2
      .intervals = Get number of intervals: nucleus_tier
   until (.x2 >= .t2 or .i == .intervals)
   selectObject: nucleiID
   result = Count intervals where: nucleus_tier, "is equal to", "a"
   @debug_msg: "safe_nuclei: exit, result='result'"
endproc


procedure is_nucleus: .inucl
   selectObject: nucleiID
   .label$ = Get label of interval: nucleus_tier, .inucl
   result = 0
   if (.label$ == "a")
      result = 1
   endif
endproc


procedure initialize_nucldat: .at1, .at2
# Initialize the table of nucleus data. This involves 2 steps:
# 1. Link TextGrid nucleiID and table nucldatID, by storing index of row into pointer tier of TextGrid 
# 2. Initialize some columns in table nucldatID 
   @debug_msg: "initialize_nucldat: entry"
   @intervals_from_time_range: nucleiID, nucleus_tier, .at1, .at2, "first_interval", "last_interval"
; Connect TextGrid nucleiID and table nucldatID, by storing index of row into tier of TextGrid 
   selectObject: nucleiID
   nrofnuclei = Get number of intervals: nucleus_tier
   if (not reuse_nucl)
      @copy_tier: nucleiID, nucleus_tier, nucleiID, pointer_tier
      @tier_clear_text: nucleiID, pointer_tier
   endif

   ; Initialize some columns in table nucldatID: nucleus starttime, nucleus endtime and nucleus duration 
   .row = 0
   for .i from first_interval to last_interval
      selectObject: nucleiID
      @is_nucleus: .i
      if (result)
         .x1 = Get start time of interval: nucleus_tier, .i
         .x2 = Get end time of interval: nucleus_tier, .i
         .row += 1
         selectObject: nucleiID
         Set interval text: pointer_tier, .i, "'.row'"
         selectObject: nucldatID
         Set row label (index): .row, "'.x1:3'"
         Set value: .row, j_nucl_t1, .x1
         Set value: .row, j_nucl_t2, .x2
         Set value: .row, j_nucldur, .x2-.x1
      endif
   endfor

   ; Initialize some columns in table nucldatID: vowel duration, syllable duration, rhyme duration, hesitation
   selectObject: nucldatID
   .nrows = Get number of rows
   for .row to .nrows
      Set value: .row, j_voweldur, 0
      Set value: .row, j_syllabledur, 0
      Set value: .row, j_rhymedur, 0
      if (use_duration_model)
         Set value: .row, j_en_sylldur, 0
         Set value: .row, j_en_rhymedur, 0
      endif
      Set value: .row, j_hesitation, 0
   endfor

   ; Initialize some columns in table nucldatID: locate pauses
   selectObject: nucleiID
   .t0 = Get start time
   for .j from 1 to nrof_nuclei_analysed			; assign "before_pause" and "pause_dur"
      selectObject: nucldatID
      .before_pause = 0
	  .pause_dur = 0
      .t2 = Get value: .j, j_nucl_t2				; end of current nucleus
      if (.j == nrof_nuclei_analysed)				; last nucleus in speech signal
         .before_pause = 1
         .pause_dur = signal_finish-.t2
      else
         .t = Get value: .j+1, j_nucl_t1			; start of next nucleus
         if (.t-.t2 >= mindur_pause_gap)			; potential pause
            @find_nucleus: "-", .t2, .t, 1			; find rejected nucleus in gap between current nucleus & next valid nuclei
            if (result)
               .t3 = Get start time of interval: nucleus_tier, result
               if (.t3-.t2 >= mindur_pause_gap)		; actual pause
                  .before_pause = 1
                  .pause_dur = .t3-.t2
               endif
            else
               .before_pause = 1					; actual pause
               .pause_dur = .t-.t2
			endif
         endif
      endif
      selectObject: nucldatID
      Set value: .j, j_before_pause, .before_pause
      Set value: .j, j_pause_dur, number(fixed$(.pause_dur, 3))
   endfor
   for .j from 1 to nrof_nuclei_analysed			; assign "after_pause"
      selectObject: nucldatID
      .t = Get value: .j, j_nucl_t1
      if (.j > 1)
         .t2 = Get value: .j-1, j_nucl_t2			; end of previous valid nucleus
      else
         .t2 = .t0 
      endif
      .after_pause = 0
      if (.t-.t2 >= mindur_pause_gap)				; potential pause
         @find_nucleus: "-", .t2, .t, 0				; find rejected nucleus in gap between current nucleus & prev valid nuclei
         if (result)
            .t3 = Get end time of interval: nucleus_tier, result
            if (.t-.t3 >= mindur_pause_gap)			; actual pause
               .after_pause = 1
            endif
         else
            .after_pause = 1						; actual pause
         endif
      else
         .after_pause = 0
      endif
      selectObject: nucldatID
      Set value: .j, j_after_pause, .after_pause
   endfor
   @debug_msg: "initialize_nucldat: exit"
endproc


procedure stylize_nuclei: .t1, .t2
# Stylize all nuclei within the specified time interval
   @debug_msg: "stylize_nuclei: entry"
   .tier = nucleus_tier
   mindur_syl = 0.01	; minimum duration for syllable, otherwise skipped
   @interval_from_time: nucleiID, .tier, .t1, "first_interval"
   @interval_from_time: nucleiID, .tier, .t2, "last_interval"

   .prev_nucleus = 0	; index (into textgrid tier) of previous valid nucleus; 0 indicates "not found yet"
   for .i from first_interval to last_interval
      selectObject: nucleiID
      @is_nucleus: .i
      if (result)
         @stylize_nucleus: .tier, .i, .prev_nucleus
         .prev_nucleus = .i
      endif
   endfor

   if (show_localrate)
      @calc_localrate: nucldatID, j_localrate_nucl, 2.5, 2.5
   endif
   @calc_isodur: nucldatID

   @debug_msg: "stylize_nuclei: phones_available='phones_available'"
   if (phones_available and segm_type <> segm_asyll)	; calculate vowel duration for spreadsheet
      @calc_vowel_duration: nucldatID, j_voweldur
      if (syllables_available)							; calculate syllable duration for spreadsheet
         @calc_rhyme_duration: nucldatID, j_rhymedur
         if (use_duration_model and fileReadable (duration_model_filename$))
            @calc_norm_duration: "syllable", j_en_sylldur, duration_model_filename$
            @calc_norm_duration: "rhyme", j_en_rhymedur, duration_model_filename$
         endif
      endif
   endif
   @debug_msg: "stylize_nuclei: exit"
endproc


procedure defined_intersection: .paramID, .t1, .t2
# Find region within <.t1>..<.t2> for which parameter is defined
# <.t1>..<.t2>			pitch-frame-synchronized times of region
# Returns <result> = 0 when fully undefined
# Returns <result1> and <result2>, the resulting defined interval
   @debug_msg: "defined_intersection: entry, t1='.t1:6' t2='.t2:6'"
   selectObject: .paramID
   .dx = Get time step
   repeat
      ; Trim undefined pitch frames at start and end of initial nucleus 
      .ok = 0
      while (.ok == 0 and .t1 <= .t2)		; skip undefined frames at start
         .v = Get value at time: .t1, "Hertz", "Linear"
         if (.v == undefined)
            ; @debug_msg: "defined_intersection: undefined frame at start of nucleus, t1='.t1:6'"
            .t1 += .dx 
         else
            .ok = 1
         endif
      endwhile
      .ok = 0
      while (.ok == 0 and .t2 > .t1)			; skip undefined frames at end
         .v = Get value at time: .t2, "Hertz", "Linear"
         if (.v == undefined)
            ; @debug_msg: "defined_intersection: undefined frame at end of nucleus, t2='.t2:6'"
            .t2 -= .dx 
         else
            .ok = 1
         endif
      endwhile
      ; Find undefined pitch frames inside the initial nucleus 
      if (.t2-.t1 > .dx)
         .ok = 1
         .t = .t1
         while (.ok and .t < .t2)			; find undefined frame somewhere between updated .t1 and .t2
            .v = Get value at time: .t, "Hertz", "Linear"
            if (.v == undefined)
               ; @debug_msg: "defined_intersection: undefined frame in middle of nucleus, t='.t:6'"
               .ok = 0
            else
               .t += .dx 
            endif
         endwhile
         if (.ok == 0)						; found undefined frame
            if (.t-.t1 < .t2-.t)			; select longest part 
               .t1 = .t+.dx					; prepare for next repeat-until loop starting at .t+.dx
            else
               .t2 = max(.t-.dx,.t1)		; use first part of interval
               .ok = 1						; exit repeat-until loop
            endif
            ; @debug_msg: "defined_intersection: kept interval: t1='.t1:6' t2='.t2:6'"
         endif
      endif
   until (.ok and .t1 < .t2) or (.t2-.t1 <= .dx) 
   result1 = .t1
   result2 = .t2
   if (.t2-.t1 >= .dx)						; result = 0 when too short
      result = 1
   else
      result = 0
      .dur = .t2-.t1
      ; @debug_msg: "defined_intersection: too short, dur='.dur:6' t1='.t1:6' t2='.t2:6' result='result'"
   endif
   @debug_msg: "defined_intersection: end, t1='.t1:6' t2='.t2:6' result='result'"
endproc


procedure octavejump: .grid, .param, .f0_median_Hz, .t1, .t2
# Find region within <t1>..<t2> for which pitch does not present discontinuities such as octave jumps
# Stores the position of discontinuity in point tier <discontinuity_tier> of <.grid>
# <.t1>..<.t2>				pitch-frame-synchronized times of region
# Return values:
# <result1>.. <result2>		the interval without octave jump
# <result3>					= 1 when discontinuity found, in which case <result2> is end of safe interval
   @debug_msg: "octavejump: entry, t1='.t1:3'"
   result3 = 0			; no discontinuity found
   selectObject: .param
   .dx = Get time step
   ; Find an octave jump. Select the part whose F0 is closer to median F0, provided it has sufficient duration.
   ; Repeat procedure for selected time interval, to deal with multiple octave jumps in same nucleus.
   repeat
      selectObject: .param
      result1 = .t1
      .ok = 1
      .t = .t1
      .f1 = Get value at time: .t, "Hertz", "Linear"
      while (.ok == 1 and .t <= .t2)
         selectObject: .param
         .f2 = Get value at time: .t, "Hertz", "Linear"
         if (abs(.f2-.f1)/min(.f1,.f2) > 0.3)	; was 0.2 initially and 0.5 in v2.7g
            .tdisc = .t-(.dx/2)
            ; @debug_msg: "octavejump: discontinuity at t='.tdisc:6'"
            .ok = 0
         else
            result2 = .t
            .t += .dx
            .f1 = .f2
         endif
      endwhile
      if (not .ok)
         result3 = 1		; discontinuity found
         selectObject: .grid
         Insert point: discontinuity_tier, .t-(.dx/2), ""
         selectObject: .param
         .durL = .t-.dx-.t1
         .durR = .t2-.t
         selectObject: intensityID
         .intL = Get mean: .t1, .t-.dx, "dB"
         .intR = Get mean: .t, .t2, "dB"
         ; Take into account both duration and deviation from median pitch of speech signal
         ; @msg: "octavejump: discontinuity at t='.tdisc:6' intensity L='.intL:2' R='.intR:2' duration L='.durL:4' R='.durR:6'"
         if (.durL >= mindur_syl and .durR >= mindur_syl)	; both parts are sufficiently long
            if (.intL - .intR >= 5) 						; left part clearly higher intensity 
               ; @msg: "octavejump: t='.tdisc:6', left part wins by intensity"
               result2 = .t-.dx
               .t = .t2+1									; force end of repeat-until loop
            elsif (.durL >= 0.5*(.t2-.t1))
               ; @msg: "octavejump: t='.tdisc:6', left part wins by duration"
               result2 = .t-.dx
               .t = .t2+1									; force end of repeat-until loop
            elsif (abs(.f1-.f0_median_Hz)/.f0_median_Hz < abs(.f2-.f0_median_Hz)/.f0_median_Hz) ; left part closer to median pitch
               ; @msg: "octavejump: t='.tdisc:6', left part wins: closer to mean F0"
               result2 = .t-.dx
               .t = .t2+1									; force end of repeat-until loop
            else
               ; @msg: "octavejump: t='.tdisc:6', right part wins: closer to mean F0"
               result1 = .t
               result2 = .t2
               .t1 = .t
            endif
         elsif (.durL >= mindur_syl)
            result2 = .t-.dx
            .t = .t2+1			; force end of repeat-until loop
         else ; (.durL < mindur_syl)
            result1 = .t
            result2 = .t2
            .t1 = .t
         endif
      endif
   until (.t >= .t2)
   @debug_msg: "octavejump: exit"
endproc


procedure stylize_nucleus: .tier, .i, .prev_nucleus
; <.tier>			tier where boundaries are stored 
; <.i>				index of nucleus to be stylized
; <.prev_nucleus>	index (of interval in TextGrid) of previous syllabic nucleus (i.e. where label == "a")
   @debug_msg: "stylize_nucleus: entry, i='.i'"

   ; Find row index in nucldatID for nucleus to be stylized
   selectObject: nucleiID
   x1 = Get start time of interval: .tier, .i
   x2 = Get end time of interval: .tier, .i
   .i = Get interval at time: pointer_tier, x1+(x2-x1)/2
   .s$ = Get label of interval: pointer_tier, .i
   .row = number(.s$)		; index of row in table nucldatID

   ; Set adaptive glissando value
   selectObject: nucldatID
   pause_follows = Get value: .row, j_before_pause
   glissando_local = glissando
   if (adaptive_glissando and pause_follows)
      glissando_local = glissando_low
   endif

   ; Find pitch frame index for start and end of nucleus
   selectObject: pitchID
   .dx = Get time step
   .f = Get frame number from time: x1
   frame1 = round(.f)
   .t = Get time from frame number: frame1
   if (abs(.t-x1) > .dx)					
      @debug_msg: "stylize_nucleus: x1='x1' f='.f' frame1='frame1' dx='.dx' time(round(frame))='.t'"
      frame1 += 1
   endif
   .f = Get frame number from time: x2
   frame2 = round(.f)
   .t = Get time from frame number: frame2
   if (abs(.t-x2) > .dx)					
      @debug_msg: "stylize_nucleus: x2='x2' f='.f' frame2='frame2' dx='.dx' time(round(frame))='.t'"
      frame2 -= 1
   endif
   if (frame2 <= frame1)
      @fatal_msg: "stylize_nucleus: frame2 (='frame2') <= frame1 (='frame1')"
   endif


# Step 1. Segmentation of pitch contour into tonal segments.
# Find turning points (TP) in contour, by order of importance.
# A TP is the point of largest distance between the raw F0 and the linear fit between F0 values at start and end.
# A TP is kept only 
# - if the difference in slope between the parts before and after the TP exceeds the differential glissando threshold, and 
# - if at least 1 of the parts is an audible pitch movement.
# When a TP is found, additional TPs are searched for in the left part, until none are found.
# Then the search continues for the interval between the last TP and the end of the nucleus interval.
   nrofts = 1				; number of tonal segments
   selectObject: stylID
   Add point: x1, 1
   Add point: x2, 1
   i1 = Get nearest index from time: x1
   xL = x1					; xL..xR is time interval where TP may be found
   xR = x2
   repeat
      nrofsplit = 0			; nrof turning points found in repeat loop 
      repeat				; find turning points
         split = 0			; nrof times split at turning point
         @slopeSTs: xL, xR, "g", "aud_A"
         if (aud_A)			; time interval contains audible pitch change  
            @turning_point: xL, xR
            if (maxdiftime >= 0)	; found a candidate turning point
               if ((maxdiftime - xL >= mindur_ts) and (xR - maxdiftime >= mindur_ts))
                  @slopeSTs: xL, maxdiftime, "g1", "aud_L"
                  @slopeSTs: maxdiftime, xR, "g2", "aud_R"
                  if ((abs (g2-g1) > diffgt) and (aud_L or aud_R) )  
                     split = 1		; found a valid turning point 
                  endif
               endif
            endif ; (maxdiftime >= 0)
            if (split)	       
               selectObject: stylID
               Add point: maxdiftime, 1
               xR = maxdiftime
               nrofsplit += 1		; turning points inserted in this loop
               nrofts += 1			; additional tonal segment found
            endif
         endif
      until (split = 0)
      if (xR < x2)			; interval was split; continue segmentation for right side
         i1 += 1			; adjust xL..xR analysis window
         selectObject: stylID
         xL = Get time from index: i1
         xR = Get time from index: i1+1
      else				; no split...
         nrofsplit = 0			; prepare for end of repeat loop
         xL = x2
      endif
   until (nrofsplit == 0 and xL >= x2)

# Step 2. Actual stylization. Also calculates prosodic features.
   ; values before stylization:
   sum_intra = 0			; sum of intrasyllabic pitch variation, before stylization
   sum_intra_up = 0			; sum of intrasyllabic pitch rises, before stylization
   sum_intra_down = 0		; sum of intrasyllabic pitch falls, before stylization
   sum_abs_intra = 0		; sum of absolute intrasyllabic pitch variation, before stylization
   ; values after stylization:
   sum_intra_styl = 0		; sum of intrasyllabic pitch variation, after stylization
   sum_intra_up_styl = 0	; sum of intrasyllabic pitch rises, after stylization
   sum_intra_down_styl = 0	; sum of intrasyllabic pitch falls, after stylization
   sum_abs_intra_styl = 0	; sum of absolute intrasyllabic pitch variation, after stylization
   dynamic_type = 0			; type: 0 = static, 1 = rising, -1 = falling
   selectObject: stylID
   i = Get nearest index from time: x1
   i2 = Get nearest index from time: x2
   ts = 1				; index of tonal segment under analysis
   while (i < i2)		; for each tonal segment
      selectObject: stylID
      xL = Get time from index: i
      xR = Get time from index: i+1
      @slopeSTs: xL, xR, "g", "aud_A"
      intST = dist		; pitch interval (in ST) in current tonal segment
      sum_intra += intST
      sum_abs_intra += abs (intST)
      sum_intra_up += max (intST, 0)
      sum_intra_down += min (intST, 0)
      # Check special case of two inaudible parts. e.g. bell-shaped contour
      if (aud_A = 1 and nrofts = 1)
         @turning_point: xL, xR
         if (maxdiftime >= 0)	; turning point found
            @slopeSTs: xL, maxdiftime, "g1", "aud_L"
            @slopeSTs: maxdiftime, xR, "g2", "aud_R"
            d1 = maxdiftime - xL
            d2 = xR - maxdiftime
            if (aud_L == 0 and aud_R == 0)  
               if ((g1 > 0 and g2 < 0) or (g1 < 0 and g2 > 0))  
                  aud_A = 0	; consider inaudible
                  intST = 0
               endif
            endif
         endif
      endif ; special case
      selectObject: pitchID
      yR = Get value at time: xR, "Hertz", "Linear"
      yL = Get value at time: xL, "Hertz", "Linear"
      yM = Get quantile: xL, xR, 0.5, "Hertz"
      selectObject: stylID
      if (ts == 1)			; first tonal segment of nucleus => also set value at xL, start of tonal segment
         if (aud_A = 0)
            yR = yM			; normalize pitch to median pitch of tonal segment
            yL = yM
         endif
         Remove point: i	; to replace value of point at xL
         Add point: xL, yL	; set Y value of turning point at xL
         pv_lo = min (yL, yR)	; initialize pv_lo
         pv_hi = max (yL, yR)	; initialize pv_hi
         pv_start = yL
      endif
      ; Update Y value of turning point at time xR 
         Remove point: i+1		; to replace value of point at xR
         Add point: xR, yR		; set Y value of turning point at xR
      ; Following lines use values after stylization
      intST = 12 * log2 (yR/yL)
      if (aud_A)
         sum_intra_styl += intST
         sum_abs_intra_styl += abs (intST)
         sum_intra_up_styl += max (intST, 0)
         sum_intra_down_styl += min (intST, 0)
         dynamic_type = 1
      endif
      pv_lo = min (pv_lo, yR)
      pv_hi = max (pv_hi, yR)
      ts += 1
      i += 1
   endwhile			; for each tonal segment
   if (dynamic_type == 1)
      if (abs(sum_intra_down_styl) > sum_intra_up_styl)
         dynamic_type = -1
      endif
   endif
   selectObject: pitchID
   v_f0_min = Get minimum: x1, x2, "Hertz", "Parabolic"
   v_f0_max = Get maximum: x1, x2, "Hertz", "Parabolic"
   pv_median = Get quantile: x1, x2, 0.50, "Hertz"
   pv_mean = Get mean: x1, x2, "Hertz"
   if (.prev_nucleus == 0)		; first nucleus in analysis window
      .prev_x2 = anal_t1			; start of analysis window
      pv_intersyllab = 0
   else				
      selectObject: nucldatID
      .prev_x2 = Get value: .row-1, j_nucl_t2		; endtime of previous nucleus
      .v = Get value: .row-1, j_f0_end				; F0 at end of previous nucleus
      pv_intersyllab = 12 * log2 (pv_start/.v)
      selectObject: nucleiID
   endif

   if (syllables_available)
      selectObject: nucleiID
      .imid = Get interval at time: syllable_tier, x1+(x2-x1)/2
      syllt1 = Get start time of interval: syllable_tier, .imid
      syllt2 = Get end time of interval: syllable_tier, .imid
      sylldur = syllt2 - syllt1
   else
      sylldur = undefined
      syllt2 = undefined
   endif

; Determine whether the original nucleus contains an F0 discontinuity, which was detected by @octavejump (called by safe_nuclei)
   selectObject: nucleiID
   .k = Get number of points: discontinuity_tier
   .i = Get high index from time: discontinuity_tier, x1
   if (.i > 0 and .i <= .k)		; time x1 > time of last discontinuity
      .t = Get time of point: discontinuity_tier, .i
   else
      .t = -1 
   endif
   v_f0_discont = 0
   if (.t >= x1-time_step and .t <= x2+time_step)	; discontinuity within nucleus
      v_f0_discont = 1
   endif

; Store all parameters for syllable in table (some post-editing for speaker turns is done in @pitchrange_speakers)
   selectObject: nucldatID
   Set value: .row, j_f0_start, 	floor(pv_start)
   Set value: .row, j_f0_end, 		floor(yR)
   Set value: .row, f0_min, 		floor(v_f0_min)
   Set value: .row, f0_max, 		floor(v_f0_max)
   Set value: .row, f0_median, 		floor(pv_median)
   Set value: .row, j_f0_mean, 		floor(pv_mean)
   Set value: .row, j_f0_meanST, 	number(fixed$(12 * log2 (pv_mean),2))
   Set value: .row, lopitch, 		floor(pv_lo)
   Set value: .row, hipitch, 		floor(pv_hi)
   Set value: .row, j_hipitchST, 	number(fixed$(12 * log2 (pv_hi),2))
   Set value: .row, j_dynamic, 		dynamic_type
   Set value: .row, j_intrasyl, 	number(fixed$(sum_intra_styl,2))
   Set value: .row, j_traj, 		number(fixed$(sum_abs_intra_styl,2))
   Set value: .row, j_intrasylup,	number(fixed$(sum_intra_up_styl,2))
   Set value: .row, j_intrasyldown,	number(fixed$(sum_intra_down_styl,2))
   Set value: .row, j_intersyl,		number(fixed$(pv_intersyllab,2))
   Set value: .row, j_f0_discont,	v_f0_discont
; j_internucldur = time between end of previous nucleus and start of current one
   Set value: .row, j_internucldur,	number(fixed$(x1-.prev_x2,4))
   if (syllables_available)
      Set value: .row, j_syllabledur, number(fixed$(sylldur,4))
   endif
;   endtime_syll: used by plot_salience (only for appropriate segmentation method)
   if (calc_prominence)
      Set value: .row, endtime_syll, syllt2
   endif
   if (needs_loudness and loudness_available)
      selectObject: loudnessID
      v = Get maximum: x1, x2, "None"
      selectObject: nucldatID
      Set value: .row, j_loudness, number(fixed$(v,3))
   endif
   selectObject: intensityID
   .v = Get maximum: x1, x2, "None"
   if (.v == undefined) 
      @msg: "Warning: (stylize_nucleus:) maximum intensity undefined for time interval 'x1:3'..'x2:3'"
      .v = 0
   endif
   selectObject: nucldatID
   Set value: .row, j_int_peak, number(fixed$(.v,1))

   @debug_msg: "stylize_nucleus: exit"
endproc


procedure slopeSTs: .t1, .t2, varname1$, varname2$
# Calculate slope of F0 variation (in ST/s) in time interval <.t1>..<.t2>.
# Return slope in global variable named in <varname1$>.
# Determine whether pitch change is audible, i.e. above glissando threshold.
# Return audibility in global variable named in <varname2$>.
# Return values:
#   <slopeSTs>	slope
#   <dist>		pitch interval (in ST)
   selectObject: pitchID
   .max = Get maximum: .t1, .t2, "Hertz", "None"
   .min = Get minimum: .t1, .t2, "Hertz", "None"
   .tmax = Get time of maximum: .t1, .t2, "Hertz", "None"
   .tmin = Get time of minimum: .t1, .t2, "Hertz", "None"
   if (.tmin <= .tmax) 
      dist = 12 * log2 (.max/.min)
   else 
      dist = 12 * log2 (.min/.max)
   endif
   .dur = .t1-.t2
   .slopeSTs = dist/.dur
   'varname1$' = .slopeSTs
   if (abs (.slopeSTs) >= glissando_local/(.dur*.dur))
      'varname2$' = 1
   else 
      'varname2$' = 0
   endif
endproc


procedure turning_point: .t1, .t2
# Find most important turning point.
# Returns: 
#    <maxdiftime> = time of turning point in time interval <.t1>..<.t2> OR -1 if max difference is too small ( < 1 ST ) 
   selectObject: pitchID
   .dx = Get time step
   .jf1 = Get frame number from time: .t1
   .jf1 = round(.jf1)
   if (.jf1 < frame1)
      @msg: "turning_point: jf1='.jf1' < frame1='frame1', at t1='.t1:4'"
      .jf1 = frame1
   endif
   .f01 = Get value in frame: .jf1, "Hertz"
   if (.f01 == undefined)
      .t = Get time from frame number: .jf1
      @fatal_error: "turning_point: Pitch undefined at (left boundary) time='.t:6' .t1='.t1:6' .t2='.t2:6' .jf1='.jf1' .jf2='.jf2'" 
   endif
   .jf2 = Get frame number from time: .t2
   .jf2 = round(.jf2)
   if (.jf2 > frame2)
      @msg: "turning_point: jf2='.jf2' > frame2='frame2' at t2='.t2:4'" 
      .jf2 = frame2
   endif
   .f02 = Get value in frame: .jf2, "Hertz"
   if (.f02 == undefined)
      .t = Get time from frame: .jf2
      @fatal_error: "turning_point: Pitch undefined at (right boundary) time='.t:6' .t1='.t1:6' .t2='.t2:6' .jf1='.jf1' .jf2='.jf2'" 
   endif
   .b = (.f02 - .f01) / ((.jf2-.jf1)*.dx)
   .maxdif = 0
   .maxdiffit = 1
   maxdiftime = .t1
   .jmaxdif = .jf1
   for .j from .jf1 to .jf2
      .f0 = Get value in frame: .j, "Hertz"
      if (.f0 == undefined)
         .t = Get time from frame: .j
         @fatal_error: "turning_point: Pitch undefined at '.t:6' .t1='.t1:6' .t2='.t2:6' .jf1='.jf1' .jf2='.jf2'" 
      endif
      .fit = .f01 + .b * ((.j-.jf1)*.dx)
      .dy = abs (.f0 - .fit)
      if (.dy > .maxdif)
         .maxdif = .dy
         .jmaxdif = .j
         .maxdiffit = .fit
      endif
   endfor
   if (.maxdif == 0)
      maxdiftime = -1
   elsif (abs(12 * log2 (.maxdif/.maxdiffit)) < 1)	; smaller than 1 ST 
      maxdiftime = -1
   else
      maxdiftime = Get time from frame: .jmaxdif
   endif
endproc


procedure calc_vowel_duration: .table, .dst
# Calculate vowel duration.
# <dst>		column where results are stored
   .nrerr = 0
   selectObject: .table
   .rows = Get number of rows
   for .row to .rows	; for each nuclei in the signal
      selectObject: .table
      Set value: .row, .dst, 0
      .t1 = Get value: .row, j_nucl_t1
      .t2 = Get value: .row, j_nucl_t2
      .xmid = .t1+(.t2-.t1)/2
      ; @msg: "calc_vowel_duration: row='.row' nucl_t1='.t1:5' nucl_t2='.t2:5' xmid='.xmid:5'"
      selectObject: nucleiID
      if (syllables_available and phones_available)
         .i = Get interval at time: syllable_tier, .xmid
         .syll$ = Get label of interval: syllable_tier, .i 
         .x1 = Get start time of interval: syllable_tier, .i
         .x2 = Get end time of interval: syllable_tier, .i
         .i = Get interval at time: phone_tier, .x1
         .i2 = Get interval at time: phone_tier, .x2
         repeat
            selectObject: nucleiID
            .label$ = Get label of interval: phone_tier, .i
            @is_vowel: .label$
            if (is_vowel)
               .x1 = Get start time of interval: phone_tier, .i
               .x2 = Get end time of interval: phone_tier, .i
            elsif (.i+1 >= .i2)
               @msg: "No vowel in syllable <'.syll$'> at '.xmid:3'"
               .nrerr += 1
               ; use syllable duration (.x1, .x2)
            endif
            .i += 1
         until (is_vowel or .i >= .i2)
      elsif (phones_available)
         .i = Get interval at time: phone_tier, .t1
         .i2 = Get interval at time: phone_tier, .t2
         repeat
            selectObject: nucleiID
            .label$ = Get label of interval: phone_tier, .i
            @is_vowel: .label$
            ; @msg: "calc_vowel_duration: repeat: i='.i' label='.label$' is_vowel='is_vowel'"
            if (is_vowel)
               .x1 = Get start time of interval: phone_tier, .i
               .x2 = Get end time of interval: phone_tier, .i
            elsif (.i+1 >= .i2)
               @debug_msg: "No vowel in nucleus at '.xmid:3'"
               .nrerr += 1
               ; use nucleus duration
               .x1 = .t1
               .x2 = .t2
            endif
            .i += 1
         until (is_vowel or .i >= .i2)
      endif
      selectObject: .table
      Set value: .row, .dst, number(fixed$(.x2-.x1,4))
   endfor
   if (.nrerr)
      @msg: "Warning: calc_vowel_duration: No vowel label found for '.nrerr' nuclei"
   endif
endproc


procedure calc_rhyme_duration: .table, .dst
# Calculate duration of rhyme using intervals in the syllable and phoneme tiers
# <.dst>		column where results are stored
   selectObject: .table
   .rows = Get number of rows
   for .j to .rows	; nrof_nuclei_analysed
      Set value: .j, .dst, undefined	; prepare for possible error in annotation or lacking annotation
   endfor
   @tier_number_by_name: segmentationID, "^phon"
   if (result)
      for .j to nrof_nuclei_analysed		; calculate rime duration
         selectObject: .table
         .x1 = Get value: .j, j_nucl_t1
         .x2 = Get value: .j, j_nucl_t2
         @interval_from_time: nucleiID, syllable_tier, .x1+(.x2-.x1)/2, "syll"
         .syll_x1 = Get start time of interval: syllable_tier, syll
         .syll_x2 = Get end time of interval: syllable_tier, syll
         @interval_from_time: nucleiID, phone_tier, .syll_x1, "ph1"
         @interval_from_time: nucleiID, phone_tier, .syll_x2-0.001, "ph2"
         .phon = ph1
         .nrof_syllabics = 0
         repeat
            selectObject: nucleiID
            .label$ = Get label of interval: phone_tier, .phon
            @is_syllabic: .label$
            if (result)
               .t = Get start time of interval: phone_tier, .phon
               selectObject: .table
               Set value: .j, .dst, number(fixed$(.syll_x2-.t,4))	; duration of rhyme
               .nrof_syllabics += 1
            endif
            .phon += 1
         until (result or .phon > ph2)
         if (.nrof_syllabics == 0)
            @msg: "calc_rhyme_duration: Syllable without syllabic sound at time 'syll_x1:3'"
         endif
      endfor
   endif
endproc


procedure calc_nPVI: .table, .src
# Calculate Normalized Pairwise Variability Index on data in <table>
# <.src>	column where values are taken from
# <result>	nPVI for all data in src
   result = 0
   selectObject: .table
   .rows = Get number of rows
   .sum = 0
   if (.rows > 1)		; need at least 2 syllables
      for .row from 2 to .rows	; for each nucleus in the table
         .y = Get value: .row, .src
         .y1 = Get value: .row-1, .src
         .sum += abs( (.y1-.y) / ((.y1+.y)/2) )
      endfor
      result = 100*.sum/(.rows-1)
   endif
endproc


procedure pitchrange_speakers
; Compute 
; - pitch range for each speaker using high and low pitch values of each syllable
; - "pitch profile": pitch trajectory, proportion of glissandos, rises, falls...
; - temporal profile (speech_rate, pause duration, nucleus duration, nPVI...) 
; Should be called after stylization.
   @debug_msg: "pitchrange_speakers: entry"
   if (speakers < 1)				; speaker are numbered from 1 to N
      @error_msg: "pitchrange_speakers: Expected >= 1 speaker"
   endif
   if (needs_pitchrange < 1)
      @fatal_error: "pitchrange_speakers: no pitch range measurement activated"
   endif
   selectObject: nucldatID
   .rows = Get number of rows
   if (.rows < 1)
      @fatal_error: "pitchrange_speakers: 0 rows in nucldatID" 
   endif
   @debug_msg: "pitchrange_speakers: '.rows' rows in nucldatID"

; For each speaker:
   for speaker_j from 1 to speakers
      label$ = speaker_label'speaker_j'$
      speaker_nnucl [speaker_j] = 0	; nrof nuclei for this speaker

      @debug_msg: "pitchrange_speakers: speaker_range_'speaker_j'"

; (1) For speaker X, compute quantiles of low and high pitch values of each nucleus after stylization. 
; To select syllables from speaker, create temporary table from which other speakers are discarded.
; Also discard nuclei containing a pitch discontinuity.
      selectObject: nucleiID
      k = Get number of points: discontinuity_tier
      selectObject: nucldatID
      rows = Get number of rows
      rows2 = rows * 2		; uses 2 pitch values per syllable
      ; TableOfReal object does not have command "Get quantile...", so we use "Table without column names" object instead
      tmptableID = Create Table without column names: "pitchvalues", rows2, 2
      Set column label (index): 1, "pitch_ST"
      Set column label (index): 2, "f0_Hz"
      n = 0			; nrof data points 
      for j from 1 to rows
         selectObject: nucldatID
         id = Get value: j, j_speaker_id
         if (id == speaker_j)
            speaker_nnucl [speaker_j] += 1
            .t1 = Get value: j, j_nucl_t1
            .t2 = Get value: j, j_nucl_t2
            .vlo = Get value: j, lopitch
            .vhi = Get value: j, hipitch
            .f0_hi = Get value: j, f0_max
            .f0_lo = Get value: j, f0_min
            selectObject: nucleiID
            .i = Get high index from time: discontinuity_tier, .t1
            if (.i > 0 and .i <= k)		; time .t1 > time of last discontinuity
               .t = Get time of point: discontinuity_tier, .i
            else
               .t = -1 
            endif
            if (.t >= .t1-time_step and .t <= .t2+time_step)	; avoid data at discontinuity
               @debug_msg: "pitchrange_speakers: discontinuity, skipped nucleus at '.t1:2'"
            else
               selectObject: tmptableID
               n += 1
               Set numeric value: n, "pitch_ST", hertzToSemitones (.vlo) - hertzToSemitones(1)
               Set numeric value: n, "f0_Hz", .f0_lo
               n += 1
               Set numeric value: n, "pitch_ST", hertzToSemitones (.vhi) - hertzToSemitones(1)
               Set numeric value: n, "f0_Hz", .f0_hi
            endif
         endif
      endfor
      if (n = 0)		; no valid points are available for this speaker, use default values
         @msg: "Warning: pitchrange measurement: no pitch data for speaker 'speaker_j' <'label$'>"  
         mean = 150
         median = 150
         bottom = 150
         top    = 150
         stdev  = 0
         .mean_of_ST = 0
         .stdev_of_ST = 0
      else
         if (n < 200)
            @msg: "Warning: too few syllables for robust pitchrange detection of speaker <'label$'>"  
         endif
         selectObject: tmptableID
         if (n < rows2)			; number of row in table should match number of values
            .row = rows2
            while (.row > n)	; remove unused rows at end of table
               Remove row: .row
               .row -= 1
            endwhile
         endif
         medianST = Get quantile: "pitch_ST", 0.5
         .rows = Get number of rows
         .row = .rows
         while (.row >= 2)			; remove PAIRS of rows if outsiders
            .v2 = Get value: .row, "pitch_ST"
            .v1 = Get value: .row-1, "pitch_ST"
            if (abs(.v2 - medianST) > 18 or abs(.v1 - medianST) > 18)	; discard manifest errors
               Remove row: .row
               Remove row: .row-1
            endif
            .row -= 2
         endwhile
         n = Get number of rows
         mean = Get mean: "f0_Hz"
         median = Get quantile: "f0_Hz", 0.5
         bottom = Get quantile: "f0_Hz", 0.02
         top    = Get quantile: "f0_Hz", 0.98
         quartile1 = Get quantile: "f0_Hz", 0.25
         quartile3 = Get quantile: "f0_Hz", 0.75
         ; p60 = Get quantile: "f0_Hz", 0.60
         .mean_of_ST = Get mean: "pitch_ST"
         .stdev_of_Hz = Get standard deviation: "f0_Hz"
         .stdev_of_ST = Get standard deviation: "pitch_ST"
         mean_rf0 = Get mean: "f0_Hz"				; rf0 = raw F0
         p50_rf0 = Get quantile: "f0_Hz", 0.5
         p02_rf0 = Get quantile: "f0_Hz", 0.02
         p98_rf0 = Get quantile: "f0_Hz", 0.98
         if (.stdev_of_Hz == undefined or .stdev_of_ST == undefined)		; when insufficient data
            .stdev_of_Hz = 0
            .stdev_of_ST = 0
         endif
      endif ; else
      .nrofvalid = n/2	; nrof nuclei after removal of outliers and discontinuities
      meanST   = hertzToSemitones (mean) - hertzToSemitones(1)
      medianST = hertzToSemitones (median) - hertzToSemitones(1)
      bottomST = hertzToSemitones (bottom) - hertzToSemitones(1)
      topST    = hertzToSemitones (top) - hertzToSemitones(1)
      removeObject: tmptableID
      label$ = speaker_label'speaker_j'$
      range = topST - bottomST
      upper_range = 12 * log2 (top/median)
      lower_range = range - upper_range

     ; Following line needed for prosoplot.praat
      speaker_range_'speaker_j'$ = "TOP_ST='topST:1' BOTTOM_ST='bottomST:1' MEDIAN_ST='medianST:1' "

	  
; To select syllables from speaker X, create temporary TableOfReal, from which other speakers are discarded.
; Computed values include mean and stdev for intrasyllabic and intersyllabic pitch variation,
; temporal profile (speech_rate, pause duration, nucleus duration, nPVI...), etc. 
      selectObject: nucldatID
      .tmptableID = Copy: "tmptable2"
      selectObject: .tmptableID
      Rename: "tmptable2"
      .j = Get number of rows
      repeat 					; Discard info (i.e. rows) from other speakers
         .spkr = Get value: .j, j_speaker_id
         if (speaker_j <> .spkr)
            Remove row (index): .j
         endif
         .j -= 1
      until (.j = 0)
      .rows = Get number of rows
      ; @debug_msg: "pitchrange_speakers: '.rows' rows in tmptable2"
      .sum_traj_intra = 0		; intrasyllabic trajectory, after stylization
      .sum_traj_inter = 0		; intersyllabic trajectory, after stylization
      .sum_nucldur = 0
      .sum_internucldur = 0		; time between nuclei, corrected for pauses
      .sum_pausedur = 0			; time of pauses for current speaker
      .nsyll = 0				; number of syllables for current speaker
      .nrises = 0
      .nfalls = 0				
      .ngliss = 0
      .nflat = 0
      for .j from 1 to .rows
         .nsyll += 1
         .sum_nucldur += Get value: .j, j_nucldur
         .v_traj_inter = Get value: .j, j_intersyl
         .v_internucldur = Get value: .j, j_internucldur
         if (.v_internucldur < mindur_pause_gap)	; gap is not a pause
            .sum_internucldur += .v_internucldur
            .sum_traj_inter += abs(.v_traj_inter)
         else						; pauses are not counted for total internucleus duration and internucleus trajectory 
            .sum_pausedur += .v_internucldur
            Set value: .j, j_intersyl, 0			; in .tmptableID !! used later for mean and stddev
         endif
;@debug_msg: "j='.j' internucldur='.v_internucldur:3' intersyllab='.v_traj_inter:3' sum_internucldur='.sum_internucldur:2' sum_traj_inter='.sum_traj_inter:2' sum_pausedur='.sum_pausedur:2'"
         .v = Get value: .j, j_dynamic
         if (.v == 0)
            .nflat += 1
         endif
         .sum_traj_intra += Get value: .j, j_traj
         .v_up = Get value: .j, j_intrasylup
         if (.v_up >= 4)
            .nrises += 1
         endif
         .v_down = Get value: .j, j_intrasyldown
         if (.v_down <= -4)
            .nfalls += 1
         endif
         if (.v_up >= 4 or .v_down <= -4)		; either rise or fall, but counted once
            .ngliss += 1
         endif
      endfor
      .sum_speechdur = .sum_nucldur + .sum_internucldur + .sum_pausedur
      .rate_j = 0 
      if (.sum_nucldur + .sum_internucldur > 0) 
         .rate_j = .nsyll / (.sum_nucldur + .sum_internucldur)	; speech rate for current speaker 
      endif
      .traj_intra_rate = 0		; time normalized trajectory values; initialize
      .traj_inter_rate = 0
      .traj_phon_rate = 0
      if (.sum_nucldur > 0)
         .traj_intra_rate = .sum_traj_intra/.sum_nucldur
      endif
      if (.sum_internucldur > 0)
         .traj_inter_rate = .sum_traj_inter/.sum_internucldur
         .traj_phon_rate = (.sum_traj_inter + .sum_traj_intra)/(.sum_internucldur + .sum_nucldur)
      endif
;@msg: "speaker 'speaker_j': sum_traj_inter='.sum_traj_inter:2' sum_internucldur='.sum_internucldur:2'"
;@msg: "speaker 'speaker_j': trajectory intra='.traj_intra_rate:2' inter='.traj_inter_rate:2' phon='.traj_phon_rate:2'"
      ; .mean_intra_up = Get column mean (index): j_intrasylup
      ; .stdev_intra_up = Get column stdev (index): j_intrasylup
      ; .mean_abs_inter = Get column mean (index): j_intersyl
      ; .stdev_abs_inter = Get column stdev (index): j_intersyl
      .mean_nucldur = Get column mean (index): j_nucldur
      if (.rows > 1)
         .stdev_nucldur = Get column stdev (index): j_nucldur
      else		; otherwise undefined
         .stdev_nucldur = 0
      endif
;@msg: "speaker 'speaker_j': stdev_nucldur='.stdev_nucldur'"
;@msg: "pitchrange_speakers: calc_nPVI: j_nucldur"
      @calc_nPVI: .tmptableID, j_nucldur
      .nPVI_nucldur = result
      .nPVI_voweldur = 0
      if (phones_available and segm_type <> segm_asyll)
;@msg: "pitchrange_speakers: calc_nPVI: j_voweldur"
         @calc_nPVI: .tmptableID, j_voweldur
         .nPVI_voweldur = result
      endif
      .nPVI_sylldur = 0
      if (syllables_available and segm_type <> segm_asyll)
;@msg: "pitchrange_speakers: calc_nPVI: j_syllabledur"
         @calc_nPVI: .tmptableID, j_syllabledur
         .nPVI_sylldur = result
      endif
;@msg: "pitchrange_speakers: calc_nPVI: ready"
      removeObject: .tmptableID
      if (.ngliss > 0) 
         .prises = 100*.nrises/.rows
         .pfalls = 100*.nfalls/.rows
         .pgliss = 100*.ngliss/.rows
      else
         .prises = 0
         .pfalls = 0
         .pgliss = 0
      endif
      .pstatic = 100*.nflat/.rows
      .v1 = .traj_intra_rate/.stdev_of_ST		; time-normalised intrasyllabic trajectory in z-score
      .v2 = .traj_inter_rate/.stdev_of_ST		; time-normalised intersyllabic trajectory in z-score
      .v3 = .traj_phon_rate/.stdev_of_ST		; time-normalised total trajectory in z-score
      if (.v1 == undefined)			; when insufficient nrof data
         .v1 = 0
         .v2 = 0
         .v3 = 0
      endif
      .ppauses = 100*.sum_pausedur/(.sum_internucldur+.sum_nucldur+.sum_pausedur)
      .pphon = 100*(.sum_nucldur+.sum_internucldur)/(.sum_internucldur+.sum_nucldur+.sum_pausedur)
;      .est_phon_time = .sum_internucldur + .sum_nucldur
      .nnucl = speaker_nnucl [speaker_j]

      selectObject: profileID
      .row = speaker_j
      Set value: .row, j_speaker_nr,		speaker_j
      Set value: .row, j_speech_rate,		'.rate_j:3'
      Set value: .row, j_speech_time,		'.sum_speechdur:3'
      Set value: .row, j_nrofnucl,			'.nnucl'
      Set value: .row, j_nrofvalid,			'.nrofvalid'
      Set value: .row, j_tot_nucldur,		'.sum_nucldur:3'
      Set value: .row, j_tot_internucldur,	'.sum_internucldur:3'
      Set value: .row, j_tot_pausedur,		'.sum_pausedur:3'
      Set value: .row, j_propphon,			'.pphon:2'
      Set value: .row, j_proppause,			'.ppauses:2'
      Set value: .row, j_pitch_median_Hz,	'median:0'
      Set value: .row, j_pitch_median_ST,	'medianST:1'
      Set value: .row, j_pitch_mean_Hz,		'mean:0'
      Set value: .row, j_pitch_mean_ST,		'meanST:1'
      Set value: .row, j_pitch_mean_of_ST,	'.mean_of_ST:2'
      Set value: .row, j_pitch_stdev_of_Hz,	'.stdev_of_Hz:3'
      Set value: .row, j_pitch_stdev_of_ST,	'.stdev_of_ST:3'
      Set value: .row, j_pitch_range,		'range:1'
      Set value: .row, j_pitch_top_ST,		'topST:1'
      Set value: .row, j_pitch_bottom_ST,	'bottomST:1'
      Set value: .row, j_pitch_top_Hz,		'top:1'
      Set value: .row, j_pitch_bottom_Hz,	'bottom:1'
      Set value: .row, j_rawf0_p02,			'p02_rf0:0'
      Set value: .row, j_rawf0_p25,			'quartile1:0'
      Set value: .row, j_rawf0_p50,			'p50_rf0:0'
      Set value: .row, j_rawf0_p75,			'quartile3:0'
      Set value: .row, j_rawf0_p98,			'p98_rf0:0'
      Set value: .row, j_rawf0_mean,		'mean_rf0:0'
      Set value: .row, j_prop_level,		'.pstatic:1'
      Set value: .row, j_prop_gliss,		'.pgliss:1'
      Set value: .row, j_prop_rises,		'.prises:1'
      Set value: .row, j_prop_falls,		'.pfalls:1'
      Set value: .row, j_traj_intra,		'.traj_intra_rate:2'
      Set value: .row, j_traj_inter,		'.traj_inter_rate:2'
      Set value: .row, j_traj_phon,			'.traj_phon_rate:2'
      Set value: .row, j_traj_intra_z,		'.v1:2'
      Set value: .row, j_traj_inter_z,		'.v2:2'
      Set value: .row, j_traj_phon_z,		'.v3:2'
      Set value: .row, j_nucldur_mean,		'.mean_nucldur:3'
      Set value: .row, j_nucldur_stdev,		'.stdev_nucldur:3'
      Set value: .row, j_nPVI_nucldur,		'.nPVI_nucldur:2'
      if (phones_available)
         Set value: .row, j_nPVI_voweldur,	'.nPVI_voweldur:2'
      endif
      if (syllables_available)
         Set value: .row, j_nPVI_sylldur,	'.nPVI_sylldur:2'
      endif

   endfor ; for speaker_j
   @debug_msg: "pitchrange_speakers: exit"
endproc


procedure pitchrange_speakers_report
   selectObject: profileID
   @length_longest_speaker_label
   leading$ = "     "
   .buf$ = "Pitch range of speaker(s): (based on 2 stylization values per nucleus)'newline$'"
   @sprint_fs: len, "Speaker label"
   .buf$ += leading$ + result$ + ": Range, Bottom, Mean, Median, Top, MeanOfST, StdevOfST" + newline$
   for .speaker from 1 to speakers
      ;.row = reportID_row_offset + .speaker
      .row = .speaker
      .label$ = speaker_label'.speaker'$
      @sprint_fs: len, .label$
      .buf$ += leading$ + result$ + ": "
      .v =  Get value: .row, j_pitch_range
      .buf$ += "'.v:1'ST, "
      .v =  Get value: .row, j_pitch_bottom_Hz
      .buf$ += "'.v:0'Hz "
      .v =  Get value: .row, j_pitch_bottom_ST
      .buf$ += "('.v:1'ST), "
      .v =  Get value: .row, j_pitch_mean_Hz
      .buf$ += "'.v:0'Hz "
      .v =  Get value: .row, j_pitch_mean_ST
      .buf$ += "('.v:1'ST), "
      .v =  Get value: .row, j_pitch_median_Hz
      .buf$ += "'.v:0'Hz "
      .v =  Get value: .row, j_pitch_median_ST
      .buf$ += "('.v:1'ST), "
      .v =  Get value: .row, j_pitch_top_Hz
      .buf$ += "'.v:0'Hz "
      .v =  Get value: .row, j_pitch_top_ST
      .buf$ += "('.v:1'ST), "
      .v =  Get value: .row, j_pitch_mean_of_ST
      .buf$ += "'.v:1'ST, "
      .v =  Get value: .row, j_pitch_stdev_of_ST
      .buf$ += "'.v:1'ST"
      .buf$ += newline$
   endfor
   result$ = .buf$
endproc


procedure raw_pitchrange_speakers_report
   selectObject: profileID
   @length_longest_speaker_label
   leading$ = "     "
   .buf$ = "Pitch range of speaker(s): (based on 2 raw F0 values per nucleus)'newline$'"
   @sprint_fs: len, "Speaker label"
   .buf$ += leading$ + result$ + ": P02, Mean, Median, P98" + newline$
   for .speaker from 1 to speakers
      ;.row = reportID_row_offset + .speaker
      .row = .speaker
      .label$ = speaker_label'.speaker'$
      @sprint_fs: len, .label$
      .buf$ += leading$ + result$ + ": "
      .v =  Get value: .row, j_rawf0_p02
      .buf$ += "'.v:0'Hz, "
      .v =  Get value: .row, j_rawf0_mean
      .buf$ += "'.v:0'Hz, "
      .v =  Get value: .row, j_rawf0_p50
      .buf$ += "'.v:0'Hz, "
      .v =  Get value: .row, j_rawf0_p98
      .buf$ += "'.v:0'Hz"
      .buf$ += newline$
   endfor
   result$ = .buf$
endproc


procedure pitchprofile_speakers_report
   selectObject: profileID
   @length_longest_speaker_label
   leading$ = "     "
   .buf$ = "Pitch variability of speaker(s):'newline$'"
   @sprint_fs: len, "Speaker label"
   .buf$ += leading$ + result$ + ": TrajIntra, TrajInter, TrajPhon, TrajIntraZ, TrajInterZ, TrajPhonZ, PropLevel, Gliss, Rises, Falls" + newline$
   for .speaker from 1 to speakers
      ;.row = reportID_row_offset + .speaker
      .row = .speaker
      .label$ = speaker_label'.speaker'$
      @sprint_fs: len, .label$
      .buf$ += leading$ + result$ + ": "
      .v =  Get value: .row, j_traj_intra
      .buf$ += "'.v:1' ST/s, "
      .v =  Get value: .row, j_traj_inter
      .buf$ += "'.v:1' ST/s, "
      .v =  Get value: .row, j_traj_phon
      .buf$ += "'.v:1' ST/s, "
      .v =  Get value: .row, j_traj_intra_z
      .buf$ += "'.v:1' sd/s, "
      .v =  Get value: .row, j_traj_inter_z
      .buf$ += "'.v:1' sd/s, "
      .v =  Get value: .row, j_traj_phon_z
      .buf$ += "'.v:1' sd/s, "
      .v =  Get value: .row, j_prop_level
      .buf$ += "'.v:1'%, "
      .v =  Get value: .row, j_prop_gliss
      .buf$ += "'.v:1'%, "
      .v =  Get value: .row, j_prop_rises
      .buf$ += "'.v:1'%, "
      .v =  Get value: .row, j_prop_falls
      .buf$ += "'.v:1'% "
      .buf$ += newline$
   endfor
   result$ = .buf$
endproc


procedure duration_profile_speakers_report
   @length_longest_speaker_label
   leading$ = "     "
   selectObject: profileID
   .buf$ = "Temporal profile of speaker(s):" + newline$
   @sprint_fs: len, "Speaker label"
   .buf$ += leading$ + result$ + ": SpeechRate, TotalDur, %Phonation, %Pauses, PhonTime, NuclDur, InterNuclDur, PauseDur" + newline$
   for .speaker from 1 to speakers
      ;.row = reportID_row_offset + .speaker
      .row = .speaker
      .label$ = speaker_label'.speaker'$
      @sprint_fs: len, .label$
      .buf$ += leading$ + result$ + ": "
      .v =  Get value: .row, j_speech_rate
      .buf$ += "'.v:2' syll/s, "
      .v_nucldur =  Get value: .row, j_nucldur
      .v_internucldur =  Get value: .row, j_internucldur
      .v_pausedur =  Get value: .row, j_tot_pausedur
      .v_total = .v_nucldur + .v_internucldur + .v_pausedur
      .buf$ += "'.v_total:3' s, "
      .p = 100*(.v_nucldur + .v_internucldur)/.v_total
      .buf$ += "'.p:1'%, "
      .p = 100*.v_pausedur/.v_total
      .buf$ += "'.p:1'%, "
      .v = .v_nucldur + .v_internucldur
      .buf$ += "'.v:3' s, "
      .buf$ += "'.v_nucldur:3' s, "
      .buf$ += "'.v_internucldur:3' s, "
      .buf$ += "'.v_pausedur:2' s "
      .buf$ += newline$
   endfor
   .buf$ += newline$ + "Duration variability of speaker(s):" + newline$
   @sprint_fs: len, "Speaker label"
   .buf$ += leading$ + result$ + ": NuclDurMean, NuclDurStdev, nPVI_nucldur, nPVI_voweldur, nPVI_sylldur" + newline$
   for .speaker from 1 to speakers
      ;.row = reportID_row_offset + .speaker
      .row = .speaker
      .label$ = speaker_label'.speaker'$
      @sprint_fs: len, .label$
      .buf$ += leading$ + result$ + ": "
      .v =  Get value: .row, j_nucldur_mean
      .buf$ += "'.v:3' s, "
      .v =  Get value: .row, j_nucldur_stdev
      .buf$ += "'.v:3', "
      .v =  Get value: .row, j_nPVI_nucldur
      .buf$ += "'.v:2', "
      .v =  Get value: .row, j_nPVI_voweldur
      .buf$ += "'.v:2', "
      .v =  Get value: .row, j_nPVI_sylldur
      .buf$ += "'.v:2' "
      .buf$ += newline$
   endfor
   result$ = .buf$
endproc


procedure length_longest_speaker_label
; Return in variable <len> the length of longest speaker label
   len = length ("Speaker label")
   for .speaker from 1 to speakers
      len = max (len, length (speaker_label'.speaker'$))
   endfor
endproc


procedure sprint_fs: .len, .text$
; print string using fixed length, appending blanks if necessary
   .j = .len - length(.text$) 
   if (.j > 0)
     for .k to .j
        .text$ = .text$ + " "
     endfor
   endif
   result$ = .text$
endproc


procedure pitchrange_normalized_pitch
   @debug_msg: "pitchrange_normalized_pitch: entry"
   .current_speaker = 0					; force initialization
   selectObject: nucldatID
   .nrows = Get number of rows
   for .j to .nrows
      .speaker = Get value: .j, j_speaker_id 
      if (.speaker <> .current_speaker)
         selectObject: profileID
         ; .row = reportID_row_offset + .speaker
         .row = .speaker
         .bottomST = Get value: .row, j_pitch_bottom_ST
         .topST = Get value: .row, j_pitch_top_ST
         .rangeST = .topST - .bottomST
         .current_speaker = .speaker
      endif
      selectObject: nucldatID
      .v = Get value: .j, j_f0_start
      .v = hertzToSemitones(.v) - hertzToSemitones(1)	; convert to ST rel 1Hz
      .v = 100 * (.v - .bottomST) / .rangeST		
      ; normalize to range 0..100 (arbitrary units: percentage of pitch range)
      Set value: .j, j_prnp_start, min(100, max(0, floor(.v)))
      .v = Get value: .j, j_f0_end
      .v = hertzToSemitones(.v) - hertzToSemitones(1)	; convert to ST rel 1Hz
      .v = 100 * (.v - .bottomST) / .rangeST		
      ; normalize to range 0..100 (arbitrary units)
      Set value: .j, j_prnp_end, min(100, max(0, floor(.v)))
      .dyn = Get value: .j, j_dynamic
      .hi = Get value: .j, hipitch
      .hi = hertzToSemitones(.hi) - hertzToSemitones(1)
      .lo = Get value: .j, lopitch
      .lo = hertzToSemitones(.lo) - hertzToSemitones(1)
      if (.dyn == 0)
         .v = 0
      elsif (.dyn < 0)
         .v = 100 * (.lo - .hi) / .rangeST
      elsif (.dyn > 0)
         .v = 100 * (.hi - .lo) / .rangeST
      endif
      Set value: .j, j_prnp_intra, floor(.v)
   endfor
   @debug_msg: "pitchrange_normalized_pitch: exit"
endproc


procedure calc_localrate: .table, .dst, .sizeleft, .sizeright
; Calculate the local speech rate in nuclei/s for a local window of (sizeleft + sizeright) seconds
; <.dst>		column where results are stored
; <.sizeleft>	size of window on left (in seconds)
; <.sizeright>	size of window on right (in seconds) 
   @debug_msg: "calc_localrate: entry"
   selectObject: .table
   .rows = Get number of rows
   for .row to .rows			; for each nucleus/syllable in the signal
      .x1 = Get value: .row, j_nucl_t1
      .x2 = Get value: .row, j_nucl_t2
      .xmid = .x1+(.x2-.x1)/2
      .n = 0				; nrof elements in context
      .tleft = 0				; total time of left context
      if (.sizeleft > 0 and .row > 1)
         .j = 1
         repeat				; extend left context
            if (.row - .j >= 1)
               .t = Get value: .row-.j, j_nucl_t1
               .t2 = Get value: .row-.j, j_nucl_t2
               if (.xmid - .t <= .sizeleft)
                  .n += 1
                  .tleft = .xmid - .t
               elsif (.xmid - .t2 <= .sizeleft)
                  .tleft = .xmid - .t2
               endif
               if (.row - .j == 1)
                  .j = .row		; prepare for end of loop 
               endif
            endif
            .j += 1
         until (.row - .j < 1 or .xmid - .t >= .sizeleft)
      endif
      .tright = 0			; total time of right context
      if (.sizeright > 0 and .row < .rows)
         .j = 1
         repeat				; extend right context
            if (.row + .j <= .rows)
               .t1 = Get value: .row+.j, j_nucl_t1
               .t = Get value: .row+.j, j_nucl_t2
               if (.t - .xmid <= .sizeright)
                  .n += 1
                  .tright = .t - .xmid
               elsif (.t - .x2 <= .sizeright)
                  .tright = .t1 - .xmid
               endif
            endif
            .j += 1
         until (.row + .j > .rows or .t - .xmid >= .sizeright) 
      endif
      Set value: .row, .dst, (.n + 1)/(.tleft + .tright)
   endfor
   @debug_msg: "calc_localrate: exit"
endproc


procedure w: .text$
 appendFileLine: statsfile$, .text$
endproc


procedure prosodic_profile
   @msg: "Writing prosodic profile report for current input file to: 'statsfile$'"
   deleteFile: statsfile$
   @w: "Prosodic profile for input file: 'signalfile$'"
   @w: "Prosogram version: 'version$', (c) Piet Mertens"
   .date$ = date$ ()
   @w: "Date (of analysis): '.date$'"
   @w: ""

 if (nrof_nuclei_analysed == 0)
   @w: "The prosodic profile could not be calculated because no nuclei were detected."
   @msg: "Prosodic profile not calculated: no nuclei detected in speech signal."
 elsif (not needs_pitchrange)
   @w: "Prosodic profile calculation requires selection of full time range of speech signal."
   @msg: "Prosodic profile not calculated: requires selection of full time range of signal."
 else ; nrof_nuclei_analysed > 0 and pitchrange calculated

   @w: "Segmentation type: 'segmentation_name$'"
   @w: "Nucleus: 'nrof_nuclei_analysed' nuclei in signal"
   if (nrof_nuclei_analysed < 100)
      @w: "'newline$' WARNING: The global measures below are only meaningful for speech samples of at least 100 nuclei (syllables).'newline$'"
   endif
   if (nrof_nuclei_analysed <= 1)
      @w: "'newline$' ERROR: Insufficient number of syllables ('nrof_nucl') for calculation of measures.'newline$'"
   endif

   @w: ""
   @pitchrange_speakers_report
   @w: result$

   @raw_pitchrange_speakers_report
   @w: result$

   @pitchprofile_speakers_report
   @w: result$

   @duration_profile_speakers_report
   @w: result$

@w: ""
@w: "TotalDur   = total speech time (in s) = internucleus time + intranucleus time + pause time"
@w: "PhonTime   = phonation time (in s) = without pauses = internucleus time + intranucleus time"
@w: "%Phonation = proportion (%) of estimated phonation time (= internucleus time + intranucleus time) to speech time"
@w: "%Pauses    = proportion (%) of estimated pause time (= when internucleus time >= 0.3) to speech time"
@w: "SpeechRate = estimated speech rate (in syll/s) = nrof_nuclei/phonation_time"
@w: "MeanOfST   = mean of pitch values, where values are min and max pitch in ST for each syllable"
@w: "StdevOfST  = stdev of pitch values, where values are min and max pitch in ST for each syllable"
@w: "PitchRange = estimated pitch range (in ST) (2%-98% percentiles of data in nuclei without discontinuities)"
@w: "Gliss      = proportion (%) of syllables with large pitch movement (abs(distance) >= 4ST)"
@w: "Rises      = proportion (%) of syllables with pitch rise (>= 4ST)"
@w: "Falls      = proportion (%) of syllables with pitch fall (<= -4ST)"
@w: "NuclDur    = sum of durations for nuclei for this speaker"
@w: "InterNuclDur = sum of durations between successive nuclei for this speaker"
@w: "TrajIntra  = pitch trajectory (sum of absolute intervals) within syllabic nuclei, divided by duration (in ST/s)" 
@w: "TrajInter  = pitch trajectory (sum of absolute intervals) between syllabic nuclei (except pauses or speaker turns), divided by duration (in ST/s)" 
@w: "TrajPhon   = sum of TrajIntra and TrajInter, divided by phonation time (in ST/s)" 
@w: "TrajIntraZ = as TrajIntra, but for pitch trajectory in standard deviation units on ST scale (z-score) (in sd/s)" 
@w: "TrajInterZ = as TrajInter, but for pitch trajectory in standard deviation units on ST scale (z-score) (in sd/s)"
@w: "TrajPhonZ  = as TrajPhon,  but for pitch trajectory in standard deviation units on ST scale (z-score) (in sd/s)"

 endif ; nrof_nuclei_analysed > 0 and ...
endproc


procedure update_global_report
# Store feature values for the global profile spreadsheet (covering multiple speech files)
   @debug_msg: "update_global_report: entry"
   if (nrof_nuclei_analysed > 0)
      globalsheet_empty = 0
      if (not globalsheet_available)
         selectObject: profileID
         .ncols = Get number of columns
         globalprofileID = Create TableOfReal: "globalsheet", 1, .ncols
         globalsheet_available = 1
         globalsheet_empty = 1
         for .col to .ncols					; copy column labels
            selectObject: profileID
            .label$ = Get column label: .col
            selectObject: globalprofileID
            Set column label (index): .col, .label$
         endfor
      endif
      for .speaker from 1 to speakers		; copy all values from profile to globalprofile
         selectObject: globalprofileID
         .nrows = Get number of rows
         Insert row (index): .nrows+1
         if (globalsheet_empty)				; contains 1 empty row 
            Remove row (index): 1
            globalsheet_empty = 0
         endif
         selectObject: profileID
         .label$ = Get row label: .speaker
         selectObject: globalprofileID
         .row = Get number of rows
         Set row label (index): .row, basename$ + "_" + .label$
         for .col to .ncols
            selectObject: profileID
            .v = Get value: .speaker, .col
            selectObject: globalprofileID
            Set value: .row, .col, .v
         endfor
      endfor
      ; overwrite the global report file, saved after processing previous speech input file(s) in run
         deleteFile: globalfile$
         selectObject: globalprofileID
         Write to headerless spreadsheet file: globalfile$
   endif
   @debug_msg: "update_global_report: exit"
endproc


procedure draw_table: .wx1, .wx2, .wy1, .wy2, .nrows, .ncols, .format$, .data$
; Draw a table with cell content as in <.data$>
; <.wx1>..<.wx2>	left and right in world coordinates of drawable area
; <.wy1>..<.wy2>	top and bottom in world coordinates of drawable area
; <.data$>   		cell values in rows and colums, separated by ";"
; <.format$> 		width of colums (in % of table width), separated by ";" Optional specification of border and header lines.
   ; viewport dimensions of table in world of page
   .mx1 = ppvp_x1+(ppvp_x2-ppvp_x1)*(.wx1-ppw_x1)/(ppw_x2-ppw_x1)
   .mx2 = ppvp_x1+(ppvp_x2-ppvp_x1)*(.wx2-ppw_x1)/(ppw_x2-ppw_x1) 
   .my1 = ppvp_y1+(ppvp_y2-ppvp_y1)*(ytop-.wy1)/(ytop-ybot)			; top
   .my2 = ppvp_y1+(ppvp_y2-ppvp_y1)*(ytop-.wy2)/(ytop-ybot)   		; bottom
   Select inner viewport: .mx1, .mx2, .my1, .my2
   Axes: 0, 1, 0, 1
   if (show_area)
      Yellow
      Draw rectangle: 0, 1, 0, 1
      Black
   endif
   for .col to .ncols						; read format specification
      .pos = index(.format$, ";")
	  .field$ = left$(.format$, .pos-1)
      .justif'.col'$ = "left"	; left justification by default
      .just$ = left$(.field$, 1)
      if (.just$ == "R") 
         .justif'.col'$ = "right"
      elsif (.just$ == "C") 
         .justif'.col'$ = "centre"
      endif
	  .field$ = replace_regex$(.field$, "^[LRC]", "", 1)
      .width'.col' = number(.field$)						; width as percentage of table width
      .format$ = mid$(.format$, .pos+1, length(.format$)-.pos)	; rest of format
   endfor
   if (index_regex(extractWord$(.format$, "border="),"y"))
         Grey
         Draw rectangle: 0, 1, 0, 1
   endif
   if (index_regex(extractWord$(.format$, "header="),"y"))
         Grey
         .y = (.nrows-1)/.nrows
         Draw line: 0, .y, 1, .y
   endif
   Black
   .cell_margin = 0.01		; 1% of table width, on both sides (left and right) of cell
   for .row to .nrows
      .y = (.nrows-.row)/.nrows
      .xoffs = 0
      for .col to .ncols
         .pos = index(.data$, ";")							; end of current cell/column
         .elem$ = left$(.data$, .pos-1)						; cell content
         .data$ = mid$(.data$, .pos+1, length(.data$)-.pos)	; prepare for next cell
         .col_width = (.width'.col'/100)					; from percentage to fraction
         .just$ =.justif'.col'$								; justification for current column
         if (.just$ == "left")
            .x = .xoffs + .cell_margin
         elsif (.just$ == "right")
            .x = .xoffs + .col_width - .cell_margin
         elsif (.just$ == "centre")
            .x = .xoffs + .col_width/2 + .cell_margin
         endif 
         Text: .x, .just$, .y, "Bottom", .elem$
         .xoffs += .col_width								; prepare xoffset for next column
      endfor
   endfor
   ; Restore mother window
   Select inner viewport: ppvp_x1, ppvp_x2, ppvp_y1, ppvp_y2
   Axes: ppw_x1, ppw_x2, ybot, ytop
endproc


procedure draw_histogram: .table, .col, .wx1, .wx2, .wy1, .wy2, .xr1, .xr2, .nrbins, .xstep, .xcenter, .top$, .bottom$
; Draw a column in a table as a histogram.
; Specify position of window in world coordinates of page (<.wy1> is botton) 
; <.wx1>..<.wx2>	left and right in world coordinates of drawable area
; <.wy1>..<.wy2>	bottom and top in world coordinates of drawable area
; <.xr1>..<.xr2> 	value range of x-axis of histogram
; <.xstep>       	step for marks at bottom
; <.xcenter>     	x position of central value, shown in red
   @debug_msg: "draw_histogram: entry, table='.table', nrbins='.nrbins'" 
   .mx1 = ppvp_x1+(ppvp_x2-ppvp_x1)*(.wx1-ppw_x1)/(ppw_x2-ppw_x1)
   .mx2 = ppvp_x1+(ppvp_x2-ppvp_x1)*(.wx2-ppw_x1)/(ppw_x2-ppw_x1) 
   .my1 = ppvp_y1+(ppvp_y2-ppvp_y1)*(ytop-.wy1)/(ytop-ybot)
   .my2 = ppvp_y1+(ppvp_y2-ppvp_y1)*(ytop-.wy2)/(ytop-ybot)   
   if (show_area)
      Select inner viewport: .mx1, .mx2, .my1, .my2
      Axes: 0, 1, 0, 1
      Yellow
      Draw rectangle: 0, 1, 0, 1
      Black
   endif
   Select outer viewport: .mx1, .mx2, .my1, .my2
   .xmargin = (.mx2 - .mx1) * 0.14
   .ymargin = (.my2 - .my1) * 0.2
   Select inner viewport: .mx1+.xmargin, .mx2-.xmargin, .my1+.ymargin, .my2-.ymargin
   Solid line
   Black
   if (.table != undefined)
      selectObject: .table
      ; Draw column as distribution: column, value range 1, 2, frequency range 1, 2, nrbins, Garnish
      ; Draw column as distribution: .col, .xr1, .xr2, 0, 0, .nrbins, "no"
      @draw_column_as_distribution: .table, .col, .xr1, .xr2, .nrbins 
   else			; when there is no data
      Axes: .xr1, .xr2, 0, 1
   endif
   Marks bottom every: 1, .xstep, "yes", "yes", "no"
   if (.xstep >= 100)
      Marks bottom every: 1, .xstep/2, "no", "yes", "no"
   endif
   Text top: "no", .top$
   Text bottom: "yes", .bottom$
   Draw inner box
   if (.xcenter != undefined)
      Axes: .xr1, .xr2, 0, 1
      Dotted line
      Red
      Draw line: .xcenter, 0, .xcenter, 1
   endif
   Solid line
   Black
   ; Restore mother window
   Select inner viewport: ppvp_x1, ppvp_x2, ppvp_y1, ppvp_y2
   Axes: ppw_x1, ppw_x2, ybot, ytop
   @debug_msg: "draw_histogram: exit" 
endproc


procedure draw_column_as_distribution: .table, .col, .xrange1, .xrange2, .nbins
; Draw histogram of values in column <.col> of TableOfReal <.table>, using <.nbins> for value range <.xrange1>..<.xrange2>
   @debug_msg: "draw_column_as_distribution: entry" 
   ; Create temporay table for histogram bins and fill them
   .histo = Create TableOfReal: "histo", .nbins, 1
   .maxcount = 0
   .dxbin = (.xrange2-.xrange1)/.nbins
   selectObject: .table
   .nrows = Get number of rows
   for .row to .nrows
      selectObject: .table
      .v = Get value: .row, .col
      if (.v >= .xrange1 and .v <= .xrange2)
         .bin = floor((.v - .xrange1)/ .dxbin) + 1			; get index of bin
		 if (.bin <= .nbins)
            selectObject: .histo
            .count = Get value: .bin, 1
            .count += 1
            Set value: .bin, 1, .count
            .maxcount = max (.maxcount, .count)
         else
            @debug_msg: "draw_column_as_distribution: row='.row' v='.v:4' bin='.bin' > nbins='.nbins'"
         endif
      endif
   endfor
   ; Select appropriate range for Y-axis of histogram
      .r = 10
      repeat
         .top = ceiling(.maxcount/.r)*.r
         .r = .r*10
      until (.top >= .maxcount)
      .top = max(.top, 1)		; top should not equal 0 
   @debug_msg: "draw_column_as_distribution: Select appropriate range top='.top' maxcount='.maxcount'"
   Axes: .xrange1, .xrange2, 0, .top
   selectObject: .histo
   for .bin to .nbins
      .count = Get value: .bin, 1
      Draw rectangle: .xrange1+(.bin-1)*.dxbin, .xrange1+.bin*.dxbin, 0, .count
   endfor
   ; Select appropriate step for marks on Y-axis of histogram 
   ; .step = ceiling(.top/50)*10
   .nsteps = 6
   .d = 10000
   repeat
      .r = .top div .d
      if (.r < 1)
         .d = .d / 10
	  endif
   until (.r >= 1 or .d <= 1)
   .nm = .top/.d/10		; range 0..1
   .p = .nm/.nsteps
   if (.p < 0.01)
      .step = 0.1 * .d
   elsif (.p < 0.022)
      .step = 0.2 * .d
   elsif (.p < 0.05)
      .step = 0.5 * .d
   elsif (.p < 0.1)
      .step = 1 * .d
   else
      .step = 2 * .d
   endif
   @debug_msg: "draw_column_as_distribution: Select appropriate step for marks top='.top' step='.step'"
   Marks left every: 1, .step, "yes", "yes", "no"
   removeObject: .histo
   @debug_msg: "draw_column_as_distribution: exit" 
endproc


procedure prosodic_profile_new
   @debug_msg: "prosodic_profile_new: entry, speakers='speakers'"
   for .j from 1 to speakers
      .speaker_label$ = speaker_label'.j'$
      .row_speaker = .j
     ; Get number of nuclei
      selectObject: nucldatID
	  .nrows = Get number of rows
;@msg: "prosodic_profile_new: speaker='.j' nuclei='.nrows'"
     ; Prepare window
      Erase all
	  ; .fontsize_std = 10
      ; Font size (points): .fontsize_std 
     ; Size of A4 paper in inches: 8.27 x 11.69
      .margin = 0.5
      ppvp_x1 = .margin			; left (inches)
      ppvp_x2 = 8.27 - .margin	; right (inches)
      ppvp_y1 = .margin			; top (inches)
      ppvp_y2 = 11.69 - .margin	; bottom (inches)
      Select inner viewport: ppvp_x1, ppvp_x2, ppvp_y1, ppvp_y2
      show_area = 0
      if (show_area)
         Axes: 0, 1, 0, 1
         Red
         Dashed line
         Draw rectangle: 0, 1, 0, 1
      endif
      Black
	  Solid line
      ppw_x1 = 0	; world coordinates, left of page
      ppw_x2 = 100	; world coordinates, right of page
      ytop = 0		; world coordinates, top of page
      ybot = 100	; world coordinates, bottom of page
      Axes: ppw_x1, ppw_x2, ybot, ytop
       ; Axes: left, right, bottom, top
      ypos = ytop	; current y position on page
      ystep = 1.65
      .x = 0
      Font size: fontsize
      .date$ = date$ ()

      .t$ = "Prosodic Profile, by 'version$', \co Piet Mertens; ;"
      @draw_table: 0, 60, ypos, ypos+ystep, 1, 2, "50;50;", .t$
      ypos += ystep

      selectObject: profileID
      .nrofnuclei = Get value: .row_speaker, j_nrofnucl
      .nrofvalid = Get value: .row_speaker, j_nrofvalid
      .t$ = "Input file:;'signalfile$';"
      .t$ += "Date (of analysis):;'.date$';"
      .t$ += "Speaker:;'.j' (of 'speakers'), with label ""'.speaker_label$'"";"
      .t$ += "Number of nuclei:;'.nrofvalid' (of '.nrofnuclei', incl. outliers & discontinuities);"
      @draw_table: 5, 90, ypos, ypos+4*ystep, 4, 2, "25;75;border=y", .t$
      .t$ ="Segmentation type: 'segmentation_name$'"
      @draw_table: 60, 90, ypos+ystep, ypos+2*ystep, 1, 1, "100;border=y", "Segmentation type: 'segmentation_name$';"
      ypos += 4*ystep + ystep

      @draw_table: 0, 60, ypos, ypos+ystep, 1, 1, "100;", "Pitch Range (after stylization);"
	  ypos += ystep

      selectObject: profileID
      .t$ = " ;ST;Hz;"
      .v = Get value: .row_speaker, j_pitch_range
         .t$ += "range (span);'.v:1'; ;"
      .vH = Get value: .row_speaker, j_pitch_top_Hz
      .v = Get value: .row_speaker, j_pitch_top_ST
         .t$ += "top;'.v:1';'.vH:0';"
      .vH = Get value: .row_speaker, j_pitch_mean_Hz
      .f0_mean = .vH
      .v = Get value: .row_speaker, j_pitch_mean_of_ST
         .t$ += "mean;'.v:1';'.vH:0';"
      .vH = Get value: .row_speaker, j_pitch_median_Hz
      .v = Get value: .row_speaker, j_pitch_median_ST
         .t$ += "median;'.v:1';'.vH:0';"
      .vH = Get value: .row_speaker, j_pitch_bottom_Hz
      .v = Get value: .row_speaker, j_pitch_bottom_ST
         .t$ += "bottom;'.v:1';'.vH:0';"
      .vH = Get value: .row_speaker, j_pitch_stdev_of_Hz
      .v = Get value: .row_speaker, j_pitch_stdev_of_ST
         .t$ += "stddev;'.v:2';'.vH:2';"
      @draw_table: 5, 30, ypos, ypos+7*ystep, 7, 3, "50;R25;R25;border=y header=y", .t$
      ypos += 8*ystep

    ; Histograms of pitch variability
      @draw_table: 0, 60, ypos, ypos+ystep, 1, 1, "100;", "Pitch variability;"
	  ypos += ystep
	  
      .hr0 = ypos
      .hr1 = .hr0 + 2*ystep
      .hr2 = .hr1 + 11*ystep
      .hr3 = .hr2 + 11*ystep
      .hr4 = .hr3 + ystep
      .hr5 = .hr4 + 3*ystep
      .hr6 = .hr5 + ystep
      .hr7 = .hr6 + ystep
      .hr8 = .hr7 + 2*ystep
      .hr9 = .hr8 + 11*ystep
      Draw line: 30, .hr0, 30, .hr5
      Draw line: 60, .hr0, 60, .hr5
      Draw line: 30, .hr7, 30, .hr9
      Draw line: 60, .hr7, 60, .hr9

;@msg: "SPEAKER '.j'"
    ; Create temporary table with values for current speaker
      ; @create_subtable_where: nucldatID, j_speaker_id, "==", .j, "table1"
      selectObject: nucldatID
      .table1 = Extract rows where column: j_speaker_id, "equal to", .j
    ; Create temporary table with values for current speaker, without f0 discontinuity
      ; @create_subtable_where: table1, j_f0_discont, "==", 0, "table2"
      selectObject: .table1
      .table2 = Extract rows where column: j_f0_discont, "equal to", 0


;@msg: "Histogram: Row 1 left: mean F0 raw Hz"
    ; Histogram: mean F0 raw in Hz in nuclei
      selectObject: .table2
      .n = Get number of rows
      .nrbins = 40
      ; draw_histogram: .table, col, x1, x2, y1, y2, x1_histo, x2_histo, nrbins, xstep (marks), xcenter, legend1, legend2
      @draw_histogram: .table2, j_f0_mean, 0, 30, .hr1, .hr2, 50, 450, .nrbins, 100, .f0_mean, "mean F0 (raw) in nuclei", "Hz (N='.n')"


;@msg: "Histogram: Row 2 left: mean F0 raw ST"
   ; Histogram: mean F0 in ST in nuclei
      selectObject: .table2
      .n = Get number of rows
	  .mid = Get column mean (index): j_f0_meanST
      .nrbins = 40
      @draw_histogram: .table2, j_f0_meanST, 0, 30, .hr2, .hr3, .mid-20, .mid+20, .nrbins, 6, .mid, "mean F0 (raw) in nuclei", "ST (N='.n')"


;@msg: "Histogram: Row 1 mid: low&high F0 raw Hz"
   ; Histogram: low and high F0 in nuclei
    ; Combine lopitch and hipitch values; for selected speaker
      selectObject: .table2
      .nrows = Get number of rows
      .table3 = Create TableOfReal: "tmp3", .nrows*2, 1
      .i = 0
      for .row to .nrows
         selectObject: .table2
         .vlo = Get value: .row, lopitch
         .vhi = Get value: .row, hipitch
         selectObject: .table3
         .i += 1
         Set value: .i, 1, .vlo
         .i += 1
         Set value: .i, 1, .vhi
      endfor
      selectObject: .table3
	  .n = Get number of rows
      .nrbins = 40
      @draw_histogram: .table3, 1, 30, 60, .hr1, .hr2, 50, 450, .nrbins, 100, .f0_mean, "low & high F0 (stylized) in nuclei", "Hz (N='.n')"


;@msg: "Histogram: Row 2 mid: low&high F0 raw ST"
    ; Histogram: low and high F0 in ST in nuclei
      selectObject: .table3
	  .n = Get number of rows
      for .i to .n
         .v = Get value: .i, 1
         Set value: .i, 1, hertzToSemitones (.v) - hertzToSemitones(1)
      endfor
	  .mid = Get column mean (index): 1
      .nrbins = 40
      @draw_histogram: .table3, 1, 30, 60, .hr2, .hr3, .mid-20, .mid+20, .nrbins, 6, .mid, "low & high F0 (stylized) in nuclei", "ST (N='.n')"
	  removeObject: .table3


      selectObject: profileID
      .v = Get value: .row_speaker, j_prop_level
      @draw_table: 60, 90, .hr0, .hr0+2*ystep, 2, 1, "C100;", "stylized pitch contour;'.v:1'\%  nuclei without glissando;"


;@msg: "Histogram: Row 1 right: intrasyllabic up and down glissando"
	; Histogram: intrasyllabic glissandos
    ; Combine intrasyllab up and down values; for selected speaker
      selectObject: .table2
      .table4 = Create TableOfReal: "table4", .nrows*2, 1
      .i = 0			; nrof used rows in destination table
	  .ns = 0			; nrof syllables with glissando
	  .ns_up = 0		; nrof syllables with upward glissando
	  .ns_down = 0		; nrof syllables with downward glissando
      for .row from 1 to .nrows
         selectObject: .table2
            .dyn = Get value: .row, j_dynamic
            if (.dyn <> 0)
               .v1 = Get value: .row, j_intrasylup
               .v2 = Get value: .row, j_intrasyldown
               selectObject: .table4
               if (.v1 <> 0)
                  .i += 1
                  Set value: .i, 1, .v1
                  .ns_up += 1
               endif
               if (.v2 <> 0)
                  .i += 1
                  Set value: .i, 1, .v2
                  .ns_down += 1
               endif
               if (.v1 <> 0 or .v2 <> 0)
                  .ns += 1
               endif
            endif
      endfor
;@msg: "selection of rows done, i='.i'"
      selectObject: .table4
      .nrows = Get number of rows
	  .row = .nrows
      if (.i > 0)
	     repeat
            if (.row > .i)
               Remove row (index): .row
            endif
            .row -= 1
	     until (.row <= .i)
	     .n = Get number of rows
		 .ref = .table4
      else
		 .ref = undefined
      endif
;@msg: "removal of rows done"
      .nrbins = 24
      @draw_histogram: .ref, 1, 60, 90, .hr1, .hr2, -12, 12, .nrbins, 2, undefined, "nuclei with glissando", "ST (|\^||='.ns_up' & |\_||='.ns_down' in '.ns' nucl)"
	  removeObject: .table4


;@msg: "Histogram: Row 2 right: intersyllabic"
    ; Histogram: intersyllabic
    ; Combine intersyllab values; for selected speaker
      selectObject: nucldatID
	  .nrows = Get number of rows
      selectObject: .table2
	  .n = Get number of rows
	  .zeros = 0
      for .i to .n
         .v = Get value: .i, j_intersyl
         if (.v == 0)
            .zeros += 1
         endif
	  endfor
;@msg: "prosodic_profile_new: histogram of intersyllabic pitch intervals, n='.n' zeros='.zeros'"
      .nrbins = 32
      @draw_histogram: .table2, j_intersyl, 60, 90, .hr2, .hr3, -8, 8, .nrbins, 2, undefined, "intervals between nuclei", "ST (N='.n')"
	  ;  |0|='.zeros' or '.prop_zeros:1'\% "

      selectObject: profileID
      .v = Get value: .row_speaker, j_traj_phon
         .t$ = "Total trajectory (ST/s);'.v:1';"
      .v = Get value: .row_speaker, j_traj_intra
         .t$ += "Intrasyllabic traj. (ST/s);'.v:1';"
      .v = Get value: .row_speaker, j_traj_inter
         .t$ += "Intersyllabic traj. (ST/s);'.v:1';"
      @draw_table: 63, 87, .hr4, .hr4+3*ystep, 3, 2, "70;R30;border=n", .t$


      @draw_table: 0, 60, .hr6, .hr6+ystep, 1, 1, "100;", "Temporal organisation;"

      selectObject: profileID
      .v = Get value: .row_speaker, j_speech_rate
         .t$ = "Speech rate (syll/s);'.v:1';"
      .v = Get value: .row_speaker, j_speech_time
         .t$ += "Speech time (s);'.v:2';"
      @draw_table: 5, 27, .hr7, .hr7+2*ystep, 2, 2, "70;R30;border=n", .t$

      selectObject: profileID
      .v = Get value: .row_speaker, j_nucldur_mean
         .t$ = "Nucleus dur. mean (s);'.v:3';"
      .v = Get value: .row_speaker, j_nucldur_stdev
         .t$ += "Nucleus dur. stdev;'.v:3';"
      @draw_table: 33, 57, .hr7, .hr7+2*ystep, 2, 2, "70;R30;border=n", .t$

      selectObject: profileID
      .v = Get value: .row_speaker, j_propphon
         .t$ = "Proportion phonation (\% );'.v:1' ;"
      .v = Get value: .row_speaker, j_proppause
         .t$ += "Proportion pauses (\% );'.v:1' ;"
      @draw_table: 63, 87, .hr7, .hr7+2*ystep, 2, 2, "70;R30;border=n", .t$


;@msg: "Histogram: Row 3 mid: nucleus duration"
    ; Histogram: nucleus duration
      selectObject: profileID
      .mean = Get value: .row_speaker, j_nucldur_mean
      selectObject: .table2
      .nrbins = 40
      @draw_histogram: .table2, j_nucldur, 30, 60, .hr8, .hr8+11*ystep, 0, 0.4, .nrbins, 0.1, .mean, "nucleus duration", "s (N='.n')"


    ; Histogram: pause duration
      selectObject: .table1
;@msg: "prosodic_profile_new: pause duration: speaker='.j', nucldat rows='.nrows', valid='.n'"
	  .n = Get number of rows
      .i = 1
      .zeros = 0
      while (.i <= .n)
         .v = Get value: .i, j_pause_dur
         if (.v < mindur_pause_gap)
            .v = 0
			Set value: .i, j_pause_dur, 0
         endif
         if (.v > 0)
            .i += 1
         else
            .zeros += 1
            if (.n > 1)		; cannot remove row when it is the only row
               Remove row (index): .i
            endif
            .n -= 1
         endif
      endwhile
	  .mean = Get column mean (index): j_pause_dur
	  .mean = max (.mean, mindur_pause_gap)
;@msg: "prosodic_profile_new: pause duration: speaker='.j', nucldat rows='.nrows', zeros='.zeros' valid='.n'"
      .nrbins = 40
      @draw_histogram: .table1, j_pause_dur, 60, 90, .hr8, .hr8+11*ystep, 0.3, 1, .nrbins, 0.2, .mean, "pause duration (gap \>_ 'mindur_pause_gap:2')", "s (N='.n')"


	  removeObject: .table1, .table2


      Select inner viewport: ppvp_x1, ppvp_x2, ppvp_y1, ppvp_y2
     ; Build filename for prosodic profile output file
      .fname$ = replace_regex$(statsfile$, "^(.*)(\.txt)", "\1", 1)
      .fname$ += "_speaker_'.j'"
      if (index(output_format$,"EPS"))
         @msg: "Writing prosodic profile for speaker '.j' to: '.fname$'.eps"
         Save as EPS file: .fname$+".eps"
      elsif (index(output_format$,"PNG 600 dpi"))
         @msg: "Writing prosodic profile for speaker '.j' to: '.fname$'.png"
         Save as 600-dpi PNG file: .fname$+".png"
      else
         @msg: "Writing prosodic profile for speaker '.j' to: '.fname$'.png"
         Save as 300-dpi PNG file: .fname$+".png"
      endif
   endfor
endproc


procedure table_copy: .src, .var_name$
; Replacement for Praat's TableOfReal: Copy, which is broken in Praat 6.0.50
   selectObject: .src
   .nrows = Get number of rows
   .ncols = Get number of columns
   .dst = Create TableOfReal: "tmp_copy", .nrows, .ncols
   for .col to .ncols
      selectObject: .src
      .label$ = Get column label: .col
      selectObject: .dst
      Set column label (index): .col, .label$
   endfor
   for .row to .nrows
      selectObject: .src
      .label$ = Get row label: .row
      selectObject: .dst
      Set row label (index): .row, .label$
      for .col to .ncols
         selectObject: .src
         .v = Get value: .row, .col
         selectObject: .dst
         Set value: .row, .col, .v
      endfor
   endfor
   '.var_name$' = .dst   
endproc


procedure table_column_by_label: .table, .column_label$, .varname$
   selectObject: .table
   .ncols = Get number of columns
   .col = 1
   .ok = 0
   repeat 
      .label$ = Get column label: .col
	  if (.label$ == .column_label$)
         .ok = .col
      endif
      .col += 1
   until (.ok or .col > .ncols)
   '.varname$' = .ok
endproc


procedure table_row_where_col_value_gt: .table, .col, .value, .varname$
   selectObject: .table
   .nrows = Get number of rows
   .ncols = Get number of columns
   .row = 1
   .ok = 0
   repeat 
      .y = Get value: .row, .col
	  if (.y > .value)
         .ok = .row
      endif
      .row += 1
   until (.ok or .row > .nrows)
   '.varname$' = .ok
endproc



procedure calc_isodur: .table
; Calculate duration of inter syllable onset
; Requires nucleus, speaker and pause information
   @debug_msg: "calc_isodur: entry"
   selectObject: .table
   .rows = Get number of rows
   for .row to .rows			; for each nucleus/syllable in the signal
      .t1 = Get value: .row, j_nucl_t1
      .t2 = Get value: .row, j_nucl_t2
      .sp = Get value: .row, j_speaker_id
      .pause = Get value: .row, j_before_pause
      .isodur = .t2-.t1		; default when followed by speaker turn or end-of-signal
      if (.row < .rows)
         .t = Get value: .row+1, j_nucl_t1
         .sp2 = Get value: .row+1, j_speaker_id
         if (.sp2 == .sp)
            if (.pause)
               .isodur = (.t2-.t1)		; + 0.3
            else
               .isodur = .t - .t1
            endif
         endif
      elsif (.row == .rows)
         .isodur = (.t2 - .t1)			; + 0.3
      endif
      Set value: .row, j_isodur, .isodur
   endfor
   @debug_msg: "calc_isodur: exit"
endproc

