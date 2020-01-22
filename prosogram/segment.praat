# segment.praat ---   
# This file is included (indirectly) by prosogram.praat. It isn't a stand-alone script. Use prosogram.praat instead.
# Author: Piet Mertens

# Last modification: 2019-12-07
# 2018-03-31	added segmentation method "specsim"
# 2018-04-01	added segmentation method "pitchchange"
# 2018-08-09	added segmentation method "pitchterrace"
# 2019-01-29	debugged voiced_portions
# 2019-02-12	changed to new Praat syntax
# 2019-02-12	handle double length diacritic in @is_vowel
# 2019-12-04	changed @get_boundary


# Procedure hierarchy
# make_segmentation
#   local_peaks_vowels
#   convexhull
#   local_peaks_loudness
#   local_peaks_duo
#   pseudo_syllables
#   local_peaks_syllables_vowels
#   local_peaks_syllables
#   local_peaks_rhyme
#
#      get_local_peak
#         is_unvoiced_region
#         get_boundary
#         voiced_intersection
#         add_boundary
#         set_boundary_label



procedure make_segmentation: segm_method, .at1, .at2, destID, mindiff
# Make a segmentation based on a parameter (which can be intensity, loudness, etc, stored in Intensity object), 
# in time range <.at1> to <.at2>.
# The resulting interval tier is written in textgrid <destID>, created before calling this procedure.
# mindiff	intensity difference threshold for local dips in convex hull (default = 3)
   @debug_msg: "make_segmentation: entry"
   if (segm_method == segm_vnucl)
      @local_peaks_vowels: destID, intensityID, .at1, .at2
   elsif (segm_method == segm_aloudness)
      @convexhull: destID, dip_tier, loudnessID, .at1, .at2, mindiff
      @tier_point_to_interval: destID, dip_tier, syllable_tier, .at1, .at2
      @local_peaks_loudness: destID, loudnessID, .at1, .at2
   elsif (segm_method == segm_anucl)
      @convexhull: destID, dip_tier, intbpID, .at1, .at2, mindiff
      @tier_point_to_interval: destID, dip_tier, syllable_tier, .at1, .at2
      @local_peaks_duo: destID, intensityID, intbpID, .at1, .at2
   elsif (segm_method == segm_asyll)
      @pseudo_syllables: destID, intensityID, intbpID, .at1, .at2, mindiff
   elsif (segm_method == segm_msyllvow)
      @local_peaks_syllables_vowels: destID, intensityID, .at1, .at2
   elsif (segm_method == segm_msyllpeak)
      @local_peaks_syllables: destID, intensityID, .at1, .at2
   elsif (segm_method == segm_mrhyme)
      @local_peaks_rhyme: destID, intensityID, .at1, .at2
   elsif (segm_method == segm_voiced)
      @voiced_portions: destID, intensityID, .at1, .at2
   elsif (segm_method == segm_specsim)
      if (not signal_available)
        @read_signal
      endif
      if (not intensity_available)
         .intensityID = To Intensity: 100, time_step
      endif
      @spectral_similarity: soundID, destID, intensityID, .at1, .at2, 0.02
      removeObject: soundID
      signal_available = 0
   elsif (segm_method == segm_pitchchange)
      @segmentation_pitch: pitchID, pc_threshold, intensityID, intbpID, .at1, .at2, destID, mindiff
   elsif (segm_method == segm_pitchterrace)
      @segmentation_pitch2: pitchID, pc_threshold, intensityID, .at1, .at2, mindur_terrace, destID
;selectObject: destID
;Copy: "Copy nucleiID " + fname$
   endif
   @debug_msg: "make_segmentation: exit"
endproc


procedure local_peaks_loudness: .grid, .paramID, .t1, .t2
# Find nucleus as local peak in loudness function
# <.grid>		ID of destination textgrid where nuclei will be stored
# <.paramID>	parameter for which local peak is to be found
# <.t1>..<.t2>	time range of analysis
   selectObject: grid
   .t = .t1
   while (.t < .t2)
      .j = Get interval at time.: syllable_tier, .t
      .x1 = Get start time of interval: syllable_tier, .j
      .x2 = Get end time of interval: syllable_tier, .j
      @get_local_peak: .grid, .paramID, .x1, .x2, 1, .x1, .x2
      .t = .x2
   endwhile
endproc


procedure local_peaks_vowels: .grid, .paramID, .t1, .t2
# Find nucleus as local peak in intensity of vowel
# <.grid>		ID of destination textgrid where nuclei will be stored
# <.paramID>	parameter for which local peak is to be found
# <.t1>..<.t2>	time range of analysis
   @intervals_from_time_range: .grid, phone_tier, .t1, .t2, "first_interval", "last_interval"
   selectObject: .grid
   for .j from first_interval to last_interval
      .x1 = Get start time of interval: phone_tier, .j
      .x2 = Get end time of interval: phone_tier, .j
      .label$ = Get label of interval: phone_tier, .j
      @is_vowel: .label$
      if (is_vowel)
         @get_local_peak: .grid, .paramID, .x1, .x2, 1, .x1, .x2
      endif
   endfor
endproc


procedure local_peaks_syllables: .grid, .paramID, .t1, .t2
# Find nucleus as local peak in intensity of syllable
# <.grid>		ID of destination textgrid where nuclei will be stored
# <.paramID>	parameter for which local peak is to be found
# <.t1>..<.t2>	time range of analysis
   @intervals_from_time_range: .grid, syllable_tier, .t1, .t2, "first_interval", "last_interval"
   selectObject: .grid
   for .j from first_interval to last_interval
      .x1 = Get start time of interval: syllable_tier, .j
      .x2 = Get end time of interval: syllable_tier, .j
      .label$ = Get label of interval: syllable_tier, .j
      .label$ = replace_regex$ (.label$, "^ *", "", 1)
      .label$ = replace_regex$ (.label$, " *$", "", 1)
      if (.label$ <> "PAUSE" and .label$ <> "_")
         @get_local_peak: .grid, .paramID, .x1, .x2, 1, .x1, .x2
      endif
   endfor
endproc


procedure local_peaks_syllables_vowels: .grid .paramID, .t1, .t2
# Find nucleus as local peak in intensity of syllable, starting from local peak in vowel
# <.grid>		ID of destination textgrid where nuclei will be stored
# <.paramID>	parameter for which local peak is to be found
# <.t1>..<.t2>	time range of analysis
   @intervals_from_time_range: .grid, syllable_tier, .t1, .t2, "first_interval", "last_interval"
   selectObject: .grid
   for .j from first_interval to last_interval
      .x1 = Get start time of interval: syllable_tier, .j
      .x2 = Get end time of interval: syllable_tier, .j
      .syll$ = Get label of interval: syllable_tier, .j
      @intervals_from_time_range: .grid, phone_tier, .x1, .x2-0.001, "ph1", "ph2"
      nrof_vowels = 0
      nrof_syllabics = 0
      for phon from ph1 to ph2
         .label$ = Get label of interval: phone_tier, phon
         .phon_x1 = Get start time of interval: phone_tier, phon
         .phon_x2 = Get end time of interval: phone_tier, phon
         @is_syllabic: .label$
         if (result)
            nrof_syllabics += 1
            if (is_vowel)
               nrof_vowels += 1
            endif
            if (nrof_syllabics == 1)
               @get_local_peak: .grid, .paramID, .x1, .x2, 1, .phon_x1, .phon_x2
            else
               @msg: "Warning: Multiple vowels (or syllabics) in syllable <'.syll$'> at time '.phon_x1:3'"
            endif
         endif
      endfor
   endfor
endproc


procedure local_peaks_rhyme: .grid, .paramID, .t1, .t2
# Find nucleus as local peak in intensity of rhyme, starting from local intensity peak in vowel
# <.grid>		ID of destination textgrid where nuclei will be stored
# <.paramID>	parameter for which local peak is to be found
# <.t1>..<.t2>	time range of analysis
   @debug_msg: "local_peaks_rhyme: entry ('.t1:4'-'.t2:4')"
   @intervals_from_time_range: .grid, syllable_tier, .t1, .t2, "first_interval", "last_interval"
   selectObject: .grid
   for .j from first_interval to last_interval
      syll_x1 = Get start time of interval: syllable_tier, .j
      syll_x2 = Get end time of interval: syllable_tier, .j
      syll_label$ = Get label of interval: syllable_tier, .j
      @intervals_from_time_range: .grid, phone_tier, syll_x1, syll_x2-0.001, "ph1", "ph2"
      nrof_vowels = 0
      nrof_syllabics = 0
      for phon from ph1 to ph2
         phon_x1 = Get start time of interval: phone_tier, phon
         phon_x2 = Get end time of interval: phone_tier, phon
         label$ = Get label of interval: phone_tier, phon
         if (phon == ph1 and phon_x1 <> syll_x1)
            @msg: "Syllable ('syll_label$') starting at 'syll_x1:4' not aligned with phoneme ('label$') start ('phon_x1:4')"
         endif
         if (phon == ph2 and phon_x2 <> syll_x2)
            @msg: "Syllable ('syll_label$') ending at 'syll_x2:4' not aligned with phoneme ('label$') end ('phon_x2:4')"
         endif
         @is_syllabic: label$
         if (result)
            nrof_syllabics += 1
            if (is_vowel)
               nrof_vowels += 1
            endif
            if (nrof_syllabics == 1)
               @get_local_peak: .grid, .paramID, phon_x1, syll_x2, 1, phon_x1, phon_x2
            else
               @msg: "Warning: Multiple vowels (or syllabics) in syllable at time 'phon_x1:3'"
            endif
         endif
      endfor
   endfor
   @debug_msg: "local_peaks_rhyme: exit"
endproc


procedure local_peaks_duo: .dstID, .paramID, .ifilID, .t1, .t2
# <.dstID>		ID of destination textgrid where nuclei will be stored
# <.paramID>	parameter for which local peak is to be found (intensity)
# <.ifilID>		intensity of bandpass filtered signal
# <.t1>..<.t2>	time range of analysis
#
# Find syllabic nucleus within syllable-like interval
# - For a voiced portion... 
# - Evaluate importance of difference between maximum of global intensity 
#   and dip at right end of segment. This affects right side of nucleus.
# - From intensity peak (of filtered), go left/right until max difference reached.
   @debug_msg: "local_peaks_duo: entry"

   mindur_nucl = 0.025	; minimum duration of nucleus (otherwise rejected)

   .time = .t1
   while (.time < .t2)
      selectObject: .dstID
      .j = Get interval at time: syllable_tier, .time
      .x1 = Get start point: syllable_tier, .j
      .x2 = Get end point: syllable_tier, .j
      @is_unvoiced_region: dstID, .x1, .x2
      # default values used in case of error
         left = .x1
         right = .x2
      if (result = 1)	; fully unvoiced
         @set_boundary_label: .dstID, syllable_tier, .x1, .x2, "U"
      else		; fully or partly voiced
         @get_peak_duo: .x1, .x2
         @add_boundary: .dstID, nucleus_tier, left
         @add_boundary: .dstID, nucleus_tier, right
         if (valid)	; variable set by get_peak_duo 
            @add_boundary: .dstID, syllable_tier, left
            @add_boundary: .dstID, syllable_tier, right
            if (left-.x1 > time_step)
               @set_boundary_label: .dstID, syllable_tier, .x1, left, "<"
            endif
            if (.x2-right > time_step)
               @set_boundary_label: .dstID, syllable_tier, right, .x2, ">"
            endif
            @set_boundary_label: .dstID, nucleus_tier, left, right, "a"
            @set_boundary_label: .dstID, syllable_tier, left, right, "a"
         else ; invalid nucleus (too short)
            @set_boundary_label: .dstID, nucleus_tier, left, right, "reject"
            @set_boundary_label: .dstID, syllable_tier, .x1, .x2, "<>"
         endif ; valid
      endif ; voiced
      .time = .x2
   endwhile
   @debug_msg: "local_peaks_duo: exit"
endproc


procedure get_peak_duo: x1p, x2p
; returns values in <valid>, <left>, <right>
   valid = 0
   selectObject: intbpID
   tmaxfil = Get time of maximum: x1p, x2p, "Parabolic"
   selectObject: intensityID		; intensity full bandwidth
   tmax = Get time of maximum: x1p, x2p, "Parabolic"
   max = Get maximum: x1p, x2p, "Parabolic"
   if (max == undefined)	; can happen at end of signal, where intensity is undefined
      @msg: "get_peak_duo: max undefined at time x1='x1p:3', x2='x2p:3'"
   else
      repeat			; can happen at end of signal, where intensity is undefined
         selectObject: intensityID
         dip_int = Get value at time: x2p, "Nearest"
         selectObject: intbpID
         dip_intbp = Get value at time: x2p, "Nearest"
         if (dip_int == undefined or dip_intbp == undefined)
            @msg: "get_peak_duo: dip undefined at time 'x2p:3'"
            x2p -= time_step 
         endif
      until (dip_int <> undefined and dip_intbp <> undefined)
         @get_boundary: intbpID, tmaxfil, x1p, -1, diff_left
         left_filt = result
         @get_boundary: intensityID ,tmax, x1p, -1, diff_left
         left = max (left_filt, result)		; select rightmost of both candidates
;	 diff_right = min(9, max (3, (max-dip_intbp)/2 )) 
         diff_right =        max (3, (max-dip_intbp)/2 ) 
         @get_boundary: intbpID, tmaxfil, x2p, 1, diff_right
         right_filt = result
;	 diff_right = min(9, max (3, (max-dip_int)/2 )) 
         diff_right =        max (3, (max-dip_int)/2 ) 
         @get_boundary: intensityID, tmax, x2p, 1, diff_right
         right = max (right_filt, result)	; select rightmost of both candidates
         right = min (x2p, right)		; different time unit in get_boundary
         @voiced_intersection: left, right
         if (result > 0 and right-left >= mindur_nucl)
            valid = 1
         endif
   endif
endproc


procedure find_silences: paramID_, dstID, dst_tier
; Use Praat's procedure to identify silent pauses on the basis of <paramID_> (usu. intensity) 
; and copy results to tier <dst_tier> of <dstID>
   @debug_msg: "find_silences: entry"
   selectObject: dstID
   min_silent_interval = 0.15		; Praat standard = 0.1
   min_sounding_interval = 0.05		; Praat standard = 0.1
   silence_threshold = -30.0		; Praat standard = -25.0
   selectObject: paramID_
   .silencesID = To TextGrid (silences): silence_threshold, min_silent_interval, min_sounding_interval, "_", "a"
   Rename: "silences"
   .n = Get number of intervals: 1
   ; Copy boundaries from silences TextGrid to dstID in time range
   for .j to .n
      selectObject: .silencesID
      .label$ = Get label of interval: 1, .j
      .x1 = Get start point: 1, .j
      .x2 = Get end point: 1, .j
      @add_boundary: dstID, dst_tier, .x1
      @add_boundary: dstID, dst_tier, .x2
      @set_boundary_label: dstID, dst_tier, .x1, .x2, .label$
   endfor
   removeObject: .silencesID
   @debug_msg: "find_silences: exit"
endproc


procedure voiced_portions: .dstID, .intID, .at1, .at2
# Find voiced portions in range <.at1>..<.at2> of signal and store corresponding intervals in nucleus_tier of <.dstID> TextGrid.
   @debug_msg: "voiced_portions: entry"
   @find_silences: .intID, .dstID, syllable_tier
   selectObject: .dstID
   # number of intervals grows during loop
   .time = .at1
   @add_boundary: .dstID, nucleus_tier, .time
   while (.time + time_step < .at2)		; avoid rounding error
      selectObject: .dstID
      .j = Get interval at time: syllable_tier, .time 
      .label$ = Get label of interval: syllable_tier, .j
      .x1 = Get start time of interval: syllable_tier, .j
      .x2 = Get end time of interval: syllable_tier, .j
      .nexttime = .x2
      ;@debug_msg: "voiced_portions: x1='.x1:4' x2='.x2:4'"
      @add_boundary: .dstID, nucleus_tier, .x2
      if (.label$ == "_")	; pause
         @set_boundary_label: .dstID, nucleus_tier, .x1, .x2, .label$
      else
         repeat
            @voiced_intersection: .x1, .x2
            ; returns result, left, right
            if (result)		; it contains a voiced part
               if (left-.x1 > time_step)
                  @add_boundary: .dstID, nucleus_tier, left
                  @set_boundary_label: .dstID, nucleus_tier, .x1, left, "U"
               endif
               @add_boundary: .dstID, nucleus_tier, right
               @set_boundary_label: .dstID, nucleus_tier, left, right, "a"
               .x1 = right
            else
               .x1 = .x2
            endif
         until (.x1 >= .x2)
      endif
      .time = .nexttime
   endwhile
   @debug_msg: "voiced_portions: exit"
endproc


procedure pseudo_syllables: .dstID, .intID, .ifilID, .at1, .at2, .mindiff
   @debug_msg: "pseudo_syllables: entry"
   @find_silences: .intID, .dstID, syllable_tier	; silent intervals are stored as intervals on syllabe tier
   @mark_unvoiced: .dstID, dip_tier, .at1, .at2		; voiced-unvoiced transitions are stored as points on dip tier
   # number of intervals grows during following loop
   # for each interval in syllable tier (either a silence or a speech fragment)
   .time = .at1
   while (.time + time_step < .at2)		; avoid rounding error
      selectObject: .dstID
      .j = Get interval at time: syllable_tier, .time
      .label$ = Get label of interval: syllable_tier, .j
      .x1 = Get start time of interval: syllable_tier, .j
      .x2 = Get end time of interval: syllable_tier, .j
      .nexttime = .x2
      if (.label$ = "a")
         @convexhull: .dstID, dip_tier, .ifilID, .x1, .x2, .mindiff		; major dips are stored as points on dip tier
         if (result)
            @tier_point_to_interval: .dstID, dip_tier, syllable_tier, .x1, .x2
            @local_peaks_duo: .dstID, .intID, .ifilID, .x1, .x2
         endif
      endif
      .time = .nexttime
   endwhile
   @pass4: .intID, .dstID, syllable_tier
   @debug_msg: "pseudo_syllables: exit"
endproc


procedure segmentation_pitch: .pitchID, .thresholdST, .intID, .intbpID, .t1, .t2, .dstID, .mindiff
   @debug_msg: "segmentation_pitch: entry"
   @find_silences: .intID, .dstID, syllable_tier		; silence and speech are stored as intervals on syllable tier
   @mark_unvoiced: .dstID, dip_tier, .t1, .t2			; voiced-unvoiced transitions are stored as points on dip tier
   ; @all_convexhull: .dstID, syllable_tier, .intID, .intbpID, .t1, .t2, .mindiff
   @pitch_changes: .pitchID, .t1, .t2, .thresholdST, .dstID, dip_tier
   @tier_point_to_interval: .dstID, dip_tier, syllable_tier, .t1, .t2
   @label_syllable_tier: .dstID
   ; @local_peaks_syllables: .dstID, .intID, .t1, .t2
   @debug_msg: "segmentation_pitch: exit"
endproc
 

procedure segmentation_pitch2: .pitchID, .thresholdST, .intID, .t1, .t2, .mindur, .dstID
   @debug_msg: "segmentation_pitch2: entry"
   @pitch_terraces: .pitchID, .t1, .t2, .thresholdST, .mindur, .dstID, dip_tier

   selectObject: .dstID
   ; Remove short duration pitch terraces (< .mindur), i.e. they will be integrated in preceeding interval
if (1)
   .j = Get number of points: dip_tier
   while (.j > 1)
      .label$ = Get label of point: dip_tier, .j
      .x2 = Get time of point: dip_tier, .j
      .x1 = Get start time
      if (.j > 1)
         .x1 = Get time of point: dip_tier, .j-1
	  endif
      if (.label$ == "T" and .x2-.x1 < .mindur)
         Remove point: dip_tier, .j
      endif
      .j -= 1
   endwhile
endif
   ; Merge adjacent slope parts 
if (1)
   .j = Get number of points: dip_tier
   while (.j > 1)
      .label$ = Get label of point: dip_tier, .j
      .prev$ = Get label of point: dip_tier, .j-1
      if (.label$ == "S" and .prev$ == "S")
         Remove point: dip_tier, .j-1
      endif
      .j -= 1
   endwhile
endif
   ; Convert point tier ("dip") to interval tier (syllables) and label pitch terraces as "a" 
   .n = Get number of points: dip_tier
   for .j to .n
      .time = Get time of point: dip_tier, .j
      .label$ = Get label of point: dip_tier, .j
      ; if (.time >= .t1 and .time <= .t2)
      Insert boundary: syllable_tier, .time
      if (.label$ == "T")
         .k = Get low interval at time: syllable_tier, .time
         Set interval text: syllable_tier, .k, "a"
      endif
   endfor
   @debug_msg: "segmentation_pitch2: exit"
endproc


procedure spectral_similarity: .soundID, .dstID, .intID, .at1, .at2, .dcorr
   @debug_msg: "spectral_similarity: entry"
   @find_silences: .intID, .dstID, syllable_tier		; silence and speech are stored as intervals on syllabe tier
   @mark_unvoiced: .dstID, dip_tier, .at1, .at2			; voiced-unvoiced transitions are stored as points on dip tier
   @spectral_correlation: .soundID, .at1, .at2, 0.03	; compute correlation between spectral slices;  time interval between spectral slices being correlated = 30ms
   .corrID = result
   # number of intervals grows during loop
   .time = .at1
   while (.time + time_step < .at2)		; avoid rounding error
      selectObject: .dstID
      .j = Get interval at time: syllable_tier, .time
      .label$ = Get label of interval: syllable_tier, .j
      .x1 = Get start point: syllable_tier, .j
      .x2 = Get end point: syllable_tier, .j
      .nexttime = .x2
      if (.label$ = "a")
         @convexhull: .dstID, dip_tier, .corrID, .x1, .x2, .dcorr		; major dips are stored as points on dip tier
         if (result)
            @tier_point_to_interval: .dstID, dip_tier, syllable_tier, .x1, .x2
            .t = .x1
            repeat
               .i = Get interval at time: syllable_tier, .t
               Set interval text: syllable_tier, .i, "a"
               .t = Get end point: syllable_tier, .i
			until (.t >= .x2)
         endif
      endif
      .time = .nexttime
   endwhile
   .ns = Get number of intervals: syllable_tier
   for .j to .ns
      selectObject: .dstID
      .label$ = Get label of interval: syllable_tier, .j
      if (.label$ = "a")      
         .x1 = Get start point: syllable_tier, .j
         .x2 = Get end point: syllable_tier, .j
         @get_local_peak: .dstID, .intID, .x1, .x2, 0, .x1, .x2
      endif
   endfor
   removeObject: .corrID
   @debug_msg: "spectral_similarity: exit"
endproc


procedure spectral_correlation: .soundID, .t1, .t2, .timegap
; Compute correlation between successive spectral slices to find point of spectral change.
; <.timegap>	the time between spectral slices being correlated.
; The array of correlation values is casted to an intensity object (the ID od which is returned in <result>, 
; for later use in convex hull.
   ncols = (.t2-.t1)/time_step + 1
   ; Create Matrix: name$, xmin, xmax, ncols, dx, x1, ymin, ymax, nrows, dy, y1, formula$
   .corr = Create Matrix: "correlation", .t1, .t2, ncols, time_step, .t1, 0, 1, 1, 1, 0, "0"
   selectObject: .soundID
   ; To Cochleagram: timestep, freqresolution(Bark), windowlength, forward_masking_time
   .cochleaID = To Cochleagram: 0.01, 0.1, 0.03, 0.03
   selectObject: .soundID

   .time = .t1
   while (.time < .t2)
      @cochl_slice: .cochleaID, .time - .timegap/2
      .tmp1 = result
      @cochl_slice: .cochleaID, .time + .timegap/2
      .tmp2 = result
      selectObject: .tmp1, .tmp2
      .tmp3 = Append columns
      .corID = To Correlation
      .r = Get value: 1, 2
      selectObject: .corr
      .col = ((.time-.t1)/time_step)+1
      Set value: 1, .col, .r
      removeObject: .tmp1, .tmp2, .tmp3, .corID
      .time += time_step
   endwhile
   selectObject: .corr
   result = To Intensity
   removeObject: .cochleaID, .corr
endproc


procedure cochl_slice: .cochleaID, .time
   selectObject: .cochleaID
   excID = To Excitation (slice): .time
   matID = To Matrix
   mattID = Transpose
   result = To TableOfReal
   removeObject: excID, matID, mattID
endproc


procedure mark_unvoiced: dstID, tier_, at1, at2
# add boundaries to <tier_> (point tier) in dstID, at voiced-unvoiced transitions
   @debug_msg: "mark_unvoiced: entry"
   selectObject: dstID
   n_ = Get number of intervals: vuv_tier
   prev$ = ""
   time_ = at1
   while (time_ < at2)
      j_ = Get interval at time: vuv_tier, time_ 
      label_$ = Get label of interval: vuv_tier, j_
      x2_ = Get end point: vuv_tier, j_
      if (label_$ = "U" and prev$ = "V")
         x1_ = Get start point: vuv_tier, j_
         @tier_point_add: dstID, tier_, x1_, "0"
      endif
      prev$ = label_$
      time_ = x2_
      if (j_ == n_)
         time_ = at2	; exit loop
      endif
   endwhile
   @debug_msg: "mark_unvoiced: exit"
endproc


procedure pass4: intID, dstID, tier_
; 1. Group sequences with pattern: [<] a [>]
   @debug_msg: "pass4: entry"
   selectObject: dstID
   n_ = Get number of intervals: tier_
   j = 2
   while (j <= n_)
      selectObject: dstID
      label_$ = Get label of interval... tier_ j
      if (label_$ = "a")
         x1_ = Get start point... tier_ j
         x2_ = Get end point... tier_ j
         select intID
         ymax_ = Get maximum... x1_ x2_ Parabolic
         select dstID
         Set interval text... tier_ j syl
         if (j > 1) 
            prevlabel_$ = Get label of interval... tier_ j-1
            if (prevlabel_$ = "<")
               Remove left boundary... tier_ j
               j -= 1
               n_ -= 1
               Set interval text... tier_ j syl
            endif
         endif
         if (j < n_) 
            nextlabel_$ = Get label of interval... tier_ j+1
            x_ = Get end point... tier_ j
            select intID
            y_ = Get value at time... x_ Nearest
            if (nextlabel_$ = ">" and ymax_ - y_ < 25)
               select dstID
               Remove right boundary... tier_ j
               n_ -= 1
               Set interval text... tier_ j syl
            endif
         endif
      endif ; label_$ = "a"
      j += 1
   endwhile
; 2. Group unvoiced rejected nuclei with next syllable
      j = 1
      while (j <= n_)
         selectObject: dstID
         label_$ = Get label of interval: tier_, j
         if (label_$ = "syl")
               if (j > 1) 
                  prevlabel_$ = Get label of interval... tier_ j-1
                  ; if (prevlabel_$ = "<>" or prevlabel_$ = "U")
                  if (prevlabel_$ = "U")
                     Remove left boundary... tier_ j
                     n_ -= 1
                     j -= 1
                     Set interval text... tier_ j syl
                  endif
               endif
         endif
         j += 1
      endwhile
   @debug_msg: "pass4: exit"
endproc


procedure get_local_peak: dstID, .paramID, x1, x2, dyn_threshold, seed_x1, seed_x2
# Find maximum in interval <seed_x1>..<seed_x2>. Find boundaries inside <x1>..<x2>.
# Time ranges <x1>..<x2> and <seed_x1>..<seed_x2> may be identical.
# <dyn_threshold>	Use dynamic intensity threshold for right boundary
   @debug_msg: "get_local_peak: entry x1='x1:3' x2='x2:3'"
   mindur_nucl = 0.025	; minimum duration of nucleus (otherwise rejected)
   selectObject: .paramID
   time_step = Get time step
   ; default values used in case of error
      left = x1		; left boundary of peak
      right = x2	; right boundary of peak
      label2$ = "-"	; not a nucleus
      valid = 0		; not a valid nucleus
   @is_unvoiced_region: dstID, x1, x2
   if (result = 0)		; not fully unvoiced, i.e. partly voiced
      selectObject: .paramID
      maxtime = Get time of maximum: seed_x1, seed_x2, "Parabolic"
      max = Get maximum: seed_x1, seed_x2, "Parabolic"
      if (max = undefined)	; can happen at both ends of signal, where intensity is undefined
         @msg: "get_local_peak: max undefined at 'maxtime:3', seed_x1='seed_x1:3' seed_x2='seed_x2:3'"
         # use defaults
      else			; max is defined
         # find left boundary
         dipL = Get value at time: x1, "Nearest"
         if (dipL = undefined)
            @msg: "get_local_peak: dip left undefined at time 'x1:3'"
         else
            diff_left = 3
            @get_boundary: .paramID, maxtime, x1, -1, diff_left
            left = result
            if ((segm_method == segm_vnucl or segm_method == segm_mrhyme) and left-seed_x1 >= 0.075)
               left = seed_x1 + 0.02
            endif
         endif
         # find right boundary
         dip = Get minimum: maxtime, x2, "Parabolic"
         if (dip = undefined)
            @msg: "get_local_peak: dip right undefined at time 'x2:3'"
         else
            diff_right = 9
            if (dyn_threshold)
               diff_right = max(3, (max - dip)*0.80) 
            endif
            @get_boundary: .paramID, maxtime, x2, 1, diff_right
            right = result
         endif
         if (dipL != undefined and dip != undefined)
            @voiced_intersection: left, right
            if (result > 0 and right-left >= mindur_nucl)
               label2$ = "a"
               valid = 1
            endif
         endif
      endif
   endif ; voiced
;@msg: "get_local_peak: ['seed_x1:3' 'seed_x2:3'] peak='maxtime:3' ADDING boundaries L='left:3' R='right:3' label='label2$'"
   @add_boundary: dstID, nucleus_tier, left
   @add_boundary: dstID, nucleus_tier, right
   @set_boundary_label: dstID, nucleus_tier, left, right, label2$
   if (valid)
      @add_boundary: dstID, nucleus_tier, x1
      @add_boundary: dstID, nucleus_tier, x2
      if (left-x1 > time_step)
         @set_boundary_label: dstID, nucleus_tier, x1, left, "<"
      endif
      if (x2-right > time_step)
         @set_boundary_label: dstID, nucleus_tier, right, x2, ">"
      endif
   endif ; valid
endproc


procedure all_convexhull: .gridID, .tier, .paramID, .t1, .t2, .mindiff
# Apply convexhull to each interval of tier in grid, storing major dips in dip_tier
   .time = .t1
   while (.time + time_step < .t2)		; avoid rounding error
      selectObject: .gridID
      .j = Get interval at time: .tier, .time
      .label$ = Get label of interval: .tier, .j
      .x1 = Get start point: .tier, .j
      .x2 = Get end point: .tier, .j
      if (.label$ = "a")
         @convexhull: .gridID, dip_tier, .paramID, .x1, .x2, .mindiff
      endif
      .time = .x2
   endwhile
endproc


procedure convexhull: .gridID, .tier, .paramID, .x1, .x2, .mindiff
# <.gridID>		textgrid in which segmentation points are stored
# <.tier>		point tier where segmentation points are stored
# <.paramID>	parameter on which segmentation is based
# <.x1>			start time of analysis
# <.x2>			end time of analysis
# <.mindiff>	difference threshold for segmentation
# <result>		
   @debug_msg: "convexhull: entry"
   convexhull_winlen = 0.75
   selectObject: .paramID
   .dx = Get time step

   # Skip part at start of signal for which parameter is undefined
   .ok_L = 0
   .xL = .x1
   repeat
      .y = Get value at time: .xL, "Nearest"
      if (.y == undefined)
         .xL += .dx
      else
         .ok_L = 1
      endif
   until (.ok_L or .xL > .x2)

   # Skip part at end of signal for which parameter is undefined
   .ok_R = 0
   .xR = .x2
   repeat
      .y = Get value at time: .xR, "Nearest"
      if (.y == undefined)
         .xR -= .dx
      else
         .ok_R = 1
      endif
   until (.ok_R or .xR < .x1)

   result = 0
   if (.ok_L and .ok_R)
      while (.xL < .xR)
         .xlast = min (.xL + convexhull_winlen, .xR)
         .dip = 0
         repeat
            @time_maxdiff: .paramID, .xL, .xlast, .dx, .mindiff
            if (tmaxdif >= 0)
               .xlast = tmaxdif
               .dip = maxdif
            endif
         until (tmaxdif < 0)
         if (.dip > .mindiff)
;@msg: "ADDING DIP at '.xlast:4', diff='.dip:3'" 
            @tier_point_add: .gridID, .tier, .xlast, "'.dip:1'"
         endif
         .xL = .xlast ; shift start of analysis window to end of previous
      endwhile
      result = 1
   endif
   @debug_msg: "convexhull: exit"
endproc


procedure time_maxdiff: paramID, xh1, xh2, dx, mindif
# returns <tmaxdif>, <maxdif>, <result>
# <result> (boolean) true if dip found (dif >= mindif)
      selectObject: paramID
      maxdif = 0.0
      tmax_ = Get time of maximum... xh1 xh2 Parabolic
      if (tmax_ > xh1)
         x = xh1
         tmaxdif = xh1
         h = Get value at time... x Nearest
         while (x < tmax_)	; locate max diff while going up hull to peak
            y = Get value at time... x Nearest
            if (h-y > maxdif)
               maxdif = h-y
               tmaxdif = x
            endif
            h = max (y,h)
            x += dx
         endwhile
      endif
      if (tmax_ < xh2)
         x = xh2
         h = Get value at time... x Nearest
         while (x > tmax_)
            y = Get value at time... x Nearest
            if (h-y > maxdif)
               maxdif = h-y
               tmaxdif = x
            endif
            h = max (y,h)
            x -= dx
         endwhile
      endif
      if (maxdif >= mindif)
         result = 1
      else
         result = 0
         tmaxdif = -1
      endif
endproc


procedure add_boundary: .destID, .tier, .xbound
# Add a boundary, but 
# - avoid adding left boundary at right boundary of previous segment
# - avoid adding boundary at starttime of TextGrid
# - avoid adding boundary at endtime of TextGrid
# <.xbound>	time of boundary
   ;@debug_msg: "add_boundary: entry"
   selectObject: .destID
   .xstop = Get end time
   .xstart = Get start time
   .i = Get interval at time: .tier, .xbound
   if (.i <= 0)
      @msg: "add_boundary: i<=0 xbound='.xbound'"     
   endif
   .t1 = Get start time of interval: .tier, .i
   .t2 = Get end time of interval: .tier, .i
   if (abs(.t1-.xbound) > time_step and abs(.t2-.xbound) > time_step and .xbound > .xstart and .xbound < .xstop)
      Insert boundary: .tier, .xbound
   endif
   ;@debug_msg: "add_boundary: exit"
endproc


procedure tier_point_add: .destID, .tier, .x, .text$
   selectObject: .destID
   .i = Get nearest index from time: .tier, .x
   if (.i == 0) ; no points in tier
      Insert point: .tier, .x, .text$
   else
      .t = Get time of point: .tier, .i
      if (.t == .x)
         @msg: "tier_point_add: already a point at time '.x:3'" 
      else
         Insert point: .tier, .x, .text$
      endif
   endif
endproc


procedure set_boundary_label: .destID, .tier, .xleft, .xright, .label$
   ;call debug_msg set_boundary_label: entry
   select .destID
   .i = Get interval at time: .tier, .xleft+(.xright-.xleft)/2
   Set interval text: .tier, .i, .label$
   ;call debug_msg set_boundary_label: exit
endproc


procedure get_boundary_old: .paramID, .peaktime, .xlimit, .incr, .diff
# Get time of left or right boundary.
# <.peaktime>	time of peak in interval
# <.xlimit>		time limit for left or right boundary
# <.incr>		-1 for left boundary, 1 for right boundary
# <.diff>		max difference between peak and value at boundary
# Returns time of left or right boundary in <result>
   result = 0
   selectObject: .paramID
   .peakframe = Get frame number from time: .peaktime
   .i = round (.peakframe)
   .max = Get value in frame: .i
   .limit = Get frame number from time: .xlimit
   .limit = round (.limit)
;@msg: "get_boundary: t='.peaktime:4' xlimit='.xlimit:5' limit='.limit'"
   .ok = 1
   while (.ok)
      .nexti = .i + .incr
      .y = Get value in frame: .i
      if ((.incr < 0 and .nexti < .limit) or (.incr > 0 and .nexti > .limit) or (.max-.y > .diff))
         .ok = 0
      else
         .i = .nexti
         if (.incr < 0 and .i <= 0)
            .ok = 0
            .i = 1
         endif
      endif
   endwhile
   result = Get time from frame: .i
   if (.incr > 0)	; frame is real number -> rounding errors
      result = min (result, .xlimit)
   else
      result = max (result, .xlimit)
   endif
endproc


procedure get_boundary: .paramID, .peaktime, .xlimit, .incr, .diff
# Get time of left or right boundary.
# <.peaktime>	time of peak in interval
# <.xlimit>		time limit for left or right boundary
# <.incr>		-1 for left boundary, 1 for right boundary
# <.diff>		max difference between peak and value at boundary
# Returns time of left or right boundary in <result>
   selectObject: .paramID
   .dx = Get time step
   .t = .peaktime
   .max = Get value at time: .t, "Nearest"
   .ok = 1
   while (.ok)
      .y = Get value at time: .t, "Nearest"
      .tn = .t + (.incr * .dx)
      if ((.incr < 0 and .tn < .xlimit) or (.incr > 0 and .tn > .xlimit) or (.max-.y > .diff))
         .ok = 0
      else
         .t = .tn
         if (.incr < 0 and .t <= 0)
            .ok = 0
            .t = 0
         endif
      endif
   endwhile
;@msg: "get_boundary: tp='.peaktime:4' xlimit='.xlimit:5' t='.t:5' incr='.incr'"
   if (.incr > 0)
      result = min (.t, .xlimit)
   else
      result = max (.t, .xlimit)
   endif
endproc



procedure tier_point_to_interval: .gridID, .ptier, .itier, .t1, .t2
# Convert points in PointTier <ptier> to intervals in <itier>
   @debug_msg: "tier_point_to_interval: entry"
   selectObject: .gridID
   .n = Get number of points: .ptier
   for .j to .n
      .time = Get time of point: .ptier, .j
      if (.time >= .t1 and .time <= .t2)
         @add_boundary: .gridID, .itier, .time
      endif
   endfor
   @debug_msg: "tier_point_to_interval: exit"
endproc


procedure is_unvoiced_region: .gridID, .x1, .x2
# Returns 1 in variable <result> if <.x1> and <.x2> are within same unvoiced interval of the VUV grid
   @debug_msg: "is_unvoiced_region: entry"
   result = 0
   selectObject: .gridID
   .i1 = Get interval at time: vuv_tier, .x1
   if (.i1 == 0)		; peeking before analysed signal; return unvoiced
      @msg: "is_unvoiced_region: i1='.i1' x1='x1'"
      result = 1
   else
      .i2 = Get interval at time: vuv_tier, .x2
      .label$ = Get label of interval: vuv_tier, .i1
      if (.i1 == .i2 and .label$ = "U")
         result = 1
      endif
   endif
   @debug_msg: "is_unvoiced_region: exit"
endproc


procedure unvoiced_proportion x1_ x2_
# returns proportion of unvoiced part inside <x1_>..<x2_> 
# returns left, right: the unvoiced part inside <x1_>..<x2_>
   result = 0
   select nucleiID
   ni_ = Get number of intervals... vuv_tier
   i_ = Get interval at time... vuv_tier x1_
   if (i_ == 0)
      # peeking outside analysed signal; return unvoiced
      call msg error in unvoiced_proportion i='i_' x1='x1_'
      result = 0
   else
      ux1 = x2_
      ux2 = ux1
      repeat
         label_$ = Get label of interval... vuv_tier i_
         t1_ = Get start point... vuv_tier i_
         t2_ = Get end point... vuv_tier i_
         if (label_$ = "U")
            ux1 = max(x1_, t1_)
         endif        
         i_ += 1
      until (ux1 < x2_ or t1_ >= x2_ or i_ >= ni_)
      ux2 = min (t2_, x2_)
      result = (ux2 - ux1) / (x2_ - x1_)
   endif
endproc


procedure voiced_intersection: .x1, .x2
# returns 1 in <result> if there is a voiced part inside <.x1>..<.x2> 
# returns <left>, <right>: the voiced part inside <.x1>..<.x2>
   result = 0
   selectObject: nucleiID
   .i1 = Get interval at time: vuv_tier, .x1
   .i2 = Get interval at time: vuv_tier, .x2
   if (.i1 == 0 or .i2 == 0)	; peeking outside analysed signal; return unvoiced
      @error_msg: "voiced_intersection: .i1='.i1' x1='.x1'"
   else
      while (.i1 <= .i2)
         .label$ = Get label of interval: vuv_tier, .i1
         if (.label$ = "V")
            if (result == 0)
               .t = Get start time of interval: vuv_tier, .i1
               left = max (.t, .x1)
            endif
            .t = Get end time of interval: vuv_tier, .i1
            right = min (.x2, .t)
            result = 1
         else
            if (result == 1)
               .i1 = .i2+1
            endif
         endif
         .i1 += 1
      endwhile
   endif
endproc


procedure is_vowel: s$
# Sets variable <is_vowel> to 1 if <s$> is a vowel (SAMPA or Praat conventions) 
# and to 0 otherwise
   .input$ = s$
   is_vowel = 0
; remove some symbols, mainly diacritics
   s$ = replace_regex$ (s$, "^""", "", 1)		; primary stress (at start of label) in SAMPA
   s$ = replace$ (s$, "`", "", 0)				; remove all rhoticity diacritics
   s$ = replace_regex$ (s$, "^i_d$", "i", 1)	; dental i in X-SAMPA
   s$ = replace_regex$ (s$, ":+$", "", 1)		; diacritic(s) indicating lengthening (at end of label)
   s$ = replace$ (s$, "\:f", "", 0)				; diacritic indicating lengthening
   len = length (s$)
   first$ = left$ (s$, 1)
   if (index_regex (s$, "^[aeiouyAEIOUYOQV@23679&\{\}]+$") )	; 1 or more vocalic elements
      is_vowel = 1
   elsif (len = 1 and s$ = "V")				; ad hoc convention (on special request)
      is_vowel = 1
   elsif (len = 2)
      if (index ("~a~e~o~E~O~A~9~U~", s$))		; nasal vowels; U~ used by some
         is_vowel = 1
      ;elsif (index (":\o:", ":'s$':"))
      ;   is_vowel = 1
      endif
   elsif (len = 3 and first$ = "\" )
      z$ = mid$ (s$,1,3)    ; first three characters
      z2$ = mid$ (s$,1,2)   ; first two characters
      third$ = mid$ (s$, 3, 1)
      if (third$ = """" and (index (":\a:\e:\i:\o:\u:\y:", ":'z2$':")))
         is_vowel = 1 
      elsif (third$ = "-" and (index (":\e:\i:\o:\u:", ":'z2$':")))
         is_vowel = 1 
      elsif (index (":\a~:\o~:", ":'s$':"))    ; nasal vowels
         is_vowel = 1       
      elsif (index (":\o/:\ab:\as:\ae:\at:\ep:\ef:\er:\oe:\Oe:\ct:\vt:\ic:\yc:\sw:\sr:\rh:\hs:\kb:\mt:\u-:", ":'z$':"))
         is_vowel = 1 
      endif
   elsif (len > 3)
      if (index (":a\~^:\ep~:\as\~^:\ep\~^:\ct\~^:", ":'s$':"))
         is_vowel = 1
      endif
   endif
endproc


procedure is_syllabic: s$
; sets <result> = true is string is vowel or syllabic consonant
; sets <is_vowel> = true if string is a vowel
   @is_vowel: s$
   if (is_vowel)
      result = 1
   ; elsif (index_regex (s$, "^[mnJNlrR]\\\|v$"))	; Praat phonetic symbol "\|v"
   elsif (index_regex (s$, "^[mnJNlrR]=$"))		; X-SAMPA symbol "="
      result = 1
   else
      result = 0
   endif
endproc


procedure pitch_changes: .pitchID, .t1, .t2, .thresholdST, .grid, .tier
# Detect pitch changes exceeding <.thresholdST> (in semitones) and store them as points in tier <.tier> of <.grid>
   selectObject: .pitchID
   .n = Get number of frames
   .j = Get frame number from time: .t1
   .j = max (.j, 1)
   .j2 = Get frame number from time: .t2
   .j2 = min (floor(.j2), .n)
   .prevHz = Get value in frame: .j, "Hertz"
   while (.j < .j2)
      .pitchHz = Get value in frame: .j, "Hertz"
      if (.pitchHz == undefined)
         repeat		; find end of part with undefined pitch 
            .right = .j
            .j += 1
            .nextHz = Get value in frame: .j, "Hertz"
         until (.j == .j2 or not (.nextHz == undefined))
      else
         repeat		; find point where pitch change exceeds threshold or where pitch is undefined
            .right = .j
            .j += 1
            .nextHz = Get value in frame: .j, "Hertz"
            .distST = 12 * log2 (.nextHz/.prevHz)
         until (.nextHz == undefined or .j == .j2 or abs(.distST) > .thresholdST)
      endif
      .t = Get time from frame number: .right
      selectObject: .grid
	  Insert point: .tier, .t, ""
      selectObject: .pitchID
      .prevHz = Get value in frame: .j, "Hertz"
   endwhile
endproc


procedure pitch_terraces: .pitchID, .t1, .t2, .thresholdST, .mindur, .grid, .tier
# Detect pitch changes exceeding <.thresholdST> (in semitones per frame period) and store them as points in tier <.tier> of <.grid>
   selectObject: .pitchID
   .n = Get number of frames
   .prevt = Get start time
   .j = Get frame number from time: .t1
   .j = max (.j, 1)
   .j2 = Get frame number from time: .t2
   .j2 = min (floor(.j2), .n)
   .yHz = Get value in frame: .j, "Hertz"
   if (.yHz == undefined)
      .state$ = "U" ; "unvoiced" 
   else
      .state$ = "T" ; "terrace" 
   endif
   while (.j < .j2)
 .t = Get time from frame number: .j
      .switch = 0
      .yHz = Get value in frame: .j, "Hertz"
      .j += 1
      .nextHz = Get value in frame: .j, "Hertz"
      .dist = 12 * log2 (.nextHz/.yHz)
; printline *** t='.t:3' state='.state$' j='.j' nextF0='.nextHz:0' dist='.dist:2'
      if (.state$ == "T")
         if (.nextHz == undefined)
            .switch = 1
            .newstate$ = "U" 
         elsif (abs(.dist) > .thresholdST)
            .switch = 1
            .newstate$ = "S" ; "slope" 
         endif
      elsif (.state$ == "S")
         if (.nextHz == undefined)
            .switch = 1
            .newstate$ = "U" 
         elsif (abs(.dist) <= .thresholdST)
            .switch = 1
            .newstate$ = "T" 
         endif
      elsif (.state$ == "U")
         if not (.nextHz == undefined)
            .switch = 1
            .newstate$ = "T" 
         endif
      endif
      if (.switch)
         .t = Get time from frame number: .j-1
 .dd = .t - .prevt
; printline switch_to='.newstate$' duration='.dd:4'
;         if (.t - .prevt >= .mindur or .newstate$ <> "T")
            selectObject: .grid
            Insert point: .tier, .t, .state$
		    .state$ = .newstate$
            selectObject: .pitchID
            .prevt = .t
;         else
;printline terrace too short...
;         endif
      endif
   endwhile
   selectObject: .pitchID
   .t = Get time from frame number: .j2
   selectObject: .grid
   Insert point: .tier, .t, .state$
endproc


procedure label_syllable_tier: .gridID
   selectObject: .gridID
   .n = Get number of intervals: syllable_tier
   for .j to .n
      .x1 = Get start point: syllable_tier, .j
      .x2 = Get end point: syllable_tier, .j
	  @is_unvoiced_region: .gridID, .x1, .x2 
      if (not result) 
         Set interval text: syllable_tier, .j, "a"
      endif
   endfor
endproc
