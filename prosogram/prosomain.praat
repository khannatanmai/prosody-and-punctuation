# prosomain.praat -- include file for prosogram
# This file is included by prosogram.praat. It isn't a stand-alone script. Use prosogram.praat instead.
# Author: Piet Mertens
# Requires Praat 6.0.43 or higher.
# For documentation see http://sites.google.com/site/prosogram/
# Last modification: 2020-01-16
# 2017-01-13	Global variable "filename_extensions_sound$" specifies accepted file extensions for sound files 
# 2017-03-14	added mp3 as an accepted file extension for sound files 
# 2017-09-04	added prosodic profile summary in construct_filenames  
# 2017-12-28	replaced double quotes in error messages  
# 2017-12-28	check for double quotes in field Tiers_to_show  
# 2018-01-10	corrected bug
# 2018-02-25	added batch command "segmentation"
# 2018-03-06	added options "g", "dg", "dmin", "outputsuffix" to batch command "prosogram"
# 2018-03-15	added options "fc_low", "fc_high", "mindiff" to batch command "segmentation"
# 2018-03-22	added arguments to batch command "segmentation"; improved "tier_trim_nucleus"
# 2018-03-23	corrected bug in calculate_pitch_int
# 2018-03-26	improved "tier_trim_nucleus"
# 2018-03-30	added segmentation method "specsim" to batch command "segmentation"
# 2018-03-30	added segmentation method "pitchchange" to batch command "segmentation" (work in progress)
# 2018-05-26	corrected recently introduced bug
# 2018-06-05	added segmentation output file for method "pitchchange"
# 2018-08-09	added segmentation method "pitchterrace" (work in progress)
# 2018-10-29	many changes
# 2018-11-07	many changes, partial update to current Praat scripting syntax
# 2018-11-12	added options "targets" to batch command "prosogram"
# 2019-01-24	added show_trajectories
# 2019-02-12	removed unused code
# 2019-04-08	tasks types merged and replaced by flags: draw_pitch_target_values, save_intermediate_data, polytonia_annotation
# 2019-04-25	added _profile_data file with prosodic profiles for current input file
# 2019-04-30	many changes
# 2019-07-01	small changes to correct bugs introduced by v2.19a
# 2019-09-03	remove double double quotes in strings passed to @msg
# 2019-11-29	added batch command "hesitations"
# 2019-12-03	hesitation detection



# Modify the following lines to match the path of the application on your computer.
path_ghostscript$ = "c:\Program Files\gs\gs9.21\bin\gswin64c.exe"	; typical path on Windows-10 64 bit
path_nconvert$ = "c:\Program Files\XnView\nconvert"					; only used for creating GIF files

# Don't change the following lines:
path_ghostscript$ = replace$ (path_ghostscript$, "\Program Files\", "\Progra~1\", 1)
path_nconvert$ = replace$ (path_nconvert$, "\Program Files\", "\Progra~1\", 1)

# Accepted filename extensions for sound files:
filename_extensions_sound$ = ":wav:aiff:aifc:nist:flac:sound:s16:mp3:"

# Procedure hierarchy
# main								main procedure, in both modes (script form or batch mode)
#   initialization_main
# process_form						process fields in script form (in script mode, not in batch mode)
# process_multiple_input_files		all processing in all tasks, for 1 or more input speech files
#   task_flags						set flags depending on task
#     map_segmentation_type         mapping between segmentation name and tier requirements
#   initialization_multiple_files
#   process_one_input_file
#     initialization_per_file
#     construct_filenames			rule-based filenames
#     read_annotation_file
#     read_parameter_files			read parameters from file
#     calculate_parameters			calculate parameters
#       calculate_pitch_int         calculate pitch (internal command, not batch command)
#       calculate_intensity_bp_filtered
#     corpus_conversion				apply corpus conventions: tiernames, tier content, etc.
#     prepare_plotted_textgrid		create textgrid used in prosogram drawing
#     load_intermediate_data_files
#     get_segmentation				read or calculate segmentation (phonetic alinment, etc.)
#       read_nuclei_file_interactive_mode	read internal TextGrid from earlier run
#       copy_tiers_from_segmentation_textgrid
#       make_segmentation			calculate segmentation
#       safe_nuclei					check boundaries of nuclei: unvoiced fragments, octave jumps, etc.
#     create_table_of_nuclei		create table of nuclei with prosodic features
#     initialize_nucldat			initialize some columns in table of nuclei
#     speaker_info_get				list all speakers in annotation and add speaker id to nucldat table 
#     stylize_nuclei				pitch contour stylization
#     pitchrange_speakers			calculate pitch range of each speaker
#     pitchrange_normalized_pitch
#     prosodic_profile
#     prosodic_profile_new
#     detect_hesitations			detect hesitations from phonetic labels, acoustic prosodic features or words
#     store_stylization
#     speaker_autorange
#     gr_start_picturewin
#     gr_write_all_prosograms
#     cleanup_current_file
#     cleanup_global
#     store_features				write spreadsheet with prosodic features for syllables
#
# Commands in batch mode:
# prosogram							(batch mode) calculate prosogram
#   prosogram_variants				variants for polytonia, boundary detection, stress detection, etc.
# polytonia							(batch mode) calculate tonal annotation 
# segmentation						(batch mode) calculate automatic segmentation
# calculate_pitch					(batch mode) calculate pitch (two-pass algorithm)

version$ = "Prosogram v2.20a"
; @logging: "reset debug timed", "_log.txt"


corpus$ = ""			; "" = use default corpus conventions; 
						; Known corpora: rhapsodie, cprom, project_frfc, RUHRCAT, Prsir, gvlex, Kotsifas_2015


include prosoplot.praat
include segment.praat
include stylize.praat
include util.praat

;if (boundary_annotation or polytonia_annotation)
; include polytonia.praat
; include rules.praat
; include prominence.praat
;include duration.praat
;include elasticity.praat
;endif



; types of tasks
   task_prosogram = 4		; stylization, prosodic features, prosodic profile, Polytonia
   task_interactive = 5
   task_calc_pitch = 1
   task_calc_loudness = 10
   task_calc_bp_intensity = 13
   task_pitch_plot = 2
   task_segmentation = 3	; automatic segmentation into syllabic nuclei without phonetic alignment annotation
   task_annotation = 6		; Draw annotation only
   task_validate_tiers = 12
   task_autoanno = 9		; only when using Praat form, not in batch mode

; types of segmentation
   segm_vnucl = 1
   segm_extern = 2
   segm_aloudness = 3
   segm_anucl = 4
   segm_asyll = 5
   segm_msyllvow = 6
   segm_msyllpeak = 7
   segm_mrhyme = 8		; syllable rhyme using phoneme and syllable annotation
   segm_voiced = 9		; voiced portions
   segm_specsim = 10	; spectral similarity
   segm_pitchchange = 11 
   segm_pitchterrace = 12 


if (not variableExists ("font"))
   font_family$ = "Times"	; Font used in plot; else use "Helvetica"
   fontsize = 10
else
   font_family$ = extractWord$(font$, "")
   fontsize = extractNumber(font$, " ")
endif

if (not variableExists ("batch_mode"))	; prosomain script started directly, not from prosogram.praat
   batch_mode = 1
   # variables initialized before batch functions
   output_filename$ = ""
   clip_level = 0
   scale_signal_amplitude = 0
   minimum_pitch = 0
   maximum_pitch = 450
   volatile = 0
   viewsize$ = "wide"
   @msg: "Batch mode..."
endif


procedure initialization_main
   draw_prosograms = 1			; draw_prosograms 			(modified by task_flags and by prosogram_variants)
   save_intermediate_data = 0	; save intermediate data 	(modified by task_flags and by prosogram_variants)
   hesitation_annotation = 0	; save detected hesitations 
   hesitation_method$ = "none"	; "annotation" / "phon+pros" / "words" / "none"
   pause_method$ = "nuclei"		; "nuclei" / "phon_tier" / "syll_tier"
   polytonia_annotation = 0		; automatic annotation of pitch movements and pitch levels
   calc_prominence = 0			; compute prominence measures
   boundary_annotation = 0		; automatic detection of prosodic boundaries
   stress_annotation = 0		; automatic detection of stress
   needs_loudness = 0			; compute loudness based on excitation spectrum
   
   ; Plotting 
   viewport_width = 7.5			; width of viewport used for prosogram (inches)
   clip_to_Y_range = 0			; clip stylisation to Y range
   ; Plotting options:
   greyscale = 0				; use greyscale instead of colors
   show_pitchrange = 0			;
   show_pauses = 0				; show detected pauses by "P"
   show_prominence = 0			;
   show_pseudosyllables = 0		; show pseudosyllables in textgrid
   show_harmonicity = 0			;
   show_elasticity = 0			;
   show_localrate = 0			;
   show_rhythm = 0				;
   show_hesitations = 0			;

   single_fname_graphics_output = 0; when true, all graphic files will be numbered using the same basename
   mindur_pause_anno = 0.2		; min duration for pause in annotation
   prefilterHP100 = 0			; Apply HP filtering to avoid problems with low quality audio recordings
   prefilterLP = 0				; Apply LP filtering when fricatives are too strong (low quality audio recording)
   avoid_insuff_memory = 0		; Used in calculation of intensity of BP filtered signal
   use_duration_model = 0		;
   ; duration_model_filename$ = "db_phon_dur_rhapsodie.txt"
   ; duration_model_filename$ = "db_phon_dur_cprom.txt"
   ; duration_model_filename$ = "db_phon_dur_gvlex.txt"
   duration_model_filename$ = ""
   collect_output = 0			; collect output in a single output file, for feature files, duration data, etc.
   long_feature_table = 0		; use long format feature table, along standard format
   rich_format_stylization = 0	; enable rich format stylization output file (one of the intermediate data files)
   do_cleanup = 1				; cleanup all loaded or computed data at end of process (disable for debugging)
   save_BP = 0					; only used in batch_mode segmentation
   globalsheet_available = 0	; multiple inputfile prosodic profile
endproc


procedure main
   @debug_msg: "main: entry"
   clearinfo				; clear the info window
   @debug_msg: "main: Praat version = 'praatVersion'"
   if (praatVersion < 6043)
      @fatal_error: "Requires Praat version 6.0.43 or higher. Please update your Praat application." 
   endif

   @initialization_main
   @process_form
   @process_multiple_input_files: task, input_files$, anal_t1, anal_t2

   @msg: "Ready"
endproc


procedure process_multiple_input_files: .task, .filespec$, pmf_t1, pmf_t2
# <pmf_t1>	starttime as used in call to multiple file batch processing
   @debug_msg: "process_multiple_input_files: entry, filespec=<'.filespec$'>"
   @msg: "'newline$'Script version 'version$'..."
   task = .task		; needed in batch_mode, because global variable <task> is defined in process_form
   nrofFiles = 0
   if (index (.filespec$, "*"))		; wildcard found
      filelistID = Create Strings as file list: "filelist", .filespec$
      nrofFiles = Get number of strings
      .wildcard_found = 1
   else
      .wildcard_found = 0
      if (fileReadable (.filespec$))
         nrofFiles = 1
      endif
   endif
   if (nrofFiles == 0)
      @fatal_error: "No input files found for <'.filespec$'>"
   endif
   ; filename returned by 'Get string' does not include path
   @fname_parts: .filespec$
   indir$ = result4$
   @task_flags
   @initialization_multiple_files
   if (task == task_interactive and nrofFiles > 1)
      @msg: "Interactive mode uses a single speech file, not many."
      nrofFiles = 1
   endif
   for iFile to nrofFiles
      if (not .wildcard_found)
         .filespec$ = replace_regex$ (.filespec$, "\*", "",1)	; remove wildcard '*' (Kleene star)
         @fname_parts: .filespec$
         .fname$ = result1$
      else       
         select Strings filelist
         .fname$ = Get string: iFile
         ; filename returned by 'Get string' does not include path
      endif
      @process_one_input_file: pmf_t1, pmf_t2, "'indir$''.fname$'"
   endfor
   @cleanup_global
endproc


procedure process_one_input_file: anal_t1, anal_t2, .fullname$
# Also uses global variables <iFile> and <nrofFiles> (which are read by @gr_write_all_prosograms)
   @debug_msg: "process_one_input_file: entry, save_intermediate_data='save_intermediate_data'"
   @initialization_per_file
   @construct_filenames: .fullname$
   @fname_parts: .fullname$
   .fname$ = result2$
   @msg: "Processing input file <'.fname$'>..."

   if (task == task_prosogram and iFile == 1)
      ; delete global report file created in previous run of Prosogram script
      deleteFile: globalfile$
   endif

   if (needs_segm_type)
      if (task == task_segmentation and not batch_mode)
         segmentation_name$ = "asyll"
      endif
      if (task = task_interactive)		; peek segmentation_name$
         @peek_settings
      endif
      if ((task = task_prosogram or task = task_interactive) and segmentation_name$ = "optimal")
	     @select_optimal_segmentation_method
         @msg: "Selected optimal segmentation method: 'segmentation_name$'" 
      endif
      @map_segmentation_type: segmentation_name$
   endif

   if (needs_segm_tg and not fileReadable (segfile$))
      @error_msg: "Cannot find annotation file <'segfile$'>"
      success = 0
      goto file_done
   endif

   if (task == task_validate_tiers)
      @validate_syllable_tier: segfile$
      goto file_done
   elsif (task = task_calc_pitch)
      @calculate_pitch_int
      removeObject: pitchID
      pitch_available = 0
      goto file_done
   elsif (task = task_calc_bp_intensity)
      @calculate_intensity_bp_filtered
      removeObject: intbpID
      intbp_available = 0
      goto file_done
   elsif (task = task_calc_loudness)
      @calculate_loudness
      removeObject: loudnessID
      loudness_available = 0
      goto file_done
   endif

   if (task == task_interactive)	; Read segmentation_name from nucleus file
      @peek_settings
      @gr_start_demowin: basename$
      @gr_printline: "Interactive mode: Processing input file '.fname$'..."
   endif

   @peek_signal
   anal_t1 = max(anal_t1, signal_start)
   if (anal_t2 == 0)
      anal_t2 = signal_finish
   elsif (anal_t2 > 0)
      anal_t2 = min(anal_t2, signal_finish)
   endif
   if (anal_t1 >= signal_finish)
      @fatal_error: "The start time you supplied ('anal_t1:3') is outside the time range of the sound ('signal_start:3' - 'signal_finish:3')"
   endif
   if (anal_t1 >= anal_t2)
      @fatal_error: "The start time you supplied ('anal_t1:3') is greater than the used end time ('anal_t2:3')"
   endif

   if (task == task_prosogram or task == task_interactive)
      if (anal_t1 == signal_start and (abs(anal_t2 - signal_finish) < time_step))
         needs_pitchrange = 1	; will calculate pitchrange (for prosodic profile or for interactive mode)
         ; needs_pitchrange is also set for prosodic profile in task_flags
         ; if (needs_pitchrange = 1) then full signal is analyzed
      endif
   endif

   .minutes = (anal_t2 - anal_t1) div 60
   .sec = (anal_t2 - anal_t1) mod 60
   @msg: "Analysis time range: 'anal_t1:3' - 'anal_t2:3' s ('.minutes' min, '.sec:3' s)"

   if (needs_parameters)
      if (not volatile)
         @gr_printline: "Reading parameter files..."
         @read_parameter_files
      endif
      @gr_printline: "Calculating parameters..."
      @calculate_parameters
   endif

   if (task != task_interactive and signal_available)	; Free memory if possible
      removeObject: soundID
      signal_available = 0
   endif

   if (needs_segm_tg) 
      @read_annotation_file
   endif
   if (segfile_available) ; apply conversion to segmentation textgrid before making textgrid for plotting
      @corpus_conversion: 1, "segmentationID", corpus$
   endif

   if (draw_prosograms)
      @gr_printline: "Preparing TextGrid plotted in prosograms..."
      if (not segfile_available)
         @read_annotation_file
      endif
      @prepare_plotted_textgrid: tiers_to_show$
      @gr_printline: "Preparing TextGrid plotted in prosograms... Ready"
   endif

   if (needs_segm_tg)	; apply conversion to segmentation textgrid after making textgrid for plotting
      @corpus_conversion: 2, "segmentationID", corpus$
   endif

   if (needs_stylization or task == task_segmentation)
      ; if (polytonia_annotation or needs_pitchrange)	; analyse entire signal
      if (needs_pitchrange)								; analyse entire signal
         t1s = signal_start
         t2s = signal_finish
      else												; analyse only part of the speech signal
         t1s = anal_t1
         t2s = anal_t2
      endif
      if (not task == task_interactive)
         @gr_printline: "Segmentation into syllabic nuclei. Method='segmentation_name$', Time range='t1s:3'-'t2s:3'"
      endif
      @get_segmentation: segm_type
      ; also does @safe_nuclei
   endif

   if (task == task_segmentation)					; post-processing segmentation results and write to file
      selectObject: nucleiID
      .tmp = Extract tier: nucleus_tier
      .tmpN = Into TextGrid
      removeObject: .tmp
      selectObject: nucleiID
      .tmp = Extract tier: syllable_tier
      .tmpS = Into TextGrid
      removeObject: .tmp
      if (segm_type == segm_pitchchange or segm_type == segm_pitchterrace) ; only keep syllable tier and name it "segm"
         selectObject: .tmpS
         Set tier name: 1, "segm"
         .tmp = .tmpS
         removeObject: .tmpN
      else			; segm_asyll, segm_specsim ... ; keep nucleus tier and syllable tier
         selectObject: .tmpN, .tmpS
         .tmp = Merge
         @nucleus_tier_postproc: .tmp, 1, 2
         if (batch_mode)
            if (trimleft > 0 or trimright > 0)
               @tier_trim_nucleus: .tmp, 1, trimleft, trimright
            endif
         endif
         removeObject: .tmpN, .tmpS
      endif
      selectObject: .tmp
      Write to text file: autosegfile$
      @msg: "Segmentation written to <'autosegfile$'>"
      removeObject: .tmp
      if (batch_mode and save_BP)
         selectObject: intbpID
         Write to text file: intbpfile$
         @msg: "Intensity of BP-filtered signal written to <'intbpfile$'>"
      endif
      goto file_done
   endif

   if (needs_stylization)
      if (nrof_nuclei_analysed < 1)
         @error_msg: "No syllabic nuclei found in speech signal."
         @error_msg: "If you are using segmentation from annotation tier (phon... or syll...), check tier name and tier content."
         if (save_intermediate_data)
            selectObject: nucleiID
            Write to text file: nuclfile$
         endif
         success = 0
         goto file_done
      endif
      @create_table_of_nuclei
   endif

   if (task == task_interactive and reuse_nucl)
    ; stylization cannot be used by boundary_annotation, because procedure stylize computes data in nucldatID
      @load_intermediate_data_files
   endif

   if (needs_stylization)
      @initialize_nucldat: t1s, t2s

      if (task <> task_interactive)
         ; hesitations are used in duration data, in Polytonia, in detection of boundaries and prominence
         @msg: "Detecting hesitations (method='hesitation_method$')..."
         @detect_hesitations: anal_t1, anal_t2, hesitation_method$
         if (hesitation_annotation)
            selectObject: nucleiID
            .tmpID = Extract one tier: hesitation_tier
            Write to text file: hesitfile$
            @msg: "Hesitation annotation saved to file... 'hesitfile$'"
            removeObject: .tmpID
         endif
      endif

      @debug_msg: "process_one_input_file: needs_stylization"
      if (not stylization_available)
         # Create pitch tier object for stylization
         if (adaptive_glissando)
            s$ = "G(adapt)='glissando_low'-'glissando'/T^2"
         else
            s$ = "G='glissando'/T^2"
         endif
         s$ = "'segmentation_name$', " + s$ + ", DG='diffgt', dmin='mindur_ts:3'"
         @gr_printline: "Calculating stylization... ('s$')"
         stylID = Create PitchTier: "stylization", signal_start, signal_finish
         @stylize_nuclei: t1s, t2s
         stylization_available = 1
         @gr_printline: "Calculating stylization... Ready"
      endif
	  ; next line comes after stylize_nuclei, because it uses values in nucldatID

      ; @debug_msg: "process_one_input_file: profile_available='profile_available' needs_pitchrange='needs_pitchrange'"
      if (not profile_available)		; in interactive mode profile may have been read from file
         @speaker_info_get
         @profile_table_create
         @profile_table_prepare
         profile_available = 1
         ; Table is available, but needs to be filled by @pitchrange_speakers
         if (needs_pitchrange)
            ;if (variableExists ("corpus$") and corpus$ = "Proust" and variableExists ("corpus_speaker_range_1$"))
            ;   @msg: "Reading pitch range from corpus_speaker_range..."
            ;   speaker_range_1$ = corpus_speaker_range_1$
            @gr_printline: "Calculating pitch range of each speaker..."
            @pitchrange_speakers
            ; pitchrange and other values stored in prosodic profile table
            if (task <> task_interactive)
               @msg: "Calculating pitch range normalized pitch..."
               @pitchrange_normalized_pitch
            endif
         endif
      endif

      if (needs_prosodic_profile and (task <> task_interactive) and (not polytonia_annotation))
         ; Write prosodic profile report file
         @prosodic_profile
         @msg: "Writing prosodic profile of current input file to: 'profile_file$'"
         selectObject: profileID
         Save as headerless spreadsheet file: profile_file$
         @prosodic_profile_new
         @msg: "Writing global prosodic profile of all input files to: 'globalfile$'"
         @update_global_report
      endif

      if (calc_prominence)
         @msg: "Calculating prominence..."
         @calculate_prominence_measures
      endif
      if (show_pseudosyllables and draw_prosograms)
        @grid_append_tier: nucleiID, syllable_tier, "newgridID"
      endif

      if (polytonia_annotation)
         @msg: "Tonal annotation... (hesitation_method='hesitation_method$')"
         @polytonia_main: anal_t1, anal_t2, hesitation_method$
         if (draw_prosograms)
            @grid_append_tier: nucleiID, polytonia_tier, "newgridID"
            if (not segfile_available)
               selectObject: newgridID
               Remove tier: 1	; dummy tier
            endif
         endif
         selectObject: nucleiID
         .tmpID = Extract one tier: polytonia_tier
         Write to text file: polytonia_file$
         @msg: "Tonal annotation saved to file... 'polytonia_file$'"
         removeObject: .tmpID
      endif

      if (boundary_annotation)
         @msg: "Boundary annotation..."
         show_prominence = 0		; plot prominence measures
         show_lengthening = 1		; plot lengthening
         boundary_annotation_verbose = 1
         save_intermediate_data = 1
         needs_prosodic_profile = 0
         if (not (segm_type == segm_msyllvow or segm_type == segm_mrhyme or segm_type == segm_msyllpeak))
            @error_msg: "Boundary annotation requires Syllabic segmentation method" 
            segm_type = segm_msyllvow
         endif
         if (pause_method$ == "syll_tier")
            boundary_use_nuclei = 0	; use pauses from annotation
         elsif (pause_method$ == "nuclei"
            boundary_use_nuclei = 1	; use gaps between nuclei 
         endif
         @msg: "Calculating boundaries (boundary_use_nuclei='boundary_use_nuclei')..."
         boundary_skip_hesit = 0 
         @boundary_analysis: anal_t1, anal_t2, boundary_use_nuclei, boundary_skip_hesit
         selectObject: nucleiID
         tmpID = Extract one tier: boundary_tier
         Write to text file: boundaryfile$
         if (draw_prosograms)
            @grid_append_tier: nucleiID, boundary_tier, "newgridID"
            @grid_append_tier: nucleiID, boundary_tier, "segmentationID"
         endif
         @boundary_pass2: anal_t1, anal_t2
         if (draw_prosograms)
            @grid_append_tier: nucleiID, boundary2_tier, "newgridID"
         endif
         call tier_get segmentationID "^boundary-manu$" ref_tier "Cannot find boundary reference tier in segmentation grid" 0
         call create_evaluation_data segmentationID ref_tier 'evalfile$' 
         @msg: "Boundaries ready"
      endif

      if (stress_annotation)
         @stress_analysis: anal_t1, anal_t2, pause_use_nuclei
         @grid_append_tier: nucleiID, stress_tier, "newgridID"
      endif

      if (save_intermediate_data)
         if (not reuse_nucl)
            selectObject: nucleiID
            Write to text file: nuclfile$
         endif
         if (not reuse_styl)
            selectObject: stylID
            Write to text file: stylfile$
         endif
         if (rich_format_stylization)
            @store_stylization: stylID, nucleiID, targetsfile$
         endif
         ; call store_stylization_resampled stylID nucleiID time_step 'stylpitchfile$'
         ;.outfname$ = prefix$ + "_dur.txt"		; uses basename
         ;.outfname$ = indir$ + "_dur_syllprom.txt"		; uses single output file
         ;call duration_data_from_textgrid segmentationID iFile "syllprom" "'.outfname$'" 'basename$'
      endif

      selectObject: stylID	; stylization pitch tier in Hz
      stylSTID = Copy: "styl_ST"
      @convert_Hz_ST: stylSTID

    # Initialization for plotting results of current input file
      if (draw_prosograms and auto_pitchrange)
         ; GLOBAL automatic pitch range selection for entire corpus to be analysed
         @speaker_autorange: anal_t1, anal_t2
         ySTmax = ymax
         ySTmin = ymin
      endif
   else ; no stylization needed
       if (pitch_available)
          selectObject: pitchID
          .y = Get mean: 0, 0, "semitones re 1 Hz"
          ySTmin = .y - 12
          ySTmax = .y + 12
       endif
   endif ; needs_stylization

# Draw all prosograms for current input file
   if (draw_prosograms)
      if (task == task_interactive)
         @gr_start_demowin: basename$
         @gr_run_demowin: anal_t1, anal_t2, timeincr, ySTmin, ySTmax
         @cleanup_current_file
         @cleanup_global
         exit
      else
         @msg: "Drawing and saving all prosograms..." 
         @gr_start_picturewin
         @gr_write_all_prosograms: anal_t1, anal_t2, timeincr
      endif
   endif ; draw_prosograms

   if (task == task_annotation)
      @gr_start_picturewin
      if (nrof_pages == 0)
         @gr_first_viewport_of_page
         nrof_pages = 1
         @gr_write_all_annotation: anal_t1, anal_t2, timeincr
      endif
   endif

   if (save_intermediate_data)		; only for some tasks
      ; this step must follow plotting, because store_features removes columns
         if (not collect_output)
            @store_features: 1, 0, sheetfile$
            if (long_feature_table)
               @store_features: 1, 1, tablefile$
            endif
         else
            @store_features: iFile, 0, outputfname$
         endif
   endif

   label file_done

   # Delete temporary objects for current input file
        @cleanup_current_file
endproc


procedure select_optimal_segmentation_method
   if (not fileReadable (segfile$))
      segmentation_name$ = "asyll"
   else
      @debug_msg: "select_optimal_segmentation_method: task='task', segmentation file found"
      .tmpID = Read from file: segfile$
      @tier_number_by_name: .tmpID, "^phon"
      .phon_tier = result
      @tier_number_by_name: .tmpID, "^syll"
      .syll_tier = result
      removeObject: .tmpID
      @debug_msg: "select_optimal_segmentation_method: phon_tier='.phon_tier', syll_tier='.syll_tier'"
      if (.phon_tier and .syll_tier)
         segmentation_name$ = "rhyme"
      elsif (.phon_tier)
         segmentation_name$ = "vow-nucl"
      elsif (.syll_tier)
         segmentation_name$ = "syll"
      else
         segmentation_name$ = "asyll"
      endif
   endif
endproc


procedure map_segmentation_type: .method$
# Maps segmentation name to a type and sets some flags.
   @debug_msg: "map_segmentation_type: entry, method='.method$'"
   needs_segm_tg = 1		; selected segmentation type needs segmentation TextGrid
   needs_phon_tier = 0		; selected segmentation type needs phoneme tier
   needs_syll_tier = 0		; selected segmentation type needs syllable tier
   segmentation_name$ = .method$
   if (.method$ = "vow-nucl")
      segm_type = segm_vnucl
      needs_phon_tier = 1
   elsif (.method$ = "extern")
      segm_type = segm_extern
   elsif (.method$ = "loudness")		; automatic, loudness peaks
      segm_type = segm_aloudness
      needs_segm_tg = 0
   elsif (.method$ = "int-BP")	; automatic, peaks in bandpass filters speech
      segm_type = segm_anucl
      needs_intbp = 1
      needs_segm_tg = 0
   elsif (.method$ = "asyll")
      segm_type = segm_asyll
      needs_intbp = 1
      needs_segm_tg = 0
   elsif (.method$ = "rhyme")
      segm_type = segm_mrhyme
      needs_phon_tier = 1
      needs_syll_tier = 1
   elsif (.method$ = "syll+vow")
      segm_type = segm_msyllvow
      needs_phon_tier = 1
      needs_syll_tier = 1
   elsif (.method$ = "syll")
      segm_type = segm_msyllpeak
      needs_syll_tier = 1
   elsif (.method$ = "voiced")
      segm_type = segm_voiced
      needs_segm_tg = 0
   elsif (.method$ = "specsim")
      segm_type = segm_specsim
      needs_segm_tg = 0
   elsif (.method$ = "pitchchange")
      segm_type = segm_pitchchange
      needs_segm_tg = 0
      needs_intbp = 1
   elsif (.method$ = "pitchterrace")
      segm_type = segm_pitchterrace
      needs_segm_tg = 0
   else
      segmentation_name$ = "unknown"
      @fatal_error: "Unknown segmentation type: '.method$'"
   endif
   if (boundary_annotation)
      @debug_msg: "map_segmentation_type: boundary_annotation, segmentation name='segmentation_name$' ('segm_type')"
         if (not (segm_type == segm_msyllvow or segm_type == segm_mrhyme or segm_type == segm_msyllpeak))
            @error_msg: "Boundary annotation requires Syllabic segmentation method" 
            segm_type = segm_msyllvow
         endif
   endif
   @debug_msg: "map_segmentation_type: exit"
endproc


procedure task_flags
# set some flags depending on task type
   @debug_msg: "task_flags: entry"
   needs_parameters = 1
   needs_stylization = 1
   needs_loudness = 0
   needs_intbp = 0
   needs_pitchrange = 0
   needs_prosodic_profile = 0
   needs_segm_tg = 0
   needs_segm_type = 0
   needs_picture_win = 0
   nrofplottedtiers = 0
   show_settings = 1
   show_portee = 1
   show_tg_bound = 1		; plot vertical lines for interval boundaries in TextGrid
   show_y_scale = 1			; plot calibration in ST on vertical axis
   show_y_scale_r = 1		; plot calibration in Hz on vertical axis at right side
   show_x_scale = 1
   show_vuv = 1				; plot V/UV parameter
   show_intensity = 1		; plot intensity parameter
   show_intbp = 1			; plot int-BP parameter
   show_lengthening = 0		; don't plot lengthening
   show_tiernames = 1		; plot tier names to the right of tier
   show_trajectories = 0	; show intrasyllab up/down and intersyllab pitch intervals
   if (task == task_calc_pitch or task == task_calc_bp_intensity or task == task_calc_loudness)
      needs_parameters = 0
      volatile = 0
      needs_stylization = 0
      draw_prosograms = 0
      if (task == task_calc_loudness)
         needs_loudness = 1
      endif
   elsif (task == task_annotation)
      needs_parameters = 0
      volatile = 0
      needs_stylization = 0
      needs_picture_win = 1
      draw_prosograms = 0
      show_y_scale = 0
   elsif (task == task_prosogram)
      needs_segm_type = 1			; may be modified later by procedure: map_segmentation_type
      if (calc_prominence)
         needs_loudness = 1
      endif
      needs_picture_win = 1
      if (viewsize$ = "compact")
         show_tiernames = 0
      endif
      if (save_intermediate_data)
         needs_prosodic_profile = 1
         needs_pitchrange = 1
      endif
      if (polytonia_annotation)
         needs_prosodic_profile = 1	; because pitch range information is stored in profileID !!
         needs_pitchrange = 1
         show_hesitations = 1
      endif
      show_prominence = 0			; plot prominence measures
      if (boundary_annotation)
         calc_prominence = 1
         show_lengthening = 1		; plot lengthening
         boundary_annotation_verbose = 1
         save_intermediate_data = 1
      endif
      if (stress_annotation)
         calc_prominence = 1
         show_prominence = 1		; plot prominence measures 
         pause_use_nuclei = 1		; 1= use gaps between nuclei; 0= use pauses from annotation
      endif
      if (hesitation_annotation)
         needs_segm_tg = 1
         needs_pitchrange = 0
         needs_picture_win = 0
      endif
   elsif (task == task_pitch_plot)
      needs_stylization = 0
      segmentation_name$ = ""
      needs_picture_win = 1
      show_settings = 0
      show_vuv = 0
      ; show_intensity = 0
      show_intbp = 0
      rich = 1			; because pitch is plotted in rich mode
      show_pauses = 0
      nrof_nuclei_analysed = 0
   elsif (task == task_interactive)
      needs_segm_type = 1
      show_settings = 0
      viewsize$ = "wide"
      rich = 0
   elsif (task == task_segmentation)
      needs_segm_type = 1
      needs_stylization = 0
      draw_prosograms = 0
      if (not variableExists ("batch_mode"))
         segm_type = segm_asyll
      endif
   elsif (task == task_validate_tiers)
      needs_segm_tg = 1
      needs_parameters = 0
      needs_stylization = 0
      draw_prosograms = 0
   endif
   if (variableExists("rich"))
      if (not rich)
         show_tg_bound = 0
      endif
   endif
endproc


procedure process_form
; Interpret all fields from script form and set global flag variables accordingly
   @debug_msg: "process_form: entry"
   needs_segm_type = 0		; true if task requires a segmentation type
   if (index (task$, "Recalculate pitch"))
      task = task_calc_pitch
   elsif (index (task$, "Recalculate intensity"))
      task = task_calc_bp_intensity
   elsif (index (task$, "Recalculate loudness"))
      task = task_calc_loudness
   elsif (index (task$, "Prosogram") or index (task$, "intermediate data files") or index (task$, "Polytonia"))
      task = task_prosogram
      needs_segm_type = 1		; may be modified later by procedure: map_segmentation_type
      draw_prosograms = 0
      save_intermediate_data = 0
      if (index (task$, "Prosogram"))
         draw_prosograms = 1
      endif
      if (index (task$, "intermediate data files"))
         save_intermediate_data = 1
      endif
      if (index (task$, "Polytonia"))
         polytonia_annotation = 1
      endif
   elsif (index (task$, "Plot pitch"))
      task = task_pitch_plot
   elsif (index (task$, "automatic segmentation"))
      task = task_segmentation
      segm_type = segm_asyll
      segmentation_name$ = "asyll"
      needs_segm_type = 1
   elsif (index (task$, "Interactive"))
      task = task_interactive
      needs_segm_type = 1
   elsif (index (task$, "Draw annotation"))
      task = task_annotation
   elsif (index (task$, "boundary"))
      task = task_autoanno
      boundary_annotation = 1
      needs_segm_type = 1
   elsif (index (task$, "Validate"))
      task = task_validate_tiers
   else
      @fatal_error: "Invalid task"
   endif
   ; @debug_msg: "process_form: save_intermediate_data='save_intermediate_data'"

; Map segmentation type descriptions in form to names that will also be displayed in prosogram graph.
   if (needs_segm_type)
      .method$ = segmentation_method$
      if (index (.method$, "optimal"))
         segmentation_name$ = "optimal"
      elsif (index (.method$, "in vowels"))
         segmentation_name$ = "vow-nucl"
      elsif (index (.method$, "external"))
         segmentation_name$ = "extern"
      elsif (index (.method$, "loudness"))			; automatic, loudness peaks
         segmentation_name$ = "loudness"
      elsif (index (.method$, "BP-filtered"))		; automatic, peaks in bandpass filters speech
         segmentation_name$ = "int-BP"
      elsif (index (.method$, "Automatic: acoustic syllables"))
         segmentation_name$ = "asyll"
      elsif (index (.method$, "in rhyme"))
         segmentation_name$ = "rhyme"
      elsif (index_regex (.method$, "in syllables.*and vowels"))
         segmentation_name$ = "syll+vow"
      elsif (index_regex (.method$, "in syllables.*and local peak"))
         segmentation_name$ = "syll"
      elsif (index (.method$, "Automatic: voiced portions"))
         segmentation_name$ = "voiced"
      else
         @fatal_error: "Unknown segmentation type: '.method$'"
      endif
   endif

; time_step (frame rate) used for calculation of intensity and pitch
   time_step = 'frame_period$'
   anal_t1 = left_Time_range
   anal_t2 = right_Time_range
; View
   viewsize$ = "wide"
   if (index (view$, "Compact"))
      viewsize$ = "compact"
   elsif (index (view$, "Large"))
      viewsize$ = "large"
   endif
   rich = 0
   if (index (view$, "rich"))
      rich = 1
   endif
   draw_pitch_target_values = 0				; Draw pitch target values (in ST) in prosogram
   if (index (view$, "pitch targets"))
      draw_pitch_target_values = 1
      if (index (view$, "pitch targets in Hertz"))
         draw_pitch_target_values = 2
      endif
   endif
   show_pitchrange = 0						; Show pitch range
   if (index (view$, "pitch range"))
      show_pitchrange = 1
   endif
; Thresholds
   j = index_regex (thresholds$, "/T")
   s$ = mid$ (thresholds$, 3, j-3)		; s$ = glissando threshold(s)
   if (index (thresholds$, "adaptive"))		; adaptive glissando threshold (lower before pause)
      adaptive_glissando = 1
      s2$ = left$ (s$, 4)
      glissando_low = 's2$'
      s2$ = mid$ (s$, 6, 4)
      glissando = 's2$'
   else						; fixed glissando threshold
      adaptive_glissando = 0
      glissando = 's$'
      glissando_low = glissando
   endif
   diffgt = extractNumber (thresholds$, "DG=")
   mindur_ts = 0.035			; Minimum duration for a tonal segment (default)
   j = index (thresholds$, "dmin")
   if (j > 0)
      s$ = mid$ (thresholds$, j+5, length (thresholds$) -(j+4))
      mindur_ts = 's$'
   endif
   if (not variableExists ("output_mode"))
      output_mode$ = "Fill page with strips"
   endif
   outputmode$ = left$ (output_mode$, index(output_mode$, " ") -1)
   volatile = 0				; by default, process full signal and store results
   if (index (parameter_calculation$, "Partial") > 0)
      volatile = 1
   endif
   scale_signal_amplitude = 0
   if (variableExists ("scale_signal_intensity"))	; Form contains field "Scale signal intensity"
      if (index (scale_signal_intensity$, "Scale"))
         scale_signal_amplitude = 1
      endif
   endif
   clip_level = 0
   if (variableExists ("clipping_threshold"))		; Form contains field "Clipping threshold"
      if (clipping_threshold > 100 or clipping_threshold < 0)
         @fatal_error: "Invalid value for clipping threshold"
      endif
      clip_level = clipping_threshold
   endif

   minimum_pitch = left_F0_detection_range
   maximum_pitch = right_F0_detection_range
   if (minimum_pitch = 0)
      ; autorange_f0detect = 1
   else
      if (minimum_pitch < 40 or maximum_pitch > 800)
         @fatal_error: "Invalid F0 range: expected to be within 40 - 800 Hz range"
      endif
      if (minimum_pitch >= maximum_pitch)
         @fatal_error: "Invalid F0 range: lower limit > higher limit"
      endif
   endif
   if (left_Pitch_range == 0)
      auto_pitchrange = 1		; automatic pitch range adjustment in plot (not in F0 detection)
   else
      auto_pitchrange = 0		; manual pitch range adjustment for plot
      if (right_Pitch_range <= left_Pitch_range or left_Pitch_range <= 0)
         @fatal_error: "Invalid values for pitch range"
      endif
   endif
   ySTmin = left_Pitch_range
   ySTmax = right_Pitch_range

   timeincr = time_interval_per_strip	; time increment for plot

   @task_flags

   if (draw_prosograms and (index (output_format$,"EMF")) and not windows)
      @error_msg: "Windows Metafiles (EMF) are supported on Windows systems only."
   endif
   if (draw_prosograms and (index (output_format$,"JPG")))
      if (not fileReadable (path_ghostscript$))
         @error_msg: "JPG output requires Ghostscript, which is not found. Verify configuration for Ghostcript."
      endif
   endif

   s$ = input_files$
   s$ = replace$ (s$, " ", "", 0)
   if (length(s$) < 1)
      input_files$ = chooseReadFile$ ("Select file")
   endif
   @debug_msg: "process_form: exit"
endproc


procedure initialization_multiple_files
   @debug_msg: "initialization_multiple_files: entry"
   # Unit (ST or Hz) used for expressing shown pitch range
      units$ = "Semitones"
   ySTmin = 0
   ySTmax = 100
   # diffST = difference in ST between ST-scale relative to 100Hz and that rel to 1Hz
      .diffST = 12 * log2(100/1)
   if (units$ = "Semitones")
      yHzmin = semitonesToHertz(ySTmin-.diffST)
      yHzmax = semitonesToHertz(ySTmax-.diffST)
   else
      ySTmin = hertzToSemitones(yHzmin) - hertzToSemitones(1)
      ySTmax = hertzToSemitones(yHzmax) - hertzToSemitones(1)
   endif
   fc_low = 300		; BP filter
   fc_high = 3500	; BP filter
   mindiff = 3		; intensity difference threshold for local dips in convex hull
   diff_left = 2	; intensity difference between local peak and left boundary of nucleus
   @debug_msg: "initialization_multiple_files: exit"
endproc


procedure initialization_per_file
   @debug_msg: "initialization_per_file: entry"
   signal_available = 0			; speech signal loaded
   intensity_available = 0
   pitch_available = 0
   harmonicity_available = 0
   intbp_available = 0
   inthp_available = 0
   nuclei_available = 0
   nucldat_available = 0
   loudness_available = 0
   segfile_available = 0		; annotation file with alignements of phonemes, syllables, etc.
   segmentation_available = 0	; 1 if segmentation has been read from saved <basename>_nucl.TextGrid file
   stylization_available = 0	; 1 if stylization has been read from saved <basename>_styl.PitchTier file
   profile_available = 0		; 1 if profile data has been read from saved <basename>_profile_data.txt
   reuse_nucl = 0				; 1 if nucleus file could be read and used as segmentation
   reuse_styl = 0
   phones_available = 0			; 1 if phoneme tier is found in annotation TextGrid
   syllables_available = 0		; 1 if syllable tier is found in annotation TextGrid
   words_available = 0			; 1 if word tier is found in annotation TextGrid
   creak_available = 0			; creak textgrid/tier found and read ?
   speaker_available = 0		; speaker textgrid/tier found and read ?
   hesitation_available = 0		; hesitation textgrid/tier found and read ?
   newgrid_available = 0
   nrof_nuclei_analysed = 0
   success = 1				; 0 if error encountered while processing input file
endproc


procedure construct_filenames: .infname$
# Construct filenames to be used for output, parameter and intermediate data files, 
# starting from filename of sound input file
# <.infname$>	full path of input file, including filename extension
   @debug_msg: "construct_filenames: entry, infname=<'.infname$'>"
   if (rindex (.infname$,".") == 0)
      @fatal_error: "Invalid filename for input file. Should include filename extension."
   endif
   @fname_parts: .infname$
   .fext$ = replace_regex$ (result3$, "(.)", "\L\1", 0)	; get filename extension and convert it to lowercase
   if (task <> task_validate_tiers)
      if (index(filename_extensions_sound$, ":'.fext$':") == 0)
         @debug_msg: "construct_filenames: filename extension=<'.fext$'>"
         @fatal_error: "Input file ('.infname$') should be an audio file supported by Praat Open LongSound, with extension .wav, .aiff, .aifc, .nist, .sound, .flac, or .mp3"
      endif
   endif
   basename$    = result2$
   indir$       = result4$
   .path$      = indir$ + basename$
   signalfile$  = .path$ + "." + .fext$
   pitchfile$   = .path$ + ".Pitch"
   segfile$		= .path$ + ".TextGrid"				; TextGrid with already available segmentation (phonetic and/or syllabic alignment)
   globalfile$	= indir$  + "globalsheet.txt"		; speaker profile data, for all input speech files in run (headerless spreadsheet)

   if (batch_mode and length (corpus$))				; in batch mode, file paths may be defined for a given corpus
      ; "corpus_subdir_sound$" is handled by batch commands
      ; "corpus_subdir_img$" is handled below
      if (variableExists ("corpus_subdir_pitch$"))
         pitchfile$	= corpus_home$ + corpus_subdir_pitch$ + basename$ + ".Pitch"
      endif
      if (variableExists ("corpus_subdir_tg$"))
         segfile$		= corpus_home$ + corpus_subdir_tg$ + basename$ + ".TextGrid"
      endif
      if (variableExists ("corpus_subdir_data$"))
         .path$ = corpus_home$ + corpus_subdir_data$ + basename$
         globalfile$ = corpus_home$ + corpus_subdir_data$ + "globalsheet.txt"	; no basename here
      endif
   endif

   intensityfile$ = .path$ + ".Intensity"
   intbpfile$	  = .path$ + "_BP.Intensity"
   inthpfile$	  = .path$ + "_HP.Intensity"
   loudnessfile$  = .path$ + "_loud.Intensity"
   creakfile$	  = .path$ + "_creak.TextGrid"		; TextGrid file with creak annotation
   speakerfile$	  = .path$ + "_speaker.TextGrid"	; TextGrid file with speaker annotation
   nuclfile$	  = .path$ + "_nucl.TextGrid"		; nuclfile contains automatic segmentation etc.
   stylfile$	  = .path$ + "_styl.PitchTier"		; stylization as a sequence of targets, in Praat's PitchTier format
   stylpitchfile$ = .path$ + "_styl.Pitch"			; stylization as pitch samples
   targetsfile$	  = .path$ + "_styl.txt"			; stylization as a sequence of targets and segmentations format 
   statsfile$	  = .path$ + "_profile.txt"			; prosodic profile report in text format
   sheetfile$	  = .path$ + "_data.txt"			; syllable data (format: headerless spreadsheet file)
   tablefile$     = .path$ + "_table.txt"			; long format syllabic features, adding categorical variables for statistic analysis
   profile_file$  = .path$ + "_profile_data.txt"	; prosodic profile data, for current input speech file (headerless spreadsheet file)
   autosegfile$   = .path$ + "_auto.TextGrid"		; TextGrid file with output from automatic segmentation into syllables
   polytonia_file$ = .path$ + "_polytonia.TextGrid"	; TextGrid file with output from Polytonia
   boundaryfile$  = .path$ + "_boundary.TextGrid"	; TextGrid file with detected prosodic boundaries
   hesitfile$	  = .path$ + "_hesit.TextGrid"		; TextGrid file with detected hesitations
   evalfile$	  = .path$ + "_eval.txt"
   harmonicityfile$ = .path$ + ".Harmonicity"


   ; Special handling of output filename in batch mode
   ; The variable <output_filename$> is set in script form or in batch command "prosogram_variants"
   output_fname$ = output_filename$	; make local copy for modifications
   if (batch_mode and (task == task_prosogram or task == task_segmentation))
      if (length (output_filename$))
         ; filename explicitly given, keep it as such; does not use corpus_home nor corpus_subdir_img
         file_numbering = 0
      elsif (length (corpus$))
         if (variableExists ("corpus_subdir_img$"))
            output_fname$ = corpus_home$ + corpus_subdir_img$ + basename$ + "_" + output_suffix$
         else
            output_fname$ = corpus_home$ + basename$ + "_" + output_suffix$
         endif
      else
         output_fname$ = indir$ + basename$ + "_" + output_suffix$
      endif
      if (task == task_prosogram and draw_prosograms)
         @msg: "Graphics file will be written to (path+basename): <'output_fname$'>"
      endif
      @debug_msg: "construct_filenames: batch_mode, output_fname=<'output_fname$'>"
      if (task == task_segmentation)
         autosegfile$ = corpus_home$ + basename$ + output_suffix$ + ".TextGrid"
      endif
   endif
   if (not batch_mode)		; script mode
      if (length (indir$))
         output_fname$ = replace$ (output_fname$, "<input_directory>", indir$, 1)
      else
         output_fname$ = replace$ (output_fname$, "<input_directory>/", "", 1)
      endif
      output_fname$ = replace$ (output_fname$, "<basename>", basename$, 1)
      output_fname$ = replace$ (output_fname$, "//", "/", 1)
      if (single_fname_graphics_output)	; all graphics filenames have same basename followed by number
         output_filename$ = output_fname$	; keep name for all graphics output files
      else
         file_ctr = 1	; counter for graphics filename
         nrof_pages = 0
      endif
      @debug_msg: "construct_filenames: script_mode, output_fname=<'output_fname$'>"
   endif

   @debug_msg: "construct_filenames: exit, output_fname=<'output_fname$'>"
endproc


procedure read_annotation_file
   @debug_msg: "read_annotation_file: entry"
   segfile_available = 0
   if (fileReadable (segfile$))
      segfile_available = 1
   else
      ; @msg: "read_annotation_file: Cannot open TextGrid file with segmentation: <'segfile$'>"
   endif
   if ((not segfile_available) and needs_segm_tg)
      @fatal_error: "Cannot open TextGrid with segmentation from file <'segfile$'>"
   endif
   if (segfile_available)
      @gr_printline: "Loading annotation TextGrid from file <'segfile$'>"
      segmentationID = Read from file: segfile$
      Rename: "annotation_input"
   endif
   @debug_msg: "read_annotation_file: exit"
endproc


procedure corpus_conversion: .step, .gridname$, .corpus$
# In-place conversion of a TextGrid object
# <.step>	1 = before preparation of plotted tiers, 2 = after preparation
   @debug_msg: "corpus_conversion: entry, gridname=<'.gridname$'>"
   .grid = '.gridname$'
   selectObject: .grid

   if (.corpus$ = "Portes" and .step = 1)
      @tier_number_by_name: .grid, "speaker"
      if (not result)
         @tier_number_by_name: .grid, "turns"
         if (result)
            Duplicate tier: result, 1, "speaker"
            Replace interval texts: 1, 1, 0, "([a-zA-Z]*)[0-9]*", "\1"
         endif
      endif
   endif

   if (.corpus$ = "rhapsodie" and .step = 1)
      @tier_number_by_name: .grid, "locuteur"
      if (result)
         Set tier name: result, "speaker"
      endif
   endif

   if (.corpus$ = "cprom" and .step = 1)
      @process_cprom: .grid
   endif

   if (.corpus$ = "gvlex" and .step = 1)
      @lpa_sampa: .grid, corpus_tier_phon$
      @lpa_sampa: .grid, corpus_tier_syll$
   endif

   if (.corpus$ = "Kotsifas_2015" and .step = 1)
      @tier_number_by_name: .grid, "Intonation"
      if (result)
         Set tier name: result, "intonation"
      endif
      @tier_number_by_name: .grid, "phon"
      if (result)
         .phon_tier = result
         .ni = Get number of intervals: .phon_tier
         for .j to .ni
            .t1s = Get start point: .phon_tier, .j
            .label$ = Get label of interval: .phon_tier, .j
            .pos = index (.label$, """")	; position of SAMPA primary stress mark " (double quote)
            if (.pos > 0 and .pos <> 1)		; bad position
              .s$ = replace_regex$ (.label$, """", "", 0)
              Set interval text: .phon_tier, .j, .s$
              @msg "Error: interval in phon tier at '.t1s:4' contains stress in wrong position. (label= '.label$')"
            endif
         endfor
      endif
   endif

   if (.corpus$ = "RUHRCAT" and .step = 2)
      @tier_number_by_name: .grid, "phon"
      if (result)
         Replace interval text... result 0 0 "^j([aOoueE@])" "i\1" Regular Expressions
         Replace interval text... result 0 0 "^w([o0iaeE])" "u\1" Regular Expressions
         Replace interval text... result 0 0 "^w\\ct" "uO" Regular Expressions
         Replace interval text... result 0 0 "^j\\ct" "iO" Regular Expressions
         Replace interval text... result 0 0 "^j\\ef" "iE" Regular Expressions
         Replace interval text... result 0 0 "(\\:f)*" "" Regular Expressions
         Replace interval text... result 0 0 "^mmm$" "m=" Regular Expressions
      endif
      @tier_number_by_name: .grid, "syll"
      .syll_tier = result
      if (result)
         Replace interval text... result 0 0 "^mmm$" "m=" Regular Expressions
      endif
  ; Create prominence and hesitation tiers from "DM+Hes" tier
      @tier_number_by_name: .grid, "DM.Hes"
      if (result)
         .deliv_tier = result
         .nt = Get number of tiers
         .hes_tier = .nt + 1
         Duplicate tier: .syll_tier, .hes_tier, "hes"
         .ni = Get number of intervals: .syll_tier
         for .j to .ni
            .t1s = Get start time of interval: .syll_tier, .j
            .t2s = Get end time of interval: .syll_tier, .j
            @interval_from_time: .grid, .deliv_tier, .t1s+(.t2s-.t1s)/2, "interv"
            .label$ = Get label of interval: .deliv_tier, interv
            if (index (.label$, "Hes"))
              .s$ = "H"
            else
              .s$ = ""
            endif
            Set interval text: .hes_tier, .j, .s$
         endfor
      endif
   endif ; (.corpus$ = "RUHRCAT" and .step = 2)
   @debug_msg: "corpus_conversion: exit"
endproc


procedure prepare_plotted_textgrid: ltiers$
# Prepare textgrid for plot using tiers  
   @debug_msg: "prepare_plotted_textgrid: entry"
   if (index(ltiers$,""""))
      @fatal_error: "Invalid character (double quote) in field <Tiers to show>. Please check content of this field."
   endif
   nrofplottedtiers = 0
   if (segfile_available == 0 and (polytonia_annotation or boundary_annotation))
      newgridID = Create TextGrid: signal_start, signal_finish, "dummy"
      newgrid_available = 1
   endif
   if (segfile_available)
      selectObject: segmentationID
      tiers = 0
      nrofTiers = Get number of tiers
      @tier_number_by_name: segmentationID, "^[Ss]peaker$"
      speaker_tier_in = result
      repeat
         @next_field: ltiers$
         if (result)			; next field found
            field$ = result2$
            @debug_msg: "prepare_plotted_textgrid: next_field=<'field$'>"
            if (left$ (field$, 1) = "*")
               field$ = right$ (field$, length(field$) - 1)
               convert = 1
            else
               convert = 0
            endif
            @is_number: field$
            if (result == 0)		; next field is not a number
               @tier_number_by_name: segmentationID, field$
               tier_in = result
               if (result == 0)		; tier name not found
                  @error_msg: "No tier named <'field$'> found in input textgrid. Change content of field <Tiers to show>."
                  @error_msg: "This tier will be skipped in prosogram."
               endif
            else			; next field is a number
               tier_in = 'field$'
               if (tier_in > nrofTiers)
                  @error_msg: "Tier 'tier_in' not found. Input textgrid has only 'nrofTiers' tiers. Change content of field <Tiers to show>."
                  @error_msg: "This tier will be skipped in prosogram."
                  tier_in = 0
               endif
            endif
            if (tier_in > 0)		; valid tier to add
               selectObject: segmentationID
               if (tiers == 0)		; this is first tier to add to plotted grid
                  tmpID = Extract tier: tier_in
                  newgridID = Into TextGrid
                  Rename: "plotted_tiers"
                  newgrid_available = 1
                  removeObject: tmpID
               else
                  @grid_append_tier: segmentationID, tier_in, "newgridID"
                  selectObject: newgridID
               endif
               tiers += 1
               if (convert)
                  call convert_sampa_ipa newgridID tiers
               endif
            endif
         endif
         ltiers$ = result3$
      until (result == 0)
      nrofplottedtiers = tiers
      if (nrofplottedtiers > 0)
         @textgrid_disable_textstyle: newgridID
      endif
   endif ; segfile_available
   if (polytonia_annotation or show_pseudosyllables)
      nrofplottedtiers += 1		; results will appear in extra tier
   endif
   if (boundary_annotation)
      nrofplottedtiers += 1		; results will appear in extra tier
   endif
   if (stress_annotation)
      nrofplottedtiers += 1		; results will appear in extra tier
   endif
   @debug_msg: "prepare_plotted_textgrid: exit"
endproc


procedure peek_signal
   @debug_msg: "peek_signal: entry"
   @fname_parts: signalfile$
   .fext$ = replace_regex$ (result3$, "(.)", "\L\1", 0)	; to lowercase
   if (.fext$ = "sound")
      .tmpID = Read from file: signalfile$
   else
      .tmpID = Open long sound file: signalfile$
   endif
   signal_start = Get starting time
   signal_finish = Get finishing time
   removeObject: .tmpID
   @debug_msg: "peek_signal: exit"
endproc


procedure read_signal
# Read signal. Apply clipping if clip > 0. Apply scaling of (scale_signal_amplitude > 0).
   @debug_msg: "read_signal: entry"
   soundID = Read from file: signalfile$
   fullsoundID = soundID
   signal_start = Get starting time
   signal_finish = Get finishing time
   if (clip_level > 0)	; Avoid error related to speech files clipped at max amplitude.
      @msg: "Clipping speech signal at 'clip_level' percent..."
      clip = clip_level/100
      Formula... if (abs(self)>clip) then if self>0 then clip else -clip fi else self fi
   endif
   if (scale_signal_amplitude)
      @msg: "Scaling speech signal at 70 dB..."
      Scale intensity: 70.0
   endif
   signal_available = 1
   @debug_msg: "read_signal: exit"
endproc


procedure read_parameter_files
   @debug_msg: "read_parameter_files: entry"
   if ((not volatile) and fileReadable (pitchfile$))
      pitchID = Read from file: pitchfile$
      .t = Get time step
      if (.t > time_step)
         @msg: "Pitch file on disk has larger time step."
         removeObject: pitchID
      else
         .t2 = Get end time
         if (.t2 + 5*time_step < signal_finish)
            @msg: "Pitch file end time ('.t2' s) < speech signal end time ('signal_finish' s)"
         endif
         pitch_available = 1
         @gr_printline: "Consulted pitch from file <'pitchfile$'> (time step='.t:3')"
      endif
   endif
   if (show_harmonicity)
      if (fileReadable (harmonicityfile$))
         harmonicityID = Read from file: harmonicityfile$
         @msg: "Consulted harmonicity from file <'harmonicityfile$'>"
         harmonicity_available = 1
      endif
   endif
   if (needs_loudness)
      if (fileReadable (loudnessfile$))
         loudnessID = Read from file: loudnessfile$
         @msg: "Consulted loudness from file <'loudnessfile$'>"
         loudness_available = 1
      endif
   endif
   if (needs_intbp and not volatile)
      if (fileReadable (intbpfile$))
         intbpID = Read from file: intbpfile$
         .t = Get time step
         intbp_available = 1
         @gr_printline: "Consulted BP intensity from file <'intbpfile$'> (time step='.t:3')"
      endif
   endif
   @debug_msg: "read_parameter_files: exit"
endproc


procedure calculate_pitch_int
   @debug_msg: "calculate_pitch_int: entry"
   if (not signal_available)
      @read_signal
   endif
; Standard settings, see FAQ Pitch Analysis
   tstep = 0			; automatic, i.e. 0.75/pitch_floor, e.g. for 60Hz, time step = 0.0125 s
   voicing_threshold = 0.45
   silence_threshold = 0.03
   octave_cost = 0.01
   octave_jump_cost = 0.35
   vuv_cost = 0.14
; Modified settings
   tstep = time_step			; typically 0.01 or 0.005 s

; Actual pitch determination, either explicit F0 range or autorange (2 pass, when minimum_pitch = 0)
   .f0min = minimum_pitch	; local variable 
   .f0max = maximum_pitch   ; local variable 
   if (.f0min = 0)			; autorange, pass 1
      @debug_msg: "calculate_pitch_int: pass 1"
      .tstep1 = 0.01		; speed up pass 1
      .f0min = 65
      .f0max = 800
      @from_Hz_to_ST_rel_1: .f0min
      .minST = result
      @from_Hz_to_ST_rel_1: .f0max 
      .maxST = result
      @msg: "Calculating pitch in autorange mode, Pass 1..."
      @msg: ".'tab$'Time step='.tstep1:2', F0 range='.f0min:0'-'.f0max:0' Hz ('.minST:1'-'.maxST:1' ST), Voicing threshold='voicing_threshold:2'"
      selectObject: soundID
      pitchID = To Pitch (ac): .tstep1, .f0min, 15, "no", silence_threshold, voicing_threshold, octave_cost, octave_jump_cost, vuv_cost, .f0max
      .p05 = Get quantile: 0, 0, 0.05, "Hertz"
      .p25 = Get quantile: 0, 0, 0.25, "Hertz"
      .p50 = Get quantile: 0, 0, 0.50, "Hertz"
      .p75 = Get quantile: 0, 0, 0.75, "Hertz"
      .p95 = Get quantile: 0, 0, 0.95, "Hertz"
      removeObject: pitchID
      @msg: ".'tab$'Pitch percentiles 5%='.p05:0' 25%='.p25:0' 50%='.p50:0' 75%='.p75:0' 95%='.p95:0' (Hz)"
      .f0min = floor ( max (semitonesToHertz(hertzToSemitones(.p50) - 12), 50) )		; 12 ST below median pitch, with minimum of 50 Hz
      .f0max = floor ( min (semitonesToHertz(hertzToSemitones(.p50) + 18), 1000) )	; 18 ST above median pitch, with maximum of 1000 Hz 
      .minST   = hertzToSemitones (.f0min) - hertzToSemitones(1)
      .maxST   = hertzToSemitones (.f0max) - hertzToSemitones(1)
      @msg: ".'tab$'Selected F0 detection range for pass 2: '.f0min'-'.f0max' Hz, '.minST:1'-'.maxST:1' ST"
   endif
      @debug_msg: "calculate_pitch_int: pass 2"
      @from_Hz_to_ST_rel_1: .f0min
      .minST = result
      @from_Hz_to_ST_rel_1: .f0max 
      .maxST = result
      @msg: "Calculating pitch for fixed frequency range..."
      @msg: ".'tab$'Time step='tstep:2', F0 range='.f0min'-'.f0max' Hz ('.minST:1'-'.maxST:1' ST), Voicing threshold='voicing_threshold:2'"
      selectObject: soundID
      pitchID = To Pitch (ac): tstep, .f0min, 15, "no", silence_threshold, voicing_threshold, octave_cost, octave_jump_cost, vuv_cost, .f0max
      .p50 = Get quantile: 0, 0, 0.50, "Hertz"
      .p95 = Get quantile: 0, 0, 0.95, "Hertz"
      .p05 = Get quantile: 0, 0, 0.05, "Hertz"
      @msg: ".'tab$'Pitch percentiles 50%='.p50:0' 5%='.p05:0' 95%='.p95:0' (Hz)"
      if (not volatile)
         @msg: "Writing Pitch object to 'pitchfile$'"
         Write to binary file: pitchfile$
      endif
      pitch_available = 1
   @debug_msg: "calculate_pitch_int: exit"
endproc


procedure calculate_intensity_bp_filtered
   @msg: "Calculating intensity of BP filtered signal (fc_low='fc_low:0', fc_high='fc_high:0') ..."
   if (avoid_insuff_memory)
      call intensity_bp_filter_longsound_compact "'signalfile$'" fc_low fc_high intbpID
   else
      if (not signal_available)
         @read_signal
      endif
      selectObject: soundID
      tmpfsID = Filter (pass Hann band): fc_low, fc_high, 100
      selectObject: tmpfsID
      intbpID = To Intensity: 100, time_step
      intbpID = selected ("Intensity", -1)		; keep this line !!
      removeObject: tmpfsID
   endif
   if (not volatile)
      selectObject: intbpID
      Write to text file: intbpfile$
   endif
   intbp_available = 1
endproc


procedure bp_filter: .soundin, .fc_low, .fc_high, .varname$
# BP-filter sound object and put reference to resulting object in variable with name <.varname>
   selectObject: .soundin
   .out = Filter (pass Hann band): .fc_low, .fc_high, 100
   '.varname$' = .out
endproc


procedure intensity_bp_filter_longsound_compact .fname$ .fc_low .fc_high .varname$
# Filter signal and compute intensity. Filtered signal is temporarily stored as file. 
# Use this procedure to avoid insufficient memory.
# Use double quotes if filename contains blanks:
#   call intensity_bp_filter_longsound_compact "'filename$'" 300 2500 myvar
# .fname$	filename of sound file to be filtered
   .tmpfile$ =  prefix$ + "_filtered.wav"
   .longsound = Open long sound file: .fname$
   .t1 = Get start time
   .endtime = Get end time
   .maxdurpart = 300		; duration of part of signal to be filtered separatedly due to available memory
   .partnr = 1
   repeat
      .t2 = min (.endtime, .t1 + .maxdurpart)
      selectObject: .longsound
      .part = Extract part... .t1 .t2 yes
      if (clip_level > 0)
         clip = clip_level/100
         Formula... if (abs(self)>clip) then if self>0 then clip else -clip fi else self fi
      endif
      .filt = Filter (pass Hann band)... .fc_low .fc_high 100
      Formula... if (abs(self)>0.98) then if self>0 then 0.98 else -0.98 fi else self fi
      if (.partnr == 1)
         Save as WAV file: .tmpfile$
      else
         Append to existing sound file... '.tmpfile$'
      endif
      removeObject: .filt, .part
      .t1 = .t2
      .partnr += 1 
   until (.t2 >= .endtime)
   removeObject: .longsound				; free memory
   .tmp = Read from file... '.tmpfile$'
   intbpID = To Intensity: 100, time_step
   intbpID = selected ("Intensity", -1)		; keep this line !!
   '.varname$' = intbpID
   removeObject: .tmp
   filedelete '.tmpfile$'
endproc


procedure calculate_loudness
   if (not signal_available)
      @read_signal
   endif
   execute loudness.praat 'soundID' 'signal_start' 'signal_finish' 'time_step'
   loudnessID = selected ("Intensity", -1)
   if (not volatile)
      Save as short text file: loudnessfile$
   endif
   loudness_available = 1
endproc


procedure calculate_parameters
   @debug_msg: "calculate_parameters: entry"
   if (not signal_available)
      @read_signal
   endif
   if (volatile)
   # Calculate parameters on the fly only for interval to stylize
      selectObject: soundID
      tmpsoundID = Extract part: anal_t1, anal_t2, "rectangular", 1.0, "yes"
      removeObject: soundID	; remove full signal
      soundID = tmpsoundID	; redefine soundID
      selectObject: soundID
      if (scale_signal_amplitude)
         Scale intensity: scale_signal_amplitude
      endif
      intensityID = To Intensity: 100, time_step
      intensityID = selected ("Intensity", -1)		; keep this line !!
      intensity_available = 1
      @calculate_pitch_int
      if (needs_loudness and not loudness_available)
         execute loudness.praat 'soundID' 'anal_t1' 'anal_t2' 'time_step'
         loudnessID = selected ("Intensity", -1)
         loudness_available = 1
      endif
      if (needs_intbp and not intbp_available)
         @calculate_intensity_bp_filtered
         intbp_available = 1
      endif
   else ; (volatile == 0)
      if (not intensity_available)

          if (corpus$ = "rhapsodie")
             prefilterHP100 = 0
             if (basename$ = "Rhap-D0008")
                prefilterHP100 = 1
             endif
          endif

    if (prefilterHP100)	; Apply HP filtering to avoid problems with low quality recordings by filtering low frequency band
        @msg: "Preprocessing (high-pass filtering) speech signal ('basename$')..."
        selectObject: soundID
        fc_low = 100
        tmpID = Filter (pass Hann band): fc_low, 0, 100
        removeObject: soundID
        soundID = tmpID
    endif
    if (prefilterLP)	; Apply LP filtering when fricatives are very intense
        @msg: "Preprocessing (low-pass filtering) speech signal... 'basename$'"
        selectObject: soundID
        fc = 2500
        tmpID = Filter (pass Hann band): 0, fc, 100
        removeObject: soundID
        soundID = tmpID
    endif

        selectObject: soundID
        tmpID = To Intensity: 100, time_step
        intensityID = selected ("Intensity", -1)		; keep this line !!
        intensity_available = 1
      endif
      if (not pitch_available)
        @calculate_pitch_int
      endif
      if (show_harmonicity and not harmonicity_available)
        selectObject: soundID
        silence_threshold = 0.1
        nrofperiods = 1.0
        harmonicityID = To Harmonicity (cc)... time_step minimum_pitch silence_threshold nrofperiods
        harmonicity_available = 1
      endif
      if (needs_loudness and not loudness_available)
        execute loudness.praat 'soundID' 'signal_start' 'signal_finish' 'time_step'
        loudnessID = selected ("Intensity", -1)
        Write to short text file: loudnessfile$
        loudness_available = 1
      endif
      if (needs_intbp and not intbp_available)
        @calculate_intensity_bp_filtered
      endif
    endif ; # volatile
    # Calculate V/UV decision and store in TextGrid
      selectObject: pitchID      
      pointprocID = To PointProcess
      vuvgridID = To TextGrid (vuv): 0.02, 0.01
      Rename: "vuv"
      removeObject: pointprocID
   @debug_msg: "calculate_parameters: exit"
endproc


procedure peek_settings
# Read settings from intermediate data file (_nucl.TextGrid).
   @debug_msg: "peek_settings: entry"
   if (not fileReadable (nuclfile$))
      @fatal_error: "Cannot find nucleus file: <'nuclfile$'>'newline$'Obtain it first using Prosogram task: Calculate intermediate data files"
   else
      .tmpID = Read from file: nuclfile$
      @tier_number_by_name: .tmpID, "^settings$"
      @debug_msg: "peek_settings: settings tier nr='result'"
      if (result)
         .s$ = Get label of interval: result, 1
         @debug_msg: "peek_settings: settings tier label=<'.s$'>"
         segmentation_name$ = extractWord$ (.s$, "SEG=")
         adaptive_glissando = extractNumber (.s$, "ADAPT=")
         glissando = extractNumber (.s$, "GT=")
         glissando_low = glissando
         if (adaptive_glissando = undefined)
            adaptive_glissando = 0
         endif
         if (adaptive_glissando)
            glissando_low = extractNumber (.s$, "GT_low=")
         endif
         diffgt = extractNumber (.s$, "DG=")
         mindur_ts = extractNumber (.s$, "MINTS=")
      else
         @fatal_error: "Cannot find settings tier in nucleus file"
      endif
      removeObject: .tmpID
   endif
   @debug_msg: "peek_settings: exit"
endproc


procedure load_intermediate_data_files
# Reads intermediate data files into data objects. Used in interactive mode.
   .nerrs = 0
   if (not fileReadable (stylfile$))
      @gr_printline: "Cannot open stylization file <'stylfile$'>"
      .nerrs += 1
   endif
   if (not fileReadable (sheetfile$))
      @gr_printline: "Cannot open prosodic features from file <'sheetfile$'>"
      .nerrs += 1
   endif
   if (not fileReadable (profile_file$))
      @gr_printline: "Cannot open prosodic file <'profile_file$'>"
      .nerrs += 1
   endif
   if (.nerrs == 0)
      @gr_printline: "Loading stylization from file <'stylfile$'>..."
      stylID = Read from file: stylfile$
      stylization_available = 1
      reuse_styl = 1
      removeObject: nucldatID
      @gr_printline: "Loading prosodic features from file <'sheetfile$'>..."
      nucldatID = Read TableOfReal from headerless spreadsheet file: sheetfile$
      @gr_printline: "Loading prosodic profile from file <'profile_file$'>..."
      @profile_table_load
   else
      @cleanup_current_file
      @cleanup_global
      @fatal_error: "Some data files could not be found. Please run task <Calculate intermediate data files> again before using Interactive mode."
   endif
   selectObject: nucldatID
   .nrows = Get number of rows
   if (nrof_nuclei_analysed <> .nrows)
      @fatal_error: "Mismatch between data in intermediate data files. Please run task <Calculate intermediate data files> again before using Interactive mode."
   endif
endproc


procedure get_segmentation: method
# Calculate segmentation or read it from file 
   @debug_msg: "get_segmentation: entry"
   if (method == segm_vnucl)		; vowel nuclei
      segmentation_name$ = "vow-nucl"
   elsif (method == segm_extern)
      segmentation_name$ = "extern"
   elsif (method == segm_aloudness)	; automatic, loudness peaks
      segmentation_name$ = "loudness"
   elsif (method == segm_anucl)		; automatic, peaks in bandpass filters speech
      segmentation_name$ = "int-BP"
   elsif (method == segm_msyllvow)	; syllabic nuclei
      segmentation_name$ = "syll+vow"
   elsif (method == segm_msyllpeak)	; syllabic nuclei
      segmentation_name$ = "syll"
   elsif (method == segm_mrhyme)	; syllable rhyme
      segmentation_name$ = "rhyme"
   elsif (method == segm_asyll)		; pseudo-syllables
      segmentation_name$ = "asyll"
   elsif (method == segm_voiced)	; voiced portions
      segmentation_name$ = "voiced"
   elsif (method == segm_specsim)	; 
      segmentation_name$ = "specsim"
   elsif (method == segm_pitchchange)	; coding not completed
      segmentation_name$ = "pitchchange"
   elsif (method == segm_pitchterrace)	; coding not completed
      segmentation_name$ = "pitchterrace"
   else
      segmentation_name$ = "unknown"
   endif
   @debug_msg: "get_segmentation: method='method' name='segmentation_name$'"

   if (task == task_interactive)
      @read_nuclei_file_interactive_mode
;@gr_printline: "segmentation_available='segmentation_available' reuse_nucl='reuse_nucl' nr='nrof_nuclei_analysed'"
   endif ; task_interactive

   if (not segmentation_available)
       @debug_msg: "get_segmentation: segmentation not available"
# Make segmentation into nuclei depending upon segmentation method
    # tiers in annotation TextGrid file
        phone_tier_in = 1	; default location
    # create TextGrid nucleiID
        phone_tier = 1
        dip_tier = 2
        nucleus_tier = 3
        syllable_tier = 4
        vuv_tier = 5
        discontinuity_tier = 6
        safe_tier = 7
        pointer_tier = 8
        speaker_tier = 9
        creak_tier = 10
        settings_tier = 11
        hesitation_tier = 12
        polytonia_tier = 0	; may be added later for polytonia_annotation
        boundary_tier = 0	; may be added later for automatic boundary detection
        stress_tier = 0		; may be added later for stress_annotation
        .n = 12
        .tiers$ = "phone dip nucleus syll vuv discont safe pointer speaker creak settings hesitation"
        .point_tiers$ = "dip discont"
        if (boundary_annotation)
           .tiers$ += " boundary-auto"
           .point_tiers$ += " boundary-auto"
           .n += 1
           boundary_tier = .n
        endif
        if (polytonia_annotation)
           .tiers$ += " polytonia"
           .n += 1
           polytonia_tier = .n
        endif
        if (stress_annotation)
           .tiers$ += " stress"
           .n += 1
           stress_tier = .n
        endif
        nucleiID = Create TextGrid: signal_start, signal_finish, .tiers$, .point_tiers$
        Rename: "nucl"
		nuclei_available = 1
    # copy VUV decision from vuvgrid to nuclei grid
        @copy_tier: vuvgridID, 1, nucleiID, vuv_tier
        removeObject: vuvgridID


   if (segfile_available) ; and not internal data format available
      @debug_msg: "get_segmentation: segfile available"
      @gr_printline: "Copying annotation tiers..."
      @copy_tiers_from_segmentation_textgrid
   endif

   if (not speaker_available)
      if (fileReadable (speakerfile$))
         @msg: "Reading speaker information from 'speakerfile$'"
         tmpID = Read from file: speakerfile$
         @tier_number_by_name: tmpID, "^[Ss]peaker$"
         if (result > 0)
            @copy_tier: tmpID, result, nucleiID, speaker_tier
            speaker_available = 1
         else
            @msg: "No speaker tier found in 'speakerfile$'" 
         endif
         removeObject: tmpID
      endif
   endif

   if (not creak_available)
      if (fileReadable (creakfile$))
         @msg: "Reading 'creakfile$'" 
         tmpID = Read from file: creakfile$
         @tier_number_by_name: tmpID, "^[Cc]reaky?$"
         if (result > 0)
            @msg: "Reading creak information from tier 'result' of 'creakfile$'"
            @copy_tier: tmpID, result, nucleiID, creak_tier
            creak_available = 1
         endif
         removeObject: tmpID
      endif
   endif

   if (corpus$ = "cprom" or corpus$ = "rhapsodie")
         @tier_number_by_name: segmentationID, "^contour$"
         if (result)
            @grid_append_tier: segmentationID, result, "nucleiID"
         endif
         @tier_number_by_name: segmentationID, "^prom$"
         if (result)
            @grid_append_tier: segmentationID, result, "nucleiID"
         endif
   endif

   if (needs_phon_tier and not phones_available)
      @fatal_error: "Cannot find phoneme tier (named phon) in annotation TextGrid"
   endif
   if (needs_syll_tier and not syllables_available)
      @fatal_error: "Cannot find syllable tier (named syll) in annotation TextGrid"
   endif

   @msg: "Calculating actual segmentation. Method='segmentation_name$'..."
   if (segm_type != segm_extern)
      @make_segmentation: segm_type, t1s, t2s, nucleiID, mindiff
   else		; copy consulted external segmentation in tier "segm" of file to nucleus tier of object nucleiID 
      @tier_get: segmentationID, "^segm$", "tier_in", "Cannot find segmentation tier (named segm) in segmentation TextGrid", 1
      @copy_tier: segmentationID, tier_in, nucleiID, nucleus_tier
      selectObject: nucleiID
      .n = Get number of intervals: nucleus_tier
      for .j from 1 to .n
         label$ = Get label of interval: nucleus_tier, .j
         @is_vowel: label$
         if (is_vowel)
            Set interval text: nucleus_tier, .j, "a"
         endif
      endfor
   endif

   @safe_nuclei: t1s, t2s
   nrof_nuclei_analysed = result

  # Store analysis, segmentation and stylization settings in nucleiID TextGrid 
    selectObject: nucleiID
    Set interval text: settings_tier, 1, "File='basename$' SEG='segmentation_name$' t1='t1s' t2='t2s' " +
      ... "GT='glissando' GT_low='glissando_low' ADAPT='adaptive_glissando' DG='diffgt' MINTS='mindur_ts' MINPAUSE='mindur_pause_gap'"

  # prepare for Polytonia analysis
    if (polytonia_annotation)
       selectObject: nucleiID
       Remove tier: polytonia_tier		; is empty tier when object is created
       if (segm_type == segm_msyllvow or segm_type == segm_msyllpeak or segm_type == segm_mrhyme)	
          ; use syllable boundaries for tonal labels
          .src_tier = syllable_tier
       elsif (segm_type == segm_vnucl and segfile_available and syllables_available)
          .src_tier = syllable_tier
       else
          .src_tier = nucleus_tier
       endif
       Duplicate tier: .src_tier, polytonia_tier, "polytonia"
       @tier_merge_intervals_except: nucleiID, polytonia_tier, "a"
       @tier_clear_text: nucleiID, polytonia_tier
    endif
    if (stress_annotation)
       if (segm_type == segm_msyllvow or segm_type == segm_msyllpeak or segm_type == segm_mrhyme)
          @tier_replace2: nucleiID, syllable_tier, nucleiID, stress_tier
       elsif (segm_type == segm_vnucl and segfile_available and syllables_available)
          @tier_replace2: nucleiID, syllable_tier, nucleiID, stress_tier
       else
          @tier_replace2: nucleiID, nucleus_tier, nucleiID, stress_tier
       endif
       @tier_clear_text: nucleiID, stress_tier
    endif
   endif ; not segmentation available
   @debug_msg: "get_segmentation: exit"
endproc


procedure read_nuclei_file_interactive_mode
; Read settings and tiers from saved nuclei file into nucleiID TextGrid:
   if (task == task_interactive)
      if (not fileReadable (nuclfile$))
         @gr_printline: "Cannot find internal data TextGrid file <'nuclfile$'>"
      else
         @gr_printline: "Loading internal data TextGrid from file <'nuclfile$'>"
         tmpID = Read from file: nuclfile$
         @tier_number_by_name: tmpID, "^settings$"
         if (result)
            @debug_msg: "get_segmentation: interactive mode, found settings"
            settings_tier = result
            s$ = Get label of interval: settings_tier, 1
            method_$ = extractWord$ (s$, "SEG=")
            t1_ = extractNumber (s$, "t1=")
            t2_ = extractNumber (s$, "t2=")
            gt_ = extractNumber (s$, "GT=")
            gt_low_ = extractNumber (s$, "GT_low=")
            if (gt_low_ == undefined)
              gt_low_ = gt_
            endif
            .adapt = extractNumber (s$, "ADAPT=")
            if (.adapt == undefined)
              .adapt = 0
            endif
            dg_ = extractNumber (s$, "DG=")
            mints_ = extractNumber (s$, "MINTS=")
            if (anal_t1 >= t1_ and abs(anal_t2 - t2_) < time_step) ; Correct for rounding errors in 32 vs. 64 bit 
              segmentation_name$ = method_$
                  @tier_number_by_name: tmpID, "^phone$"
                  phone_tier = result
               if (result)
                  @tier_number_by_name: tmpID, "^syll$"
                  syllable_tier = result
               endif
               if (result)
                  @tier_number_by_name: tmpID, "^nucleus$"
                  nucleus_tier = result
               endif
               if (result)
                  @tier_number_by_name: tmpID, "^vuv$"
                  vuv_tier = result
                  removeObject: vuvgridID
               endif
               if (result)
                  @tier_number_by_name: tmpID, "^discont$"
                  discontinuity_tier = result
               endif
               if (result)
                  @tier_number_by_name: tmpID, "^safe$"
                  safe_tier = result
               endif
               if (result)
                  @tier_number_by_name: tmpID, "^pointer$"
                  pointer_tier = result
               endif
               if (result)
                  @tier_number_by_name: tmpID, "^speaker$"
                  speaker_tier = result
                  if (speaker_tier)
                     speaker_available = 1	; speaker tier available
                  endif
               endif
               if (result)		; success
                  reuse_nucl = 1
                  segmentation_available = 1
                  nucleiID = tmpID
				  nuclei_available = 1
                  ; nrof_nuclei_analysed = Count labels: safe_tier, "a"
                  nrof_nuclei_analysed = Count labels: nucleus_tier, "a"
                  @gr_printline: "Loaded internal data TextGrid from file <'nuclfile$'>"
               endif
           endif ; if anal_t1
         endif ; tier with settings found
         if (not segmentation_available) ; textgrid read but not used
            removeObject: tmpID
         endif
      endif ; nuclfile found
   endif
endproc


procedure copy_tiers_from_segmentation_textgrid
; Copy tiers (phon, syll, speaker, words, creak, hes) from segmentation TextGrid to nucleiID 
   @tier_number_by_name: segmentationID, "^phon"
   # The following if-block may override default name of phoneme tier
   if (variableExists ("corpus$") and variableExists ("corpus_tier_phon$"))
      if (length (corpus_tier_phon$))
         @tier_number_by_name: segmentationID, "^'corpus_tier_phon$'$"
         if (result = 0)
            @msg: "Cannot find phoneme tier named <'corpus_tier_phon$'>"
         endif
      endif
   endif
   if (result)	; found tier dedicated to phonemes
      phone_tier_in = result
      @msg: "Using phonetic alignment from tier named <'result2$'>"
      phones_available = 1
   else
      phone_tier_in = 0	
      phones_available = 0
      @msg: "Cannot find tier with phonetic alignment"
   endif
   if (phones_available)
      @tier_replace: segmentationID, phone_tier_in, nucleiID, phone_tier, "nucleiID"
      selectObject: nucleiID
      Set tier name: phone_tier, "phone"
   endif

   @tier_number_by_name: segmentationID, "^syll"
   # The following if-block may override default name of syllable tier
   if (variableExists ("corpus$") and variableExists ("corpus_tier_syll$"))
      if (length (corpus_tier_syll$))
        @tier_number_by_name: segmentationID, "^'corpus_tier_syll$'$"
      endif
   endif
   if (result and segm_type <> segm_asyll)
      ; In segm_asyll, the syllable tier is used for pseudo-syllables and should not be used for true syllables.
         syll_tier_in = result
         @msg: "Using syllable alignment from tier named <'result2$'>"
         @copy_tier: segmentationID, syll_tier_in, nucleiID, syllable_tier
;         call tier_replace segmentationID syll_tier_in nucleiID syllable_tier nucleiID
;         selectObject: nucleiID
;         Set tier name... syllable_tier syll
         syllables_available = 1
   endif

   @tier_number_by_name: segmentationID, "^[Ss]peaker$"
   # The following if-block may override default name of speaker tier
   if (variableExists ("corpus$") and variableExists ("corpus_tier_speaker$"))
      if (length (corpus_tier_speaker$))
         @tier_number_by_name: segmentationID, "^'corpus_tier_speaker$'$"
      endif
   endif
   if (result)
      speaker_tier_in = result
      @msg: "Using speaker information from tier named <'result2$'>"
      if (corpus$ = "rhapsodie") 
         @copy_tier_tweeked: segmentationID, speaker_tier_in, nucleiID, speaker_tier
      else
         @copy_tier: segmentationID, speaker_tier_in, nucleiID, speaker_tier
      endif
      speaker_available = 1	; speaker tier available
   else			; input segmentation textgrid does not contain speaker tier 
      selectObject: nucleiID
      Set interval text: speaker_tier, 1, "ANON"
   endif

   @tier_number_by_name: segmentationID, "^[Cc]reaky?$"
   if (result)
      @msg: "Using creak information from tier named <'result2$'> of <'segfile$'>"
      @copy_tier: segmentationID, result, nucleiID, creak_tier
      creak_available = 1
   endif

   word_tier_in = 0
   @tier_number_by_name: segmentationID, "^words?$"
   # The following if-block may override default name of word tier
   if (variableExists ("corpus$") and variableExists ("corpus_tier_word$"))
      if (length (corpus_tier_word$))
         @tier_number_by_name: segmentationID, "^'corpus_tier_word$'$"
      endif
   endif
   if (result)
      word_tier_in = result
   endif

   if (segm_type <> segm_asyll)	; syllable_tier will be used for storing asyll intervals
      @tier_number_by_name: segmentationID, "^hes$"
      if (result)
         hesitation_tier_in = result
         @msg: "Using hesitation information from tier named <'result2$'> of <'segfile$'>"
         @copy_tier: segmentationID, hesitation_tier_in, nucleiID, hesitation_tier
         hesitation_available = 1
      else
         @copy_tier: nucleiID, syllable_tier, nucleiID, hesitation_tier
         @tier_clear_text: nucleiID, hesitation_tier
      endif
   endif
endproc



procedure clipPitchTier: objectID, .ymin, .ymax, .xmin, .xmax
# Clip a PitchTier by replacing data points outside clip range by values 1 ST outside range
   @debug_msg: "clipPitchTier: entry"
   selectObject: objectID
   .firsti = Get low index from time: .xmin
   if (.firsti = 0)
      .firsti = 1
   endif
   .lasti = Get nearest index from time: .xmax
   for .i from .firsti to .lasti
      .x = Get time from index: .i
      .y = Get value at index: .i
      .y = min(.y,.ymax+1)
      .y = max(.y,.ymin-1)
      endif
      Remove point: .i
      Add point: .x, .y
   endfor
   @debug_msg: "clipPitchTier: exit"
endproc 


procedure cleanup_current_file
# Delete temporary objects created for current input file
   @debug_msg: "cleanup_current_file: entry"
   if (do_cleanup)
      if (signal_available)
         removeObject: soundID
         if (volatile)
            removeObject: fullsoundID
         endif
         signal_available = 0
      endif
      if (intensity_available)
         removeObject: intensityID
      endif
      if (pitch_available)
         removeObject: pitchID
      endif
      if (loudness_available)
         removeObject: loudnessID
      endif
      if (intbp_available)
         removeObject: intbpID
      endif
      if (inthp_available)
         removeObject: inthpID
      endif
      if (segfile_available)
         if not boundary_annotation
            removeObject: segmentationID
         endif
         if (draw_prosograms)
            if (nrofplottedtiers > 0)
               removeObject: newgridID
            endif
         endif
      endif
      if (nuclei_available)
         removeObject: nucleiID
      endif
      if (needs_stylization)
         if (nucldat_available)
            removeObject: nucldatID
         endif
         if (stylization_available)
            removeObject: stylID, stylSTID
         endif
      endif
      if (profile_available)
         removeObject: profileID
      endif
   endif ; if (do_cleanup)
   @debug_msg: "cleanup_current_file: exit"
endproc


procedure cleanup_global
   @debug_msg: "cleanup_global: entry"
   if (do_cleanup)
      if (globalsheet_available)
         removeObject: globalprofileID
      endif
      if ((not batch_mode) and variableExists ("filelistID"))
         removeObject: filelistID
      endif
   endif ; if (do_cleanup)
   @debug_msg: "cleanup_global: exit"
endproc


procedure convert_sampa_ipa: .objectID, .tier
   selectObject: .objectID
   .n = Get number of intervals: .tier
   for .j from 1 to .n
      label$ = Get label of interval: .tier, .j
      @sampa_ipa
      Set interval text: .tier, .j, label$
   endfor
endproc


procedure sampa_ipa
# replace SAMPA label$ by IPA representation in Praat's "special symbols" format
   len = length (label$)
   s$ = ""
   i = 1
   while (i <= len)
      b$ = ""			; translation (i.e. output)
      c1$ = mid$ (label$,i,1)	; next char
      c2$ = mid$ (label$,i,2)	; next 2 chars
      c3$ = mid$ (label$,i,3)	; next 3 chars
      c4$ = mid$ (label$,i,4)	; next 4 chars
      cn$ = mid$ (label$,i+1,1)	; following char
      restlen = len - i
      if (restlen >= 1 and (cn$ = "~" or cn$ = "`" or cn$ = "="))
         if (cn$ = "~")		; nasal vowels (length=2)
            if (c2$ = "a~" or c2$ = "A~")
               b$ = "\as\~^"
            elsif (c2$ = "o~" or c2$ = "O~")
               b$ = "\ct\~^"
            elsif (c2$ = "e~" or c2$ = "E~") 
               b$ = "\ef\~^"
            elsif (c2$ = "9~")
               b$ = "\oe\~^"
            endif
         elsif (cn$ = "`")	; rhoticity
            if (index ("uoi", c1$))
               b$ = c1$ + "\hr"
            elsif (c2$ = "@`")
               b$ = "\sr"
            elsif (c2$ = "s`")
               b$ = "\s."
            elsif (c2$ = "z`")
               b$ = "\z."
            endif
         elsif (cn$ = "=")	; syllabicity in x-sampa
            if (index_regex (c1$, "[mnNJlrR]")) 
               b$ = c1$ + "\|v"
            endif
         endif
         if (length (b$) > 0)
            s$ = s$ + b$
            i += 2
         else
            s$ = s$ + c1$
            i += 1
         endif
    # SAMPA symbols of length 1
      elsif (index ("BCDSTZRMNHJGAEIOQVY@2679?:{}&", c1$) or c1$ = """")	; additional regex to speed up things
         if (c1$ = """")		; primary stress 
            b$ = "\'1"
         elsif (c1$ = "A") 
            b$ = "\as"
         elsif (c1$ = "E")
            ; b$ = "\ep"
            b$ = "\ef"
         elsif (c1$ = "I")
            b$ = "\ic"
         elsif (c1$ = "O")
            b$ = "\ct"
         elsif (c1$ = "Y")
            b$ = "\yc"
         elsif (c1$ = "Q")
            b$ = "\ab"
         elsif (c1$ = "V")
            b$ = "\vt"
         elsif (c1$ = "2")
            b$ = "\o/"
         elsif (c1$ = "6")
            b$ = "\sr"
         elsif (c1$ = "7") 
            b$ = "\rh"
         elsif (c1$ = "9") 
            b$ = "\oe"
         elsif (c1$ = "@")	; schwa
            b$ = "\sw"
         elsif (c1$ = "B")	; voiced bilabial fricative
            b$ = "\bf"
         elsif (c1$ = "C")	; voiceless palatal fricative
            b$ = "\c,"
         elsif (c1$ = "D")	; voiced dental fricative
            b$ = "\dh"
         elsif (c1$ = "T")	; voiceless dental fricative
            b$ = "\tf"
         elsif (c1$ = "S") 
            b$ = "\sh"
         elsif (c1$ = "Z") 
            b$ = "\zh"
         elsif (c1$ = "R") 
            b$ = "\rc"
         elsif (c1$ = "M") 
            b$ = "\mj"
         elsif (c1$ = "N") 
            b$ = "\ng"
         elsif (c1$ = "H") 
            b$ = "\ht"
         elsif (c1$ = "J") 
            b$ = "\nj"
         elsif (c1$ = "G") 
            b$ = "\gf"
         elsif (c1$ = "?") 
            b$ = "\?g"
         elsif (c1$ = ":") 
            b$ = "\:f"
         elsif (c1$ = "{") 
            b$ = "\ae"
         elsif (c1$ = "}") 
            b$ = "\u-"
         elsif (c1$ = "&") 
            b$ = "\Oe"
         endif
         if (length (b$) > 0)
            s$ = s$ + b$	; append b$ to output string
            i += 1
         else
            s$ = s$ + c1$
            i += 1
         endif
      elsif (c1$ = "\")
         if (index ("_\:f__\a~__\o~__\o/__\ef_", "_'c3$'_"))
            s$ = s$ + c3$
            i += 3
;         elsif (restlen >= 3 and index (":\a~:\o~:\o/:", ":'c3$':") > 0)
;            s$ = s$ + c3$
;            i += 3
         elsif (restlen >= 4 and index (":\ep~:", c4$) > 0)
            s$ = s$ + c4$
            i += 4
         else			; others: just copy them
            s$ = s$ + c1$
            i += 1
         endif
      elsif (c3$ = "i_d")
         s$ = s$ + "i\Nv"
         i += 3
      else			; others: just copy them
         s$ = s$ + c1$
         i += 1
      endif
   endwhile
   label$ = s$
endproc


procedure speaker_info_get
# 1. Make list of speakers appearing in tier "speaker" of annotated segmentation textgrid.
# 2. Add speaker identification to nucldatID, as a number (in column <speaker_id>) and as a string (in row label).
   @debug_msg: "speaker_info_get: entry, speaker_available='speaker_available'"
   speakers = 0			; nrof different speakers found
   speakers$ = ""		; used to store speaker names and their number
   if (speaker_available)
       speaker = 0		; nrof current speaker
       selectObject: nucldatID
       .rows = Get number of rows
       for .row to .rows
          .t1 = Get value: .row, j_nucl_t1
          .t2 = Get value: .row, j_nucl_t2
          selectObject: nucleiID
          .i = Get interval at time: speaker_tier, .t1+(.t2-.t1)/2
          speaker$ = Get label of interval: speaker_tier, .i
          speaker$ = replace_regex$ (speaker$, "^ +(.*) +$", "\1", 1)	; trim spaces at left and right
          if (index (speakers$, "<'speaker$'>") > 0)					; speaker already encountered in file
             speaker = extractNumber (speakers$, "<'speaker$'>:")		; get his number
          else								; encountered a new speaker
             speakers += 1
             speaker = speakers				; number of speaker
             speakers$ = speakers$ + "<'speaker$'>:'speaker' "			; store name and number in list
             speaker_label'speaker'$ = speaker$
          endif
          selectObject: nucldatID
          Set value: .row, j_speaker_id, speaker
          Set row label (index): .row, speaker$
       endfor
   else
      @msg: "No speaker tier in input textgrid. Assuming 1 speaker."
      speakers = 1
      speaker$ = "ANON"
      speaker_label1$ = speaker$
      speakers$ = "<'speaker$'>:'speakers' "
      selectObject: nucleiID
      Set interval text: speaker_tier, 1, speaker$
      selectObject: nucldatID
      .rows = Get number of rows
      for .row from 1 to .rows
         Set value: .row, j_speaker_id, 1
      endfor
   endif

   ; Correct values at speaker turns, for pause duration, internucleus duration
   selectObject: nucldatID
   .rows = Get number of rows
   for .row to .rows
      if (.row > 1)
         selectObject: nucldatID
         .sp = Get value: .row, j_speaker_id
         .prevsp = Get value: .row-1, j_speaker_id
         if (.sp <> .prevsp)
            Set value: .row, j_intersyl, 0
            Set value: .row, j_internucldur, 0
            .t1 = Get value: .row-1, j_nucl_t1
            .t2 = Get value: .row-1, j_nucl_t2
            ; pause ends and speaker turn, even when next turn starts with silence
            selectObject: nucleiID
            .i = Get interval at time: speaker_tier, .t1
            .t2s = Get end time of interval: speaker_tier, .i
            selectObject: nucldatID
            if (.t2s-.t2 >= mindur_pause_gap)
               Set value: .row-1, j_pause_dur, .t2s-.t2
            else
               Set value: .row-1, j_pause_dur, 0
            endif
         endif
      endif
   endfor
   @debug_msg: "speaker_info_get: exit"
endproc


procedure speaker_autorange: t1, t2
; propose a pitch range suitable for plotting the range of all speakers in time interval <t1>..<t2>
; return range in ST in <ymin>..<ymax>
   @debug_msg: "speaker_autorange: entry"
   if (task == task_interactive and profile_available)
      speakers$ = ""
      selectObject: profileID
      .nrows = Get number of rows
      for .j to .nrows
         .s$ = Get row label: .j
         .s$ = replace_regex$(.s$, "^('basename$'_)", "", 1)
         speakers$ += "<'.s$'>:'.j' "
         speaker_label'.j'$ = .s$
         .topST = Get value: .j, j_pitch_top_ST
         .bottomST = Get value: .j, j_pitch_bottom_ST
         .medianST = Get value: .j, j_pitch_median_ST
         speaker_range_'.j'$ = "TOP_ST='.topST:1' BOTTOM_ST='.bottomST:1' MEDIAN_ST='.medianST:1' "
      endfor
   endif
   if (task == task_pitch_plot or not needs_pitchrange)	; no nuclei available
      selectObject: pitchID
      .y = Get mean: t1, t2, "semitones re 1 Hz"
      if (.y == undefined)
         @msg: "speaker_autorange: mean pitch undefined for time 't1:3'-'t2:3', using default 85 ST"
         .y = 85
      endif
      ymin = .y - 12
      ymax = .y + 12
   else					; compute total range for all speakers in time interval <t1>..<t2>
      ymin = 1000 
      ymax = 0
      .t = t1
      selectObject: nucleiID
      .n = Get number of intervals: speaker_tier
      .t2n = Get end point: speaker_tier, .n
      .t2 = min (t2, .t2n)
      repeat		; all speakers in range t1..t2
         .i = Get interval at time: speaker_tier, .t
         speaker$ = Get label of interval: speaker_tier, .i
         speaker$ = replace_regex$ (speaker$, "^ +(.*) +$", "\1", 1)	; trim left and right
         speaker_j = extractNumber (speakers$, "<'speaker$'>:")			; get his number
         if (speaker_j == 0 or speaker_j == undefined)
            ; @msg: "speaker_autorange: speaker undefined at time '.t:3'"
            ; This happens when there are no nuclei for this speaker 
            selectObject: pitchID
            ymin = Get mean: 0, 0, "semitones re 1 Hz"
            ymax = ymin
         else
            .y = extractNumber (speaker_range_'speaker_j'$, "BOTTOM_ST=")
            if (.y == undefined)
               @msg: "speaker_autorange: BOTTOM_ST undefined t='.t:3'"
               .y = 85
            endif
            ymin = min (ymin, .y) 
            .y = extractNumber (speaker_range_'speaker_j'$, "TOP_ST=")
            if (.y == undefined)
               @msg: "speaker_autorange: TOP_ST undefined t='.t:3'"
               .y = 85
            endif
            ymax = max (ymax, .y)
         endif
         selectObject: nucleiID
         .t = Get end point: speaker_tier, .i
      until (.t >= .t2)
      if (ymax - ymin < 24)		; default range 24 ST or 2 octaves 
         ymin = max(0, (ymin + (ymax - ymin)/2) - 12)
         ymax = ymin + 24
      endif
   endif ; task
   @debug_msg: "speaker_autorange: exit"
endproc


procedure nucleus_tier_postproc: .grid, .nucl_tier, .syll_tier
   selectObject: .grid
   interval_tier = Is interval tier: .nucl_tier
   if (interval_tier)
      Set tier name: .nucl_tier, "segm"
      .n = Get number of intervals: .nucl_tier
      ; replace parts other than nucleus by zero-length strings
      for .i to .n
         .s$ = Get label of interval: .nucl_tier, .i
         if (index (":<:>:<>:xL:xR:U:skip:reject:short:", ":'.s$':"))
            Set interval text: .nucl_tier, .i, ""
         endif
      endfor
      ; reduce contiguous empty intervals
      .i = 2
      while (.i <= .n)
         .prev$ = Get label of interval: .nucl_tier, .i-1
         .s$ = Get label of interval: .nucl_tier, .i
         if (.prev$ = "" and .s$ = "")
            Remove left boundary: .nucl_tier, .i
            .n -= 1
         else
            .i += 1
         endif
      endwhile
      ; add syllable boundaries to nucleus tier
      .n = Get number of intervals: .syll_tier
      for .i to .n - 1
         .t = Get end point: .syll_tier, .i
         .j = Get interval at time: .nucl_tier, .t
         .x1 = Get start point: .nucl_tier, .j
         .x2 = Get end point: .nucl_tier, .j
         if (.t - .x1 > time_step and .t < .x2)
            Insert boundary: .nucl_tier, .t
         endif
         Set interval text: .syll_tier, .i, ""
      endfor
   endif
endproc


procedure copy_tier_tweeked: srcgrid, srctier, destgrid, desttier
; Special treatment for Rhapsodie tier "locuteur". Force interval contiguity.
      selectObject: destgrid
      .n = Get number of intervals: desttier
      .endtime = Get end time of interval: desttier, .n 
      selectObject: srcgrid
      .n = Get number of intervals: srctier
      for .i to .n
         selectObject: srcgrid
         .t1 = Get start time of interval: srctier, .i
         .t2 = Get end time of interval: srctier, .i
         .label$ = Get label of interval: srctier, .i
         if (.i < .n)
            .t3 = Get start time of interval: srctier, .i+1
            if (.t2 < .t3)
               .t2 = .t3	; intervals should be contiguous
            endif
         endif
         selectObject: destgrid
         if (.t2 < .endtime and .i < .n)
            Insert boundary: desttier, .t2
         endif
         if (.t1 <= .endtime)
            .t2 = min (.t2, .endtime)	; when destgrid is shorter than srcgrid
            j = Get interval at time: desttier, .t1 + (.t2-.t1)/2
            Set interval text: desttier, j, .label$
         endif
      endfor
endproc


procedure validate_phoneme_tier: .infname$
   @fname_parts: .infname$
   .basename$ = result2$
   .dataID = Read from file: .infname$
   @tier_get: .dataID, "^phon", "phone_tier_", "Cannot find phoneme tier in '.basename$'", 1
   .n = Get number of intervals: phone_tier_
   for .phon from 1 to .n
      .t1_phon = Get start point: phone_tier_, .phon
      .label_phon$ = Get label of interval: phone_tier_, .phon
      @verify_tier: .label_phon$, .basename$, "phoneme", .t1_phon
   endfor
   removeObject: .dataID
endproc


procedure validate_syllable_tier: .infname$
; (1) Validate symbols in phon tier: should be valid SAMPA or Praat phonetic symbols
; (2) Validate symbols in syllable tier 
; (3) Validante alignment of syllable and phoneme tiers
; (4) Check number of vowels (or syllabic consonants) within syllable.
; Uses tiers named "phon...", "syll...", and optionally "word...".
; Test is skipped when syllable label equals "_" or when word label equals "%".
   @fname_parts: .infname$
   .basename$ = result2$
   .dataID = Read from file: .infname$
   @corpus_conversion: 1, "dataID", corpus$
   #call tier_number_by_name segmentationID "^phon"
   @tier_get: .dataID, "^phon", "phone_tier",    "Cannot find phoneme tier in '.basename$'", 1
   @tier_get: .dataID, "^syll", "syllable_tier", "Cannot find syllable tier in '.basename$'", 1
   @tier_get: .dataID, "^word", "word_tier",     "Cannot find word tier in '.basename$'", 0

   @debug_msg: "validate_syllable_tier: infname='.infname$' phone_tier='phone_tier'"
   .ns = Get number of intervals: syllable_tier
   for j from 1 to .ns
      .t1_syll = Get start time of interval: syllable_tier, j
      .t2_syll = Get end time of interval: syllable_tier, j
      .label_syll$ = Get label of interval: syllable_tier, j
      if (word_tier)
         .k = Get interval at time: word_tier, (.t1_syll+(.t2_syll-.t1_syll)/2)
         .word$ = Get label of interval: word_tier, .k
         .word$ = replace_regex$ (.word$, " ", "", 0)	; rm whitespace
      else
         .word$ = ""
      endif
      if (index("_#", .label_syll$)== 0 and .word$ <> "%")	; not pause, not noise, not incomprehensible
         errors = 0
         ; Check label in tier
            @verify_tier: .label_syll$, .basename$, "syllable", .t1_syll
            errors += result
         @intervals_from_time_range: .dataID, phone_tier, .t1_syll, .t2_syll-0.001, "ph1", "ph2"
         nrof_vowels = 0
         nrof_syllabics = 0
         if (phone_tier) 
            for .phon from ph1 to ph2
               .t1_phon = Get start time of interval: phone_tier, .phon
               .t2_phon = Get end time of interval: phone_tier, .phon
               .label_phon$ = Get label of interval: phone_tier, .phon
               @verify_tier: .label_phon$, .basename$, "phoneme", .t1_phon
               errors += result
               ; Check alignment between syllable tier and phoneme tier
               if (.phon == ph1 and .t1_phon <> .t1_syll)
                 @msg: "Syllable ('.label_syll$') starting at '.t1_syll:4' not aligned with phoneme ('.label_phon$') start ('.t1_phon:4')"
                 errors += 1
               endif
               if (.phon == ph2 and .t2_phon <> .t2_syll)
                 @msg: "Syllable ('.label_syll$') ending at '.t2_syll:4' not aligned with phoneme ('.label_phon$') end ('.t2_phon:4')"
                 errors += 1
               endif
               ; Check number of vowels/syllabics
               @is_syllabic: .label_phon$
               if (result)
                  nrof_syllabics += 1
                  if (is_vowel)
                     nrof_vowels += 1
                  endif
                  if (nrof_syllabics > 1)
                    @msg: "Multiple vowels (or syllabics) in syllable ('.label_syll$') at time '.t1_syll:4'"
                  endif
               endif ; syllabic
               if (.phon == ph2 and nrof_vowels == 0)
                  @msg: "No vowel in syllable ('.label_syll$') at time '.t1_syll:4'"
               endif
            endfor ; phonemes in syllable
         endif ; phone_tier
      endif
   endfor
   Remove
endproc


procedure verify_tier: .str$, fname_$, .type$, .time
   result = 0
   if (index(.str$, " "))
      @error_msg: "Label in tier at time '.time:2' of file <'fname_$'> contains whitespace"
   endif
   if (.type$ = "phoneme")
      .str$ = replace_regex$(.str$, "^""", "", 0)	; remove stress symbol
      ; .str$ = replace_regex$(.str$, "^_$", "", 0)	; remove pause symbol
      .str$ = replace$(.str$, "\rc", "R", 0)
      .str$ = replace$(.str$, "\ep", "E", 0)
      if (.str$ = "_" or .str$ = "")
      elsif (not index_regex(.str$, "^[ptcCkbdgfsTDSvzZxGhmnNJlrRiyue2oE9OajwH@\?][=~]?$"))
         @msg: "Label in phone tier at time '.time:2' contains invalid character: <'.str$'>"
         result = 1
      endif
   elsif (.type$ = "syllable")
      .str$ = replace$(.str$, "\rc", "R", 0)
      .str$ = replace$(.str$, "\ep", "E", 0)
      if (not index_regex(.str$, "^[ptcCkbdgfsTDSvzZxGhmnNJlrRiyue2oE9OajwH@\?=~]+$"))
         @msg: "Label in syllable tier at time '.time:2' contains invalid character: <'.str$'>"
         result = 1
      endif
   elsif (.type$ = "hesitation")
      if (not index_regex(.str$, "^H?$"))
         @msg: "Label in hesitation tier at time '.time:2' contains invalid character: <'.str$'>"
         result = 1
      endif
   elsif (.type$ = "prominence")
      if (not index_regex(.str$, "^[0SW]$"))
         @msg: "Label in prominence tier at time '.time:2' contains invalid character: <'.str$'>"
         result = 1
      endif
   elsif (.type$ = "polytonia")
      if (not index_regex(.str$, "^[BLMHT_RrFfSC,]*$"))
         @msg: "Label in polytonia tier at time '.time:2' contains invalid sequence: <'.str$'>"
         result = 1
      endif
      if (index_regex(.str$, "CC") or index_regex(.str$, "__"))
         @msg: "Label in polytonia tier at time '.time:2' contains invalid sequence: <'.str$'>"
         result = 1
      endif
   elsif (.type$ = "stressref")
      if (not index_regex(.str$, "^[SW0]$"))
         @msg: "Label in stress-ref tier at time '.time:2' contains invalid character: <'.str$'>"
         result = 1
      endif
   else
      @msg: "verify_tier: ERROR invalid tier type: <'.type$'>"
   endif
endproc


procedure store_stylization: paramID, nucleiID, .filename$
# Store stylization targets, indicating whether they are initial/final/intermediate relative to nucleus boundaries
   @debug_msg: "store_stylization: entry"
   filedelete '.filename$'
   selectObject: nucleiID						; segmentation into nuclei, etc
   .nnucl = Get number of intervals: nucleus_tier
   select paramID						; stylization (PitchTier)
   .ni = Get number of points
   for .interv from 1 to .nnucl					; indices of nuclei for which to plot stylization 
      selectObject: nucleiID					; segmentation into nuclei, etc
      @is_nucleus: .interv
      if (result)
         .nx1 = Get start point... nucleus_tier .interv		; time of start of nucleus
         .nx2 = Get end point... nucleus_tier .interv		; time of end of nucleus
         select paramID						; stylization (PitchTier)
       # Check that the nearest indices are within nucleus
         .i1 = Get nearest index from time... .nx1
         repeat
            .t1 = Get time from index... .i1
            if (.t1 < .nx1)
               .i1 += 1
            endif
         until (.t1 >= .nx1 or .t1 >= .nx2 or .i1 >= .ni)
         .i2 = Get nearest index from time... .nx2
         repeat
            .t2 = Get time from index... .i2
            if (.t2 > .nx2)
               .i2 -= 1
            endif
         until (.t2 <= .nx2 or .t2 <= .nx1 or .i2 <= 1)
         for .i from .i1 to .i2					; each stylization (tonal)  segment i
            selectObject: paramID					; stylization (PitchTier)
            .x = Get time from index... .i
            .y = Get value at index... .i
            .yST =  hertzToSemitones(.y) - hertzToSemitones(1)
            if (.i == .i1)
               if (syllables_available)				; write syllable start, end and label
                  selectObject: nucleiID
                  .isyll = Get interval at time... syllable_tier .x
                  .t1s = Get start point... syllable_tier .isyll
                  .t2s = Get end point... syllable_tier .isyll
                  .s$ = Get label of interval... syllable_tier .isyll
                  fileappend '.filename$' syll'tab$''.t1s:4''tab$''.t2s:4''tab$''.s$''newline$'
               endif
               fileappend '.filename$' nucl'tab$''.nx1:4''tab$''.nx2:4''newline$'
               .type$ = "tstart"	; target at start of nucleus
            elsif (.i == .i2)
               .type$ = "tend"	; target at end of nucleus
               ; fileappend '.filename$' Ne'tab$''.nx2:4''newline$'
            else
               .type$ = "tin"	; target inside nucleus
            endif
            fileappend '.filename$' '.type$''tab$''.x:4''tab$''.y:1''tab$''.yST:1''newline$'
         endfor
      endif
   endfor
   @debug_msg: "store_stylization: exit"
endproc


procedure store_stylization_resampled paramID nucleiID .dx .filename$
# Store resampled stylization, as if it were a pitch curve, sampled at .dx (second)
# <nucleiID>	segmentation into nuclei, etc
   filedelete '.filename$'
   select paramID						; stylization (PitchTier)
   .np = Get number of points
 ; nrof targets in Pitch Tier
   .starttime = Get start time
   .endtime = Get end time
   .nx = floor((.endtime-.starttime)/.dx)			; nrof pitch samples in Pitch object
   fileappend '.filename$' File type = "ooTextFile"'newline$'
   fileappend '.filename$' Object class = "Pitch 1"'newline$''newline$'
   fileappend '.filename$' xmin = '.starttime''newline$'
   fileappend '.filename$' xmax = '.endtime''newline$'
   fileappend '.filename$' nx = '.nx''newline$'
   fileappend '.filename$' dx = '.dx''newline$'
   fileappend '.filename$' x1 = '.starttime''newline$'
   fileappend '.filename$' ceiling = 600'newline$'
   fileappend '.filename$' maxnCandidates = 1'newline$'
   fileappend '.filename$' frame []:'newline$'
   .i = 0							; frame index
   .t = .starttime
   while (.t < .endtime)
      .i += 1							; frame index for current time
      .value = 0						; default F0 (outside nuclei)
      selectObject: nucleiID						; segmentation into nuclei, etc
      .interv = Get interval at time... nucleus_tier .t
      .label$ = Get label of interval... nucleus_tier .interv
      .nx1 = Get starting point... nucleus_tier .interv		; time of start of nucleus
      .nx2 = Get end point... nucleus_tier .interv		; time of end of nucleus
      select paramID						; stylization (PitchTier)
      .i1 = Get low index from time... .t			; index of point in pitch tier >= .t
      if (not .i1 == undefined) and (.i1 > 0) 
         .t1 = Get time from index... .i1
         if (.i1 < .np)
            .i2 = .i1 + 1					; index of next point in pitch tier
            .t2 = Get time from index... .i2			; time of next point
         else
            .i2 = .i1
            .t2 = .nx2
         endif
         if (.label$ = "a" and .t >= .nx1 and .t <= .nx2 and .t2 <= .nx2)
            .y1 = Get value at index... .i1
            .y2 = Get value at index... .i2
            .value = .y1 + ((.y2-.y1)*(.t-.t1)/(.t2-.t1)) 
;fileappend '.filename$' ****** t='.t:4' i1='.i1' i2='.i2' t1='.t1:4' 'newline$'
;fileappend '.filename$' ****** t1='.t1:4' y1='.y1:1' t2='.t2:4' y2='.y2:1' v='.value:1' 'newline$'
         endif
      endif
      fileappend '.filename$' 'tab$'frame ['.i']:'newline$'
      fileappend '.filename$' 'tab$''tab$'intensity = 0'newline$'
      fileappend '.filename$' 'tab$''tab$'nCandidates = 1'newline$'
      fileappend '.filename$' 'tab$''tab$'candidate = []'newline$'
      fileappend '.filename$' 'tab$''tab$''tab$'candidate = [1]:'newline$'
      fileappend '.filename$' 'tab$''tab$''tab$''tab$'frequency = '.value:0''newline$'
      fileappend '.filename$' 'tab$''tab$''tab$''tab$'strength = 1'newline$'
      .t += .dx
   endwhile
endproc


procedure option_get .type$ .optname$ .varname$ .default$ .data$
# Extract an option value from a command string.
# <.type>	option type: {word|real|boolean} 
#			word: a string without whitespace
#			line: a string with whitespace
#			real: a number
#			boolean: a boolean, where TRUE is encoded by one of: y, Y, yes, 1, T, TRUE
# <.optname>	option name, e.g. "file=", "threshold="
# <.varname>	name of variable, where value of option will be stored
# <.default>	default option value, when option is not mentioned in command
# <.data>	command argument string, where options will be searched
   if (.type$ = "word")
      .s$ = extractWord$ (.data$, .optname$)
      '.varname$'$ = .default$
      if (length (.s$))
         '.varname$'$ = .s$
      endif
   elsif (.type$ = "line")
      .s$ = extractLine$ (.data$, .optname$)
      '.varname$'$ = .default$
      if (length (.s$))
         '.varname$'$ = .s$
      endif
   elsif (.type$ = "real")
      .v = extractNumber (.data$, .optname$)
      '.varname$' = '.default$'
      if not (.v == undefined)
         '.varname$' = .v
      endif
   elsif (.type$ = "boolean")
      .s$ = extractWord$ (.data$, .optname$)
      '.varname$' = 0		; default value
      if (length (.s$) < 1)
         .s$ = .default$
      endif
      if (index (":y:1:Y:yes:T:TRUE:", ":'.s$':") > 0)
         '.varname$' = 1
      endif
   endif
endproc


procedure option_check .type$ .optname$ .valids$ .value$
# Check whether the value of an option belongs to a set of accepted values
   if (.type$ = "word")
      if (index (.valids$, ":'.value$':") <= 0)
         @msg: "invalid value <'.value$'> for option <'.optname$'>"
      endif
   endif
endproc


procedure calculate_pitch: .cmda$
   @debug_msg: "calculate_pitch: entry" 
   @initialization_main
   call option_get word file= inputfname "" '.cmda$'
   call option_get real time_step= time_step 0.005 '.cmda$'
   call option_get real f0min= minimum_pitch 0 '.cmda$'
   call option_get real f0max= maximum_pitch 450 '.cmda$'

   if (variableExists ("corpus$") and variableExists ("corpus_home$"))
      if (variableExists ("corpus_subdir_sound$"))
         inputfname$ = corpus_home$ + corpus_subdir_sound$ + inputfname$
      else
         inputfname$ = corpus_home$ + inputfname$
      endif
   endif
   @files_get_regex: "filelistID", inputfname$
   selectObject: filelistID
   nrofFiles = Get number of strings
   if (nrofFiles == 0)
      @msg: "No input files found for <'inputfname$'>"
   endif
   task = task_calc_pitch
   for iFile to nrofFiles
      selectObject: filelistID
      .fname$ = Get string: iFile
      @initialization_per_file
      @construct_filenames: .fname$
      @calculate_pitch_int
      removeObject: soundID, pitchID
   endfor
   removeObject: filelistID
   @msg: "Batch command calculate_pitch: Ready"
   @debug_msg: "calculate_pitch: exit" 
endproc


# Batch command "prosogram_variants"
# arguments: option_name, default_value
#	file=		""		; input file of files specified by a regular expression
#	time_step=	0.005
#	f0min=		0		; lower value for F0 detection - f0min=0 selects auto pitch range detection
#	f0max=		450		; upper value for F0 detection
#	t1=			0		; start time of analysis
#	t2=			0		; end time of analysis
#	volatile=	no		; analysis of speech signal restricted to analysis time interval
#	segmentation=	optimal	; segmentation type, selected from: {optimal, vow-nucl, extern, int-BP, asyll, rhyme, syll+vow, syll, voiced}
#	g=					; glissando threshold; default selects adaptive threshold 0.16 + 0.32
#	dg=			30		; differential glissando threshold
#	dmin=		0.035	; minimum duration of tonal segment
#	draw=		yes		; draw Prosogram in Graphics window and write graphics files
#	time_incr=	3.0		; duration of prosogram pane
#	wide=		yes		; selects wide size for Prosogram, otherwise compact size 
#	rich=		yes		; selects rich format, otherwise light format
#	pitchrange=	yes		; plot pitch range
#   targets=	0		; draw pitch targets in ST (=1) or in Hz (=2)
#	tiers=		*1,2,3	; tiers shown in the graphics output; tiers are specified by name or number (no whitespace allowed); * indicates SAMPA to IPA conversion.
#	outputfile=	""		; filename of graphics file (without filename extension). Zero-length string selects corpus-defined path and numbering in filename. 
#	outputformat=EPS	; format of graphics file, selected from: {EPS, EMF, PNG300, PNG600, JPG300, JPG600}
#	number=		yes		; number graphics files (path+basename+number+extension) 
#	collect=	no		; collect output of duration calculation in a single output file
#	save=		no		; save intermediate data in files
#	cleanup=	yes		; cleanup data after processing
#	settings=	yes		; show settings (segmentation type, thresholds) in Prosogram
#	x_scale=	yes		; show X scale (numbers on axis)
#	y_scale=	yes		; show Y scale (numbers on axis)
#	y_scale_r=	yes		; show Y scale (values in Hz on right axis)
#   pauses=		yes		; show pauses (by "P")
#	tg_bound=	yes/no	; show vertical boundaries of tier intervals in textgrid (default is yes in rich mode and no in light mode
#	portee=		yes		; show portee (horizontal ST calibration lines)
#	traject=	no		; show intrasyllab up/down and intersyllab pitch intervals
#	hesit=		no		; show hesitations
#	hesit_method=phon+pros	; set method for hesitation detection
#	rhythm=		no		; show rhythm


procedure prosogram_variants: .cmda$
; @logging: "reset debug timed", "_log.txt"
   @debug_msg: "prosogram_variants: entry, command=<'.cmda$'>"
   @initialization_main
   task = task_prosogram
   auto_pitchrange = 1		; automatic pitch range adjustment in plot
   ; draw_pitch_target_values = 0
   outputmode$ = "Fill"
   call option_get word file= inputfname "" '.cmda$'
   call option_get real time_step= time_step 0.005 '.cmda$'
   call option_get real f0min= minimum_pitch 0 '.cmda$'
   call option_get real f0max= maximum_pitch 450 '.cmda$'
   call option_get real t1= pmf_t1 0 '.cmda$'
   call option_get real t2= pmf_t2 0 '.cmda$'
   call option_get real time_incr= timeincr 3.0 '.cmda$'
   call option_get word segmentation= segmentation_name optimal '.cmda$'
   call option_check word segmentation :optimal:vow-nucl:extern:loudness:int-BP:asyll:rhyme:syll+vow:syll:voiced: 'segmentation_name$'
   call option_get real g= glissando -0.32 '.cmda$'
   call option_get real dg= diffgt 30 '.cmda$'
   call option_get real dmin= mindur_ts 0.035 '.cmda$'
   call option_get boolean wide= wide yes '.cmda$'
   call option_get boolean rich= rich yes '.cmda$'
   if (rich)
      tg_bound_default$ = "yes"
      show_intensity = 1
   else
      tg_bound_default$ = "no"
      show_intensity = 0
   endif
   call option_get word tiers= tiers_to_show *1,2,3 '.cmda$'
   call option_get boolean collect= collect_output no '.cmda$'
   call option_get word outputfile= output_filename "" '.cmda$'
   call option_get word outputsuffix= output_suffix "" '.cmda$'
   call option_get word outputformat= output_format EPS '.cmda$'
   call option_check word outputformat :EPS:EMF:PNG300:PNG600:JPG300:JPG600: 'output_format$'
   output_format$ = replace_regex$ (output_format$, "PNG(\d+)", "PNG \1 dpi", 1)
   call option_get boolean number= file_numbering yes '.cmda$'
   call option_get real jpegq= jpegq 75 '.cmda$'
   call option_get boolean save= save_intermediate_data no '.cmda$'
   call option_get boolean polytonia= polytonia_annotation no '.cmda$'
   call option_get boolean boundaries= boundary_annotation no '.cmda$'
   call option_get boolean cleanup= do_cleanup yes '.cmda$'
   if (glissando < 0) 
      adaptive_glissando = 1
      glissando = abs(glissando)
   else
      adaptive_glissando = 0
   endif
   glissando_low = 0.16
   if (wide)
      viewsize$ = "wide"
   else
      viewsize$ = "compact"
   endif
   if (variableExists ("corpus_home$"))
      if (variableExists ("corpus_subdir_sound$"))
         inputfname$ = corpus_home$ + corpus_subdir_sound$ + inputfname$
      else
         inputfname$ = corpus_home$ + inputfname$
      endif
   endif

   @debug_msg: "prosogram_variants: inputfile='inputfname$' t1='pmf_t1' t2='pmf_t2' time_step='time_step:3' segmentation_name='segmentation_name$' wide='wide' rich='rich' adapative='adaptive_glissando'"
   @debug_msg: "prosogram_variants: outputfile='output_filename$'"

   @files_get_regex: "filelistID", inputfname$
   selectObject: filelistID
   nrofFiles = Get number of strings
   if (nrofFiles == 0)
      @msg: "No input files found for <'inputfname$'>"
   endif
   ; @task_flags and @initialization_multiple_files initialize various default settings, which may be overridden by optional settings in batch command
   @task_flags
   if (save_intermediate_data)
      needs_prosodic_profile = 1
   endif

   @initialization_multiple_files
   ; The following lines override some variables initialized by @task_flags and @initialization_multiple_files
   call option_get boolean volatile= volatile no '.cmda$'
   call option_get boolean pitchrange= show_pitchrange no '.cmda$'
   if (task == task_pitch_plot)
      call option_get boolean intensity= show_intensity no '.cmda$'
   endif
   if (variableExists ("show_pitchrange"))
      if (show_pitchrange)
         needs_pitchrange = 1
      endif
   endif
   @debug_msg: "prosogram_variants: show_pitchrange='show_pitchrange' needs_pitchrange='needs_pitchrange'"

   call option_get boolean draw= draw_prosograms yes '.cmda$'
   call option_get boolean portee= show_portee yes '.cmda$'
   call option_get boolean tg_bound= show_tg_boundaries 'tg_bound_default$' '.cmda$'
   call option_get boolean settings= show_settings yes '.cmda$'
   call option_get boolean x_scale= show_x_scale yes '.cmda$'
   call option_get boolean y_scale= show_y_scale yes '.cmda$'
   call option_get boolean y_scale_r= show_y_scale_r yes '.cmda$'
   call option_get boolean tiernames= show_tiernames yes '.cmda$'
   @debug_msg: "prosogram_variants: show_tiernames='show_tiernames'"
   call option_get boolean pauses= show_pauses no '.cmda$'
   call option_get real targets= draw_pitch_target_values 0 '.cmda$'
   call option_get boolean traject= show_trajectories no '.cmda$'
   call option_get boolean grey= greyscale no '.cmda$'
   call option_get boolean hesit= show_hesitations no '.cmda$'
   call option_get boolean hesit_anno= hesitation_annotation no '.cmda$'
   call option_get word hesit_method= hesitation_method automatic '.cmda$'
   call option_get boolean rhythm= show_rhythm no '.cmda$'
   @debug_msg: "prosogram_variants: wide='wide' rich='rich' pitchrange='show_pitchrange' adapative='adaptive_glissando'"
   for iFile to nrofFiles		; both are global variables ; see process_one_input_file
      selectObject: filelistID
      .fname$ = Get string: iFile
      @debug_msg: "prosogram_variants: pmf_t1='pmf_t1:4', pmf_t2='pmf_t2:4', fname='.fname$'"
      @process_one_input_file: pmf_t1, pmf_t2, .fname$
   endfor
   @cleanup_global
   removeObject: filelistID
   @debug_msg: "prosogram_variants: exit"
endproc


procedure prosogram: .cmda$
   @msg: " 'newline$'Batch command prosogram"
   @prosogram_variants: .cmda$
   @msg: "Batch command prosogram: Ready"
endproc


procedure polytonia: .cmda$
   @msg: " 'newline$'Batch command polytonia"
   @prosogram_variants: .cmda$ + " polytonia=yes"
   @msg: "Batch command polytonia: Ready"
endproc


procedure detect_boundaries: .cmda$
   @msg: " 'newline$'Batch command detect_boundaries"
   @prosogram_variants: .cmda$ + " boundaries=yes"
   @msg: "Batch command detect_boundaries: Ready"
endproc


procedure hesitations: .cmda$
   @msg: " 'newline$'Batch command detect_hesitations"
   @prosogram_variants: .cmda$ + " hesit_anno=yes"
   @msg: "Batch command detect_hesitations: Ready"
endproc


procedure plot_pitch: .cmda$
   @msg: " 'newline$'Batch command plot_pitch"
   task = task_pitch_plot
   @prosogram_variants: .cmda$
   @msg: "Batch command plot_pitch: Ready"
endproc


procedure prominence: .cmda$
   task = task_prosogram
   needs_loudness = 1
   calc_prominence = 1
   show_prominence = 1
   @prosogram_variants: .cmda$
   calc_prominence = 0
   show_prominence = 0
   @msg: "Batch command prominence: Ready"
endproc


procedure segmentation: .cmda$
   @debug_msg: "segmentation: entry, command=<'.cmda$'>" 
   @initialization_main
   call option_get word file= inputfname "" '.cmda$'
   call option_get real time_step= time_step 0.005 '.cmda$'
   call option_get real t1= pmf_t1 0 '.cmda$'
   call option_get real t2= pmf_t2 0 '.cmda$'
   call option_get real left= trimleft 0 '.cmda$'
   call option_get real right= trimright 0 '.cmda$'
   call option_get word method= segmentation_name asyll '.cmda$'
   call option_check word method :asyll:specsim:pitchchange:pitchterrace: 'segmentation_name$'
   call option_get word output_suffix= output_suffix "" '.cmda$'
   call option_get boolean save_BP= save_BP no '.cmda$'
   if (variableExists ("corpus_home$"))
      if (variableExists ("corpus_subdir_sound$"))
         inputfname$ = corpus_home$ + corpus_subdir_sound$ + inputfname$
      else
         inputfname$ = corpus_home$ + inputfname$
      endif
   endif
   task = task_segmentation
   @map_segmentation_type: segmentation_name$
   @debug_msg: "segmentation: segmentation_name='segmentation_name$'"

   @files_get_regex: "filelistID", inputfname$
   selectObject: filelistID
   nrofFiles = Get number of strings
   if (nrofFiles == 0)
      @msg: "No input files found for <'inputfname$'>"
   endif
   @task_flags
   @initialization_multiple_files
   ; The following lines override some variables initialized by @task_flags and @initialization_multiple_files
   if (segmentation_name$ == "asyll")
      call option_get real fc_low= fc_low 300 '.cmda$'
      call option_get real fc_high= fc_high 3500 '.cmda$'
   endif
   call option_get real mindiff= mindiff 3 '.cmda$'
   call option_get real leftdiff= diff_left 2 '.cmda$'
   if (segmentation_name$ == "pitchchange")
      call option_get real threshold= pc_threshold 2 '.cmda$'
   endif
   if (segmentation_name$ == "pitchterrace")
      call option_get real threshold= pc_threshold 2 '.cmda$'
      call option_get real mindur= mindur_terrace 0 '.cmda$'
   endif
   volatile = 1             ; avoid reading and overwriting intensity of BP filtered speech
   @msg: " 'newline$'Batch command segmentation: method='segmentation_name$' fc_low='fc_low:0' fc_high='fc_high:0' mindiff='mindiff' leftdiff='diff_left' output_suffix=<'output_suffix$'>"
   for .iFile to nrofFiles
      selectObject: filelistID
      .fname$ = Get string: .iFile
      @msg: " 'newline$'Batch command segmentation: Process file <'.fname$'>"
      @process_one_input_file: .iFile, pmf_t1, pmf_t2, .fname$
   endfor
   @cleanup_global
   removeObject: filelistID
   @msg: "Batch command segmentation: Ready"
endproc


procedure calculate_duration_data: .cmda$
   @initialization_main
   ; Specify the filename as TextGrid files
   call option_get word file= filespec "" '.cmda$'
   call option_get word unit= unit "sound" '.cmda$'
   ; Collect output = collect results of multiple input files in one output file
   call option_get boolean collect= collect_output yes '.cmda$'
   @files_get_regex: "filelistID", filespec$
   selectObject: filelistID
   .nfiles = Get number of strings
   if (.nfiles == 0)
      call msg No input files found for <'filespec$'>
   endif
   ; prepare default for outputfile
      @fname_parts: filespec$
      indir$ = result4$
      .s$ = indir$ + "_dur_'unit$'.txt"
      call option_get word out= outfname "'.s$'" '.cmda$'
   call msg calculate_duration_data: filespec='filespec$', '.nfiles' files, collect='collect_output', outfname='outfname$'
   for .j to .nfiles
      selectObject: filelistID
      .fname$ = Get string: .j
      @fname_parts: .fname$
      basename$ = result2$
      ;.fname$ = indir$ + basename$ + ".TextGrid"
      polytonia_file$ = indir$ + basename$ + "_polytonia.TextGrid"
      @msg: "file='.j'  <'.fname$'> out='outfname$' polytonia='polytonia_file$'"
      dataID = Read from file: .fname$
      @corpus_conversion: 1, "dataID", corpus$
      if (unit$ = "sound")
         .f = .j
         if (not collect_output)
            .f = 1
         endif
         @duration_data_from_textgrid: dataID, .f, unit$, outfname$, basename$
      endif
      removeObject: dataID
   endfor
   removeObject: filelistID
   @msg: "Batch command Ready"
endproc


procedure convert_eps .cmda$
# Assumes Ghostscript, nconvert and pdftk are installed.
# nconvert is required when converting to GIF format. http://www.xnview.com/
# pdftk is required when creating multipage PDF files. http://www.pdflabs.com/tools/pdttk-the-pdf-toolkit/
; Specify the filename as eps files
   call option_get word file= filespec "*.eps" '.cmda$'
   call fname_parts 'filespec$'
   basename$ = result2$
   indir$ = result4$
   call option_get word outfname= outfname "'basename$'.pdf" '.cmda$'
   call option_get word format= graphics_format "JPG" '.cmda$'
   call option_check word graphics_format :PNG:GIF:JPG:PDF:multipage_PDF: 'graphics_format$'
   call option_get word resolution= resolution "300" '.cmda$'
   call option_check word resolution :72:96:120:300:600: 'resolution$'
   call option_get word outdir= outdir "<same_as_input>" '.cmda$'
   if (outdir$ = "<same_as_input>")
      outdir$ = indir$
   else
      call fname_parts 'outdir$'
      outdir$ = result4$
   endif
   Create Strings as file list... filelist 'filespec$'
   .nfiles = Get number of strings
   pdf_files$ = ""
   call msg convert_eps: filespec='filespec$', '.nfiles' files, outdir='outdir$'
   call msg convert_eps: format='graphics_format$'
   eps_pdf$ = path_ghostscript$ + " -dNOPAUSE -dBATCH -dQUIET -sDEVICE=pdfwrite"
   eps_jpg$ = path_ghostscript$ + " -dNOPAUSE -dBATCH -dQUIET -sDEVICE=jpeg -sEPSCrop"
   eps_png$ = path_ghostscript$ + " -dNOPAUSE -dBATCH -dQUIET -sDEVICE=png16 -sEPSCrop"
   png_gif$ = path_nconvert$
   for .j to .nfiles
      select Strings filelist
      .fname$ = Get string... .j
      call fname_parts '.fname$'
      basename$ = result2$
      pdf_files$ = pdf_files$ + "'basename$'.pdf "
      src$ = indir$ + basename$
      dst$ = outdir$ + basename$
      res = 'resolution$'
      if (graphics_format$ = "GIF")
         system 'eps_png$' -r'res' -sOutputFile="'dst$'.png" "'src$'.eps"
         system 'png_gif$' -quiet -out gif 'dst$'.png
         filedelete 'dst$'.png
      elsif (graphics_format$ = "PNG")
         system 'eps_png$' -r'res' -sOutputFile="'dst$'.png" "'src$'.eps"
      elsif (index (graphics_format$, "PDF"))	; PDF or multipage PDF
         if (res == 96 or res == 120)
            call msg Invalid resolution for PDF. Using 300 dpi.
            res = 300
         endif
;printline command= system 'eps_pdf$' -r'res' -sOutputFile="'dst$'.pdf" "'src$'.eps"
         system 'eps_pdf$' -r'res' -sOutputFile="'dst$'.pdf" "'src$'.eps"
      elsif (graphics_format$ = "JPG")
         if (res == 96 or res == 120)
            call msg Invalid resolution for JPEG. Using 300 dpi.
         endif
         system 'eps_jpg$' -r'res' -sOutputFile="'dst$'.jpg" "'src$'.eps"
      endif
   endfor
   if (index (graphics_format$, "multipage"))
; use wildcard expansion by pdftk in order to avoid command line buffer overflow
      ;pdf_out$ = replace_regex$ (filespec$, "[0-9\*]*\.eps", "\.pdf", 1)
      pdf_out$ = outdir$ + "/" + outfname$
      pdf_out$ = replace$ (pdf_out$, "//", "/", 0)
      pdf_in$ = replace_regex$ (filespec$, "\.eps", "\.pdf", 1)
printline convert_eps: system pdftk 'pdf_in$' cat output 'pdf_out$'
      system pdftk 'pdf_in$' cat output 'pdf_out$'
   endif
   select Strings filelist
   Remove
   call msg Batch command convert_eps Ready
endproc


procedure corpus_open: .cmda$
   @initialization_main
; Load signal files and corresponding TextGrid files. Specify filenames as wav files regular expression.
   call option_get word file= inputfname ".*.wav$" '.cmda$'

   if (variableExists ("corpus_home$"))
      if (variableExists ("corpus_subdir_sound$"))
         inputfname$ = corpus_home$ + corpus_subdir_sound$ + inputfname$
      else
         inputfname$ = corpus_home$ + inputfname$
      endif
   endif
   @files_get_regex: "filelistID", inputfname$
   selectObject: filelistID
   .nrofFiles = Get number of strings
   if (.nrofFiles == 0)
      @msg: "No input files found for <'inputfname$'>"
   endif
   for .j to .nrofFiles
      selectObject: filelistID
      .fname$ = Get string: .j
      @fname_parts: .fname$
      .basename$ = result2$
      Open long sound file: .fname$
      .dur = Get total duration
      @msg: "'.basename$''tab$''.dur:3'"
      .tg$ = .basename$ + ".TextGrid"         
      if (variableExists ("corpus_home$"))
         .tg$ = corpus_home$ + .basename$ + ".TextGrid"
         if (variableExists ("corpus_subdir_tg$"))
            .tg$ = corpus_home$ + corpus_subdir_tg$ + .basename$ + ".TextGrid"
         endif
      endif
      if (fileReadable (.tg$))
         Read from file: .tg$
      endif
   endfor
   removeObject: filelistID
   @msg: "Batch command corpus_open Ready"
endproc


procedure validate_tier: .cmda$
   @initialization_main
   call option_get word file= inputfname ".*.TextGrid" '.cmda$'
   ; call option_get word tiertype= tiertype "phoneme" '.cmda$'

   if (variableExists ("corpus$") and length (corpus$) and variableExists ("corpus_home$"))
      if (variableExists ("corpus_subdir_tg$"))
         inputfname$ = corpus_home$ + corpus_subdir_tg$ + inputfname$
      else
         inputfname$ = corpus_home$ + inputfname$
      endif
   endif
   @files_get_regex: "filelistID", inputfname$
   selectObject: filelistID
   .nrofFiles = Get number of strings
   if (.nrofFiles == 0)
      @msg: "No input files found for <'inputfname$'>"
   endif
   for .j to .nrofFiles
      selectObject: filelistID
      .tg$ = Get string: .j
      if (fileReadable (.tg$))
         @validate_phoneme_tier: .tg$
      endif
   endfor
   removeObject: filelistID
   @msg: "Batch command validate_tier Ready"
endproc


procedure find_nucleus: .type$, .t1, .t2, .forward
; Find nucleus of given <.type$> in time range <.t1>..<.t2>
; <.type$>		"a" = valid_nucleus, "-" = rejected nucleus
; <.forward>	search from .t1 to .t2, otherwise from .t2 to .t1
; Return index of nucleus (in nucleus_tier) in variable <result> or 0 if not found
   result = 0
   selectObject: nucleiID
   .endt = Get end time
   .startt = Get start time
   .endt = min (.endt, .t2)
   .startt = max (.startt, .t1)
   if (.forward)
      .t = .t1
   else
      .t = .t2 - 0.002
   endif
   while ((.forward and .t < .endt) or (.forward == 0 and .t > .startt) and result == 0)
      .i = Get interval at time: nucleus_tier, .t
      .s$ = Get label of interval: nucleus_tier, .i
      if (.s$ == .type$)
         result = .i
      endif
      if (.forward)
         .t = Get end time of interval: nucleus_tier, .i
      else
         .t = .startt
         if (.i > 1)
            .t = Get start time of interval: nucleus_tier, .i-1
         endif
      endif
   endwhile
endproc


procedure detect_hesitations: .t1a, .t2a, .method$
# Detect hesitations, either from hesitation tier in TextGrid of from acoustic cues and phoneme identity.
# <.method>
#	"phon+pros"		use phoneme label and prosodic features (phon_dur >= 0.3 && intrasyll <= 0 && intrasyll > -3 ST)
#					phoneme is "\oe", "\o/", "9", "@" or "2"
#	"words"			use word tier from annotation TextGrid, where hesitations are labeled "euh" or "eh"
#	"none"			don't attempt detection
#	(not yet) "annotation"	use hesitation tier from annotation TextGrid, where a hesitation is marked as "H" 
   @debug_msg: "detect_hesitations: entry, method='.method$'"

   if segfile_available and (.method$ = "phon+pros" or .method$ = "words")
      @intervals_from_time_range: nucleiID, nucleus_tier, .t1a, .t2a, "first_interval", "last_interval"
      for .j from first_interval to last_interval
         selectObject: nucleiID
         @is_nucleus: .j
         if (result)
            .hesit = 0						; default = no hesitation detected
            .t1 = Get start time of interval: nucleus_tier, .j
            .t2 = Get end time of interval: nucleus_tier, .j
            .tmid = .t1 + (.t2-.t1)/2
            if (.method$ = "phon+pros" and phones_available)
               @interval_from_time: nucleiID, phone_tier, .tmid, "i"
               .label$ = Get label of interval: phone_tier, i
               .px1 = Get start time of interval: phone_tier, i
               .px2 = Get end time of interval: phone_tier, i
               .phon_dur = .px2-.px1
               .s$ = Get label of interval: pointer_tier, .j
               .pj = '.s$'
               selectObject: nucldatID
               .intST = Get value: .pj, j_intrasyl
               if (index ("=\o/=\oe=9=2=@=", "='.label$'=") 
                  ... and .phon_dur >= 0.3 and .intST <= 0 and abs(.intST) < 3) 
                  .hesit = 1
               endif
            elsif (.method$ = "words" and word_tier_in > 0)
               @interval_from_time: segmentationID, word_tier_in, .tmid, "jw"
               .label$ = Get label of interval: word_tier_in, jw
               if (index ("=euh=eh=", "='.label$'=")) 
                  .hesit = 1
               endif
            elsif (.method$ = "annotation" and hesitation_available)
            endif
            if (.hesit)
               @msg: "detect_hesitations: hesitation detected at '.t1:3', method='.method$'"
               @interval_from_time: nucleiID, hesitation_tier, .tmid, "i"
               Set interval text: hesitation_tier, i, "H"
            endif
         endif
      endfor
      hesitation_available = 1
   endif

   ; Copy info in hesitation tier to column of nucldatID
   selectObject: nucldatID
   .nrows = Get number of rows
   for .j to .nrows
      .t1n = Get value: .j, j_nucl_t1
      .t2n = Get value: .j, j_nucl_t2
      selectObject: nucleiID
      .i = Get interval at time: hesitation_tier, .t1n+(.t2n-.t1n)/2
      .label$ = Get label of interval: hesitation_tier, .i
      .hesit = 0
      if (.label$ = "H")
         .hesit = 1
      endif
      selectObject: nucldatID
      Set value: .j, j_hesitation, .hesit
   endfor
   @debug_msg: "detect_hesitations: exit"
endproc


procedure process_cprom: .grid
   @tier_number_by_name: .grid, "^ss$"
   if (result)
      Remove tier: result
   endif
   @tier_number_by_name: .grid, "phones"
   if (result)	; Remove labels for pauses ("_"), respirations ("*"), etc. "+"
      Replace interval text... result 0 0 "[\+\*_#%]" "" Regular Expressions
   endif
   @tier_number_by_name: .grid, "syll"
   .syll_tier = result
   if (.syll_tier)	; Simplify complex labels for pauses, respirations, etc. to "_"
      Replace interval text... .syll_tier 0 0 "[\+\*_#]+" "_" Regular Expressions
   endif
; Create prominence and hesitation tiers from "delivery" tier in TextGrid <.grid>
   if (0)
      @tier_number_by_name: .grid, "delivery"
      if (result)
         .deliv_tier = result
         .nt = Get number of tiers
         .prom_tier = .nt + 1
         .hes_tier = .nt + 2
         .ni = Get number of intervals: .deliv_tier
         Duplicate tier: .syll_tier, .prom_tier, "prom"
         Duplicate tier: .syll_tier, .hes_tier, "hes"
         for .j to .ni
            .label$ = Get label of interval: .deliv_tier, .j
            if (index (.label$, "P"))
              .s$ = "S"
            elsif (index (.label$, "p"))
              .s$ = "W"
            else
              .s$ = "0"
            endif
            Set interval text: .prom_tier, .j, .s$
            if (index (.label$, "z"))
              .s$ = "H"
            else
              .s$ = ""
            endif
            Set interval text: .hes_tier, .j, .s$
         endfor
      endif
   endif
   @tier_number_by_name: .grid, "prom"
   if (result)	; Remove or rename labels
      Replace interval text... result 0 0 "0" "" Regular Expressions
      Replace interval text... result 0 0 "W" "p" Regular Expressions
      Replace interval text... result 0 0 "S" "P" Regular Expressions
   endif
   @tier_number_by_name: .grid, "rg"
   if (result)	; Remove labels
      Replace interval text... result 0 0 ".*" "" Regular Expressions
   endif
endproc


procedure lpa_sampa: .grid, .tiername$
# Convert LPA (Limsi Phonetic Alphabet) to SAMPA
   @tier_number_by_name: .grid, .tiername$
   .tier = result
   if (result)
      ; first 3 lines deal with pause, incomprehensible sound and respiration
         Replace interval texts: .tier, 0, 0, "\.", "_", "Regular Expressions"
         Replace interval texts: .tier, 0, 0, "\*", "", "Regular Expressions"
         Replace interval texts: .tier, 0, 0, "H", "", "Regular Expressions"
      ; next line deals with intervals containing several non speech elements
         Replace interval texts: .tier, 0, 0, "_+", "_", "Regular Expressions"
         Replace interval texts: .tier, 0, 0, "@", "2", "Regular Expressions"
         Replace interval texts: .tier, 0, 0, "x", "@", "Regular Expressions"
         Replace interval texts: .tier, 0, 0, "h", "H", "Regular Expressions"
         Replace interval texts: .tier, 0, 0, "I", "e~", "Regular Expressions"
         Replace interval texts: .tier, 0, 0, "O", "o~", "Regular Expressions"
         Replace interval texts: .tier, 0, 0, "A", "a~", "Regular Expressions"
         Replace interval texts: .tier, 0, 0, "c", "O", "Regular Expressions"
         Replace interval texts: .tier, 0, 0, "X", "9", "Regular Expressions"
         Replace interval texts: .tier, 0, 0, "r", "R", "Regular Expressions"
         Replace interval texts: .tier, 0, 0, "N", "J", "Regular Expressions"
   endif      
endproc


procedure tier_trim_nucleus: .grid, .tier, .dtleft, .dtright
; Change boundaries of nucleus <.tier>, trimming left and right by amount <.dtleft> and <.dtright>, when duration remaining nucleus > 0.1 s
   @debug_msg: "tier_trim_nucleus: entry, .tier='.tier', .dtleft='.dtleft:3'"
   selectObject: .grid
   .n = Get number of tiers
   if (.tier <= .n)
      .ni = Get number of intervals: .tier
	  .i = 1
	  while (.i <= .ni)
         .s$ = Get label of interval: .tier, .i
         if (.s$ = "a")
            .t1 = Get starting point: .tier, .i
            .t2 = Get end point: .tier, .i
            if (.dtleft > 0 and .t2-.t1 > .dtleft)			; left side
               .t = min (.t1 + .dtleft, .t2 - 0.1)
               if (.t > .t1)
                  Set interval text: .tier, .i, ""
                  Insert boundary: .tier, .t
                  Set interval text: .tier, .i+1, .s$
                  .ni += 1
                  .t1 = .t 
                  .i += 1
               endif
            endif
            if (.dtright > 0 and .t2-.t1 > .dtright)		; right side
               .t = max (.t1 + 0.1, .t2 - .dtright)
               if (.t < .t2)
                  Set interval text: .tier, .i, ""
                  Insert boundary: .tier, .t
                  Set interval text: .tier, .i, .s$
                  .ni += 1
                  .i += 1
               endif
            endif
         endif
         .i += 1
	  endwhile
   endif
endproc


procedure textgrid_disable_textstyle: .gridID
   selectObject: .gridID
   .n = Get number of tiers
   for .tier to .n
      @tier_disable_textstyle: .gridID, .tier
   endfor
endproc


procedure tier_disable_textstyle: .gridID, .tier
   selectObject: .gridID
   .n = Get number of intervals: .tier
   for .interval to .n
      .label$ = Get label of interval: .tier, .interval
      .label$ = replace$ (.label$, "_", "\_ ", 0)			; avoid interpretation of underscore as subscript 
      .label$ = replace$ (.label$, "%", "\% ", 0)			; avoid interpretation of percent as italic text style
      Set interval text: .tier, .interval, .label$
   endfor
endproc


procedure tier_merge_intervals_except: .gridID, .tier, .except$
; Merge adjacent intervals other than those with label <.except$>
   selectObject: .gridID
   .n = Get number of intervals: .tier
   .i = 1
   while (.i < .n)
      .label1$ = Get label of interval: .tier, .i
      .label2$ = Get label of interval: .tier, .i+1
      if (.label1$ <> .except$ and .label2$ <> .except$)
         Remove right boundary: .tier, .i
         ; Set interval text: .tier, .i, ""
         .n -= 1
      else
         .i += 1
      endif
   endwhile
endproc

