# prosoplot.praat ---
# Praat include file containing procedures for plotting parameters related to prosody
# This file is included (indirectly) by prosogram.praat. It isn't a stand-alone script. Use prosogram.praat instead.
# Author: Piet Mertens
# Last modification: 2019-12-03

# Procedure hierarchy
# gr_init							initialize some global variables for graphics modes
# gr_start_demowin					initialize for Praat Demo window
# gr_start_picturewin				initialize for Praat Picture window
# gr_run_demowin					start running interactive window
# gr_redraw							redraw demo window contents: buttons, prosogram, textgrid, time bar
#   gr_display_prosogram
#     gr_plot_textgrid
#     gr_draw_tiernames
#     gr_garnish					draw border, time marks, pitch scales...
#     gr_draw_portee
#       gr_draw_calib				draw horizontal calibration line
#     gr_draw_octavejump
#     gr_plot_param_ST				plot f0 on ST scale
#     gr_plot_param_scaled			plot intensity on dB scale
#     gr_plot_vuv					plot voicing decision
#     gr_plot_nuclei				plot syllabic nuclei
#     gr_plot_pitchrange			plot pitch range
#     gr_plot_feature				plot feature (a categorical or a numerical variable) of a syllabic nucleus, as a symbol or a number
#     gr_plot_prop_as_shape			plot property (a numerical variable) of a syllabic nucleus, as a shape
#     gr_plot_prop_curve			plot property (a numerical variable) of a syllabic nucleus, as a curve 
#     gr_plot_PitchTier_clip_nuclei plot stylization for syllabic nuclei with or without clipping
#       gr_plot_stylisation         plot stylization for syllabic nuclei
#   gr_display_textgrid_pure		plot textgrid in demo window mode
#   gr_draw_buttons
#     gr_draw_button
# gr_write_all_prosograms			plot and save all prosogram in a file or set of files
#   gr_first_viewport_of_page		initialize viewport for Picture window and Postscript bounding box
#     gr_viewport_size				set outer and inner viewport
#   gr_write_prosogram				save Praat picture window with prosogram(s) to disk file
#     gr_next_viewport


demowin = 0				; start in Picture window mode


procedure gr_init
; Initialize some global variables for graphics modes
   show_octavejump = 1	; show symbol at times of F0 discontinuity (typical of octave jump)
   plot_f0_on_top = 0	; plot f0 on top of stylization, rather than stylization on top of F0
; Start counters for files and pages
   file_stamp = 1		; Show input speech filename on prosogram
   plot_version = 1		; Show version number
   file_numbering = 1	; automatic numbering of graphics files 
   file_ctr = 1			; counter for graphics files
   nrof_pages = 0
   ; Font size affects drawing of TextGrid!
   default_fontsize = 10
   button_fontsize = 10	; used for button text in demo window
   msg_fontsize = 10	; used for messages

; Greyscales
   grey$ = "Grey"
   red$ = "Red"
   green$ = "Green"
   blue$ = "Blue"
   cyan$ = "Cyan"
   purple$ = "Purple"
   magenta$ = "Magenta"
   pink$ = "Pink"
   purple$ = "Purple"
   if (greyscale)
      red$ = "Black"
      green$ = "Black"
      blue$ = "Black"
      cyan$ = "Grey"
      purple$ = "Grey"
      magenta$ = "Grey"
      pink$ = "Grey"
      purple$ = "Grey"
   endif
endproc


procedure gr_start_picturewin
   @gr_init
   demowin = 0
   win$ = ""
   small_fontsize = 6	; used for file stamp
   tiny_fontsize = 4	; used for version stamp
   'font_family$'
   stylization_linewidth = 7
   pitchrange_linewidth = 2
   Font size: fontsize
endproc


procedure gr_start_demowin: .filename$
   @gr_init
   demowin = 1
   win$ = "demo "
   .s$ = "Interactive 'version$' - Corpus file: '.filename$'"
   demoWindowTitle (.s$)
   grid_in_prosogram = 0
   small_fontsize = 8
   tiny_fontsize = 6
   'win$''font_family$'
   'win$'Font size: fontsize
   stylization_linewidth = 3
   pitchrange_linewidth = 2
   'win$'Black
   # The size of the Demo window is (0..100, 0..100), with (0,0) the lower left corner.
   'win$'Select outer viewport: 0, 100, 0, 100
   'win$'Select inner viewport: 0, 100, 0, 100
   'win$'Axes: 0, 100, 0, 100
   gr_text_maxlines = 40
   @gr_clearscreen
endproc


procedure gr_printline: .text$
   if (demowin)
      .yd = 100/gr_text_maxlines 
      .xt = 1
      if (gr_text_linenr >= gr_text_maxlines)	; max number of lines already used
         for j from 1 to gr_text_maxlines-1		; shift lines in msg text buffer and print again
            gr_textbuf'j'$ = gr_textbuf'j+1'$
            .s$ = gr_textbuf'j'$
            .s$ = replace$ (.s$, "_", "\_ ", 0)
            .xt2 = Text width (wc): .s$
            'win$'Paint rectangle: "White", .xt, .xt2, .yt, .yt+.yd
            .yt = 100 - (j * .yd) 
            'win$'Text special: .xt, "Left", .yt, "Bottom", "Helvetica", msg_fontsize, "0", .s$
         endfor
         gr_text_linenr = gr_text_maxlines
      endif
      .yt = 100 - (gr_text_linenr * .yd) 
      .s$ = replace$ (.text$, "_", "\_ ", 0)
      'win$'Text special: .xt, "Left", .yt, "Bottom", "Helvetica", msg_fontsize, "0", .s$
      gr_textbuf'gr_text_linenr'$ = .text$
      gr_text_linenr += 1
   else
      @msg: .text$
   endif
endproc


procedure gr_clearscreen
# Only used in demowin
   if (demowin)
      'win$'Erase all
      for .j from 1 to gr_text_maxlines
         gr_textbuf'.j'$ = ""
      endfor
      gr_text_linenr = 1
   endif
endproc


procedure gr_clear_rectangle: .x1, .x2, .y1, .y2
   if (demowin)
      'win$'Select inner viewport: 0, 100, 0, 100
      'win$'Axes: 0, 100, 0, 100
      'win$'Paint rectangle: "White", .x1, .x2, .y1, .y2
   endif
endproc


procedure gr_run_demowin: anal_t1, anal_t2, .timeincr, ySTmin, ySTmax
# <anal_t1>..<anal_t2>	analysed interval of signal, may be part of full signal duration.
# The size of the Demo window is (0..100, 0..100), with (0,0) the lower left corner.
# Define viewport for prosogram (below buttons, without TextGrid)
   dw_ovx1 = 0						; outer viewport = area for prosogram (without textgrid) including margins
   dw_ovx2 = 100
   dw_ovy1 = 60						; lower side
   dw_ovy2 = 91						; upper side
   dw_margin = 5					; margin size is relative to dimension of demo window
   dw_vx1 = dw_ovx1 + dw_margin		; inner viewport = area for prosogram (without textgrid) without margin
   dw_vx2 = dw_ovx2 - dw_margin		; right margin
   dw_vy1 = dw_ovy1					; no lower margin
   dw_vy2 = dw_ovy2 - dw_margin		; upper margin
# Define viewport for buttons
   dw_bx1 = dw_vx1					; X size for buttons = same as inner viewport
   dw_bx2 = dw_vx2
   dw_by1 = 93
   dw_by2 = 98
# Define viewport for separate textgrid
   dw_Tx1 = dw_vx1					; X size for textgrid = same as inner viewport
   dw_Tx2 = dw_vx2
   dw_Ty1 = dw_vy1					; lower side, default when no textgrid 
   dw_Ty2 = dw_vy2					; upper side, default when no textgrid
   if (nrofplottedtiers > 0)
      selectObject: newgridID
      n_ = Get number of tiers
      dw_Ty2 = dw_vy1 - 1			; upper side
      dw_Ty1 = dw_Ty2 - (5 * n_)	; lower side
      ; printline TextGrid viewport: x1=<'dw_Tx1:1'> x2=<'dw_Tx2:1'> y1=<'dw_Ty1:1'> y2=<'dw_Ty2:1'>
   endif
# Define viewport for excerpt position
   dw_Ex1 = dw_vx1					; X range same as inner viewport
   dw_Ex2 = dw_vx2
   dw_Ey2 = dw_Ty1 - 1				; upper side
   dw_Ey1 = dw_Ey2 - 2				; lower side
# Display all viewports and wait for input (mouse, keyboard)
   'win$'Erase all
   @gr_redraw: anal_t1, anal_t1+.timeincr 
   @gr_on_input: nucleiID
endproc


procedure gr_redraw: .x1, .x2
# This procedure is used only for demo window
   if (auto_pitchrange)
      @speaker_autorange: .x1, .x2
      ySTmax = ymax
      ySTmin = ymin
   endif
   @gr_clear_rectangle: dw_ovx1, dw_ovx2, dw_by2, 100			; clear top margin above buttons
   @gr_display_prosogram: .x1, .x2, ySTmin, ySTmax, grid_in_prosogram
   if (nrofplottedtiers > 0)
      @gr_clear_rectangle: dw_ovx1, dw_Tx1, dw_Ty1, dw_Ty2		; clear border to left of textgrid
      @gr_clear_rectangle: dw_Tx2, dw_ovx2, dw_Ty1, dw_Ty2		; clear border to right of textgrid
      @gr_display_textgrid_pure: newgridID, .x1, .x2, dw_Tx1, dw_Tx2, dw_Ty1, dw_Ty2
   endif
   @gr_draw_buttons
   @gr_draw_excerpt_position: .x1, .x2, anal_t1, anal_t2
endproc


procedure gr_draw_excerpt_position: .t1, .t2, .at1, .at2
# Draw position (<.t1>..<.t2>) of excerpt inside full time range of speech signal (<.at1>..<.at2>)
   if (demowin)
      @gr_clear_rectangle: dw_ovx1, dw_ovx2, dw_Ey1, dw_Ey2			; clear part of demo window, including borders
      'win$'Select inner viewport: dw_vx1, dw_vx2, dw_Ey1, dw_Ey2
	  ; when zooming beyond signal end (= .at2) :
         .tr = max(.at2, .t2)
      'win$'Axes: .at1, .tr, 0, 1
      'win$'Colour: "Black"
      'win$'Draw rectangle: .at1, .at2, 0, 1
      'win$'Paint rectangle: "Grey", .t1, min(.t2, .at2), 0, 1
    ; Draw analysis endtime
      'win$'Select inner viewport: 0, 100, 0, 100
      'win$'Axes: 0, 100, 0, 100
      'win$'Text special: dw_vx2+(dw_vx2-dw_vx1)*0.005, "left", dw_Ey1+(dw_Ey2-dw_Ey1)/2, "half", "Helvetica", default_fontsize, "0", fixed$(.at2, 2)
   endif
endproc


procedure gr_display_prosogram: x1, x2, y1, y2, with_grid
# This procedure is used both in Picture window and in Demo window
   @debug_msg: "gr_display_prosogram: entry, 'x1:3' - 'x2:3'"
   if (demowin)		; clear demo window
      @gr_clear_rectangle: dw_ovx1, dw_ovx2, dw_ovy1, dw_ovy2		; area for prosogram
      'win$'Select inner viewport: dw_vx1, dw_vx2, dw_vy1, dw_vy2
   endif
   ySTmax = y2
   ySTmin = y1
   if (with_grid)
      ySTbottom = ySTmin - nrofplottedtiers*(ySTmax-ySTmin)/4
   else
      ySTbottom = y1
   endif
   @debug_msg: "gr_display_prosogram: ySTmin='ySTmin:3' ySTmax='ySTmax:3' ySTbottom='ySTbottom:3' nrofplottedtiers='nrofplottedtiers'"
   'win$'Axes: x1, x2, ySTbottom, ySTmax

   ; start drawing
   if (with_grid and newgrid_available and nrofplottedtiers > 0)	; textgrid available, use Praat's Draw cmd to draw TG
      @gr_plot_textgrid: newgridID, x1, x2
      if (show_tiernames)
         @gr_draw_tiernames: newgridID, x1, x2
      endif
   endif
   @gr_garnish: x1, x2, ySTbottom, ySTmin, ySTmax, 10, basename$, font_family$

   if (task != task_annotation)
      # Adjust plotted intensity range such that distance between horizontal lines = 3 dB
         selectObject: intensityID
         dBmin = Get minimum: signal_start, anal_t2, "Parabolic"
         dBmax = Get maximum: signal_start, anal_t2, "Parabolic"
         # divide range by 2, since distance between lines = 2ST
         dBmin = dBmax - ((ySTmax - ySTmin)/2) * 3
      if (show_portee)
         @gr_draw_portee: x1, x2, ySTmin, ySTmax, 2, ySTbottom
      endif
      if (needs_stylization and show_octavejump)
         @gr_draw_octavejump: nucleiID, discontinuity_tier, x1, x2, red$
      endif
      if (rich)
         if (not plot_f0_on_top)
            @gr_plot_param_ST: pitchID, x1, x2, ySTbottom, ySTmax, blue$
         endif
         if (show_intensity)
            if (greyscale)
               'win$'Dashed line
            endif
            @gr_plot_param_scaled: intensityID, x1, x2, 1, green$
            'win$'Solid line
         endif
         if (intbp_available and show_intbp)
            @gr_plot_param_scaled: intbpID, x1, x2, 1, magenta$
         endif
         if (loudness_available)
            selectObject: loudnessID
            min = Get minimum: signal_start, anal_t2, "Parabolic"
            max = Get maximum: signal_start, anal_t2, "Parabolic"
            tmp = min - nrofplottedtiers*(max-min)/4
            @gr_plot_param: loudnessID, x1, x2, tmp, max, cyan$
         endif
         if (show_harmonicity)
            @gr_plot_param_scaled: harmonicityID, x1, x2, 0, purple$
         endif
         if (show_vuv)
            @gr_plot_vuv: nucleiID, vuv_tier, x1, x2, "Black"
         endif
         if (needs_stylization)
            @gr_plot_nuclei: nucleiID, x1, x2, red$
         endif
         if (show_lengthening)
            @gr_plot_lengthening: nucleiID, x1, x2, grey$
         endif
         if (show_localrate)
            @gr_plot_prop_curve: nucleiID, nucldatID, j_localrate_nucl, x1, x2, 0, 7, green$, "Solid"
         endif
         if (show_elasticity)
            ; @table_stats: nucldatID, "syll_dur", "mean", "sd", "min", "max"
            @gr_plot_prop_curve: nucleiID, nucldatID, j_syllabledur, x1, x2, -1, 1, grey$, "Dashed"
            ; @table_stats: nucldatID, "en_sylldur", "mean", "sd", "min", "max"
            @gr_plot_prop_curve: nucleiID, nucldatID, j_en_sylldur, x1, x2, -5, 5, green$, "Dashed"
            ; @table_stats: nucldatID, "en_rhymedur", "mean", "sd", "min", "max"
            @gr_plot_prop_curve: nucleiID, nucldatID, j_en_rhymedur, x1, x2, -5, 5, red$, "Dashed"
         endif
      endif

      if (show_trajectories)
         @gr_plot_prop_as_shape: "box_nucl", nucleiID, nucldatID, j_intrasyldown, x1, x2, -10, 10, "Cyan", "Solid"
         @gr_plot_prop_as_shape: "box_nucl", nucleiID, nucldatID, j_intrasylup, x1, x2, -10, 10, "Blue", "Solid"
         @gr_plot_prop_as_shape: "box_left", nucleiID, nucldatID, j_intersyl, x1, x2, -10, 10, "Green", "Solid"
      endif

      if (show_pitchrange)
         @gr_plot_pitchrange: nucleiID, x1, x2, magenta$
      endif

      if (show_pauses)
         if (rich)
            @gr_plot_feature: nucleiID, nucldatID, j_pause_dur, x1, x2, "Right", 75, "#pause", "Blue"
         else
            @gr_plot_feature: nucleiID, nucldatID, j_before_pause, x1, x2, "Right", 75, "feature:P", "Blue"
         endif
      endif
      if (show_hesitations) 
         @gr_plot_feature: nucleiID, nucldatID, j_hesitation, x1, x2, "Centre", 60, "feature:Hes", "Blue"
      endif

      if (show_prominence)
         @plot_prominence_measures: x1, x2
      endif

      if (show_rhythm)
         @gr_plot_prop_curve: nucleiID, nucldatID, j_isodur, x1, x2, 0.0, 0.5, green$, "Solid"
      endif

      if (needs_stylization)
         @gr_plot_PitchTier_clip_nuclei: stylSTID, nucleiID, x1, x2, ySTbottom, ySTmax, draw_pitch_target_values, "Black"
      endif
      if (plot_f0_on_top)
         @gr_plot_param_ST: pitchID, x1, x2, ySTbottom, ySTmax, blue$
      endif
   endif	; task <> task_annotation
endproc


procedure gr_on_input: .gridID
; Read mouse and keyboard input and perform corresponding actions 
; Variables <x1> and <x2> give world X-coordinates (starttime and endtime of displayed window)
   'win$'Select inner viewport: 0, 100, 0, 100
   'win$'Axes: 0, 100, 0, 100
   loop = 1
 while (loop and demoWaitForInput ( ))
   if demoClicked ( )
      x0 = demoX ( )
      y0 = demoY ( )
      if demoClickedIn (dw_vx1, dw_vx2, dw_vy1, dw_vy2)		; clicked on prosogram
         .t = x1 + ((x0-dw_vx1) / (dw_vx2-dw_vx1)) * (x2-x1)	; .t = position on time axis ; x1 = starttime ; x2 = endtime
         if (.t < signal_finish) 
            @interval_from_time: .gridID, nucleus_tier, .t, "interval"
            .t1 = Get starting point: nucleus_tier, interval
            .t2 = Get end point: nucleus_tier, interval
            @play_part: soundID, .t1, .t2
         endif
      elsif demoClickedIn (dw_Tx1, dw_Tx2, dw_Ty1, dw_Ty2)	; clicked textgrid
         .t = x1 + ((x0-dw_Tx1) / (dw_Tx2-dw_Tx1)) * (x2-x1)	; .t = position on time axis
         if (nrofplottedtiers > 0 and .t < signal_finish)
            selectObject: newgridID
            .n = Get number of tiers
            .tier = ceiling ((y0-dw_Ty1)/((dw_Ty2-dw_Ty1)/.n))
            .tier = min( max (1, .tier), .n)
            .tier = .n - .tier + 1				; tiers drawn from top to bottom
            @interval_from_time: newgridID, .tier, .t, "interval"
            .t1 = Get starting point: .tier, interval
            .t2 = Get end point: .tier, interval
            @play_part: soundID, max(.t1,x1), min(.t2,x2)	; play visible part of interval
         endif
      elsif demoClickedIn (dw_vx1, dw_vx2, dw_Ey1, dw_Ey2)	; clicked time bar (excerpt position)
         .t = ((x0-dw_vx1) / (dw_vx2-dw_vx1)) * anal_t2			; .t = position on time axis
         if (.t < x1)
            @gr_scroll: -timeincr
         elsif (.t > x2)
            @gr_scroll: timeincr
         else
            @play_part: soundID, x1, x2
         endif
      elsif demoClickedIn (dw_bx1, dw_bx2, dw_by1, dw_by2)	; clicked buttons
         @gr_button_get
         if (button == 1)				;   |<<	scroll to start of signal
            @gr_scroll (0)
         elsif (button == 2)			;   <<	scroll left by timeincr
            @gr_scroll (-timeincr)
         elsif (button == 3)			;   <	scroll left by 0.25 s
            @gr_scroll: -0.25
         elsif (button == 4)			;	play visible window
            @gr_draw_button: 4, "Lime", "Play"
            @play_part: soundID, x1, x2
            @gr_draw_button: 4, "Yellow", "Play"
         elsif (button == 5)			;   >	scroll right by 0.25 s
            @gr_scroll: 0.25
         elsif (button == 6)			;   >>	scroll right by timeincr
            @gr_scroll: timeincr
         elsif (button == 7)			;   >>|	scroll to end of signal
            @gr_scroll: anal_t2
         elsif (button == 8)			;	zoom out (2 x)
            @gr_zoom: 2
         elsif (button == 9)			;	zoom in (0.5 x)
            @gr_zoom: 0.5
         elsif (button == 11)			;	resynthesize
            @gr_draw_button: 11, "Lime", "Resynth"
            call resynthesis soundID stylID time_step x1 x2
            @gr_draw_button: 11, "Yellow", "Resynth"
         ;elsif (button == 12)			;	auto play and scroll
         ;   call gr_draw_button 12 Lime Stop
         ;   call play_part soundID x1 x2
         ;   while (x2 < anal_t2)
         ;      call gr_scroll timeincr
         ;      call gr_draw_button 12 Lime Stop
         ;      call play_part soundID x1 x2
         ;   endwhile
         ;   call gr_draw_button 12 Yellow Play &~Scroll
         elsif (button == 12)			;	Rich / Light view
            @toggle: "rich"
            @gr_redraw: x1, x2
         elsif (button == 13)			;	Show Pitch range
            @toggle: "show_pitchrange"
            @gr_redraw: x1, x2
         elsif (button == 14)			;	Show Pitch values
            @steps_012: "draw_pitch_target_values"
            @gr_redraw: x1, x2
         elsif (button == 15)			;	Refresh (needed after resize window)
            @gr_clearscreen
            @gr_redraw: x1, x2
         elsif (button == 16)			;	Exit
            @gr_clearscreen
            @gr_printline: "Now kill window..."
            loop = 0
         endif
      endif
   elsif demoKeyPressed ( )
      key$ = demoKey$ ( )
      if (key$ = "R")      
         @gr_scroll: timeincr
      elsif (key$ = "L")        
         @gr_scroll: -timeincr
      ;else
      ;   shift = demoShiftKeyPressed ()
      ;   @msg: "KeyPressed='key$' shift='shift'" 
      endif
   endif
   'win$'Select inner viewport: 0, 100, 0, 100
   'win$'Axes: 0, 100, 0, 100
 endwhile
endproc


procedure resynthesis: soundinID, pitchtierinID, time_step, x1, x2
   ; Make a copy of part of the pitch tier
   selectObject: pitchtierinID		; values in Hz
   .j1 = Get high index from time: x1
   .j2 = Get low index from time: x2
   tmppitchtierID = Create PitchTier: "tmp", x1, x2
   for .j from .j1 to .j2
      selectObject: pitchtierinID
      .v = Get value at index: .j
      .t = Get time from index: .j
      selectObject: tmppitchtierID
      Add point: .t, .v
   endfor
   ; Make a copy of part of the speech signal and get resynthesis
   selectObject: soundinID
   tmpsoundID = Extract part: x1, x2, "rectangular", 1.0, "yes" 
   manipID = To Manipulation: time_step, 60, 600
   selectObject: tmppitchtierID
   plus manipID
   Replace pitch tier
   selectObject: manipID
   synthID = Get resynthesis (overlap-add)
   ; Plot PitchTier
   @gr_plot_PitchTier: stylSTID, x1, x2, "Lime"
   selectObject: synthID
   Play
   ; Write to WAV file: "tmp_resynth.wav"
   removeObject: manipID, tmppitchtierID, tmpsoundID, synthID
   @gr_display_prosogram: x1, x2, ySTmin, ySTmax, grid_in_prosogram
endproc


procedure gr_draw_buttons
   nrof_buttons = 16		; room for 16 buttons of equal size (1 row horizontally)
   'win$'Select inner viewport: dw_bx1, dw_bx2, dw_by1, dw_by2
   'win$'Axes: dw_bx1, dw_bx2, dw_by1, dw_by2
   @gr_clear_rectangle (dw_ovx1, dw_ovx2, dw_by1, dw_by2)			; clear buttons area, including left and right margins
   # Define button positions
   .size = (dw_bx2-dw_bx1)/nrof_buttons
   for j to nrof_buttons
      button'j'_x1 = dw_bx1 + (j-1)*.size + .size*0.1
      button'j'_x2 = button'j'_x1 + .size*0.9
   endfor
   @gr_draw_button: 1, "Yellow", "|<<"
   @gr_draw_button: 2, "Yellow", "<<"
   @gr_draw_button: 3, "Yellow", "<"
   @gr_draw_button: 4, "Yellow", "Play"
   @gr_draw_button: 5, "Yellow", ">"
   @gr_draw_button: 6, "Yellow", ">>"
   @gr_draw_button: 7, "Yellow", ">>|"
   @gr_draw_button: 8, "Yellow", "Zoom~Out"
   @gr_draw_button: 9, "Yellow", "Zoom~In"
   @gr_draw_button: 11, "Yellow", "Resynth"
   @gr_draw_button: 12, "Cyan", "Rich~View"
   @gr_draw_button: 13, "Cyan", "Pitch~Range"
   @gr_draw_button: 14, "Cyan", "Show~Values"
   @gr_draw_button: 15, "Yellow", "Refresh"
   @gr_draw_button: 16, "Magenta", "Exit"
endproc


procedure gr_draw_button: nr, .color$, .text$
   .x1 = button'nr'_x1
   .x2 = button'nr'_x2
   .x = .x1 + (.x2 - .x1)/2
   .y = dw_by1 + (dw_by2 - dw_by1)/2
   'win$'Select inner viewport: dw_bx1, dw_bx2, dw_by1, dw_by2
   'win$'Axes: dw_bx1, dw_bx2, dw_by1, dw_by2
   'win$'Paint rectangle: .color$, .x1, .x2, dw_by1, dw_by2
   'win$'Black
   'win$'Draw rectangle: .x1, .x2, dw_by1, dw_by2
   .pos = index (.text$, "~")
   if (.pos > 0)
      .s$ = left$ (.text$, .pos - 1)
      'win$'Text special: .x, "centre", .y, "bottom", "Helvetica", button_fontsize, "0", .s$
      .s$ = right$ (.text$, length(.text$) - .pos)
      'win$'Text special: .x, "centre", .y, "top", "Helvetica", button_fontsize, "0", .s$
   else
      'win$'Text special: .x, "centre", .y, "half", "Helvetica", button_fontsize, "0", .text$
   endif
endproc


procedure gr_button_get
# Set <button> to number of button clicked or zero if no button clicked
   button = 0
   for j from 1 to nrof_buttons
      if demoClickedIn (button'j'_x1, button'j'_x2, dw_by1, dw_by2)
         button = j
         j = nrof_buttons + 1
      endif
   endfor
endproc


procedure gr_zoom: factor
;@msg: "gr_zoom: factor='factor' x1='x1:3' x2='x2:3' anal_t1='anal_t1:3' anal_t2='anal_t2:3' timeincr='timeincr:3'"
   .x = x1 + (x2-x1)/2
   timeincr = max (1, factor * timeincr)	; the time interval should be at least 1 s 
   x1 = max (.x - timeincr/2, anal_t1)		; no zooming out beyond signal boundaries
   x2 = min (.x + timeincr/2, anal_t2)		; no zooming out beyond signal boundaries
;@msg: "gr_zoom: exit x1='x1:3' x2='x2:3' timeincr='timeincr:3'"
   @gr_redraw: x1, x2
endproc


procedure gr_scroll: amount
;@msg: "gr_scroll: amount='amount' x1='x1:3' x2='x2:3' anal_t1='anal_t1:3' anal_t2='anal_t2:3' timeincr='timeincr:3'"
   if (amount = 0)
      x1 = anal_t1
   endif
   x1 = min (x1 + amount, anal_t2 - timeincr)	; no scrolling beyond endtime of analysis
   x1 = max (x1, anal_t1)						; no scrolling beyond starttime of analysis
   x2 = min (x1 + timeincr, anal_t2)			; no scrolling beyond endtime of analysis
;@msg: "gr_scroll: exit x1='x1:3' x2='x2:3'"
   @gr_redraw: x1, x2
endproc


procedure gr_draw_calib: .x1, .y1, .x2
# Draw horizontal dotted line from <.x1, .y1> to <.x2, .y1>  (Some graphics viewers display Praat's Dotted lines as solid ones.) 
   if (.x2 > .x1)
      'win$'Solid line
      .dx1 = (.x2-.x1)/200	; 200 dots per line
      .dx2 = .dx1*0.3		; dot shorter than whitespace   
      .x = .x1 + .dx2
      while (.x1 <= .x2)
         'win$'Draw line: .x1, .y1, min(.x,.x2), .y1
         .x1 += .dx1
         .x = .x1 + .dx2
      endwhile
   endif
endproc


procedure gr_draw_portee: .x1, .x2, .ymin, .ymax, .ystep, .ybottom
# Draw horizontal calibration lines which are <.ystep> apart from <.ybottom> to <.ymax>
   'win$'Axes: .x1, .x2, .ybottom, .ymax
   # Draw calibration lines
      'win$'Grey
      'win$'Line width: 1
      .y = ceiling (.ymin/2) * 2
      while (.y < .ymax)
        @gr_draw_calib: .x1, .y, .x2
        .y += .ystep
      endwhile
endproc


procedure gr_plot_textgrid: .gridID, .x1, .x2
   if (demowin)
      'win$'Select inner viewport: dw_vx1, dw_vx2, dw_vy1, dw_vy2
   endif
   'win$'Black
   'win$'Line width: 1
   'win$'Solid line
   selectObject: .gridID
   @debug_msg: "gr_plot_textgrid: show_tg_bound='show_tg_bound'"
   # Draw: From, To, ShowBoundaries, UseTextstyles, Garnish
   if (task == task_annotation or not show_tg_bound)
      'win$'Draw: .x1, .x2, "no", "no", "no"
   else
      'win$'Draw: .x1, .x2, "yes", "no", "no"
   endif
endproc


procedure gr_draw_tiernames: .gridID, .x1, .x2
 ;if (not demowin)
   .y1 = ySTbottom
   .y2 = ySTmax
   'win$'Line width: 1
   'win$'Solid line
   'win$'Black
   if (demowin) 
      'win$'Red
   endif
   'win$'Axes: .x1, .x2, .y1, .y2
   selectObject: .gridID
   .n = Get number of tiers
   .dyN = (ySTmin - ySTbottom)/.n	; height of a tier
   for .tier from 1 to .n
      .ylo = .y1 + (.n - .tier) * .dyN
      .yt = .ylo + (.dyN)/2
      .tiername$ = Get tier name: .tier
      'win$'Text special: .x2+(.x2-.x1)*0.005, "left", .yt, "half", "Helvetica", small_fontsize, "33", .tiername$
   endfor
   'win$'Black
 ;endif
endproc


procedure gr_display_textgrid_pure: .gridID, .x1, .x2, .vpx1, .vpx2, .vpy1, .vpy2
; Draw textgrid rather than using Praat's built-in Draw command
   'win$'Select inner viewport: .vpx1, .vpx2, .vpy1, .vpy2
   'win$'Axes: .vpx1, .vpx2, .vpy1, .vpy2
   ; Clear the area
   'win$'Paint rectangle: "White", .vpx1, .vpx2, .vpy1, .vpy2 
   'win$'Black
   'win$'Line width: 1
   'win$'Solid line
   selectObject: .gridID
   .n = Get number of tiers
   .y1 = 0
   .y2 = 1
   .dyN = (.y2 - .y1)/.n
   'win$'Axes: .x1, .x2, .y1, .y2
   'win$'Draw inner box
   for .tier from 1 to .n
      .ylo = (.n - .tier) * .dyN
      .yhi = .ylo + .dyN
      'win$'Draw line: .x1, .yhi, .x2, .yhi
      @interval_from_time: .gridID, .tier, .x1, "first_interval"
      @interval_from_time: .gridID, .tier, .x2, "last_interval"
      for .interval from first_interval to last_interval
         .ix1 = Get starting point: .tier, .interval
         .ix2 = Get end point: .tier, .interval
         .ix1 = max (.ix1, .x1)
         .ix2 = min (.ix2, .x2)
         .label$ = Get label of interval: .tier, .interval
         ;.label$ = replace$ (.label$, "_", "\_ ", 0)			; avoid interpretation of underscore as subscript 
         ;.label$ = replace$ (.label$, "%", "\% ", 0)			; avoid interpretation of percent as italic text style 
         'win$'Draw line: .ix2, .ylo, .ix2, .yhi
         .xt = .ix1 + (.ix2 - .ix1)/2
         .yt = .ylo + (.yhi - .ylo)/2
         'win$'Text special: .xt, "centre", .yt, "half", font_family$, fontsize, "0", .label$
      endfor
      if (show_tiernames)
        ; Draw tier name
         .tiername$ = Get tier name: .tier
         'win$'Red
         'win$'Text special: .x2+(.x2-.x1)*0.005, "left", .yt, "half", font_family$, default_fontsize, "33", .tiername$
      endif
      'win$'Black
   endfor
endproc


procedure gr_garnish: x1, x2, ybot, y1, y2, ystep, string$, .font$
; Y axis on ST scale : ybot = bottom of textgrid, y1 = bottom (min) of drawing, y2 = top (max) of drawing, ystep = y-distance between calibration marks for values
; <string$>		= name of file
; <.font$>		= font family used for garnish
   @debug_msg: "gr_garnish: show_y_scale='show_y_scale' show_tiernames='show_tiernames'"
   ; .font$ = "Helvetica"
 # Draw border of viewport
   'win$'Black
   'win$'Line width: 1
   'win$'Solid line
   'win$'Axes: x1, x2, ybot, y2
   'win$'Draw inner box
 # Time calibration marks
   ;'win$'Font size: small_fontsize		; Change of font size will affect viewport definition
   'win$''.font$'
   if (viewsize$ = "compact")      
      show_x_scale = 0
   endif
   if (show_x_scale)		; numbers, ticks, dotted lines
      'win$'Marks top every: 1, 0.1, "no", "yes", "no"
      'win$'Marks top every: 1, 1, "yes", "yes", "no"
   else
      'win$'Marks top every: 1, 0.1, "no", "yes", "no"
   endif
 # Y axis calibration
      ;if (not demowin)
         if (show_y_scale)
         # Show ST scale in left margin
            # 'win$'Marks left every: 1, ystep, "yes", "yes", "no"
            .y = (floor (y2/ystep) * ystep) - ystep/4
            'win$'Text special: x1, "right", .y, "half", .font$, small_fontsize, "0", "ST " 
            .x0 = x1-(x2-x1)/200
            .yST = 10 * floor((y1+10)/10)
            while (.yST <= y2)
               'win$'Draw line: .x0, .yST, x1, .yST
               'win$'Text special: .x0, "Right", .yST, "Half", .font$, small_fontsize, "0", fixed$(.yST,0) 
               .yST += ystep
            endwhile
            if (show_y_scale_r)
         # Show Hz scale in right margin
               'win$'Black
               'win$'Solid line
               .ystepHz = 25
               .yHz = semitonesToHertz(y1 + hertzToSemitones(1))	; lower frequency value
               .yHz = max(50, .yHz)
               .ystepHz = 25
               if (.yHz < 75)
                  .yHz = 75
               else
                   .yHz = ceiling(.yHz/50) * 50
               endif
               .y2Hz = semitonesToHertz(y2 + hertzToSemitones(1))	; upper frequency value
               .x3 = x2+(x2-x1)/200	; length of mark
               while (.yHz <= .y2Hz)
                  .yST = hertzToSemitones(.yHz) - hertzToSemitones(1)
                  if (.yHz >= 300)
                     .ystepHz = 100
                  elsif (.yHz >= 100)
                     .ystepHz = 50
                  endif
                  'win$'Draw line: x2, .yST, .x3, .yST
                  .s$ = fixed$(.yHz,0)
                  if (.yHz + .ystepHz >= .y2Hz)
                     .s$ = .s$ + " Hz"
                  endif
                  'win$'Text special: .x3, "Left", .yST, "Half", .font$, small_fontsize, "0", .s$ 
                  .yHz += .ystepHz
               endwhile
               'win$'Black
            endif ; show_y_scale_r
         else
            ; 'win$'Marks left every: 1, ystep, "no", "yes", "no"
         endif
      ;endif
   .settings$ = ""
   if (show_settings)		; Show analysis settings : segmentation mode, glissando threshold
       if (adaptive_glissando)
          s$ = "G(adapt)='glissando_low'-'glissando'/T^2"
       else
          s$ = "G='glissando'/T^2"
       endif
       .settings$ = "'segmentation_name$', " + s$ + ", DG='diffgt', dmin='mindur_ts:3'"
       ; 'win$'Text special: x2, "Right", y2, "Top", .font$, small_fontsize, "0.0", .settings$ 
   endif
   if (not demowin and show_settings)
    # Print version identification
         s$ = .settings$ + " \-- " + version$
         'win$'Text special: x2, "Right", ybot, "Top", .font$, small_fontsize, "0.0", s$
    # Show end of signal by double vertical line
      if (x2 > signal_finish)
         'win$'Line width: 3
         'win$'Draw line: signal_finish, ybot, signal_finish, y2
         'win$'Line width: 1
      endif
      if (nrofFiles > 1 or file_stamp)
         if (file_stamp)
            .s$ = replace$(string$, "_", "\_ ", 0)
            if (viewsize$ = "compact")
               .s$ = "'.s$' ('x1:2'-'x2:2's)"
            endif
            'win$'Text special: x1, "Left", ybot, "Top", .font$, small_fontsize, "0.0", .s$
         endif
      endif
   endif
   ; restore default font: 
      'win$''font_family$'
endproc


procedure gr_plot_param_ST: .paramID, .x1, .x2, .y1, .y2, .color$
   'win$''.color$'
   selectObject: .paramID
   # diffST = difference in ST between ST-scale relative to 100Hz and that rel to 1Hz
   .diffST = 12 * log2(100/1)
   'win$'Draw semitones: .x1, .x2, .y1-.diffST, .y2-.diffST, "no"
endproc


procedure gr_plot_param: .paramID, .x1, .x2, .y1, .y2, .color$
# Plot parameter paramID in current viewport 
   'win$''.color$'
   'win$'Line width: 1
   selectObject: .paramID
   'win$'Draw: .x1, .x2, .y1, .y2, "no"
endproc


procedure gr_plot_param_scaled: .paramID, .x1, .x2, .garnish, .color$
   'win$''.color$'
   'win$'Line width: 1
   selectObject: .paramID
   .y2 = Get maximum: anal_t1, anal_t2, "Parabolic"
   .y1 = .y2 - ((ySTmax - ySTmin)/2) * 3
   if (with_grid)
      .y1 -= nrofplottedtiers*(.y2-.y1)/4
   endif
   if (.garnish)		; objects for which Draw includes "garnish" option
      'win$'Draw: .x1, .x2, .y1, .y2, "no"
   else
      'win$'Draw: .x1, .x2, .y1, .y2
   endif
endproc


procedure gr_draw_zigzag: .lx1, .lxend, .ldx, .lystart, .ldy
# Draw a horizontal zigzag line from <.lx1> to <.lxend>, with x-steps of <.ldx> 
# starting from <.lystart> moving up by <.ldy> and back to <.lystart> 
   .ldir = 1 ; up
   .ly1 = .lystart - .ldy/2
   .ly2 = .lystart + .ldy/2
   while (.lx1 < .lxend)
      .lx = .lx1 + .ldx
      if (.lx > .lxend)
         .lx = .lxend
         .ly2 = .ly1 + .ldir*(.ldy*(.lx-.lx1)/.ldx)
      endif
      'win$'Draw line: .lx1, .ly1, .lx, .ly2
      .lx1 += .ldx
      .ldir = .ldir * -1 ; change direction
      .tmp = .ly1
      .ly1 = .ly2
      .ly2 = .tmp
   endwhile
endproc


procedure gr_plot_vuv: .gridID, .tier, .x1, .x2, .color$
# Draw VUV data contained in tier 1 of <gridID>, from time <x1> to <x2>
   .ldx = (.x2 - .x1) / 200
   .ldy = (ySTmax - ySTbottom) / 50
   .ly = ySTmin + .ldy
   .ly1 = .ly - .ldy/2
   .ly2 = .ly + .ldy/2
   'win$'Axes: .x1, .x2, ySTbottom, ySTmax
   selectObject: .gridID
   @interval_from_time: .gridID, .tier, .x1, "first_interval"
   @interval_from_time: .gridID, .tier, .x2, "last_interval"
   'win$''.color$'
   for .i from first_interval to last_interval
      .label$ = Get label of interval: .tier, .i
      if (.label$ = "V")
         .t1 = Get start time of interval: .tier, .i
         .t2 = Get end time of interval: .tier, .i
         if (.t1 >= .x1)
             'win$'Draw line: .t1, .ly1, .t1, .ly2
         endif
         if (.t2 <= .x2)
             'win$'Draw line: .t2, .ly1, .t2, .ly2
         endif
         .lx1 = max (.x1, .t1)
         .lx2 = min (.x2, .t2)
         @gr_draw_zigzag: .lx1, .lx2, .ldx, .ly, .ldy
      endif
   endfor
endproc


procedure gr_plot_nuclei: .gridID, .x1, .x2, .color_$
# Draw nuclei in nucleus_tier of <gridID>, from time <x1> to <x2> at the bottom of the current viewport
   .ldy = (ySTmax - ySTbottom) / 50	; divide Y-range in 50 parts
   .ly = ySTmin + .ldy
   .ly1 = .ly - .ldy*0.80
   .ly2 = .ly + .ldy*0.80
   'win$'Axes: .x1, .x2, ySTbottom, ySTmax
   selectObject: .gridID
   @interval_from_time: .gridID, nucleus_tier, .x1, "first_interval"
   @interval_from_time: .gridID, nucleus_tier, .x2, "last_interval"
   'win$''.color_$'
   for .i from first_interval to last_interval
      .label$ = Get label of interval: nucleus_tier, .i
      if (.label$ = "a")
         .t1 = Get start time of interval: nucleus_tier, .i
         .t2 = Get end time of interval: nucleus_tier, .i
         if (.t1 >= .x1)		; draw left side
             'win$'Draw line: .t1, .ly1, .t1, .ly2
         endif
         if (.t2 <= .x2)		; draw right side
             'win$'Draw line: .t2, .ly1, .t2, .ly2
         endif
         .lx1 = max (.x1, .t1)
         .lx2 = min (.x2, .t2)
         'win$'Draw line: .lx1, .ly1, .lx2, .ly1
         'win$'Draw line: .lx1, .ly2, .lx2, .ly2
      endif
   endfor
endproc


procedure gr_plot_pitchrange: .gridID, .x1, .x2, .color$
# Draw pitchrange (median, top and bottom) using <gridID>, from time <.x1> to <.x2> in the current viewport.
   'win$'Axes: .x1, .x2, ySTbottom, ySTmax
   selectObject: .gridID
   @interval_from_time: .gridID, speaker_tier, .x1, "first_interval"
   @interval_from_time: .gridID, speaker_tier, .x2, "last_interval"
   'win$'Dashed line
   'win$'Line width: pitchrange_linewidth
   for .i from first_interval to last_interval
      .t1 = Get start time of interval: speaker_tier, .i
      .t2 = Get end time of interval: speaker_tier, .i
      if (speaker_available)
         speaker$ = Get label of interval: speaker_tier, .i
         speaker_j = extractNumber (speakers$, "<'speaker$'>:")	; find speaker number
      else
         speaker_j = 1
      endif
      if (speaker_j == undefined)
         ; This happens when there are no nuclei found for this speaker.
         ; @msg: "gr_plot_pitchrange: undefined speaker=<'speaker$'> i='i' t1='.t1' t2='.t2' speakers='speakers$'"
      else
         'win$''.color$'
         'win$'Line width: pitchrange_linewidth
         .ly1 = extractNumber (speaker_range_'speaker_j'$, "BOTTOM_ST=")
         .ly2 = extractNumber (speaker_range_'speaker_j'$, "TOP_ST=")
         if (.t1 >= .x1)							; draw left side
            'win$'Draw line: .t1, .ly1, .t1, .ly2
         endif
         if (.t2 <= .x2)							; draw right side
            'win$'Draw line: .t2, .ly1, .t2, .ly2
         endif
         .lx1 = max (.x1, .t1)
         .lx2 = min (.x2, .t2)
         'win$'Draw line: .lx1, .ly1, .lx2, .ly1
         'win$'Draw line: .lx1, .ly2, .lx2, .ly2
         .ly1 = extractNumber (speaker_range_'speaker_j'$, "MEDIAN_ST=")
         'win$'Draw line: .lx1, .ly1, .lx2, .ly1
         if (speaker_available)
            .s$ = speaker_label'speaker_j'$
            'win$'Text special: .lx1, "Left", .ly2, "Bottom", font_family$, small_fontsize, "0", .s$
         endif
      endif
   endfor
   'win$'Solid line
   'win$'Black
endproc


procedure gr_draw_octavejump: nucleiID, .tier, .time1, .time2, .color$
# Draw octavejumps in tier 2 of <nucleiID>
   'win$''.color$'
   .y = ySTmin + (ySTmax - ySTmin) / 2		; y-position to draw
   selectObject: nucleiID
   .ni = Get number of points: .tier
   for .i to .ni
      .x = Get time of point: .tier, .i
      if (.x >= .time1 and .x <= .time2)
         'win$'Text: .x, "Centre", .y, "Half", "\ox"
      endif
   endfor
endproc


procedure gr_plot_PitchTier_clip_nuclei: .paramID, .nucleiID, .x1, .x2, .y1, .y2, .draw_pitch_target_values, .color$
   @debug_msg: "gr_plot_PitchTier_clip_nuclei: entry, '.x1:3' - '.x2:3'"
   'win$'Axes: .x1, .x2, .y1, .y2
   if (clip_to_Y_range)
      selectObject: .paramID
      .tmp_pitchTierID = Copy: "tmp_pitchtier"
      selectObject: .tmp_pitchTierID
      @clipPitchTier: .tmp_pitchTierID, .y1, .y2, .x1, .x2
      @gr_plot_stylisation: .tmp_pitchTierID, .nucleiID, .x1, .x2, .color$
      removeObject: .tmp_pitchTierID
   else
      @gr_plot_stylisation: .paramID, .nucleiID, .x1, .x2, .draw_pitch_target_values, .color$
   endif
endproc


procedure gr_plot_stylisation: .paramID, nucleiID, .x1, .x2, .draw_pitch_target_values, .color$
# Plot stylization **inside** nuclei
# <x1>..<x2>	times at borders of graphics window (viewport)
   @debug_msg: "gr_plot_stylisation: entry '.x1:3' - '.x2:3'"
   # .diffST = difference in ST between ST-scale relative to 100Hz and that rel to 1Hz
      .diffST = 12 * log2(100/1)
   'win$''.color$'
   'win$'Line width: stylization_linewidth
   @interval_from_time: nucleiID, nucleus_tier, .x1, "first_interval"
   @interval_from_time: nucleiID, nucleus_tier, .x2, "last_interval"
   ; @msg: "gr_plot_stylisation: first 'first_interval' - last 'last_interval'"
   selectObject: .paramID						; stylization (PitchTier)
   .np = Get number of points
   for .i from first_interval to last_interval		; indices of nuclei for which to plot stylization 
      selectObject: nucleiID				; segmentation into nuclei, etc
      @is_nucleus: .i
      if (result)
         .nx1 = Get start time of interval: nucleus_tier, .i
         .nx2 = Get end time of interval: nucleus_tier, .i
       # Check that the nearest indices of stylization PitchTier are within nucleus
         selectObject: .paramID				; stylization (PitchTier)
         .i1 = Get nearest index from time: .nx1
         repeat
            .t = Get time from index: .i1
            if (.t < .nx1)
               .i1 += 1
            endif
         until (.t >= .nx1 or .t >= .nx2 or .i1 >= .np)
         .i2 = Get nearest index from time: .nx2
         repeat
            .t = Get time from index: .i2
            if (.t > .nx2)
               .i2 -= 1
            endif
         until (.t <= .nx2 or .t <= .nx1 or .i2 <= 1)
         for .j from .i1 to .i2-1					; each tonal segment .j within nucleus
            .ox1 = Get time from index: .j
            .oy1 = Get value at index: .j
            .ox2 = Get time from index: .j+1
            .oy2 = Get value at index: .j+1
            if (.ox2 > .x1 and .ox1 < .x2)
               ; find visible part (within graphics window) of stylization for tonal segment .j 
               .lx1 = .ox1
               .lx2 = .ox2
               .ly1 = .oy1
               .ly2 = .oy2
               if (.ox1 < .x1)		; tonal segment start outside visible area
                 .ly1 = .oy1 + (.oy2-.oy1)*(.x1-.ox1)/(.ox2-.ox1)	; find y at x1
                 .lx1 = .x1
               endif
               if (.ox2 > .x2)		; tonal segment ends outside visible area
                 .ly2 = .oy1 + (.oy2-.oy1)*(.x2-.ox1)/(.ox2-.ox1)	; find y at x2
                 .lx2 = .x2
               endif
               'win$'Draw line: .lx1, .ly1, .lx2, .ly2
               if (.draw_pitch_target_values)
                  .y1 = .ly1
                  .y2 = .ly2
                  .prec = 1								; precision, when values in ST
                  if (.draw_pitch_target_values = 2)		; show values in Hz, otherwise in ST
                     .y1 = semitonesToHertz(.y1-.diffST)
                     .y2 = semitonesToHertz(.y2-.diffST)
                     .prec = 0							; precision, when values in Hz
                  endif
                  .offs = 1
                  if (.ly1 <> .ly2)
                     'win$'Text special: .lx1, "Left", .ly1+.offs, "Half", "Helvetica", small_fontsize, "90", fixed$(.y1,.prec)
                     'win$'Text special: .lx2, "Left", .ly2+.offs, "Half", "Helvetica", small_fontsize, "90", fixed$(.y2,.prec)
                  else
                     'win$'Text special: .lx1+(.lx2-.lx1)/2, "Left", .ly1+.offs, "Half", "Helvetica", small_fontsize, "90", fixed$(.y1,.prec)
                  endif
               endif
            endif
         endfor
      endif
   endfor
   'win$'Line width: 1
   @debug_msg: "gr_plot_stylisation: exit"
endproc


procedure gr_plot_PitchTier: .objectID, .x1, .x2, .color$
   if (demowin)
      'win$'Select inner viewport: dw_vx1, dw_vx2, dw_vy1, dw_vy2
   endif
   'win$'Axes: .x1, .x2, ySTbottom, ySTmax
   selectObject: .objectID
   'win$''.color$'
   'win$'Draw: .x1, .x2, ySTmin, ySTmax, "no"
endproc


; procedure gr_plot_values: .col, .gridID, .x1, .x2, .y1, .y2, .color$
; # For nuclei in <gridID> in time range <.x1>..<.x2>, draw value for <.col> as text
;    'win$'Axes: .x1, .x2, .y1, .y2
;    selectObject: .gridID
;    .ni = Get number of intervals: nucleus_tier
;    @interval_from_time: .gridID, nucleus_tier, .x1, "first_interval"
;    @interval_from_time: .gridID, nucleus_tier, .x2, "last_interval"
;    'win$''.color$'
;    for .i from first_interval to last_interval
;       selectObject: .gridID
;       .label$ = Get label of interval: nucleus_tier, .i
;       if (.label$ = "a")
;          .t1 = Get start time of interval: nucleus_tier, .i
;          .t2 = Get end time of interval: nucleus_tier, .i
;          .t = .t1 + (.t2-.t1)/2
;          .s$ = Get label of interval: pointer_tier, .i
;          .row = number(.s$)
;          if (.row <= nrof_nuclei_analysed)
;            selectObject: nucldatID
;            .v = Get value: .row, .col
;            .yST =  hertzToSemitones(.v) - hertzToSemitones(1)
;            'win$'Text special: .t, "Centre", .yST, "Bottom", "Helvetica", small_fontsize, "0", "'.v:2'"
;          endif
;       endif
;    endfor
; endproc


procedure gr_plot_feature: .gridID, .table, .col, .x1, .x2, .just$, .y, .format$, .color$
# For each syllable in time range, draw text at Y-position, provided feature is true (i.e. >= 0)  
# <.col>		index of a column in <.table>, i.e. some variable/feature/attribute.
# <.y>			vertical position : percentage of Y range
# <.x1>..<.x2>	time range
# <.just$>		position text relative to nucleus : Centre, Right, Left of nucleus
# <.format$>	"feature:<string>" OR "number:<precision>" OR "#pause"
   selectObject: .table
   bottom = -nrofplottedtiers*100/4
   'win$'Axes: .x1, .x2, bottom, 100
   'win$''.color$'
   'win$'Line width: 1
   .type$ = "none"
   .text$ = extractWord$(.format$, "feature:")
   .prec = extractNumber(.format$, "number:")
   if (index_regex(.format$, "#pause"))
      .type$ = "pause"
   elsif (length(.text$))
      .type$ = "feature"
   elsif (.prec != undefined)
      .type$ = "number"
   endif
   @intervals_from_time_range: .gridID, nucleus_tier, .x1, .x2, "first_interval", "last_interval"
   for .i from first_interval to last_interval
      selectObject: .gridID
      @is_nucleus: .i
      if (result)
         .nx1 = Get start time of interval: nucleus_tier, .i
         .nx2 = Get end time of interval: nucleus_tier, .i
         .s$ = Get label of interval: pointer_tier, .i
         if (length(.s$))
            .row = number(.s$)
            if (.row <= nrof_nuclei_analysed)
               selectObject: .table
               .v = Get value: .row, .col
               if (.type$ == "feature" and .v > 0) or (.type$ == "number") or (.type$ = "pause")
                  if (.type$ == "number")
                     .text$ = fixed$(.v, .prec)
                  elsif (.type$ == "pause")
                     .text$ = ""
                     if (.v >= mindur_pause_gap)
                        .text$ = "P " + fixed$(.v*1000, 0)		; pause duration in ms
                     endif
                  endif
                  if (.just$ = "Left")			; left of nucleus
                      .x = .nx1
                      .halign$ = "Right"		; horizontal alignment is Right (like "Right adjusted")
                  elsif (.just$ = "Right")		; left of nucleus
                      .x = .nx2
                      .halign$ = "Left"
                  else
                      .x = .nx1+(.nx2-.nx1)/2	; midtime
                      .halign$ = "Centre"
                  endif
                  if (.x >= .x1 and .x <= .x2 and length(.text$))
                      'win$'Text special: .x, .halign$, .y, "Bottom", "Helvetica", small_fontsize, "0", .text$
                  endif
               endif
            endif
         endif
      endif
   endfor
   'win$'Font size: fontsize
endproc


procedure gr_plot_prop_as_shape: .format$, .gridID, .table, .col, .wx1, .wx2, .wy1, .wy2, .color$, .linestyle$
# For nuclei in time range <.wx1>..<.wx2> in <.gridID>, plot a shape representing the magnitude of variable in <.col> in <.table>.
# The shape is given by by <.format$>.
# Also draws the value at (xmid,y).
#	<.col>								column for property
#	<.wx1>...<.wx2>, <.wy1>...<.wy2>	world coordinates to be used for the active viewport.
   .dy = nrofplottedtiers*(.wy2-.wy1)/4
   'win$'Axes: .wx1, .wx2, .wy1 - .dy, .wy2
   'win$'Line width: 1
   'win$'Solid line
   'win$''.color$'
   @intervals_from_time_range: .gridID, nucleus_tier, .wx1, .wx2, "ifirst", "ilast"
   for .i from ifirst to ilast
      selectObject: .gridID
      @is_nucleus: .i
      if (result)
         .x1 = Get start time of interval: nucleus_tier, .i
         .x2 = Get end time of interval: nucleus_tier, .i
         .s$ = Get label of interval: pointer_tier, .i
         .row = number(.s$)
         selectObject: .table
         .y = Get value: .row, .col
         if (.y >= 0)
            .pos$ = "Bottom"
         else
            .pos$ = "Top"
         endif
         if (.format$ = "box_nucl" and .x2 >= .wx1 and .x1 <= .wx2)
            .x1 = max(.x1, .wx1)	; clip to world coordinates
            .x2 = min(.x2, .wx2)	; clip to world coordinates
            'win$'Draw rectangle: .x1, .x2, 0, .y
            .xmid = .x1+(.x2-.x1)/2
            'win$'Text special: .xmid, "Centre", .y, .pos$, "Helvetica", small_fontsize, "0", fixed$(.y,1)
         elsif (.format$ = "box_left" and .row > 1)
            .x2 = .x1
            .x1 = Get value: .row-1, j_nucl_t2
            if (.x2 >= .wx1 and .x1 <= .wx2)
               .x1 = max(.x1, .wx1)	; clip to world coordinates
               .x2 = min(.x2, .wx2)	; clip to world coordinates
               'win$'Draw rectangle: .x1, .x2, 0, .y
               .xmid = .x1+(.x2-.x1)/2
               'win$'Text special: .xmid, "Centre", .y, .pos$, "Helvetica", small_fontsize, "0", fixed$(.y,1)
            endif
         endif
      endif
   endfor
   'win$'Solid line
   'win$'Line width: 1
endproc


procedure gr_plot_prop_curve: .gridID, .table, .col, .wx1, .wx2, .wy1, .wy2, .color$, .linestyle$
# Plots a line connecting the points (x,y), 
# where x is the midtime of a given nucleus, and y the value of a property value of that nucleus.
# Also draws the value y (as a number) at (x,y).
# The nuclei are those of <.gridID>, the values correspond to the value in <.col> of <.table>.
#	<.col>								column for property
#	<.wx1>...<.wx2>, <.wy1>...<.wy2>	world coordinates to be used for the active viewport.
   'win$'Axes: .wx1, .wx2, .wy1, .wy2
   'win$''.color$'
   'win$'Line width: 2
   'win$'Solid line
   if (.linestyle$ = "Dotted")
      'win$'Dotted line
   elsif (.linestyle$ = "Dashed")
      'win$'Dashed line
   endif
   @intervals_from_time_range: .gridID, nucleus_tier, .wx1, .wx2, "first_interval", "last_interval"
   for .i from first_interval to last_interval
      selectObject: .gridID
      @is_nucleus: .i
      if (result)
         .x1 = Get start time of interval: nucleus_tier, .i
         .x2 = Get end time of interval: nucleus_tier, .i
         .s$ = Get label of interval: pointer_tier, .i
         .row = number(.s$)
         .lx2 = .x1+(.x2-.x1)/2
         if (.lx2 >= .wx1 and .lx2 <= .wx2 and .row <= nrof_nuclei_analysed)	; midtime falls inside window
            selectObject: .table
            .ly2 = Get value: .row, .col
            ;.radius = max (0.1 * ((.ly2-.wy1)/(.wy2-.wy1))/(.wx2-.wx1), 0.001) ; radius is relative to X axis, i.e. time
            'win$'Draw circle: .lx2, .ly2, 0.005 
            'win$'Text special: .lx2, "Left", .ly2, "Bottom", "Helvetica", small_fontsize, "0", fixed$(.ly2,1)
            if (.row > 1)			; get position and property value of previous nucleus
               .ly1 = Get value: .row-1, .col
               .x1 = Get value: .row-1, j_nucl_t1
               .x2 = Get value: .row-1, j_nucl_t2
               .lx1 = .x1+(.x2-.x1)/2	; mid time of previous nucleus
               if (.lx1 < .wx1)			; clip to world coordinates
                  .ly1 = .ly1 + (.ly2-.ly1)*(.wx1-.lx1)/(.lx2-.lx1)
                  .lx1 = .wx1
               endif
               'win$'Draw line: .lx1, .ly1, .lx2, .ly2
            endif
            if (.row < nrof_nuclei_analysed)	; get position and property value of next nucleus
               .x1 = Get value: .row+1, j_nucl_t1
               .x2 = Get value: .row+1, j_nucl_t2
               .lx3 = .x1 + (.x2-.x1)/2	; mid time of next nucleus
               if (.lx3 > .wx2)			; clip to world coordinates
                  .ly3 = Get value: .row+1, .col
                  .ly3 = .ly2 + (.ly3-.ly2)*(.wx2-.lx2)/(.lx3-.lx2)
                  .lx3 = .wx2
                  'win$'Draw line: .lx2, .ly2, .lx3, .ly3
               endif
            endif
         endif
      endif
   endfor
   'win$'Solid line
   'win$'Line width: 1
endproc


procedure gr_viewport_size
# Initialization of viewport. Set outer viewport of Prosogram strip.
# Set number of strips per page depending on number of tiers in TextGrid plotted. 
   vx1 = 0.5					; outer viewport left side
   vx2 = vx1 + viewport_width	; outer viewport right side
   vy1 = 0						; outer viewport top
   npt = max(0, nrofplottedtiers-1)
   if (viewsize$ = "compact")
      vy2 = 1.55 + npt * 0.15
      vyincr = 1.2 + npt * 0.15		; was 0.95
      plotspagetiers$ = "0:10, 1:10, 2:9, 3:7, 4:7, 5:6, 6:5"
   elsif (viewsize$ = "wide" or task == task_pitch_plot or task == task_annotation)
      vy2 = 1.8 + npt * 0.2
      vyincr = 1.45 + npt * 0.2		; was 1.3
      plotspagetiers$ = "0:7, 1:7, 2:6, 3:5, 4:4, 5:4, 6:3"
   else ; viewsize$ = "large"
      vy2 = 4 + npt * 0.3
      vyincr = vy2
      plotspagetiers$ = "0:2, 1:2, 2:2, 3:2, 4:2, 5:2, 6:2"
   endif

# In the drawing of a TextGrid, the place held by the tiers = (nrofplottedtiers/(nrofplottedtiers+4)) of the total height (inner viewport).
   strips_per_page = 1
   if (nrofplottedtiers <= 6)
      strips_per_page = extractNumber (plotspagetiers$, "'nrofplottedtiers':")
   endif
   'win$'Select outer viewport: vx1, vx2, vy1, vy2
   ; Select margin between outer and inner viewport
   vdx = (vx2-vx1)/13
   vdy = (vy2-vy1)/5.5
   'win$'Select inner viewport: vx1+vdx, vx2-vdx, vy1+vdy, vy2-vdy
   ; @msg: "gr_viewport_size: viewsize='viewsize$' npt='npt' strips_per_page='strips_per_page'"
   ; @msg: "gr_viewport_size: vx1='vx1:2' vx2='vx2:2' vy1='vy1:2' vy2='vy2:2' vdx='vdx:2' vdy='vdy:2' vyincr='vyincr:2'"
endproc


procedure gr_first_viewport_of_page
# Initialization of viewports and Postscript bounding box, at start of page
   Erase all
   @gr_viewport_size
   Line width: 1
   nrofstrips = 0	; used to decide when to start a new page
   # area of page used by drawings; for Postscript bounding box
      bb_x1 = vx1
      bb_x2 = vx2
      bb_y1 = vy1
      bb_y2 = vy2
endproc


procedure gr_next_viewport
   vy1 += vyincr
   vy2 += vyincr
   'win$'Select outer viewport: vx1, vx2, vy1, vy2
   'win$'Select inner viewport: vx1+vdx, vx2-vdx, vy1+vdy, vy2-vdy
   bb_y2 = vy2		; update Postscript bounding box
endproc


procedure gr_write_prosogram
# Write to EPS/EMF/PDF/JPG/PNG file, after adjusting bounding box and after constructing filename, using <output_fname$>
   # Praat draws figures inside viewport, leaving margins for garnish.
   # Modify BoundingBox to decrease margins. They will be reset by next call to first_viewport_of_page.
      if (viewsize$ = "compact")
         bb_y1 += 0.25	; top
         bb_x1 += 0.35
         bb_y2 -= 0.2	; bottom
         bb_x2 -= 0.5
      else				; wide or large
         bb_y1 += 0.1	; top (0.1 for PNG)
         bb_x1 += 0.35
         bb_y2 -= 0.2	; bottom (-0.2 for PNG)
         bb_x2 -= 0.5
      endif
      if (show_tiernames or show_y_scale)
         'win$'Select outer viewport: bb_x1, bb_x2+0.3, bb_y1, bb_y2
      else
         'win$'Select outer viewport: bb_x1, bb_x2, bb_y1, bb_y2
      endif

   # Construct filename for output file
      if (file_numbering)
         .padding$ = ""
         if (file_ctr <= 9)
            .padding$ = "00"
         elsif (file_ctr <= 99)
            .padding$ = "0"
         endif
         .output_file$ = output_fname$ + .padding$ + "'file_ctr'"
         file_ctr += 1
      else
         .output_file$ = output_fname$
      endif
      @debug_msg: "gr_write_prosogram: file_numbering='file_numbering' output_format='output_format$' output_file=<'.output_file$'>"
      if (index(output_format$, "EPS") or 
          ... ((index(output_format$, "PDF") or index(output_format$, "JPG")) and windows))
         epsfile$ = .output_file$ + ".eps"
         Write to EPS file: epsfile$
      endif
      if (index(output_format$, "EMF") > 0 and windows)
         Write to Windows metafile: "'.output_file$'.emf"
      endif
      if (index(output_format$, "PDF"))
         if (macintosh)
            Save as PDF file: "'.output_file$'.pdf"
         elsif (windows)
            if (fileReadable (path_ghostscript$))
               res = 300	; resolution in dpi
               command$ = path_ghostscript$ + " -dNOPAUSE -dBATCH -dQUIET -sDEVICE=pdfwrite -r'res' -sOutputFile=""'.output_file$'.pdf"" 'epsfile$'"
               system 'command$'
            else
               @debug_msg: "gr_write_prosogram: cannot access <'path_ghostscript$'>"
            endif
         endif
      endif
      if (index(output_format$,"JPG") and windows)
         res = 300	; resolution in dpi
         if (index(output_format$,"600"))
           res = 600
         endif
         .jpegq = 75	; default value for JPEGQ quality scale 
         if (variableExists ("jpegq"))
            .jpegq = max(1,min(floor(jpegq),100)) 
         endif
         @debug_msg: "gr_write_prosogram: jpegq='.jpegq'"
         command$ = path_ghostscript$ + " -dNOPAUSE -dBATCH -dQUIET -sDEVICE=jpeg -dJPEGQ='.jpegq' -sEPSCrop -r'res' -sOutputFile=""'.output_file$'.jpg"" 'epsfile$'"
            @debug_msg: "gr_write_prosogram: format='output_format$', fname=<'.output_file$'.jpg>"
         if (fileReadable (path_ghostscript$))
            system 'command$'
         else
            @debug_msg: "gr_write_prosogram: cannot access <'path_ghostscript$'>"
         endif
      endif
      if (index(output_format$, "PNG"))
         if (index(output_format$, "PNG 600 dpi"))
            Save as 600-dpi PNG file: "'.output_file$'.png"
         elsif (index(output_format$, "PNG 300 dpi"))
            Save as 300-dpi PNG file: "'.output_file$'.png"
         endif
      endif
endproc


procedure gr_write_all_prosograms: .at1, .at2, .timeincr
# Draw and save all prosograms for time interval <.at1>..<.at2>, using a prosogram window duration of <.timeincr>
# Expects global variables <iFile> and <nrofFiles>
   @debug_msg: "write_all_prosograms: entry"
   demowin = 0		; drawing in Picture window
   win$ = ""		; drawing in Picture window
   Font size: fontsize
   grid_in_prosogram = 1
   if (nrof_pages == 0)
       # grid takes part of plotting area, so adjust lower Y-value
      ySTbottom = ySTmin - nrofplottedtiers*(ySTmax-ySTmin)/4
      @from_ST_rel_1_to_Hz: ySTmin
      yHzmin = result
      @from_ST_rel_1_to_Hz: ySTmax
      yHzmax = result
      ; @msg: "Drawing with Y-range 'ySTmin:1'-'ySTmax:1'ST ('yHzmin:0'-'yHzmax:0' Hz)"
   endif
   .start_new_page = 1
   .t1 = .at1
   repeat		; for each prosogram strip
      if (.start_new_page)	; at start or when nrofstrips >= strips_per_page
         nrof_pages += 1
         @debug_msg: "write_all_prosograms: starting page nr 'nrof_pages', t1='.t1:3'"
         @gr_first_viewport_of_page
      endif
      .t2 = .t1 + .timeincr
      ; @msg: "gr_write_all_prosograms: Drawing '.t1:3' - '.t2:3'"
      if (auto_pitchrange)  ; automatic pitch range selection, local to each strip
         @speaker_autorange: .t1, .t2
         ySTmax = ymax
         ySTmin = ymin
      endif
      @gr_display_prosogram: .t1, .t2, ySTmin, ySTmax, grid_in_prosogram
      nrofstrips += 1
    # prepare for next window pane
      if (nrofstrips >= strips_per_page 
         ... or (nrofFiles == 1 and .t2 >= .at2)
         ... or (single_fname_graphics_output = 0 and .t2 >= .at2) 
         ... or (outputmode$ = "One") )
         @gr_write_prosogram
         .start_new_page = 1
         nrofstrips = 0
      elsif (.t2 < .at2 or iFile < nrofFiles)
         @gr_next_viewport
         .start_new_page = 0
      endif
      .t1 += .timeincr
   until (.t1 >= .at2)
   if (iFile == nrofFiles and nrofstrips > 1)		; Write last output file if necessary
      @gr_write_prosogram
   endif
   @debug_msg: "write_all_prosograms: exit"
endproc


procedure gr_write_all_annotation: .at1, .at2, .timeincr
   ySTmin = 0
   ySTmax = 100
   @gr_write_all_prosograms: .at1, .at2, .timeincr
endproc


;procedure multipage_pdf
;   ; pdf_in$ = replace_regex$ (input_files$, "\.eps", "\.pdf", 1)
;   pdf_in$ = replace_regex$ (input_files$, "\.eps", "\.pdf", 1)
;   pdf_out$ = replace_regex$ (input_files$, "[0-9\*]*\.eps", "\.pdf", 1)
;   pdf_files$ = ""
;   if (index (output_format$, "multipage"))
;      ; use wildcard expansion by pdftk in order to avoid command line buffer overflow
;      command$ = "pdftk 'pdf_in$' cat output 'pdf_out$'"
;      system 'command$'
;   endif
;endproc

