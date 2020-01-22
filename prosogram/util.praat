# util.praat --- Praat include file containing some utilities
#
# This file isn't a stand-alone script. It is included (indirectly) by prosogram.praat. Use prosogram.praat instead.
# Last modification: 2020-01-16


procedure files_get_regex: .varname$, .spec$
# Get list of file names using a regular expression (whereas "Create Strings as file list" only allows wildcard '*').
# <spec> contains full path with possibly regex symbols in filename part.
# <varname> is the name of the variable that will hold the list object ID.
   @fname_parts: .spec$
   .specfname$ = result1$		; fname without path
   ; @debug_msg: "files_get_regex: specfname='.specfname$'"
   .indir$ = result4$
   ; Find ALL files in the specified directory
   .listID = Create Strings as file list: "filelist", "'.indir$'*"
   .j = Get number of strings
   selectObject: .listID
   '.varname$' = .listID
   while (.j > 0)
      .s$ = Get string: .j
      .i = index_regex (.s$, .specfname$)
      if not (index_regex (.s$, .specfname$))	; Match with regular expression
         Remove string: .j
      else
         Set string: .j, "'.indir$''.s$'"
      endif
      .j -= 1
   endwhile
endproc


procedure file_windows_notation: .fname$
   result$ = replace_regex$(.fname$, "/", "\\", 0)
endproc


procedure file_rename: .oldfname$, .newfname$
# File is relative to script directory, unless absolute path is specified.
# On windows, newfname$ cannot be an absolute path.
  if not fileReadable (.oldfname$)
     @msg: "Cannot find <'.oldfname$'>"
  else
     if (windows)
        @file_windows_notation: .oldfname$
        .oldfname$ = result$
        @file_windows_notation: .newfname$
        .newfname$ = result$
        @debug_msg: "Command=<rename '.oldfname$' '.newfname$'>"
        .command$ = "rename"
     else
        .command$ = "mv"
     endif
     runSystem: .command$, " ", .oldfname$, " ", .newfname$
  endif  
endproc


procedure file_copy: .srcfname$, .dstfname$
# File is relative to script directory, unless absolute path is specified.
  if not fileReadable (.srcfname$)
     @msg: "Cannot find <'.srcfname$'>"
  else
     if (windows)
        @file_windows_notation: .srcfname$
        .srcfname$ = result$
        @file_windows_notation: .dstfname$
        .dstfname$ = result$
        @debug_msg: "Command=<copy '.srcfname$' '.dstfname$'>"
        .command$ = "copy"
     else
        .command$ = "cp"
     endif
     runSystem: .command$, " ", .srcfname$, " ", .dstfname$
  endif  
endproc


procedure fname_parts: .s$
# Obtain filename parts
# .s$		the total filename
# result1$	the filename without path
# result2$	the basename (i.e. no path, no extension)
# result3$	the filename extension (excluding dot)
# result4$	the file path (including trailing slash) including drive
# result5$	the file path (including trailing slash) excluding drive
# result6$	the drive (excluding separator ':' )
   @debug_msg: "fname_parts: entry"
   result1$ = .s$
   result2$ = ""
   result3$ = ""
   result4$ = ""
   result5$ = ""
   result6$ = ""
   .pos = rindex (.s$, "\")
   if (.pos = 0)
      .pos = rindex (.s$, "/")
   endif
   if (.pos > 0)
      result4$ = mid$ (.s$, 1, .pos)
      .len = length (.s$) - .pos
      result1$ = mid$ (.s$, .pos + 1, .len)
   endif
   .pos = rindex (result1$, ".")
   if (.pos > 0)
      .len = length (result1$)
      result3$ = right$ (result1$, .len - .pos)
      result2$ = left$ (result1$, .pos - 1)
   else
      result2$ = result1$
   endif
   .pos = index (result4$, ":")
   if (.pos = 2)
      .len = length (result4$)
      result5$ = right$ (result4$, .len - .pos)
      result6$ = left$ (result4$, .pos - 1)
   else
      result5$ = result4$
   endif
   @debug_msg: "fname_parts: exit"
endproc


procedure interval_from_time: .gridID, .tiernr, .t, varname$
# Return in <result> the interval number within tier <tiernr> of TextGrid <gridID> 
# in which time <t> occurs, taking into account cases where time is outside grid range
   @debug_msg: "interval_from_time: entry, varname='varname$'"
   selectObject: .gridID
   .result = Get interval at time: .tiernr, .t
   if (.result == 0) ; time outside grid range
      .tmp = Get start time
      if (.t <= .tmp)
         .result = 1
      else
         .tmp = Get end time
         if (.t >= .tmp)
            .result = Get number of intervals: .tiernr
         endif
      endif
   endif
   'varname$' = .result
   @debug_msg: "interval_from_time: exit"
endproc


procedure intervals_from_time_range: .gridID, .tiernr, .t1, .t2, varname1$, varname2$
   if (.t1 > .t2)
      @fatal_error: "intervals_from_time_range: t1 > t2" 
   endif
   @interval_from_time: .gridID, .tiernr, .t1, varname1$
   @interval_from_time: .gridID, .tiernr, .t2, varname2$
endproc


procedure tier_get_label_at_time: .grid, .tier, .time, .varname$
; for interval tier only
   '.varname$'$ = ""
   selectObject: .grid
   .interval_tier = Is interval tier: .tier
   if (.interval_tier)
      .i = Get interval at time: .tier, .time
      if (.i > 0)	
         '.varname$'$ = Get label of interval: .tier, .i
      endif
   endif
endproc


procedure tier_get_label_in_range .grid .tier .t1 .t2 .varname$
; for point tier
   '.varname$'$ = ""
   selectObject: .grid
   .interval_tier = Is interval tier: .tier
   if (not .interval_tier) 
      .k = Get nearest index from time: .tier, .t1+(.t2-.t1)/2
      .t = Get time of point: .tier, .k
      if (.t > .t1 and .t <=.t2)
         '.varname$'$ = Get label of point: .tier, .k
      endif
   endif
endproc


procedure tier_number_by_name: .gridID, name$
# Return in <result> the number of first tier corresponding to tier name.
# Return 0 if tier with name does not exist in textgrid.
# The target tier <name> may be a regular expression.
# Return tier name in <result2$>
   selectObject: .gridID
   .n = Get number of tiers
   result = 0
   .tier = 1
   while (.tier <= .n and result == 0)
      result2$ = Get tier name: .tier
      if (index_regex (result2$, name$)) 
         result = .tier
      else
         .tier += 1
         result2$ = ""
      endif
   endwhile
endproc


procedure tier_get: .gridID, tiername$, varname$, message$, .fatal
# In TextGrid object <.gridID>, find a tier with a name matching the regular expression
# in <tiername$> and return its number as the value of variable named <varname$>.
# When such a tier is not found, print the error message and exit is <.fatal> is true.
   @tier_number_by_name: .gridID, tiername$
   if (result < 1)
      if (.fatal)
         @fatal_error: message$
      else
         @msg: message$
      endif
   endif
   'varname$' = result
endproc


procedure tier_names: .grid
; Return in the variable result$ the list of tiernames in the TextGrid, in the form "1:name1 2:name2 3:name3" 
   selectObject: .grid
   .n = Get number of tiers
   result$ = ""
   for .j to .n
       .s$ = Get tier name: .j
       if (.j > 1)
          result$ = result$ + ", "
       endif
       result$ = result$ + "'.j':" + .s$
   endfor
endproc


procedure tier_align_on: .grid, .tier, .ontier, .dt
; Align boundaries in <.tier> on those in <.ontier>, when the time difference <= <.dt>
   selectObject: .grid
   .n = Get number of tiers
   if (.tier <= .n and .ontier <= .n and .tier <> .ontier)
      .ni = Get number of intervals: .ontier
	  for .i from 2 to .ni
	     .t = Get starting point: .ontier, .i
         .j = Get interval at time: .tier, .t 
         .t1 = Get starting point: .tier, .j
         .t2 = Get end point: .tier, .j
         .s$ = Get label of interval: .tier, .j
         if (.t = .t1) 
         elsif (.t-.t1 < .t2-.t and .t-.t1 <= .dt)
            Set interval text: .tier, .j, ""
            Insert boundary: .tier, .t
		    Remove boundary at time: .tier, .t1
            Set interval text: .tier, .j, .s$
         elsif (.t2-.t <= .dt)
			Insert boundary: .tier, .t
			Remove boundary at time: .tier, .t2
         endif
	  endfor
   endif
endproc
 

procedure tier_replace: .srcgrid, .srctier, .destgrid, .desttier, .dest_varname$
; Replace a tier of a grid by a tier of another grid.
; For speed, when possible, extract a tier, merge it with src TextGrid and duplicate it to appropriate position.
; However, when times don't match, copy interval by interval.
   selectObject: .destgrid
   .dest_t1 = Get start time
   .dest_t2 = Get end time
   .n = Get number of tiers
   selectObject: .srcgrid
   .src_t1 = Get start time
   .src_t2 = Get end time
   .tiername$ = Get tier name: .srctier
; msg: "tier_replace: src='.srcgrid' dst='.destgrid' n='.n' tiername='.tiername$'"
   if (.src_t1 == .dest_t1 and .src_t2 <= .dest_t2)
      .newgrid = Extract one tier: .srctier
      if (.src_t2 < .dest_t2)
         Extend time: .dest_t2-.src_t2, "End"
        .newgrid_t2 = Get end time
; @msg: "tier_replace: new endtime='.newgrid_t2' (signal_finish='signal_finish')"
      endif
      selectObject: .destgrid
      plus .newgrid
      .merged = Merge
      Remove tier: .desttier
      .ntiers = Get number of tiers
      Duplicate tier: .ntiers, .desttier, .tiername$
      Remove tier: .ntiers+1
      removeObject: .destgrid, .newgrid
      '.dest_varname$' = .merged 
   else		; times don't match
      @msg: "tier_replace (1): times dont match: src '.src_t1' '.src_t2' , dest '.dest_t1' '.dest_t2'"
      @copy_tier: .srcgrid, .srctier, .destgrid, .desttier
   endif
endproc


procedure tier_replace2: .srcgrid, .srctier, .destgrid, .desttier
   @tier_clear: .destgrid, .desttier
   @copy_tier: .srcgrid, .srctier, .destgrid, .desttier
endproc


procedure copy_tier: .srcgrid, .srctier, .destgrid, .desttier
# assumes destination tier is empty
   selectObject: .srcgrid
   .interval_tier = Is interval tier: .srctier
   if (.interval_tier)
      selectObject: .destgrid
      .n = Get number of intervals: .desttier
      .endtime = Get end time of interval: .desttier, .n 
      selectObject: .srcgrid
      .n = Get number of intervals: .srctier
      for .i to .n
         selectObject: .srcgrid
         .t1 = Get start time of interval: .srctier, .i
         .t2 = Get end time of interval: .srctier, .i
         .label$ = Get label of interval: .srctier, .i
         selectObject: .destgrid
         if (.t2 < .endtime and .i < .n)
            Insert boundary: .desttier, .t2
         endif
         if (.t1 <= .endtime)
            .t2 = min (.t2, .endtime)	; when destgrid is shorter than srcgrid
            .j = Get interval at time: .desttier, .t1+(.t2-.t1)/2
            Set interval text: .desttier, .j, .label$
         endif
      endfor
   else
      selectObject: .srcgrid
      .n = Get number of points: .srctier
      for .i to .n
         selectObject: .srcgrid
         .t = Get time of point: .srctier, .i
         .label$ = Get label of point: .srctier, .i
         selectObject: .destgrid
         Insert point: .desttier, .t, .label$
      endfor
   endif
endproc


procedure tier_clear: .grid, .tier
   selectObject: .grid
   .interval_tier = Is interval tier: .tier
   if (.interval_tier)
      .i = Get number of intervals: .tier
      .i -= 1
      while (.i > 0)
         Remove right boundary: .tier, .i
         .i -= 1
      endwhile
   else
      .n = Get number of points: .tier
      for .i to .n
         Remove point: .i
      endfor
   endif
endproc


procedure tier_clear_text: .grid, .tier
   selectObject: .grid
   .interval_tier = Is interval tier: .tier
   if (.interval_tier)
      .n = Get number of intervals: .tier
      for .i from 1 to .n
         Set interval text: .tier, .i, ""
      endfor
   endif
endproc


procedure grid_append_tier: .gridin, .tierin, .gridname$
# append tier <.tierin> from <.gridin> at end of <.gridname$> by creating a new grid, the ID of which will replace the value of variable <.gridname$>
   selectObject: .gridin
   .ok = Is interval tier: .tierin
   Extract tier: .tierin
   if (.ok)
      .tmp = selected ("IntervalTier", -1)
   else
      .tmp = selected ("TextTier", -1)
   endif
   .grid = '.gridname$'
   selectObject: .grid, .tmp
   .tmp2 = Append
   removeObject: .tmp, .grid
   '.gridname$' = .tmp2
endproc


procedure tier_match_times: .grid, .tier1, .tier2
   selectObject: .grid
   .n1 = Get number of intervals: .tier1
   .n2 = Get number of intervals: .tier2
   .s1$ = Get tier name: .tier1
   .s2$ = Get tier name: .tier2
   .n = min (.n1, .n2)
   for .i to .n
      .t1 = Get start point: .tier1, .i
      .t2 = Get start point: .tier2, .i
      if (.t1 <> .t2)
         printline Comparing times in tiers '.s1$' and '.s2$': first mismatch at time '.t1:3' 
         .i = .n
      endif
   endfor
endproc


procedure next_field: .s$
# Get next field form a string with comma-separated fields.
# Return success in <result>, return field in <result2$>, return rest of string in <result3$>
   result2$ = ""
   .len = length (.s$) 
   while (.len > 0 and left$ (.s$, 1) = " ")		; ltrim input
      .s$ = right$ (.s$, .len-1)
      .len -= 1 
   endwhile
   if (.len == 0)
      result = 0
   else
      .pos = index (.s$, ",")
      if (.pos = 0)
         result3$ = ""
      else
         result3$ = right$ (.s$, .len-.pos)			; rest of strings
         .s$ = left$ (.s$, .pos-1)
         .len = length (.s$)
      endif
      while (.len > 0 and left$ (.s$, 1) = " ")		; ltrim field
         .s$ = right$ (.s$, .len-1)
         .len -= 1 
      endwhile
      while (.len > 0 and right$ (.s$, 1) = " ")	; rtrim field
         .s$ = left$ (.s$, .len-1)
        .len -= 1 
      endwhile
      if (length (.s$) == 0)
         result = 0
      else
         result = 1
         result2$ = .s$
      endif
   endif
endproc


procedure next_line: stringname$, varname$
# Get next line form a string in variable named <stringname>
# Return line in variable named <varname$> and remainder of string in variable named <stringname$>
   .text$ = 'stringname$'$
   .len = length(.text$)
   .i = index(.text$, newline$)
   if (.i = 0)
      .i = .len
   endif
   'varname$'$ = left$(.text$, .i)
   'stringname$'$ = right$(.text$, .len-.i)
   .text$ = ""
endproc


procedure is_number: .s$
   result = extractNumber(.s$, "")
   if (result == undefined)
      result = 0
   else
      result = 1
   endif
endproc


procedure convert_Hz_ST: .objectID
# Convert a PitchTier from Hz values to ST scale, relative to 1 Hz
   .yoffset = hertzToSemitones(1)
   selectObject: .objectID
   .n = Get number of points
   for .i from 1 to .n
      .x = Get time from index: .i
      .y = Get value at index: .i
      Remove point: .i
      Add point: .x, hertzToSemitones(.y) - .yoffset
   endfor
endproc


procedure convert_ST_Hz: .objectID
# Convert a PitchTier from ST values (relative to 1 Hz) to Hz values 
   .yoffset = hertzToSemitones(1)
   selectObject: .objectID
   .n = Get number of points
   for .i from 1 to .n
      .x = Get time from index: .i
      .y = Get value at index: .i
      Remove point: .i
      Add point: .x, semitonesToHertz(.y + .yoffset)
   endfor
endproc


procedure from_ST_rel_1_to_Hz: .value
   result = semitonesToHertz(.value + hertzToSemitones(1))
endproc


procedure from_Hz_to_ST_rel_1: .value
   result = hertzToSemitones(.value) - hertzToSemitones(1) 
endproc



# ------------------------------ Messages ------------------------------ #


procedure logging: .option$, logfile$
# Write messages (calls to: msg, debug_msg, time_msg, error_msg, fatal_error) to logfile. 
# Options:
# "reset"	resets the logfile (whereas otherwise new messages are appended to the logfile, if it exists)
# "timed"	selects the output format where timing information is added to the message
# "noinfo"	suppresses output to Info window and writes messages to logfile only.
# "debug"	enables debug messages (debug_msg).
# "nodebug"	disables debug messages (debug_msg).
   logging = 1
   logging_timed = 0
   logging_noinfo = 0
   if (index (.option$, "timed"))
      logging_timed = 1
   endif
   if (index (.option$, "reset"))
      filedelete 'logfile$'
   endif
   if (index (.option$, "noinfo"))
      logging_noinfo = 1
   endif
   if (index (.option$, "nodebug"))
      logging_debug = 0
      @msg: "Logging to 'logfile$' stopped"
   elsif (index (.option$, "debug"))
      logging_debug = 1
      @msg: "Logging to 'logfile$' started"
   else
      logging_debug = 0
   endif
endproc


procedure msg: .message$
   if (variableExists ("logging"))
      if (logging)
         if (logging_timed)
            .date$ = date$ ()
            appendFileLine: logfile$, "'tab$'@ '.date$'"
         endif
         appendFileLine: logfile$, .message$
         if (not logging_noinfo)
            appendInfoLine: .message$
         endif
      else
         appendInfoLine: .message$
      endif
   else
      appendInfoLine: .message$
   endif
endproc


procedure debug_msg: .message$
   if (variableExists ("logging_debug"))
      if (logging_debug)
         @msg: "*** DEBUG *** " + .message$
      endif
   endif
endproc


procedure time_msg: .message$
   .date$ = date$ ()
   @msg: "'.date$' '.message$'"
endproc


procedure error_msg: .message$
   @msg: "*** ERROR *** '.message$'"
endproc


procedure fatal_error: .message$
   @msg: "*** FATAL ERROR *** '.message$'"
   exit
endproc


# ------------------------------ Miscellaneous ------------------------------ #


procedure toggle: varname$
   if ('varname$' == 0)
      'varname$' = 1
   else
      'varname$' = 0
   endif
endproc


procedure steps_012: varname$
   .v = 'varname$'
   if (.v >= 2)
      .v = 0
   else
      .v += 1
   endif
   'varname$' = .v
endproc


procedure play_part: .objectID, .t1, .t2
     selectObject: .objectID
     .tmpsoundID = Extract part: .t1, .t2, "Rectangular", 1.0, "yes"
     Play
     removeObject: .tmpsoundID
endproc

