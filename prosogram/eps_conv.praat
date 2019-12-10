# EPS conversions 
# Apply conversion to all matching EPS files in directory
# Author: Piet Mertens
# Date: 2009-01-10
# Last modification: 2015-08-24

form Convert EPS file(s) to other file format (PDF, PNG, JPG, GIF)  Apr 6, 2012
	comment Enter the name (pattern) of input file(s) 
	text Input_files C:\corpus\*.eps
	comment Output directory (including trailing slash) 
	text Output_directory <same_as_input>
	optionmenu Output_format: 3
		option GIF
		option PDF
		option JPG
		option PNG
		option multipage PDF of multiple EPS files
	optionmenu Resolution: 4
		option 72 (GIF, JPG, PNG, PDF)
		option 96 (GIF, PNG)
		option 120 (GIF, PNG)
		option 300 (GIF, JPG, PNG, PDF)
		option 600 (JPG, PDF)
endform

# This script assumes Ghostscript, nconvert and pdftk are installed.
# nconvert is required when converting to GIF format.
# pdftk is required when creating multipage PDF files.

# Ghostscript is available from http://www.cs.wisc.edu/~ghost/
# nconvert is available from http://www.xnview.com/
# pdftk is available from http://www.pdflabs.com/tools/pdttk-the-pdf-toolkit/

# Modify the following lines to match the path of these applications on your computer.

path_ghostscript$ = "C:\Program Files\gs\gs9.16\bin\gswin64c.exe"	  
path_nconvert$ = "C:\Program Files\XnView\nconvert"

# Don't change the following lines:

path_ghostscript$ = replace$ (path_ghostscript$, "\Program Files\", "\Progra~1\", 1)
path_nconvert$ = replace$ (path_nconvert$, "\Program Files\", "\Progra~1\", 1)

# ---------------------------------------------------------------

# commands for EPS to GIF
eps_png$ = path_ghostscript$ + " -dNOPAUSE -dBATCH -dQUIET -sDEVICE=png256 -sEPSCrop"
png_gif$ = path_nconvert$

# command fragments for EPS to PDF
eps_pdf$ = path_ghostscript$ + " -dNOPAUSE -dBATCH -dQUIET -sDEVICE=pdfwrite"

# command fragments for EPS to PDF
eps_jpg$ = path_ghostscript$ + " -dNOPAUSE -dBATCH -dQUIET -sDEVICE=jpeg -sEPSCrop"

    if (rindex (input_files$,".") == 0)
       call fatal_msg Invalid filename for input file: missing dot
    endif

    call fname_parts 'input_files$'
    indir$ = result4$
    basename$ = result2$
    if (index (input_files$, "*") > 0)
       use_filelist = 1
       # Obtain list of files to process
       Create Strings as file list... filelist 'input_files$'
       nrofFiles = Get number of strings
       if (nrofFiles < 1)
         call fatal_msg No files matching input specification
       endif
    else
       use_filelist = 0
       nrofFiles = 1
       if (not fileReadable (input_files$))
          call fatal_msg Cannot open file
       endif
    endif
    if (output_directory$ = "<same_as_input>")
       outdir$ = indir$
    else
       call fname_parts output_directory$
       outdir$ = result4$
    endif
;printline INDIR='indir$'
;printline OUTDIR='outdir$'

pdf_out$ = replace_regex$ (input_files$, "[0-9\*]*\.eps", "\.pdf", 1)
pdf_in$ = replace_regex$ (input_files$, "\.eps", "\.pdf", 1)
pdf_files$ = ""
for iFile to nrofFiles
    if (use_filelist)
       select Strings filelist
       fname$ = Get string... iFile
       call fname_parts 'fname$'
       basename$ = result2$
       fname$ = basename$
    else
       fname$ = basename$
    endif
    pdf_files$ = pdf_files$ + "'basename$'.pdf "

    src$ = indir$ + fname$
    dst$ = outdir$ + fname$

    res = extractNumber(resolution$, "")
    # Call batch or system
    if (output_format$ = "GIF")
       command$ = "'eps_png$' -r'res' -sOutputFile=""'dst$'.png"" ""'src$'.eps"""
       system 'command$'
       command$ = "'png_gif$' -quiet -out gif 'dst$'.png"
       system 'command$'
       filedelete 'dst$'.png
    elsif (output_format$ = "PNG")
       system 'eps_png$' -r'res' -sOutputFile="'dst$'.png" "'src$'.eps"
    elsif (index (output_format$, "PDF"))
       if (res == 96 or res == 120)
          printline Invalid resolution for PDF. Using 300 dpi.
          res = 300
       endif
       system 'eps_pdf$' -r'res' -sOutputFile="'dst$'.pdf" "'src$'.eps"
    elsif (output_format$ = "JPG")
       if (res == 96 or res == 120)
          printline Invalid resolution for JPEG. Using 300 dpi.
       endif
       command$ = "'eps_jpg$' -r'res' -sOutputFile=""'dst$'.jpg"" ""'src$'.eps"""
;printline COMMAND='command$'
       if (not fileReadable (path_ghostscript$))
         call fatal_msg JPG output requires Ghostscript, which is not found at the path specified:  ('path_ghostscript$')'newline$'Verify configuration for Ghostcript.
       endif
       system 'command$'
    endif
endfor
if (index (output_format$, "multipage"))
; use wildcard expansion by pdftk in order to avoid command line buffer overflow
    command$ = "pdftk 'pdf_in$' cat output 'pdf_out$'"
    system 'command$'
endif

    if (use_filelist)
       select Strings filelist
       Remove
    endif
exit


procedure fname_parts s_$
# Obtain filename parts
# s_$		the total filename
# result1$	the filename without path
# result2$	the basename (i.e. no path, no extension)
# result3$	the filename extension (excluding dot)
# result4$	the file path (including trailing slash) including drive
# result5$	the file path (including trailing slash) excluding drive
# result6$	the drive (excluding separator ':' )
   result1$ = s_$
   result2$ = ""
   result3$ = ""
   result4$ = ""
   result5$ = ""
   result6$ = ""
   pos_ = rindex (s_$, "\")
   if (pos_ = 0)
      pos_ = rindex (s_$, "/")
   endif
#printline fname parts s_$ 's_$' pos 'pos_'
   if (pos_ > 0)
      result4$ = mid$ (s_$, 1, pos_)
      len_ = length (s_$) - pos_
      result1$ = mid$ (s_$, pos_ + 1, len_)
   endif
   pos_ = rindex (result1$, ".")
   if (pos_ > 0)
      len_ = length (result1$)
      result3$ = right$ (result1$, len_ - pos_)
      result2$ = left$ (result1$, pos_ - 1)
   else
      result2$ = result1$     
   endif
   pos_ = index (result4$, ":")
   if (pos_ = 2)
      len_ = length (result4$)
      result5$ = right$ (result4$, len_ - pos_)
      result6$ = left$ (result4$, pos_ - 1)
   else
      result5$ = result4$
   endif
endproc


procedure fatal_msg text$
   printline 'text$'
   exit
endproc
