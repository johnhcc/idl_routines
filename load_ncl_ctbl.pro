; ----------------------------------------------------------------------------
; pro LOAD_NCL_CTBL
; ----------------------------------------------------------------------------
  pro load_ncl_ctbl, fin, ncolors, bw=bw, ncl_bw=ncl_bw, rev=rev
;+
; NAME:
;   LOAD_NCL_CTBL
;
; PURPOSE:
;   Loads NCL color tables for use in IDL (in indexed color mode). These
;   tables are available as a tar archive from:
;     http://www.ncl.ucar.edu/Document/Graphics/color_table_gallery.shtml
;
; CALLING SEQUENCE:
;   LOAD_NCL_CTBL, FIN, NCOLORS
;
; INPUTS:
;   FIN       an NCL color table file (something.rgb)
;
; OPTIONAL OUTPUT:
;   NCOLORS   number of colors in the original NCL color table
;
; KEYWORD PARAMETERS:
;   BW        set color index 0 to black and 255 to white
;   NCL_BW    set color index 0 to black and 1 to white (the NCL standard)
;   REV       load the NCL color table in reverse order
;
; NOTES:
;   LOAD_NCL_CTBL always loads a 256 color RGB color table.
;
;   By default, colors loaded from the NCL color table will be accessible as
;   color indices 0 through NCOLORS-1. Other options:
;
;     With keyword BW, colors loaded from the NCL color table will be
;     accessible as color indices 1 to NCOLORS. Index 0 will be set to black
;     (0,0,0) and index 255 will be set to white (255,255,255).
;
;     With keyword NCL_BW, colors loaded from the NCL color table will be
;     accessible as color indices 2 to NCOLORS+1. Index 0 will be set to
;     black and index 1 will be set to white (conforms with NCL standard).
;
;     When using keyword BW or NCL_BW it may be necessary to truncate up to
;     one set of RGB values from both the bottom and top of the NCL color
;     table. The procedure will produce a warning when this is necessary.
;
;   Any remaining color indices are filled with white.
;
; MODIFICATION HISTORY
;   2014/11/19  Written by John Haynes (https://github.com/johnhcc)
;-

; // read the file
  
  n = file_lines(fin)
  the_lines = strarr(n)
  openr, lun, fin, /get_lun
  readf, lun, the_lines
  free_lun, lun

; // convert lines to color table

; - find lines whose first non-space character is a number
; - split those lines into three parts

  nums = ['0','1','2','3','4','5','6','7','8','9']

  r = fltarr(256)
  g = fltarr(256)
  b = fltarr(256)
  
  ncolors = 0
  
  for i=0,n-1 do begin
  
    val_nospace = strtrim(the_lines[i],2)
    loc = where(strmid(val_nospace,0,1) eq nums, cnt)
    
    if (cnt eq 1) then begin
    
    ; -- found a line with RGB colors
      if ncolors eq 256 then begin
         message, 'Not sure how to convert a color table with ' $
          + 'more than 256 colors'  ; this would be the 257th color
      endif
    
    ; -- extract RGB values    
      res = strsplit(val_nospace,' ,'+string(9b),/extract)
      r[ncolors] = res[0]
      g[ncolors] = res[1]
      b[ncolors] = res[2]
      ncolors++
    endif

  endfor

; // convert to 0-255 RGB values if needed

  loc = where((r gt 0.) and (r lt 1.), cnt)
  if cnt gt 0 then begin
    r = temporary(round(r[0:ncolors-1]*255.))
    g = temporary(round(g[0:ncolors-1]*255.))
    b = temporary(round(b[0:ncolors-1]*255.))
  endif else begin
    r = temporary(round(r[0:ncolors-1]))
    g = temporary(round(g[0:ncolors-1]))
    b = temporary(round(b[0:ncolors-1]))
  endelse

; // if using /bw and the original table has 255 or 256 colors, some
; // colors need to be truncated

  if (keyword_set(bw) or keyword_set(ncl_bw)) then begin
    if ncolors eq 256 then begin
      message, 'Truncating colors (1 top, 1 bottom) for table: ' + fin, /info
      r = temporary(r[1:ncolors-2])
      g = temporary(g[1:ncolors-2])
      b = temporary(b[1:ncolors-2])
      ncolors -= 2
    endif else if ncolors eq 255 then begin
      message, 'Truncating colors (1 bottom) for table: ' + fin, /info
      r = temporary(r[0:ncolors-2])
      g = temporary(g[0:ncolors-2])
      b = temporary(b[0:ncolors-2])
      ncolors -= 1
    endif
  endif

; // construct new color table
  
  r_new = make_array(256,/int,value=255)
  g_new = make_array(256,/int,value=255)
  b_new = make_array(256,/int,value=255)

  if keyword_set(bw) then begin

  ; // first color is black

    r_new[0] = 0
    g_new[0] = 0
    b_new[0] = 0

  ; // middle colors from original color table

    if keyword_set(rev) then begin    
      r_new[1:ncolors] = reverse(r)
      g_new[1:ncolors] = reverse(g)
      b_new[1:ncolors] = reverse(b)
    endif else begin
      r_new[1:ncolors] = r
      g_new[1:ncolors] = g
      b_new[1:ncolors] = b    
    endelse   

  ; // last color is white

    r_new[255] = 255
    g_new[255] = 255
    b_new[255] = 255

  endif else if keyword_set(ncl_bw) then begin

  ; // first color is black

    r_new[0] = 0
    g_new[0] = 0
    b_new[0] = 0
    
  ; // second color is white

    r_new[1] = 255
    g_new[1] = 255
    b_new[1] = 255

  ; // middle colors from original color table

    if keyword_set(rev) then begin
      r_new[2:ncolors+1] = reverse(r)
      g_new[2:ncolors+1] = reverse(g)
      b_new[2:ncolors+1] = reverse(b)
    endif else begin
      r_new[2:ncolors+1] = r
      g_new[2:ncolors+1] = g
      b_new[2:ncolors+1] = b    
    endelse

  endif else begin

    if keyword_set(rev) then begin
      r_new[0:ncolors-1] = reverse(r)
      g_new[0:ncolors-1] = reverse(g)
      b_new[0:ncolors-1] = reverse(b)
    endif else begin
      r_new[0:ncolors-1] = r
      g_new[0:ncolors-1] = g
      b_new[0:ncolors-1] = b    
    endelse

  endelse
  
; // load new color table

  tvlct, r_new, g_new, b_new
  
  end
  