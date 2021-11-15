; ----------------------------------------------------------------------------
; pro LOAD_PNG_CTBL
; ----------------------------------------------------------------------------
  pro load_png_ctbl, fin, ncolors, bw=bw, ncl_bw=ncl_bw, rev=rev
;+
; NAME:
;   LOAD_PNG_CTBL
;
; PURPOSE:
;   Loads color tables from PNG files for use in IDL (in indexed color mode)
;
; CALLING SEQUENCE:
;   LOAD_PNG_CTBL, FIN, NCOLORS
;
; INPUTS:
;   FIN       A PNG color table file. Each column represents a color, and there
;             must be no more than 256 columns. Only the first row is
;             considered.
;
; OPTIONAL OUTPUT:
;   NCOLORS   number of colors in the original PNG color table
;
; KEYWORD PARAMETERS:
;   BW        set color index 0 to black and 255 to white
;   NCL_BW    set color index 0 to black and 1 to white (the NCL standard)
;   REV       load the PNG color table in reverse order
;
; NOTES:
;   LOAD_PNG_CTBL always loads a 256 color RGB color table.
;
;   By default, colors loaded from the PNG file will be accessible as color
;   indices 0 through NCOLORS-1. Other options:
;
;     With keyword BW, colors loaded from the PNG file will be accessible as 
;     color indices 1 to NCOLORS. Index 0 will be set to black (0,0,0) and 
;     index 255 will be set to white (255,255,255).
;
;     With keyword NCL_BW, colors loaded from the PNG file will be accessible
;     as color indices 2 to NCOLORS+1. Index 0 will be set to black and index 
;     1 will be set to white (conforms with NCL standard).
;
;     When using keyword BW or NCL_BW it may be necessary to truncate up to
;     one set of RGB values from both the bottom and top of the NCL color
;     table. The procedure will produce a warning when this is necessary.
;
;   Any remaining color indices are filled with white.
;
; MODIFICATION HISTORY
;   2017/09/09  Written by John Haynes (https://github.com/johnhcc)
;-

; // read from PNG file

  dat = read_png(fin)
  r = reform(dat[0,*,0])
  g = reform(dat[1,*,0])
  b = reform(dat[2,*,0])
  ncolors = n_elements(r)

  if ncolors gt 256 then message, 'PNG file cannot contain > 256 colors'
  
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
  