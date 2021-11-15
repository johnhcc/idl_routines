; ----------------------------------------------------------------------------
; pro COLORBAR_DISCRETE
; ----------------------------------------------------------------------------
;+
; NAME:
;   COLORBAR_DISCRETE 
;
; PURPOSE:
;   Draws a colorbar with discrete color intervals
;
; CALLING SEQUENCE:
;   COLORBAR_DISCRETE, COLORS, LABELS
;
; INPUTS:
;   COLORS       a vector of color indices (0-255), one per interval;
;                dim (NINT)
;   LABELS       a vector of labels, dim (NINT+1), or (NINT) if keyword
;                ONE_EACH is used
; KEYWORD PARAMETERS:
;   POSITION     position of colorbar in normal coordinates
;   VERTICAL     produce a vertical colorbar (default is horizontal)
;   ONE_EACH     place one label per interval
;   MIN_COLOR    place a triangle at the low end of the colorbar and fill it
;                with this color index
;   MAX_COLOR    place a triangle at the high end of the colorbar and fill it
;                with this color index
;   TITLE        colorbar title
;   NO_BORDER    don't draw a line around boxes
;   CCOLOR       color index for labels and lines around boxes (default=0)
;   ROTATION     rotate labels counterclockwise by this many degrees
;   LSIZE        character size for labels
;   TSIZE        character size for title
;   ADJ_TITLE    adjust title position by this amount (e.g. +0.05 or -0.05)
;   ADJ_LABELS   adjust labels position by this amount (e.g. +0.05 or -0.05)
;   OUTER_BORDER also draw a rectangle around box
;                (* temporary hack: only works horizontal, ignores end arrows)
;
; MODIFICATION HISTORY
;   2014/11/11  Written by John M. Haynes (https://github.com/johnhcc)
;-

  pro colorbar_discrete, colors, labels, $
    position=position, $
    vertical=vertical, $
    one_each=one_each, $
    min_color=min_color, $
    max_color=max_color, $
    title=title, $
    no_border=no_border, $
    outer_border=outer_border, $
    ccolor=ccolor, $
    rotation=rotation, $
    lsize=lsize, $
    tsize=tsize, $
    adj_title=adj_title, $
    adj_labels=adj_labels
 
  if n_elements(vertical) eq 0 then vertical=0
  if n_elements(rotation) eq 0 then rotation=0.
  if n_elements(adj_labels) eq 0 then adj_labels=0.
  if n_elements(adj_title) eq 0 then adj_title=0.

; // number of intervals
  
  nint = n_elements(colors)

  if vertical eq 0 then begin

  ; ----- horizontal colorbar -----

  ; // left and right box postions

    xleft = fltarr(nint)
    xres = (position[2]-position[0])/(nint)
    xleft[0] = position[0]
    for i=1L,nint-1 do xleft[i] = xleft[i-1]+xres
    xright = xleft + xres
  
  ; // bottom and top box positions

    ybot = position[1]
    ytop = position[3]

  ; // draw boxes

    for i=0L,nint-1 do begin
      x = [xleft[i],xleft[i],xright[i],xright[i]]
      y = [ybot,ytop,ytop,ybot]
      polyfill, x, y, color=colors[i], /normal
      if keyword_set(no_border) eq 0 then $
        plots, [x,xleft[i]], [y,ybot], color=ccolor, /normal
    endfor

  ; // draw labels

    if rotation eq 0. then begin
      dely = 0.05 - adj_labels
      align=0.5
    endif else begin
      dely = 0.035 - adj_labels
      align=1.0
    endelse
    if keyword_set(one_each) then begin
      for i=0L,nint-1 do begin
        xyouts, (xleft[i]+xright[i])/2., ybot-dely, labels[i], align=align, $
          charsize=lsize, orientation=rotation, color=ccolor, /normal
      endfor
    endif else begin
      for i=0L,nint-1 do begin
        xyouts, xleft[i], ybot-dely, labels[i], align=align, $
          charsize=lsize, orientation=rotation, color=ccolor, /normal
      endfor
      xyouts, xright[nint-1], ybot-dely, labels[nint], align=align, $
        charsize=lsize, orientation=rotation, color=ccolor, /normal
    endelse

  ; // left triangle
   
    if n_elements(min_color) gt 0 then begin
      if min_color ge 0 then begin 
        x = [xleft[0],xleft[0]-xres,xleft[0]]
        y = [ybot,(ybot+ytop)/2.,ytop]
        polyfill, x, y, color=min_color, /normal
        if keyword_set(no_border) eq 0 then $
          plots, [x,xleft[0]], [y,ybot], color=ccolor, /normal   
      endif
    endif

  ; // right triangle
   
    if n_elements(max_color) gt 0 then begin
      if max_color ge 0 then begin 
        x = [xright[nint-1],xright[nint-1]+xres,xright[nint-1]]
        y = [ybot,(ybot+ytop)/2.,ytop]
        polyfill, x, y, color=max_color, /normal
        if keyword_set(no_border) eq 0 then $
          plots, [x,xright[nint-1]], [y,ybot], color=ccolor, /normal    
      endif
    endif
    
  ; // title
  
    if n_elements(title) gt 0 then begin
      xyouts, (xleft[0]+xright[nint-1])/2., ytop+0.025+adj_title, title, $
        align=0.5, charsize=tsize, color=ccolor, /normal   
    endif

    if n_elements(outer_border) gt 0 then begin
      plots, [xleft[0],xright[nint-1]], [ybot,ybot], color=ccolor, /normal
      plots, [xright[nint-1],xright[nint-1]], [ybot,ytop], color=ccolor, /normal
      plots, [xleft[0],xright[nint-1]], [ytop,ytop], color=ccolor, /normal
      plots, [xleft[0],xleft[0]], [ybot,ytop], color=ccolor, /normal

    endif

  endif else begin

  ; ----- vertical colorbar -----

  ; // left and right box postions

    xleft = position[0]
    xright = position[2]

  ; // bottom and top box positions

    ybot = fltarr(nint)
    yres = (position[3]-position[1])/(nint)
    ybot[0] = position[1]
    for i=1L,nint-1 do ybot[i] = ybot[i-1]+yres
    ytop = ybot + yres

  ; // draw boxes

    for i=0L,nint-1 do begin
      x = [xleft,xleft,xright,xright]
      y = [ybot[i],ytop[i],ytop[i],ybot[i]]
      polyfill, x, y, color=colors[i], /normal
      if keyword_set(no_border) eq 0 then $
        plots, [x,xleft], [y,ybot[i]], color=ccolor, /normal
    endfor

  ; // draw labels

    if rotation eq 0. then begin
      delx = 0.015 + adj_labels
      align=0.5
    endif else begin
      delx = 0.015 + adj_labels
      align=1.0
    endelse
    if keyword_set(one_each) then begin
      for i=0L,nint-1 do begin
        xyouts, xright+delx, (ybot[i]+ytop[i])/2., labels[i], align=0., $
          charsize=lsize, orientation=rotation, color=ccolor, /normal
      endfor
    endif else begin
      for i=0L,nint-1 do begin
        xyouts, xright+delx, ybot[i], labels[i], align=0., $
          charsize=lsize, orientation=rotation, color=ccolor, /normal
      endfor
      xyouts, xright+delx, ytop[nint-1], labels[nint], align=0., $
        charsize=lsize, orientation=rotation, color=ccolor, /normal
    endelse

  ; // bottom triangle
   
    if n_elements(min_color) gt 0 then begin
      x = [xleft,xright,(xleft+xright)/2.]
      y = [ybot[0],ybot[0],ybot[0]-yres]
      polyfill, x, y, color=min_color, /normal
      if keyword_set(no_border) eq 0 then $
        plots, [x,xleft], [y,ybot[0]], color=ccolor, /normal   
    endif

  ; // top triangle
   
    if n_elements(max_color) gt 0 then begin
      x = [xleft,xright,(xleft+xright)/2.]
      y = [ytop[nint-1],ytop[nint-1],ytop[nint-1]+yres]
      polyfill, x, y, color=max_color, /normal
      if keyword_set(no_border) eq 0 then $
        plots, [x,xleft], [y,ytop[nint-1]], color=ccolor, /normal  
    endif  

  ; // title
  
    if n_elements(title) gt 0 then begin
      xyouts, xleft-0.02+adj_title, (ybot[0]+ytop[nint-1])/2., title, $
        align=0.5, charsize=tsize, orientation=90., color=ccolor, $
        /normal   
    endif
  
  endelse
  
  end
