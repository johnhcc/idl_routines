; ----------------------------------------------------------------------------
; function DED_RECKON
; ----------------------------------------------------------------------------
  function ded_reckon, lon, lat, dist, az, check=check, sdeg=sdeg, dms=dms
;+
; NAME:
;   DED_RECKON
;
; PURPOSE:
;   DED_RECKON performs deduced (or dead) reckoning on the surface of a
;   sphere. It determines a destination position given an initial position,
;   direction of travel (along a great circle), and distance traveled. It is
;   essentially the inverse of IDL's MAP_2POINTS function.
;
; CALLING SEQUENCE:
;   Result = DED_RECKON(LON,LAT,DIST,AZ)
;
; INPUTS:
;   LON         initial longitude  (degrees)
;   LAT         initial latitude   (degrees)
;   DIST        distance of travel (meters, unless using /SDEG keyword)
;   AZ          azimuth of travel  (degrees; 0 is north, 270 is west, etc.)
;
; KEYWORD PARAMETERS:
;   CHECK       check results using MAP_2POINTS
;   SDEG        set this if DIST is in degrees instead of meters
;
; RETURNS:
;   A two-element vector containing the longitude (-180 to 180) and latitude
;   of the destination (degrees)
;
; EXAMPLE:
;   Use MAP_2POINTS to determine the angular distance and azimuth to travel
;   from (160E,42N) to (164E,56.5N):
;
;     print, map_2points(160,42,164,56.5)
;     --> 14.726904   8.7111372
;
;   This tells us that the two points are separated by an angular distance of
;   14.726904 degrees, which is equivalent to 1639409.3 m (to get this,
;   convert the angular distance to radians and multiply by the radius of the
;   earth), and the direction of travel is 8.7111372, which is just east of
;   north.
;
;   Therefore, if we start at (160E,42N) and travel this distance and azimuth,
;   we should end up at (164E,56.5N). Let's confirm that with DED_RECKON:
;
;     print, ded_reckon(160,42,14.726904,8.7111372,/sdeg)
;     --> 164.00000   56.500000
;
; REFERENCES:
;   Based on Aviation Formulary V1.46 by Ed Williams
;   (http://williams.best.vwh.net/avform.htm)
;
; MODIFICATION HISTORY:
;   2015/11/07  Programmed by John Haynes (https://github.com/johnhcc)
;-

  on_error, 2

  re = 6378206.4d0          ; radius of earth (m)

  lambda1 = lon * !dpi/180.
  phi1 = lat * !dpi/180.
  s = n_elements(sdeg) eq 0 ? dist/re : dist * !dpi/180.
  raz = az * !dpi/180.
  
  phi2 = asin(sin(phi1)*cos(s)+cos(phi1)*sin(s)*cos(-1.*raz))
  dlambda=atan(sin(-1.*raz)*sin(s)*cos(phi1),cos(s)-sin(phi1)*sin(phi2))
  y = lambda1 - dlambda + !dpi
  x = 2.*!dpi
  lambda2 = y - x*floor(y/x) - !dpi

  if keyword_set(check) then begin
    is_error = 0
    res = map_2points(lon,lat,lambda2*180./!dpi,phi2*180./!dpi,/meters)
    dist_m = s*re
    tol_m = 1.  ; flag as error if distance diff greater than this (m)
    
    if abs(res-dist_m) gt tol_m then begin
      print, "Distance difference exceeds tolerance: ", abs(res-dist_m)
      is_error = 1
    endif
    
    if is_error then message, 'Check failed'   
  endif

  return, [lambda2,phi2] * 180./!dpi
  
  end
