; ----------------------------------------------------------------------------
; procedure CDF_GET
; ----------------------------------------------------------------------------
  pro cdf_get, fname, varid, varname=varname, level=level, list=list, $
    scale=scale, scaatt=scaatt, offatt=offatt, noscale_val=noscale_val, $
    noscale_set=noscale_set, start_at=start_at, stop_at=stop_at
;+
; NAME:
;   CDF_GET
;
; PURPOSE:
;   Get a variable from a netCDF file given the variable name (supports
;     netCDF classic, 64-bit offset, netCDF-4, and netCDF-4 classic formats)
;
; CALLING SEQUENCE:
;   CDF_GET, FNAME, VARID
;
; INPUTS:
;   FNAME     netCDF filename
;   VARID     string or array of strings containing the variable name(s) to
;             retrieve (default is all)
;
; KEYWORD PARAMETERS:
;   VARNAME   string variable with the same number of elements as VARID
;             containing name(s) to assign to retrieved variable(s); specify
;             an empty string for default
;   LEVEL     program level to receive the variables (default=-1, meaning
;             the program level which called CDF_GET)
;   LIST      list variables only
;
; (Options for data scaling:)
;
;   SCALE         set this keyword to search dataset attributes for scale and
;                 offset factors given by SCAATT and OFFATT; if found,
;                 retrieves the factors and applies the scaling, returning
;                 (original value)*scale + offset
;   SCAATT        scalar string defining name of scale factor attribute
;   OFFATT        scalar string defining name of offset factor attribute
;   NOSCALE_VAL   a scalar or vector of up to five value(s) that, if present,
;                 should not be scaled, but set to the corresponding value of
;                 NOSCALE_SET instead
;   NOSCALE_SET  see above, must be same number of elements as NOSCALE_VAL
;
; (Options to retrieve a subset of the full data:)
;
;   START_AT  a vector of indice(s) where subsetting will start
;   STOP_AT   a vector of indice(s) where subsetting will stop
;
; OUTPUTS:
;   The variable(s) are returned directly to the calling routine by default
;
; EXAMPLES:
;   cdf_get, 'filename.nc', 'pressure'
;   cdf_get, 'filename.nc', ['pressure','temperature']
;   cdf_get, 'filename.nc', ['pressure','temperature'], varname=['p','t']
;
; NOTE:
;   Spaces and dashes in variable names are converted to underscores.
;   Retrieving variables with names that are otherwise unacceptable as IDL
;   variable names may cause problems.
;
; MODIFICATION HISTORY:
;   2006/01/18  Written by John Haynes (https://github.com/johnhcc)
;   2009/01/19  Rewritten for efficiency (JMH)
;   2010/10/21  Supports sending to different program levels (JMH)
;   2013/03/17  Supports scaling, added START_AT and STOP_AT arguments (JMH)
;-

  on_error, 2

  ncid = NCDF_OPEN(fname)
  ncidinfo = NCDF_INQUIRE(ncid)

; // return variables to calling routine unless otherwise specified

  if n_elements(level) eq 0 then level = -1

; // if keyword /LIST, display the variables and return

  if keyword_set(list) then begin

    for i=0L,ncidinfo.Nvars-1 do begin
      vardata = NCDF_VARINQ(ncid,i)
      print, vardata.name
    endfor
    return
    
  endif

; // get variable names and number of attributes

  var_int_name = strarr(ncidinfo.Nvars)
  var_nattr = lonarr(ncidinfo.Nvars)
  for i=0L,ncidinfo.Nvars-1 do begin
    vardata = NCDF_VARINQ(ncid,i)
    var_int_name[i] = strupcase(vardata.name)
    var_nattr[i] = vardata.natts
  endfor
  
  if (n_elements(varid) eq 0) then begin

  ; // all variables are to be retrieved

    varid = var_int_name
    varname = var_int_name
    nvars = ncidinfo.Nvars

  endif else begin

  ; // only select variables are to be retrieved

    varid = strupcase(varid)
    nvars = n_elements(varid)

    if (n_elements(varname) eq 0) then begin
      varname = varid
    endif else begin
      if (n_elements(varname) ne n_elements(varid)) then $
        message, 'varname must have same number of elements as input array'
    endelse

  endelse

; // loop over variables to retrieve

  for i=0L,nvars-1 do begin

    g = (where(var_int_name eq varid[i],cnt))[0]

    if (cnt eq 0) then begin    
      message, 'Variable not found - ' + varid[i], /info
    endif else begin

    ; // retrieve the variable with index g into variable tmp

      if (n_elements(start_at) eq 0) then begin
        NCDF_VARGET, ncid, g, tmp
      endif else begin
        NCDF_VARGET, ncid, g, tmp, offset=start_at, count=stop_at-start_at+1
      endelse

    ; // output variable name: replace ' ' and '-' with '_'

      if varname[i] eq '' then varname[i] = varid[i]
      q = strpos(varname[i],' ')
      while (q ne -1) do begin
        varout = varname[i]
        strput, varout, '_', q
        varname[i] = varout 
        q = strpos(varname[i],' ')
      endwhile
      q = strpos(varname[i],'-')
      while (q ne -1) do begin
        varout = varname[i]
        strput, varout, '_', q
        varname[i] = varout 
        q = strpos(varname[i],'-')
      endwhile
    
    ; // search for scaling factor/offset attributes

      if keyword_set(scale) then begin
        scalef = 1.
        offset = 0.
        for j=0,var_nattr[g]-1 do begin
          res = ncdf_attname(ncid, g, j)
          if (res eq scaatt) then $
            ncdf_attget, ncid, g, scaatt, scalef
          if (res eq offatt) then $
            ncdf_attget, ncid, g, offatt, offset
        endfor

      ; // this looks messy, but it avoids the necessity of making a temporary
      ;    copy of the data array, tmp

        cnt0 = 0 & cnt1 = 0 & cnt2 = 0 & cnt3 = 0 & cnt4 = 0
        if (n_elements(noscale_val) ge 1) then b0 = where(tmp eq noscale_val[0],cnt0)
        if (n_elements(noscale_val) ge 2) then b1 = where(tmp eq noscale_val[1],cnt1)
        if (n_elements(noscale_val) ge 3) then b2 = where(tmp eq noscale_val[2],cnt2)
        if (n_elements(noscale_val) ge 4) then b3 = where(tmp eq noscale_val[3],cnt3)
        if (n_elements(noscale_val) ge 5) then b4 = where(tmp eq noscale_val[4],cnt4)
    
        tmp = tmp*scalef[0] + offset[0]  

        if (cnt0 gt 0) then tmp[b0] = noscale_set[0]
        if (cnt1 gt 0) then tmp[b1] = noscale_set[1]
        if (cnt2 gt 0) then tmp[b2] = noscale_set[2]
        if (cnt3 gt 0) then tmp[b3] = noscale_set[3]
        if (cnt4 gt 0) then tmp[b4] = noscale_set[4]
      endif

    ; // return the variable to the requested program level

      (Scope_VarFetch(varname[i], LEVEL=level, /ENTER)) = tmp

    endelse
  endfor

  NCDF_CLOSE, ncid  
  
  end
