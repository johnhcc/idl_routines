; ----------------------------------------------------------------------------
; procedure HDF_GET
; ----------------------------------------------------------------------------
  pro hdf_get, fname, varid, varname=varname, reqstr=reqstr, reqatt=reqatt, $
    nver=nver, scale=scale, scaatt=scaatt, offatt=offatt, $
    noscale_val=noscale_val, noscale_set=noscale_set, start_at=start_at, $
    stop_at=stop_at, status=status, signed_2b=signed_2b
;+
; NAME:
;   HDF_GET
;
; PURPOSE:
;   Get a variable (dataset or vdata) from an HDF / HDF-EOS 4.x file given the
;   variable name
;
; CALLING SEQUENCE:
;  HDF_GET, FNAME, VARID
;
; INPUTS:
;   FNAME      HDF filename
;   VARID      string or vector of strings containing the
;              dataset/vdata variable name(s) to retrieve
;
; KEYWORD PARAMETERS:
;   VARNAME    string variable with the same number of elements as VARID
;              containing name(s) to assign to retrieved variable(s);
;              specify an empty string for default
;
; (Options for dataset and vdata scaling:)
;
;   SCALE        set this keyword to search dataset/vdata attributes for scale
;                and offset factors given by SCAATT and OFFATT; if found,
;                retrieves the factors and applies the scaling, returning
;                (original value)*(scale factor) + offset
;   SCAATT       scalar string defining name of scale factor attribute
;   OFFATT       scalar string defining name of offset factor attribute
;   NOSCALE_VAL  a scalar or vector of up to five value(s) that, if present,
;                should not be scaled, but set to the corresponding value of
;                NOSCALE_SET instead
;   NOSCALE_SET  see above, must be same number of elements as NOSCALE_VAL
;
; (Options to retrieve a subset of the full dataset/vdata:)
;
;   START_AT   a vector of indice(s) where subsetting will start
;   STOP_AT    a vector of indice(s) where subsetting will stop
;
; (Option to overcome an IDL limitation:)
;
;   SIGNED_2B  set this keyword to consider 8-bit integers as signed
;              instead of unsigned
;
; (Options for odd situations; the following must have the same number of
;  elements as VARID:)
;
;   REQSTR     require the REQATT attribute of the corresponding dataset to
;              match this string exactly, and if it does not then do not
;              retrieve the dataset (useful for certain HDF files containing
;              multiple datasets with the name, but different attributes); not
;              enforced if an empty string is specified
;   REQATT     scalar string name specifying the dataset attribute required by
;              REQSTR
;   NVER       when multiple dataset(s) or vdata are found containing the same
;              name, retrieve the NVER-th instance; the first instance
;              corresponds to NVER=1, the second to NVER=2, etc.
;
; OUTPUTS:
;   The variable(s) are returned directly to the calling routine
;
; EXAMPLES:
;   hdf_get, 'filename.hdf', 'pressure'
;   hdf_get, 'filename.hdf', ['pressure','temperature']
;   hdf_get, 'filename.hdf', ['pressure','temperature'], varname=['p','t']
;
; NOTES:
; 1. HDF files containing more than one dataset and/or vdata item with the
;    same name may produce a warning or unexpected results; you might be
;    able to use the REQSTR or NVER options to get around this.
; 2. Spaces and dashes in variable names are converted to underscores.
;    Retrieving variables with names that are otherwise unacceptable as IDL
;    variable names may cause problems.
; 3. Using START_AT/STOP_AT for vdata will read the entire vdata field, then
;    perform a subset as requested (so it's not as efficient as it is when
;    used on an actual "dataset")
;
; MODIFICATION HISTORY:
;   2006/07/07  Written by John Haynes (https://github.com/johnhcc)
;   2009/01/22  Added REQSTR, REQATT, SCAATT, OFFATT arguments (JMH)
;   2012/01/20  Added NVER argument (JMH)
;   2013/01/07  Fixed scaling (now also works for vdata), added START_AT
;               and STOP_AT arguments (JMH)
;-

; // error handling

  status=0
  catch, error_status
  if error_status ne 0 then begin
    status=1
    print, !error_state.msg
    print, 'HDF_GET exiting with status=1'
    catch, /cancel
    return
  endif

; // intitializations and checks

  varid = strupcase(varid)
  if keyword_set(reqstr) then begin
    if (n_elements(reqatt) ne 0) then reqatt = strupcase(reqatt) else $
      message, 'Must specify value of [reqatt]'
  endif
  if keyword_set(scale) then begin
    if (n_elements(scaatt) ne 0) then   $
      scaatt = strupcase(scaatt)        $
    else                                $
      scaatt = 'SCALE'
    if (n_elements(offatt) ne 0) then   $
      offatt = strupcase(offatt)        $
    else                                $
      offatt = 'OFFSET'
  endif

; // open the HFD file

  file_id = HDF_OPEN(fname, /READ)

; // start reading

  sd_id = HDF_SD_START(fname, /READ)  
  
; // get datasets and attributes

  HDF_SD_FILEINFO, sd_id, n_data, tmp
  if (n_data eq 0) then begin
    dataset_names = ''
  endif else begin
    dataset_names = strarr(n_data)
    dataset_sca = make_array(n_data, /integer, value=-1)
    dataset_off = make_array(n_data, /integer, value=-1)

    if keyword_set(reqstr) then $
      dataset_att = make_array(n_data, /string, value=-1)

    for i=0,n_data-1 do begin
      w_id = HDF_SD_SELECT(sd_id,i)
      HDF_SD_GETINFO, w_id, name=w_name, natts=w_natts

      if keyword_set(scale) then begin
        for j=0,w_natts-1 do begin
          HDF_SD_ATTRINFO, w_id, j, name=a_name
          if (strupcase(a_name) eq scaatt) then dataset_sca[i] = j
          if (strupcase(a_name) eq offatt) then dataset_off[i] = j
        endfor
      endif

      dataset_names[i] = strupcase(w_name)

      if keyword_set(reqstr) then begin
        for j=0,w_natts-1 do begin
          HDF_SD_ATTRINFO, w_id, j, name=a_name
          if (strupcase(a_name) eq reqatt) then begin
            HDF_SD_ATTRINFO, w_id, j, data=tmp
            dataset_att[i] = tmp
          endif
        endfor      
      endif      

      HDF_SD_ENDACCESS, w_id
    endfor
  endelse
  HDF_SD_END, sd_id
  
; // get vdata and attributes

  vdata_ID = HDF_VD_GETID(file_id, -1)
  is_NOT_fakeDim = 1
  n_vdata = 0
  Vdata_names = ''
  Vdata_sca = -1
  Vdata_off = -1

  while (vdata_ID ne -1) and (is_NOT_fakeDim) do begin
    vdata_H = HDF_VD_ATTACH(file_id, vdata_ID)
    HDF_VD_GET, vdata_H, name=name, nfields=nfields
    natts = HDF_VD_NATTRS(vdata_H,-1)

    sca_f = -1
    off_f = -1
    if keyword_set(scale) then begin
      for j=0,natts-1 do begin
        HDF_VD_ATTRINFO, vdata_H, -1, j, name=v_name
        if (strupcase(v_name) eq scaatt) then sca_f = j
        if (strupcase(v_name) eq offatt) then off_f = j
      endfor
    endif

    HDF_VD_DETACH, vdata_H

    is_NOT_fakeDim = strpos(name,'fakeDim') eq -1
    if (n_vdata eq 0) then begin
      Vdata_names = strupcase(name)
      Vdata_nfields = nfields
      Vdata_vid = HDF_VD_FIND(file_id,name)
      Vdata_sca = sca_f
      Vdata_off = off_f
      n_vdata = 1
    endif else if is_NOT_fakeDim then begin
      Vdata_names = [Vdata_names,strupcase(name)]
      Vdata_nfields = [Vdata_nfields,nfields]
      Vdata_vid = [Vdata_vid,HDF_VD_FIND(file_id,name)]
      Vdata_sca = [Vdata_sca,sca_f]
      Vdata_off = [Vdata_off,off_f]
      n_vdata = n_vdata + 1
    endif
    vdata_ID = HDF_VD_GETID(file_id, vdata_ID)
  endwhile
  
; // check inputs

  nvars = n_elements(varid)
  reqstr_match = bytarr(nvars)
  if keyword_set(reqstr) then begin
    if (array_equal(size(reqstr),size(varid)) eq 0) then begin
      message, '[reqstr] must have same number of elements as input array, ' $
        + 'ignoring', /info
    endif else begin
      q = where(reqstr ne '',qcnt)
      if (qcnt gt 0) then reqstr_match[q] = 1
    endelse
  endif

  if (n_elements(nver) ne 0) then begin
    if (array_equal(size(nver,/dimensions),size(varid,/dimensions)) eq 0) then $
      message, '[nver] must have same number of elements as input array'
  endif

  if (n_elements(varname) eq 0) then varname = varid
  if (array_equal(size(varname),size(varid)) eq 0) then begin
    message, '[varname] must have same number of elements as input array, ' $
      + 'ignoring', /info
    varname=varid
  endif
  
  idx = where(varname eq '',cnt)
  if (cnt ne 0) then varname[idx] = varid[idx]

; // search for id

  for i=0,nvars-1 do begin

    if reqstr_match[i] then begin

      didx = where((dataset_names eq varid[i]) and $
        (dataset_att eq reqstr[i]),cntd)
      if (cntd gt 1) then begin
        if (n_elements(nver) eq 0) then begin        
          message, strtrim(cntd,2)+' datasets match '+ $
           'required criterion for "'+varid[i]+'"'
        endif else begin
          print, strtrim(cntd,2)+' datasets match '+ $
           'required criterion for "'+varid[i]+'"; getting #'+strtrim(nver[i],2)
          didx = didx[nver[i]-1]
        endelse
      endif

    endif else begin

      didx = where(dataset_names eq varid[i],cntd)
      if (cntd gt 1) then begin
        if (n_elements(nver) eq 0) then begin
          message, strtrim(cntd,2)+' datasets match "'+varid[i]+'"'
        endif else begin
          print, strtrim(cntd,2)+' datasets match "'+varid[i]+ $
            '"; getting #'+strtrim(nver[i],2)
          didx = didx[nver[i]-1]
        endelse
      endif

    endelse
    vidx = where(vdata_names eq varid[i],cntv)
    if (cntv gt 1) then message, strtrim(cntv,2)+' vdata match "'+ $
      varid[i]+'"'

  ; // output variable name: replace ' ' and '-' with '_'
  
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

    if (cntd gt 0) then begin

    ; // retrieve dataset into variable data

      sd_id = HDF_SD_START(fname, /READ)
      sp_id = HDF_SD_SELECT(sd_id,didx)
      if (n_elements(start_at) eq 0) then begin
        HDF_SD_GETDATA, sp_id, data
      endif else begin
        HDF_SD_GETDATA, sp_id, data, start=start_at, count=stop_at-start_at+1
      endelse
      if keyword_set(signed_2b) then begin
        if (size(data,/type) eq 1) then begin
          data = temporary(fix(data))
          b = where(data ge 128,bcnt)
          if (bcnt gt 0) then data[b] -= 256
        endif
      endif

    ; // search for scaling factor/offset attributes

      if (keyword_set(scale)) then begin
        scalef = 1.
        offset = 0.
        if (dataset_sca[didx] ne -1) then $
          HDF_SD_ATTRINFO, sp_id, dataset_sca[didx], data=scalef
        if (dataset_off[didx] ne -1) then $
          HDF_SD_ATTRINFO, sp_id, dataset_off[didx], data=offset

      ; // this looks messy, but it avoids the necessity of making a temporary
      ;    copy of the data array

        cnt0 = 0 & cnt1 = 0 & cnt2 = 0 & cnt3 = 0 & cnt4 = 0
        if (n_elements(noscale_val) ge 1) then b0 = where(data eq noscale_val[0],cnt0)
        if (n_elements(noscale_val) ge 2) then b1 = where(data eq noscale_val[1],cnt1)
        if (n_elements(noscale_val) ge 3) then b2 = where(data eq noscale_val[2],cnt2)
        if (n_elements(noscale_val) ge 4) then b3 = where(data eq noscale_val[3],cnt3)
        if (n_elements(noscale_val) ge 5) then b4 = where(data eq noscale_val[4],cnt4)
    
        data = data*scalef[0] + offset[0]  

        if (cnt0 gt 0) then data[b0] = noscale_set[0]
        if (cnt1 gt 0) then data[b1] = noscale_set[1]
        if (cnt2 gt 0) then data[b2] = noscale_set[2]
        if (cnt3 gt 0) then data[b3] = noscale_set[3]
        if (cnt4 gt 0) then data[b4] = noscale_set[4]
      endif
      HDF_SD_ENDACCESS, sp_id
      HDF_SD_END, sd_id

    ; // return the variable to the requested program level

      (Scope_VarFetch(varname[i], LEVEL=-1, /ENTER)) = data

    endif else if (cntv gt 0) then begin

    ; // retrieve vdata into variable data
 
      vdata_h = HDF_VD_ATTACH(file_id,Vdata_vid[vidx])
      nscan = HDF_VD_READ(vdata_h,data)
      if (n_elements(start_at) gt 0) then $
        data = temporary(data[start_at:stop_at])
      if keyword_set(signed_2b) then begin
        if (size(data,/type) eq 1) then begin
          data = temporary(fix(data))
          b = where(data ge 128,bcnt)
          if (bcnt gt 0) then data[b] -= 256
        endif
      endif

    ; // search for scaling factor/offset attributes

      if (keyword_set(scale)) then begin
        scalef = 1.
        offset = 0.
        if (vdata_sca[vidx] ne -1) then $
          HDF_VD_ATTRINFO, vdata_H, -1, vdata_sca[vidx], data=scalef
        if (vdata_off[vidx] ne -1) then $
          HDF_VD_ATTRINFO, vdata_H, -1, vdata_off[vidx], data=offset

      ; // this looks messy, but it avoids the necessity of making a temporary
      ;    copy of the data array

        cnt0 = 0 & cnt1 = 0 & cnt2 = 0 & cnt3 = 0 & cnt4 = 0
        if (n_elements(noscale_val) ge 1) then b0 = where(data eq noscale_val[0],cnt0)
        if (n_elements(noscale_val) ge 2) then b1 = where(data eq noscale_val[1],cnt1)
        if (n_elements(noscale_val) ge 3) then b2 = where(data eq noscale_val[2],cnt2)
        if (n_elements(noscale_val) ge 4) then b3 = where(data eq noscale_val[3],cnt3)
        if (n_elements(noscale_val) ge 5) then b4 = where(data eq noscale_val[4],cnt4)
    
        data = data*scalef[0] + offset[0]  

        if (cnt0 gt 0) then data[b0] = noscale_set[0]
        if (cnt1 gt 0) then data[b1] = noscale_set[1]
        if (cnt2 gt 0) then data[b2] = noscale_set[2]
        if (cnt3 gt 0) then data[b3] = noscale_set[3]
        if (cnt4 gt 0) then data[b4] = noscale_set[4]
      endif
      HDF_VD_DETACH, vdata_h

    ; // return the variable to the calling program level

      (Scope_VarFetch(varname[i], LEVEL=-1, /ENTER)) = data

    endif else begin
 
    ; // no match
 
      if reqstr_match[i] then message, $
        'Variable matching criteria not found - ' + varid[i], /info $
      else message, 'Variable not found - ' + varid[i], /info
  
    endelse
    
  endfor
  
; // close the HFD file

  HDF_CLOSE, file_id

  end
