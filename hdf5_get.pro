; ----------------------------------------------------------------------------
; procedure HDF5_GET
; ----------------------------------------------------------------------------
  pro hdf5_get, fname, varid, varname=varname, group=group, $
    scale=scale, scaatt=scaatt, offatt=offatt, noscale_val=noscale_val, $
    noscale_set=noscale_set
;+
; NAME:
;   HDF5_GET
;
; PURPOSE:
;   Get a dataset from an HDF5 file given the dataset name (and group, if
;   the dataset is contained within one)
;
; CALLING SEQUENCE:
;   HDF5_GET, FNAME, VARID
;
; INPUTS:
;   FNAME     HDF5 filename
;   VARID     string or array of strings containing the variable name(s) to
;             retrieve (this is case sensitive)
;
; KEYWORD PARAMETERS:
;   GROUP     scalar string defining the group containing the dataset(s)
;             (default is '/')
;   VARNAME   string variable with the same number of elements as VARID
;             containing name(s) to assign to retrieved variable(s); specify
;             an empty string for default
;
; (Options for data scaling:)
;
;   SCALE        set this keyword to search dataset attributes for scale and
;                offset factors given by SCAATT and OFFATT; if found,
;                retrieves the factors and applies the scaling, returning
;                (original value)*(scale factor) + offset
;   SCAATT       scalar string defining name of scale factor attribute
;   OFFATT       scalar string defining name of offset factor attribute
;   NOSCALE_VAL  a scalar or vector of up to five value(s) that, if present,
;                should not be scaled, but set to the corresponding value of
;                NOSCALE_SET instead
;   NOSCALE_SET  see above, must be same number of elements as NOSCALE_VAL
;
; OUTPUTS:
;   The variable(s) are returned directly to the calling routine
;
; EXAMPLES:
;   hdf5_get, 'filename.h5', 'pressure', group='/grid/data'
;   hdf5_get, 'filename.h5', ['pressure','temperature'], group='/grid/data'
;   hdf5_get, 'filename.h5', ['pressure','temperature'], varname=['p','t'], $
;        group='/grid/data'
;
; NOTE:
;   Spaces and dashes in variable names are converted to underscores.
;   Retrieving variables with names that are otherwise unacceptable as IDL
;   variable names may cause problems.
;
; MODIFICATION HISTORY:
;   2013/03/17  Written by John Haynes (https://github.com/johnhcc)
;-

  on_error, 2

; // open HDF5 file

  file_id = H5F_OPEN(fname)

  nvars = n_elements(varid)
  if (n_elements(varname) eq 0) then varname = varid
  if (n_elements(group) eq 0) then group = ''

; // loop over variables to retrieve

  for i=0L,nvars-1 do begin

    dataset_id = H5D_OPEN(file_id, group+'/'+varid[i]) 
    tmp = H5D_READ(dataset_id)

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
      nattr = H5A_GET_NUM_ATTRS(dataset_id)
      for j=0,nattr-1 do begin
        attid = H5A_OPEN_IDX(dataset_id,j)
        attname = H5A_GET_NAME(attid)
        if (attname eq scaatt) then $
          scalef = H5A_READ(attid)
        if (attname eq offatt) then $
          offset = H5A_READ(attid)
        H5A_CLOSE, attid
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

  ; // return the variable to the calling program level

    (Scope_VarFetch(varname[i], LEVEL=-1, /ENTER)) = tmp
    H5D_CLOSE, dataset_id

  endfor

; // close HDF5 file

  H5F_CLOSE, file_id
  
  end
