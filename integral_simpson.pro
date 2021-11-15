; ----------------------------------------------------------------------------
; function integral_simpson
; ----------------------------------------------------------------------------
  function integral_simpson, x, y, double=double
;+
; NAME:
;   INTEGRAL_SIMPSON
;
; PURPOSE:
;   Use Simpson's rule for numerical integration, allowing for uneven intervals
;
; CALLING SEQUENCE:
;   Result = INTEGRAL_SIMPSON(X,Y)
;
; INPUTS:
;   X        an array of increasing, unique coordinate values
;   Y        tabulated functional values
;
;   NOTE: X and Y must have an odd number of elements
;
; KEYWORD PARAMETERS:
;   DOUBLE   set this keyword for double precision calculations
;
; REFERENCE:
;   Singh, A. K., & Thorpe, G. (2003). Simpson’s 1/3-rule of integration for
;   unequal divisions of integration domain. Journal of Concrete and
;   Applicable Mathematics, 1(3).
;
; MODIFICATION HISTORY:
;   2020/04/11  Written by John Haynes (https://github.com/johnhcc)
;-
  
  on_error, 2
  
  n = n_elements(x) - 1
  if n mod 2 ne 0 then message, 'Must specify odd number of data points'
  
  nint = n/2
  typecode = keyword_set(double) ? 5 : 4
  h = make_array(n+1, type=typecode)
  for i=1L,n do h[i] = x[i] - x[i-1]  

  t = make_array(nint, type=typecode)
  for j=0L,nint-1 do begin
    i = 2 * j
    t[j] = (h[i+1] + h[i+2]) * (2*h[i+1] - h[i+2]) * y[i] / h[i+1] $
         + (h[i+1] + h[i+2])^3 * y[i+1] / (h[i+1] * h[i+2]) $
         + (h[i+1] + h[i+2]) * (2*h[i+2] - h[i+1]) * y[i+2] / h[i+2]
  endfor  
  
  return, 1/6. * total(t)
  
  end
