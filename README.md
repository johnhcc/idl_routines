# IDL routines

### About

This ia a collection of a few IDL (Interactive Data Language) routines that others may find useful.

### Routines

#### Math and science

- ded_reckon.pro
  - Performs deduced (or dead) reckoning on the surface of a sphere. It determines a destination position given an initial position, direction of travel (along a great circle), and distance traveled. It is essentially the inverse of IDL's MAP_2POINTS function.

- integral_simpson.pro
  - Use Simpson's rule for numerical integration, allowing for uneven intervals

#### Data routines

- cdf_get.pro
  - Get a variable from a netCDF file given the variable name (supports netCDF classic, 64-bit offset, netCDF-4, and netCDF-4 classic formats)

    `cdf_get, 'filename.nc', ['pressure','temperature'], varname=['p','t']`

- hdf_get.pro
  - Get a variable (dataset or vdata) from an HDF / HDF-EOS 4.x file given the variable name

    `hdf_get, 'filename.hdf', ['pressure','temperature'], varname=['p','t']`

- hdf5_get.pro
  - Get a dataset from an HDF5 file given the dataset name (and group, if the dataset is contained within one)

#### Graphics

- colorbar_discrete.pro
  - Draws a colorbar with discrete color intervals

- load_ncl_ctbl.pro
  - Loads NCL color tables for use in IDL (in indexed color mode)

- load_png_ctbl.pro
  - Loads color tables from PNG files for use in IDL (in indexed color mode)

### Contact information

John M. Haynes<br/>
Website: https://github.com/johnhcc<br/>
Email: johnhcc@vorticity.cc
