.. |br| raw:: html

   <br />

.. _hco-filefmt:

#################
Input file format
#################

Currently, HEMCO can read data from the following data sources:

#.  **Gridded data from netCDF file**. More detail on the netCDF file are
    given below. In an ESMF environment, the MAPL/ESMF generic I/O
    routines are used to read/remap the data. In a non-ESMF environment,
    the HEMCO generic reading and remapping algorithms are used. Those
    support vertical regridding, unit conversion, and more (see
    below). |br|
    |br|

#.  **Scalar data directly specified in the HEMCO configuration file.**
    Scalar values can be set in the HEMCO configuration file directly. If
    multiple values - separated by the separator sign (/) - are
    provided, they are interpreted as temporally changing values: 7
    values = Sun, Mon, ..., Sat; 12 values = Jan, Feb, ..., Dec; 24
    values = 12am, 1am, ..., 11pm (local time!). Mask box boundaries can
    also be provided directly in the HEMCO configuration file. The entry
    must have exactly four values, interpreted as lower left and upper
    right mask box corners (lon1/lat1/lon2/lat2). |br|
    |br|

#.  **Country-specific data specified in a separate ASCII file.** This file
    must end with the suffix '.txt'. The first line must be the container
    name of a netCDF country mask file (e.g. :literal:`COUNTRY_MASK`). The
    rest of the file must contain the country-specific values listed by
    country name and ID. The IDs must correspond to the IDs of the netCDF
    mask file specified in the first line and must be integers. ID 0 is
    reserved for the default values that are applied to all countries with no
    specific values listed. The :literal:`CountryValues` are interpreted the
    same way as scalar values, except that they are applied to all grid
    boxes with the given country ID. Below are examples of file
    specifications and a country-specific data file. Note that in an ESMF
    environment you must use absolute path for the scale factors ASCII file
    rather than use $ROOT, and include the country mask but not the ASCII
    file in :literal:`ExtData.rc`.

.. code-block:: kconfig

   #==============================================================================
   # --- Country mask file ---
   #==============================================================================
   * COUNTRY_MASK $ROOT/MASKS/v2014-07/countrymask_0.1x0.1.nc CountryID 2000/1/1/0 C xy count * - 1 1

   #==============================================================================
   # --- Country scale factors ---
   #==============================================================================
   * COUNTRY_SCALE_FACTORS $ROOT/MASKS/country_scale_factors.txt - - - xy count 1

.. code-block:: kconfig

   # Country mask field name
   COUNTRY_MASK

   # CountryName CountryID CountryValues
   DEFAULT       0         1.0/2.0/3.0/4.0/5.0/6.0/7.0


.. _hco-filefmt-coards:

====================
COARDS compatibility
====================

Gridded input files are expected to be in the `Network Common Data
Form (netCDF) format <http://www.unidata.ucar.edu/software/netcdf/>`_ and must
adhere to the `COARDS metadata conventions
<https://ferret.pmel.noaa.gov/Ferret/documentation/coards-netcdf-conventions>`_

For an in-depth description of the COARDS netCDF conventions, please
see the Supplemental Guide entitled :ref:`coards-guide`.  Also be
aware of some additional considerations for the :ref:`time
<coards-guide-additional-time>` and :ref:`vertical level
<coards-guide-additional-lev>` dimensions.

=======================
Units of data variables
=======================

It is recommended to store data in one of the HEMCO standard units:

- :literal:`kg/m2/s` for fluxes;
- :literal:`kg/m3` for concentrations;
- :literal:`1` for unitless data;
- :literal:`count` for index-based data, i.e. discrete distributions
  (for instance, land types represented as integer values).

HEMCO will attempt to convert all data to one of those units, unless
otherwise via the :ref:`hco-cfg-base-srcunit` attribute (see the
:ref:`hco-cfg-base` section).

Mass conversion (e.g. from molecules to kg) is performed based on the
properties (e.g. molecular weight) of the species assigned to the
given data set.  It is also possible to convert between species-based
and molecule-based units (e.g. kg  vs. kg(C)). This conversion is
based on the emitted molecular  weight and the molecular ratio of the
given species (see the HEMCO-model Interface) section. More details on
unit conversion are given in module :file:`src/Core/hco_unit_mod.F90`.

Index-based data is regridded in such a manner that every grid box on
the new grid represents the index with the largest relative
contribution from the overlapping boxes of the original grid. All
other data are regridded as "concentration: quantities,
i.e. conserving the global weighted average.

For more information, we invite you to read `our Preparing data files
for use with HEMCO wiki
page <http://wiki.geos-chem.org/Preparing_data_files_for_use_with_HEMCO>`__.

.. _arbitrary_additional_netcdf_dimension:

=====================================
Arbitrary additional netCDF dimension
=====================================

HEMCO can read netCDF files with an additional, arbitrary
dimension. The dimension name and dimension index to be read must be
given explicitly in the HEMCO configuration file as part of the
:ref:`hco-cfg-base-srcdim` file attribute). This feature is currently
not available in an ESMF environment.


.. _hco-filefmt-regrid-vert:

===================
Vertical regridding
===================

HEMCO is able to perform some limited vertical interpolation.

By default, HEMCO assumes that the vertical coordinate direction is
upwards, i.e. the first level index corresponds to the surface layer.
The vertical axis can be reversed by setting the srcDim attribute in
the HEMCO configuration file accordingly (e.g. :literal:`xy-72` if the 
input data has 72 levels on a reversed vertical axis).

.. warning::

   HEMCO assumes that the input data is on the same grid as the
   model grid if it has the same number** (:literal:`NZ`) of, or plus
   one (:literal:`NZ+1`) vertical levels than the model.   

   In the case of the same number of vertical levels, HEMCO assumes
   that the input data is already on the model grid and no
   interpolation is performed. 

   In the case of input data having :literal:`NZ+1` levels, the data is
   interpreted as being on grid edges instead of grid midpoints. 

Collapsing into various GEOS grids
----------------------------------

Additional vertical regridding options are available for the various
GEOS grids.  These options are only available if the corresponding
compiler flags are set (this is the default case for GEOS-Chem users).

Conservative vertical interpolation using MESSy
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If input data is specified with vertical coordinates in :literal:`lev`
attribute of the netCDF file with units
:literal:`atmosphere_hybrid_sigma_pressure_coordinate`, HEMCO can
perform vertical interpolation using MESSy to the model grid. 

Regridding GEOS-Chem 3-D input data in other models
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In other models where HEMCO is used for emissions, but do not
necessarily use the GEOS vertical grids (e.g., WRF-GC, GEOS-Chem
within CESM, CAM-chem with HEMCO), input data from GEOS-Chem files
which have 72 levels will automatically be regridded to the model
levels, for compatibility. 

.. _hco-filefmt-regrid-horz:

=====================
Horizontal regridding
=====================

In a non-ESMF environment, HEMCO can only regrid between rectilinear
grids (e.g. lat-lon).

.. _nested_hemco_configuration_files:

================================
Nested HEMCO configuration files
================================

:ref:`HEMCO configuration files <hco-cfg>` can be nested by adding an include
statement to the master HEMCO configuration file (:file:`HEMCO_Config.rc`),
e.g.:

.. code-block:: console

   >>>include HEMCO_Config_nested.rc

The emission information contained in :file:`HEMCO_Config_nested.rc`
will then be used along with the emission configuration specified in
:file:`HEMCO_Config.rc`. Information in the master configuration file take
precedence over the information in the nested files. If the same setting
or extension switch/option is defined in both the master and the nested
configuration file, HEMCO will use the one from the master file.

Include statements can be placed anywhere in the HEMCO configuration
file. It is legal to nest multiple files (up to 5 levels deep).
