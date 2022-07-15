.. _hco-ver:

#####################
HEMCO version history
#####################

.. _hco350:

===========
HEMCO 3.5.0
===========
Release TBD

-  Support for MAPL 2.16 (needed by GCHP and GEOS)
-  Bug fix for HEMCO standalone run directory creation
-  Bug fix: If HEMCO masks are specified as
   :literal:`lon1/lat1/lon2/lat2`,
   then don’t try to read from disk
-  Documentation from the GEOS-Chem wiki (now on ReadTheDocs)
-  Badges for the ReadTheDocs front page
-  Ignore non-printing characters (e.g. tabs) when reading
   :file:`HEMCO_Config.rc`. This had caused a bug in GCHP.
-  Updated module names for MAPL 2.16 upgrade
-  “Diagnostic counter zero” warning is now printed at warning level 2
   (instead of 1)
-  :literal:`OFFLINE_BIOGENICVOC` emissions in MEGAN now include
   species MOH

.. _hco340:

===========
HEMCO 3.4.0
===========
Released 2022-05-22

-  Make sure each routine exits HEMCO with an error message (even if a
   placeholder message is used)
-  Fixed syntax error in :file:`hcox_tomas_dustdead_mod.F`
-  GCHP bug fix: revert to all zeros in criteria for restart field not
   filled
-  Several fixes for OpenMP parallelization and numerical stability
-  Bug fix: Prevent out-of-bounds error in HEMCO vertical interpolation
-  Prevent undefined variable when calculating vertical scale factor
-  Set MEGAN biogenic annual emission factors to zero in before
   computing them
-  Updated ReadTheDocs documentation
-  Retired CH4 wetlands emissions extension

.. _hco330:

===========
HEMCO 3.3.0
===========
Released 2022-05-02

-  Updated :file:`Extensionss/hcox_gc_RnPbBe_mod.F90` to use Zhang et al
   2021 emissions (now the default) (cf doi:0.5194/acp-21-1861-2021)

.. _hco322:

===========
HEMCO 3.2.2
===========
Released 2021-12-01

-  Restore updating of manual HEMCO diagnostics, which had been
   clobbered due to a prior Git merge


.. _hco321:

===========
HEMCO 3.2.1
===========
Released 2021-11-15

-  Add patch branches to continuous integration tests
-  Fix indexing of species in ’hcox_seasalt_mod.F90\` when using
   marinePOA option

.. _hco320:

===========
HEMCO 3.2.0
===========
Released 2021-11-15

-  GCHP adjoint updates
-  Add climatology input to volcano extension
-  Include PET number in error message if using ESMF
-  Send all HCO_ERROR messages to stderr and write from all threads
-  Modify volcano extension so that dry-run option also looks for
   climatology file
-  Modified HEMCO diagnostics and standalone config files for
   consistency with GEOS-Chem updates

.. _hco311:

===========
HEMCO 3.1.1
===========
Released  2021-09-10

-  Fix to HEMCO lightning flash rate diagnostic units
-  Fix HEMCO’s ``createRunDir.sh`` script to replace additional added
   tokens

.. _hco310:

===========
HEMCO 3.1.0
===========
Released 2021-09-07

-  Blowing snow emissions of sea salt and sea salt bromide added to sea
   salt extension
-  Add :literal:`HEMCO_INTERFACE` cache variable to CMake build

.. _hco300:

===========
HEMCO 3.0.0
============
Released 2021-01-08

-  Updates to speed up HEMCO
-  Updates to calculate emissions sensitivities, apply emissions scaling
   factors, and output adjoint diagnostics
-  Support for GCAP 2.0
-  Several driver programs have been added to the :literal:`src/Interfaces/`
   subdirectory for using HEMCO in other models
-  Unify HCOIO interfaces for standard and MAPL configurations
-  Updates for using HEMCO in CESM2-GC and WRF-GC
-  Fixed bug where met fields were only being read in once at the start
   of a simulation
-  Added stale bot and no-response bot to HEMCO GitHub repo
-  A script for creating HEMCO standalone rundirs is now included in the
   :literal:`run/` folder
-  CEDS GDB-MAPS is now the default anthropogenic emissions inventory
-  HEMCO source code has been split off from the GEOS-Chem repository
   into https://github.com/geoschem/HEMCO repository
-  Source code has been reorganized
-  Set and update :code:`ExtState` before computing emissions in HEMCO
   standalone
-  Use :code:`HcoState%NZ` instead of :code:`NLEV` in
   :file:`hco_interp_mod.F90`
-  Make sure data containers with :literal:`EFY` time cycle flag are
   only  updated once
-  CMake is now the default build system
-  Update isCoards script to account for files saved out by GCHP’s
   History component
-  Support for GNU make
-  Carbon-based units for VOC species
-  Hard-coded scale factors in the DustDead extension
-  Duplicate :code:`Inst%FLUXSABI` allocation in MEGAN extension

.. _hco220:

===========
HEMCO 2.2.0
===========
Released 2020-02-03

-  Implemented dry-run option
-  Now properly interpolate data with irregular timesteps
-  New logical switches for all inventories and datasets
-  New main switches for emissions, meteorology, and chemistry input
-  Fixed HEMCO’s time shift capability to properly accommodate units of
   year, month, day, hour, minute, and second
-  New checks to adjust date and timestamps so they fall within physical
   ranges
-  Now avoid running HEMCO for the end timestep of a simulation.
-  Now avoid running HEMCO twice on the first timestep of a simulation.
-  New :literal:`RFY3` time cycle option (3 hour input)
-  Now use semantic versioning (X.Y.Z)
-  Restore reading CHLR fields for marine POA simulations
-  Read met fields daily instead of hourly to improve file I/O

.. _hco21012:

=============
HEMCO 2.1.012
=============
Released 2019-04-01

-  Bug fixes for HEMCO interpolation
-  Updates from the NASA/GEOS development branch
-  New option to always use the simulation year for specified fields
-  Updates to the volcanic emissions extension
-  Bug fix: Prevent zero emissions for :literal:`MEGAN_Mono` extension

.. _hco21010:

=============
HEMCO 2.1.010
=============
Released 2019-10-05

-  Bug fix: Read data with the “E” cycle flag just once
-  Bug fix for collapsing model levels to reduced grid
-  New :literal:`CS` time cycle option
-  Fixed unit conversion in :code:`HCO_UNIT_GetAreaScal`

.. _hco21009:

=============
HEMCO 2.1.009
=============
Released 2018-09-13

-  Wrap HEMCO extensions into instances

.. _hco21008:

=============
HEMCO 2.1.008
=============
Released 2018-08-08

-  Bug fix: respect range/exact flag for 1D values set in
   :file:`HEMCO_Config.rc`

.. _hco21007:

=============
HEMCO 2.1.007
=============
Released 2018-07-18

-  Bug fix in :literal:`E` (exact) time cycling optiion
-  Now stop with error if multiple containers have the same
-  Bug fix for distributing emissions in the vertical dimension
-  New error checks in the HEMCO standalone module
-  Bug fix for ``ifort`` compiler in soil NOx extension
-  Removed null string character from netCDF unit string

.. _hco21006:

=============
HEMCO 2.1.006
=============
Released 2018-06-10

-  CH4 emissions from wetlands now uses category #1
-  CH4 emissions from rice now uses category #2
-  Unit :literal:`mol/mol` has beeeen added to the list of unitless
   quantities.

.. _hco21005:

=============
HEMCO 2.1.005
=============
Released 2018-01-27

-  Option to emit into the layer height read from netCDF file

.. _hco21004:

=============
HEMCO 2.1.004
=============
Released 2017-12-30

-  Updates to remove possible issues and excessive print statements when
   operating in GEOS environment
-  Fixed possible tracer ID mismatch in sea salt extension
-  New option to normalize MEGAN LAI, HEMCO diagnostics
-  Now write multiple time slices into one file
-  New error trap in :file:`hcox_dustginoux_mod.F90` to avoid seg faults
-  Bug fix in reference time code

.. _hco21003:

=============
HEMCO 2.1.003
=============
Released 2017-07-19

-  Now normalize MEGAN LAI by plant functional type.

.. _hco21002:

=============
HEMCO 2.1.002
=============
Released 2017-07-17

-  Enable tokens within math functions

.. _hco21001:

=============
HEMCO 2.1.001
=============
Released 2017-05-17

-  Now enable data compression in netCDF-4 output
-  Fixed bug in computation of local time in routine
-  HEMCO diagnostic and restart files now have an :literal:`unlimited`
   time dimension
-  All internal timestamp variables are now :code:`REAL*8`
-  New option to define species-specific scale factors that are applied
   across all inventories, categories, hierarchies, and extensions
-  New option to use mathematical expressions in :file:`HEMCO_Config.rc`.
-  Regridding routines can now support non-global grids

.. _hco20004:

=============
HEMCO 2.0.004
=============
Released 2017-01-26

-  New passive tracer module
-  Improve write speed of netCDF output files

.. _hco20003:

=============
HEMCO 2.0.003
=============
Released 2016-10-16

-  New option :literal:`DiagnRefTime` (specfies reference time in
   created netCDF files)
-  Fix missing pointer in call to :code:`HCO_CalcVertRegrid`
-  Bug fix: Prevent HEMCO from writing restart files more than once
-  Now uses updated timezones mask file.
-  Now accepts scale factors for extension fields.
-  New options to emit 2D fields across multiple vertical levels
-  In `:file:hcox_paranox_mod.F90`: Archive deposition flux as a
   positive number to avoid negative values propagating.
-  The HEMCO state object (:code:`HcoState`) now must be passed to
   each routine.
-  Updated the passive tracer code.

.. _hco11016:

=============
HEMCO 1.1.016
=============
Released 2015-12-24

-  New HEMCO standalone run directory

.. _hco11015:

=============
HEMCO 1.1.015
=============
Released 2015-12-07

-  Bug fix in GEOS5 -> GEOS-4 regridding
-  Bug fix in syncing the MEGAN LAI_PREVDAY variable

.. _hco11014:

=============
HEMCO 1.1.014
=============
Released 2015-11-23

-  Bug fix when interpolating/averaging between multiple files.

.. _hco11013:

=============
HEMCO 1.1.013
=============
Released 2015-11-19

-  Now allow mask grid points

.. _hco11012:

=============
HEMCO 1.1.012
=============
Released 2015-11-06

-  Now treat MEGAN restart variables as running averages
-  Bug fix: make sure that sea salt aerosol calculations work on
   curvilinear grids
-  New option :option:`DiagnTimeStamp` to control diagnostics time
   stamp format
-  Bug fix: restrict day to last day of month when searching for file
   names.
-  The :option:`SeaFlux` extension now uses HEMCO landtypes instead of
   land fraction

.. _hco11011:

=============
HEMCO 1.1.011
=============
Released 2015-10-14

-  Now allow horizontal coordinates :literal:`longitude` and
   :literal:`latitude`
-  New time flags :literal:`EF` and :literal:`RF` to force exit if
   field not found for
   current simulation datetime
-  Bug fix in :option:`Seaflux` extension: pull variables out of parallel
   loop.

.. _hco11010:

=============
HEMCO 1.1.010
=============
Released 2015-09-22

-  HEMCO can now read any additional (arbitrary) dimension.

.. _hco11009:

=============
HEMCO 1.1.009
=============
Released 2015-09-10

-  Bug fixes to allow specifying flexible diagnostics output
   frequencies.

.. _hco11008:

=============
HEMCO 1.1.008
=============
Released 2015-07-06

-  Bug fix in :file:`hcoi_standalone_mod.F90`: make sure current date
   and simulation end date are properly calculated
-  Make sure that negative emissions are correctly passed to hierarchy
   level diagnostics
-  Bug fix: When reading a data field, check if diagnostics container
   with the same name exist and write data into it.

.. _hco11007:

=============
HEMCO 1.1.007
=============
Released 2015-07-06

-  Grid edges can now be explicitly given in HEMCO standalone model

.. _hco11006:

=============
HEMCO 1.1.006
=============
Released 2015-07-01

-  Aerocom, CH4, :option:`FINN`, :option:`GFED`, and :option:`SoilNOx`
   extensions now accept scale fields
-  Bug fix: diagnostics update can now span multiple diagnostics levels

.. _hco11005:

=============
HEMCO 1.1.005
=============
Released 2015-06-09

-  Now also build HEMCO standalone executable
-  New extension module :file:`hcox_aerocom_mod.F90`
-  Capability to emit 2D data into levels other than the surface
-  Bug fix: Avoid out of bounds errors in :literal:`MEGAN_Mono`
   extension
-  Bug fix: Restore archiving biogenic CO emissions form monoterpenes
-  Now ensure that longitudes fall within the range -180 to 180, for
   COARDS compliance
-  Unit strings :literal:`%` and :literal:`percent` are now treated as
   unitless

.. _hco11004:

=============
HEMCO 1.1.004
=============
Released 2015-05-20

-  New capability to apply scale factors to meteorological fields.
-  Bug fix: Now allow FINN biomass emission diagnostics to be archived.
-  Extra flexibility to the definition of the vertical dimension used in
   :file:`HEMCO_Config.rc` (i.e. attribute :option:`SrcDim`).
-  Base emissions can now have a dynamic number of scale factors (used
   to be limited to a fixed number).
-  Now compute :literal`SUNCOS` and :literal:`SUNCOS - 5` in the
   :option:`PARANOx` extension instead of reading these as restarrt fields.

.. _hco11:

=========
HEMCO 1.1
=========
Released 2015-04-16

-  Various updates to PARANOX:

   -  Bug fix in calculation of H2O ambient air concentration’
   -  Bug fix in computation of solar zenith angle for the current date
      calculation of the current date SZA;
   -  Loss fluxes of O3 and HNO3 are now passed in kg/m2/s via the HEMCO
      diagnostics instead of converting them to a deposition velocity
      (and then recalculating a flux from this value);
   -  The SUNCOS values for the previous 5 hours are now saved out to
      the HEMCO restart file. e.g.:

-  Diagnostic collections are now organized in a linked list.
-  Masks can now be treated as fractions (instead of binary values).
-  Various updates to the HEMCO standalone code
-  Modifications to on/off switches in :file:`HEMCO_Config.rc`:

   -  Extension names can be used as switches
   -  Multiple switches can be combined with :literal:`.or.`

-  HEMCO has now two run phases:

   -  Phase 1 reads the HEMCO list
   -  Phase 2 calculates emissions.

-  Environmental fields used by HEMCO (stored in the :code:`ExtState`
   object) can now be read directly from disk.
-  Various updates to the HEMCO standalone code
-  Modifications to on/off switches in the HEMCO configuration file


.. _hco10:

=========
HEMCO 1.0
=========
Released 2014-11-07

Initial HEMCO release

-  Bug fix: Prevent seg fault when emissions are turned off - Bug fixes
   for the BIOGENIC_OCPI diagnostic
-  Bug fixes in the computation of alkalinity
-  PARANOx updates:

   -  Can now read the lookup table from netCDF or ASCII format
   -  Wind speed is now accounted for in the parameterization
   -  Dry deposition of N is included via loss of HNO3.
   -  Total tropospheric column mass is used to calculate dry deposition
      frequencies.

-  Local times can now be calculated based on a time zone map (at 1x1
   degree resolution).
-  Non-emissions data may now be specified in :file:`HEMCO_Config.rc`
   by setting the extension number to the wildcard (:literal:`*`)
   character.
-  New MAP_A2A horizontal regridding algorithm
-  The :literal:`R` time cycling option will cause HEMCO only to read data
   that is within the specified time range
-  Entries in the :file:`HEMCO_Config.rc` file may now be grouped into
   collections
-  Minor updates in the HEMCO-to-GEOS-Chem interface
-  UV albedo data is now read by HEMCO as a non-emissions data set.
-  Included GFED4 biomass burning as an extension.
-  Nested configuration files are now allowed
-  Added the capability to exclude collections with :literal:`.not.`
-  Added :file:`hco_restart_mod.F90` to define and obtain HEMCO
   restart variables
-  Added vertical mapping between 72 and 47 level GEOS-Chem grids
-  The :option:`MEGAN` extension can now read initial data from a
   HEMCO restart file
-  Index-sorted data can now be read from an ASCII file and mapped onto
   the simulation grid
-  Treat OCPI, OCPO, BCPI, and BCPO separately
-  Stratospheric production and loss are read as non-emissions data
-  Multiple emissions categories can now be assigned to each emissions
   fiel
-  ref:`hco-cfg-ext-switches` have been moved to the beginning of
   :file:`HEMCO_Config.rc`
-  :option:`Verbose` is no longer a logical switch but a number
   between 0 and 3
-  Masks can now be applied to scale factors
-  Extension data has been removed from :file:`HEMCO_Config.rc`. This
   is now a subsection of :ref:`hco-cfg-base`.
