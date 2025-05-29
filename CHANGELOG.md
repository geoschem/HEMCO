# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [3.11.0] - 2025-04-18
### Added
- Added option to enable `InvMEGAN` manual diagnostic output
- Added supplemental guide documentation updates in the `geos-chem-shared-docs` submodule
- Added `Lons` and `Lats` to `FileData` type to store bounds of mask files
- Added definition of 0.125x0.15625 grid resolution

### Changed
- Use `USTAR` from meteorology instead of calculating from reference 10m wind in DustDead extension

### Fixed
- Fixed improper handling of mask files by removing code that overwrote	mask file years and months with lat/lon values and later hardcoded year and month values to -999

## [3.10.3] - 2025-03-25
### Added
- Added commented out debug print options useful for debugging HEMCO in MPI models

### Fixed
- Fixed bug setting destination grid object sigma vector passed to MESSy for vertical regridding
- Updated README to fix display of workflow badges

## [3.10.2] - 2025-03-04
### Added
- Added `.zenodo.json` for auto-DOI generation upon version releases
- Added GitHub Actions tests to build and test HEMCO on Windows, macOS, and Ubuntu automatically with each submitted PR

### Changed
- Bumped `jinja2` to version 3.1.5 in `docs/requirements.txt` to fix a security issue
- Turned off map_a2a pole averaging when using CESM to avoid core-dependency in 2D emissions
- Updated ReadTheDocs documentation for AWS CLI

### Fixed
- Updated several prints to limit to root thread to reduce log redundancy when using MPI

## [3.10.1] - 2025-01-10
### Added
- Added optional LUN argument to ConfigInit to allow external models to pass LUN of existing log file
- Added two log LUN variables (stdLogLUN and hcoLogLUN) to HcoState%Config for stdout and HEMCO log and initialize both in ConfigInit to stdout or optional LUN (if passed)
- Added output argument LUN to HCO_LOGFILE_OPEN to update HcoState%Config%hcoLogLun to LUN of new log (if using)
- Added optional LUN argument to HCO_MSG, HCO_WARNING, and HCO_ERROR to specify log to print to
- Added special log handling for CESM to ignore LogFile entry in HEMCO_Config.rc (will get passed CAM log LUN to use instead)

### Changed
- Changed interfaces HCO_MSG, HCO_WARNING, and HCO_ERROR to each be a single subroutine that does not use HcoState%Config%Err
- Changed HCO_MSG, HCO_WARNING, and HCO_ERROR to be independent of verbose and cores
- Updated calls to HCO_MSG to send output to HcoState%Config%hcoLogLUN
- Updated calls to HCO_WARNING to print to stdout unless related to units
- Replaced usage of HCO_IsVerb with HcoState_Config%doVerbose
- Changed documentation in HCO_Error_Mod.F90 to summarize error and log handling in HEMCO
- Updated ReadTheDocs "Understand what error messages mean" supplemental guide

### Fixed
- Fixed excessive prints when using MPI
- Fixed F77 formating in hcox_dustdead_mod.F

### Removed
- Removed warnings count in HcoState%Config%Err
- Removed RC argument in HCO_WARNING
- Deleted subroutine HCO_IsVerb
- Remove print of HcoDiagn%MassScal since never set in the model
- Added ReadTheDocs documentation for the HEMCO `LogFile` setting

## [3.10.0] - 2024-11-07
### Added
- Added TSOIL1 field to `ExtState`
- Added `download_data.py` and `download_data.yml` to the `run` folder.  These will be copied into HEMCO standalone rundirs
- Added `run/cleanRunDir.sh` script to remove old output files & log files
- Added documentation for the HEMCO 3.10.0 release, including HEMCO standalone dry-run documentation

### Changed
- Added emission factors for ALK6, C4H6, EBZ, STYR, TMB for GFED and FINN biomass burning extensions
- Updated soil NOx extention to include the option to use soil temperature and parameterization based on Yi Wang et al. (ERL, 2021) instead of the temperature at 2 meters.
- Updated HEMCO standalone to print the dry-run header to the HEMCO log file unit `HcoState%Config%Err%Lun` only if the file is opened
- ReadTheDocs update: Now use GNU 12.2.0 compilers in environment file examples
- Updated `runHEMCO.sh` standalone script: Change partitions, and pipe output to log file

### Fixed
- Fixed formatting error in `.github/workflows/stale.yml` that caused the Mark Stale Issues action not to run
- Updated to `jinja2==3.1.4` in `docs/requirements.txt` (fixes a security issue)

### Removed
- Example "Scale (or zero) emissions with a rectangular mask" from ReadTheDocs. This is currently not working.

## [3.9.3] - 2024-08-13
### Fixed
- Added brackets around `exempt-issue-labels` list in `.github/workflows/stale.yml`
- Fixed incorrect pressure handling in HEMCO standalone (see issue #277)

## [3.9.2] - 2024-07-24
### Changed
- RTD updates: Converted several `:option:` tags to subsections and updated references accordingly

### Fixed
- Typos in RTD doc file `docs/source/hco_ref_guide/hemco-config.rst`

### Removed
- Manual `InvMEGAN` diagnostics from `src/Extensions/hcox_megan_mod.F90`; Activate these with `HEMCO_Diagn.rc` instead

## [3.9.1] - 2024-06-28
### Fixed
- Fixed formatting error in `.github/workflows/stale.yml` that caused the Mark Stale Issues action not to run

## [3.9.0] - 2024-05-30
### Added
- GitHub Action config file `.github/workflows/stale.yml`, which replaces StaleBot

### Removed
- GitHub config files `.github/stale.yml` and `.github/no-response.yml`

### Changed
- Converted Github issue templates into issue forms using YAML definition files
- Updated Python package versions for ReadTheDocs in `docs/requirements.txt`
- Now request Python 3.12 for ReadTheDocs builds in `.readthedocs.yaml`

## [3.8.1] - 2024-04-02
### Changed
- Now allow up to 10 nested brackets (`((( )))`) in the `HEMCO_Config.rc` file
- Now use short submodule names (i.e. w/o path) in `.gitmodules`

### Fixed
- Limit volcano climatology file read message to root core
- Updated `hco_interp_mod.F90` to handle 3D NEI emissions.

## [3.8.0] - 2024-02-07
### Changed
- Updated TOMAS_Jeagle sea salt extension

### Fixed
- Updated IsModelLevel check for CESM and WRF-GC
- Interpolation error for 8-day MODIS LAI files (removed month loop in `GetIndex2Interp`)

## [3.7.2] - 2023-12-01
### Added
- Script `.release/changeVersionNumbers.sh` to change version numbers before a new HEMCO release

### Changed
- Increased netCDF variable string length from 50 to 100

### Fixed
- Rename `HEMCO_Config.rc.sample` to `HEMCO_Config.rc` in `createRunDir.sh` if sample is used.
- Added fix to turn off emissions extensions when `EMISSIONS` logical is false

## [3.7.1] - 2023-10-10
### Changed
- Updated version numbers to 3.7.1
- Make Hg0 emission factors in `hcox_gfed_include_gfed4.H` multipliers  of the CO emission factor
- Removed superfluous routine `GetExtSpcVal_Dr` in `src/Core/hco_extlist_mod.F90`
- NetCDF routines in `src/Shared/NcdfUtil` now use the Fortran-90 API
- Overhauled vertical regridding `src/Core/hco_interp_mod.F90`
    - Removed `INFLATE` (but retained its behavior only for 47L -> 72L vertical regridding, warning users that this isn't recommended)
    - `ModelLev_Interpolate` is only called when the input is 47/48, 72/73, or 102/103 levels (otherwise, MESSy is used).
    - A bug that averaged the wrong number of levels in `COLLAPSE` is fixed (and edges are now sampled instead of averaged).
    - Removed the now superfluous `NC_ISMODELLEVEL` and `NC_SISIGMALEVEL` from `src/Shared/NcdfUtil/hco_ncdf_mod.F90`
    - Removed old code and references to `GEOS-4`.

### Fixed
- Fixed incorrect `XMIN`, `XMAX` values in  `HEMCO_sa_Grid.025x03125.rc` and `HEMCO_sa_Grid.05x0625.rc`
- Fixed line length too long for the `GC_72_EDGE_SIGMA` variable in `src/Core/hcoio_read_std_mod.F90`

## [3.7.0] - 2023-10-05
### Added
- HEMCO extensions now display a first-time message, whether `Verbose` is `true` or `false`.
- Added 'src/Shared/NcdfUtil/README.md` file directing users to look for netCDF utility scripts at https://github.com/geoschem/netcdf-scripts
- Added GFED4 biomass burning emissions for furans, PHEN, MVK, ISOP, ACTA, MGLY, MYLX, RCHO
- Add GEOSIT as an allowable meteorology directory name in HEMCO_Config.rc
- Added `.readthedocs.yaml` file to configure ReadTheDocs builds

### Changed
- `Verbose` is now a `true/false` variable in `run/HEMCO_sa_Config.rc` and `run/HEMCO_Config.rc.sample`
- HEMCO warnings are now only generated when `Verbose: true` is found in the HEMCO configuration file (no more numerical levels)
- Updated GFED4 emission factors for VOCs to Andreae et al. (2019)
- Refactored `hco_calc_mod.F90` to avoid computational bottlenecks (PR #201)
- Restart files are now written to the rundir `Restarts/` subdirectory
- Created a `Restarts/` subdirectory in HEMCO standalone run directories
- Added changes needed to build with NAG compiler
- Renamed Be7Strat and Be10Strat to Be7s and Be10s for consistency with GMAO's TR_GridComp
- Updated ReadTheDocs documentation about time cycle options `RFY`, `RFY3`
- Updated ReadTheDocs documentation about the `HEMCO_Diagn.rc` file
- Updated `AUTHORS.txt` for GEOS-Chem 14.2.0 and HEMCO 3.7.0
- Updated formatting in `README.md`
- Updated title and links to badges in `README.md`
- Updated version number to 3.7.0

### Fixed
- Do not read masks if the filename is `-` (non-ESMF environments only)
- Always assume partial coverage when reading masks in an ESMF environment (#163)
- Increased the string length for reading lines from HEMCO grid file to fix error in global 0.25x0.3125 standalone simulations

### Removed
- Warnings is now removed from `run/HEMCO_sa_Config.rc` and `run/HEMCO_Config.rc.sample`
- Removed the `src/Shared/NcdfUtil/perl` folder

## [3.6.3] - 2023-09-15
### Fixed
- Fixed nvhpc compiler error in CESM by reducing line length of `GC_72_EDGE_SIGMA` assignment

## [3.6.2] - 2023-03-02
### Added
- Added `.github/config.yml` with settings for the issue chooser page

### Changed
- Replace `description:` with `about:` in GitHub issue templates
- The PR template is now `.github/PULL_REQUEST_TEMPLATE.md`

### Fixed
- Now point to proper commit of the `geos-chem-shaed-docs` submodule

## [3.6.1] - 2023-03-01
### Added
- GEOS-only updates
- Removed several memory leaks in HEMCO Core and Standalone routines

### Changed
- Simplified Github issue and pull request templates
- Throw an error if input calendar is not supported

## [3.6.0] - 2023-02-01
### Added
- Added MAPL_ESMF compiler option for use with GCHP and GEOS
- New "Parallelize GEOS-Chem and HEMCO source code" guide on ReadTheDocs
- Updated documentation describing a masking error that can happen when performing simulations with cropped horizontal grids

### Changed
- Set HCO_MISSVAL to MAPL missing value (1e15) if using GCHP or GEOS
- Use fraction surface type inputs instead of ExtState%WLI
- The version number in docs/source/conf.py is now 3.6.0
- Updated compilation output splash screen in compiling.rst ReadTheDocs file

### Fixed
- Bug fix for inserting hard breaks in hemco-config.rst ReadTheDocs file

### Removed
- Removed old kludge for MAPL missing data if applying mask
- Removed ExtState field for water-land-ice index (WLI)

## [3.5.2] - 2022-11-29
### Added
- Added sanitizer option for detecting memory leaks in HEMCO standalone during build

### Changed
- Remove unused, commented-out code in `src/Extensions/hcox_dustdead_mod.F`
- Replaced placeholder error messages in `src/Core/hco_config_mod.F90` with more informational messages (often including the line of the HEMCO_Config.rc in the printout)
- Added improved documentation for time cycle flag `EFYO` in ReadTheDocs

### Fixed
- Removed memory leaks that were identified by the code sanitizer

## [3.5.1] - 2022-11-03
### Fixed
- Changed Inst%NP to Inst%NumP in HCOX_Seasalt_Mod for CESM compatibility

## [3.5.0] - 2022-09-19
### Added
- Support for MAPL 2.16 (needed by GCHP and GEOS)
- Bug fix for HEMCO standalone run directory creation
- Bug fix: If HEMCO masks are specified as `lon1/lat1/lon2/lat2`, then don't try to read from disk
- Documentation from the GEOS-Chem wiki (now on ReadTheDocs)
- Badges for the ReadTheDocs front page
- Bug fix for masking issues in MPI environment (for WRF, CESM)
- Mapping of CAM-Chem species to GFED4 (for CESM)
- New documentation for hemco.readthedocs.io, migrated from GC wiki
- Updated documentation on vertical regridding behavior

### Changed
- Ignore non-printing characters (e.g. tabs) when reading `HEMCO_Config.rc`. This had caused a bug in GCHP.
- Updated module names for MAPL 2.16 upgrade
- "Diagnostic counter zero" warning is now printed at warning level 2 (instead of 1)
- `OFFLINE_BIOGENICVOC` emissions in MEGAN now include species MOH

## [3.4.0] - 2022-05-02
### Added
- Make sure each routine exits HEMCO with an error message (even if a placeholder message is used)
- Fixed syntax error in `hcox_tomas_dustdead_mod.F`
- GCHP bug fix: revert to all zeros in criteria for restart field not filled
- Several fixes for OpenMP parallelization and numerical stability
- Bug fix: Prevent out-of-bounds error in HEMCO vertical interpolation

### Changed
- Prevent undefined variable when calculating vertical scale factor
- Set MEGAN biogenic annual emission factors to zero in before computing them
- Updated ReadTheDocs documentation

### Removed
- Retired CH4 wetlands emissions extension

## [3.3.0] - 2022-05-02
### Changed
- Updated `Extensionss/hcox_gc_RnPbBe_mod.F90` to use Zhang et al 2021 emissions (now the default)` (cf doi:0.5194/acp-21-1861-2021)

## [3.2.2] - 2021-12-01
### Changed
- Restore updating of manual HEMCO diagnostics, which had been clobbered due to a prior Git merge

## [3.2.1] - 2021-11-15
### Added
- Add patch branches to continuous integration tests
- Fix indexing of species in 'hcox_seasalt_mod.F90` when using marinePOA option

## [3.2.0] - 2021-11-15
### Added
- GCHP adjoint updates
- Add climatology input to volcano extension
- Include PET number in error message if using ESMF
- Send all HCO_ERROR messages to stderr and write from all threads

### Changed
- Modify volcano extension so that dry-run option also looks for climatology file
- Modified HEMCO diagnostics and standalone config files for consistency with GEOS-Chem updates

## [3.1.1] - 2021-09-10
### Added
- Fix to HEMCO lightning flash rate diagnostic units
- Fix HEMCO's `createRunDir.sh` script to replace additional added tokens

## [3.1.0] - 2021-09-07
### Added
- Blowing snow emissions of sea salt and sea salt bromide added to sea salt extension
- Add `HEMCO_INTERFACE` cache variable to CMake build

## [3.0.0] - 2021-01-08
### Added
- Updates to speed up HEMCO
- Updates to calculate emissions sensitivities, apply emissions scaling factors, and output adjoint diagnostics
- Support for GCAP 2.0
- Several driver programs have been added to the `src/Interfaces/` subdirectory for using HEMCO in other models
- Unify HCOIO interfaces for standard and MAPL configurations
- Updates for using HEMCO in CESM2-GC and WRF-GC
- Fixed bug where met fields were only being read in once at the start of a simulation
- Added stale bot and no-response bot to HEMCO GitHub repo
- A script for creating HEMCO standalone rundirs is now included in the `run/` folder
- CEDS GDB-MAPS is now the default anthropogenic emissions inventory

### Changed
- HEMCO source code has been split off from the GEOS-Chem repository into https://github.com/geoschem/HEMCO repository
- Source code has been reorganized
- Set and update `ExtState` before computing emissions in HEMCO standalone
- Use `HcoState%NZ` instead of `NLEV` in `hco_interp_mod.F90`
- Make sure data containers with `EFY` time cycle flag are only updated once
- CMake is now the default build system
- Update isCoards script to account for files saved out by GCHP's History component

### Removed
- Support for GNU make
- Carbon-based units for VOC species
- Hard-coded scale factors in the DustDead extension
- Duplicate `Inst%FLUXSABI` allocation in MEGAN HCO extension

## [2.2.0] - 2020-02-03
### Added
- Implemented dry-run option
- Now properly interpolate data with irregular timesteps
- New logical switches for all inventories and datasets
- New main switches for emissions, meteorology, and chemistry input
- Fixed HEMCO's time shift capability to properly accommodate units of year, month, day, hour, minute, and second
- New checks to adjust date and timestamps so they fall within physical ranges
- Now avoid running HEMCO for the end timestep of a simulation
- Now avoid running HEMCO twice on the first timestep of a simulation
- New `RFY3` time cycle option (3 hour input)

### Changed
- Now use semantic versioning (X.Y.Z)
- Restore reading CHLR fields for marine POA simulations
- Read met fields daily instead of hourly to improve file I/O

## [2.1.012] - 2019-04-01
### Added
- Bug fixes for HEMCO interpolation
- Updates from the NASA/GEOS development branch
- New option to always use the simulation year for specified fields
- Updates to the volcanic emissions extension

### Changed
- Bug fix: Prevent zero emissions for `MEGAN_Mono` extension

## [2.1.010] - 2019-10-05
### Added
- Bug fix: Read data with the "E" cycle flag just once
- Bug fix for collapsing model levels to reduced grid
- New `CS` time cycle option

### Changed
- Fixed unit conversion in `HCO_UNIT_GetAreaScal`

## [2.1.009] - 2018-09-13
### Added
- Wrap HEMCO extensions into instances

## [2.1.008] - 2018-08-08
### Added
- Bug fix: respect range/exact flag for 1D values set in `HEMCO_Config.rc`

## [2.1.007] - 2018-07-18
### Added
- Bug fix in `E` (exact) time cycling optiion
- Now stop with error if multiple containers have the same
- Bug fix for distributing emissions in the vertical dimension
- New error checks in the HEMCO standalone module
- Bug fix for `ifort` compiler in soil NOx extension

### Removed
- Null string character from netCDF unit string

## [2.1.006] - 2018-06-10
### Added
- CH4 emissions from wetlands now uses category #1
- CH4 emissions from rice now uses category #2
- Unit `mol/mol` has beeeen added to the list of unitless quantities.

## [2.1.005] - 2018-01-27
### Added
- Option to emit into the layer height read from netCDF file

## [2.1.004] - 2017-12-30
### Added
- Updates to remove possible issues and excessive print statements when operating in GEOS environment
- Fixed possible tracer ID mismatch in sea salt extension
- New option to normalize MEGAN LAI, HEMCO diagnostics
- Now write multiple time slices into one file
- New error trap in `hcox_dustginoux)mod.F90` to avoid seg faults
- Bug fix in reference time code

## [2.1.003] - 2017-07-19
### Added
- Now normalize MEGAN LAI by plant functional type.

## [2.1.002] - 2017-07-17
### Added
- Enable tokens within math functions

## [2.1.001] - 2017-05-16
### Added
- Now enable data compression in netCDF-4 output
- Fixed bug in computation of local time in routine
- HEMCO diagnostic and restart files now have an `unlimited` time dimension
- All internal timestamp variables are now `REAL*8`
- New option to define species-specific scale factors that are applied across all inventories, categories, hierarchies, and extensions
- New option to use mathematical expressions in `HEMCO_Config.rc`
- Regridding routines can now support non-global grids

## [2.0.004] - 2017-01-26
### Added
- New passive tracer module
- Improve write speed of netCDF output files

## [2.0.003] - 2016-10-16
### Added
- New option `DiagnRefTime` (specfies reference time in created netCDF files)
- Fix missing pointer in call to `HCO_CalcVertRegrid`
- Bug fix: Prevent HEMCO from writing restart files more than once
- Now uses updated timezones mask file.
- Now accepts scale factors for extension fields.
- New options to emit 2D fields across multiple vertical levels

### Changed
- In `hcox_paranox_mod.F90`: Archive deposition flux as a positive number to avoid negative values propagating.
- The HEMCO state object (`HcoState`) now must be passed to each routine.
- Updated the passive tracer code.

## [1.1.016] - 2015-12-14
### Added
- New HEMCO standalone run directory

## [1.1.015] - 2015-12-07
### Added
- Bug fix in GEOS5 -> GEOS-4 regridding
- Bug fix in syncing the MEGAN LAI_PREVDAY variable

## [1.1.014] - 2015-11-23
### Added
- Bug fix when interpolating/averaging between multiple files.

## [1.1.013] - 2015-11-19
### Added
- Now allow mask grid points

## [1.1.012] - 2015-11-06
### Added
- Now treat MEGAN restart variables as running averages
- Bug fix: make sure that sea salt aerosol calculations work on curvilinear grids
- New option `DiagnTimeStamp` to control diagnostics time stamp format
- Bug fix: restrict day to last day of month when searching for file names.

### Changed
- The `SeaFlux` extension now uses HEMCO landtypes instead of land fraction

## [1.1.011] - 2015-10-14
### Added
- Now allow horizontal coordinates `longitude` and `latitude`
- New time flags `EF` and `RF` to force exit if field not found for current simulation datetime
- Bug fix in `Seaflux` extension: pull variables out of parallel loop.

## [1.1.010] - 2015-09-22
### Added
- HEMCO can now read any additional (arbitrary) dimension.

## [1.1.009] - 2015-09-10
### Added
- Bug fixes to allow specifying flexible diagnostics output frequencies.

## [1.1.008] - 2015-07-06
### Added
- Bug fix in `hcoi_standalone_mod.F90`: make sure current date and simulation end date are properly calculated
- Make sure that negative emissions are correctly passed to hierarchy level diagnostics
- Bug fix: When reading a data field, check if diagnostics container with the same name exist and write data into it.

## [1.1.007] - 2015-07-06
### Added
- Grid edges can now be explicitly given in HEMCO standalone model

## [1.1.006] - 2015-07-01
### Added
- Aerocom, CH4, FINN, GFED, and soil NOx extensions now accept scale fields
- Bug fix: diagnostics update can now span multiple diagnostics levels

## [1.1.005] - 2015-06-09
### Added
- Now also build HEMCO standalone executable
- New extension module `hcox_aerocom_mod.F90`
- Capability to emit 2D data into levels other than the surface
- Bug fix: Avoid out of bounds errors in `MEGAN_Mono` extension
- Bug fix: Restore archiving biogenic CO emissions form monoterpenes

### Changed
- Now ensure that longitudes fall within the range -180 to 180, for COARDS compliance.
- Unit strings `%` and `percent` are now treated as unitless.

## [1.1.004] - 2015-05-20
### Added
- New capability to apply scale factors to meteorological fields.
- Bug fix: Now allow FINN biomass emission diagnostics to be archived.
- Extra flexibility to the definition of the vertical dimension used in `HEMCO_Config.rc` (i.e. attribute `SrcDim`).

### Changed
- Base emissions can now have a dynamic number of scale factors (used to be limited to a fixed number).
- Now compute `SUNCOS` and `SUNCOS - 5` in the PARANOX extension instead of reading these as restarrt fields.

## [1.1] - 2015-04-16
### Added
- Various updates to PARANOX:
  - Bug fix in calculation of H2O ambient air concentration'
  - Bug fix in computation of solar zenith angle for the current date calculation of the current date SZA;
  - Loss fluxes of O3 and HNO3 are now passed in kg/m2/s via the HEMCO diagnostics instead of converting them to a deposition velocity (and then recalculating a flux from this value);
  -  The SUNCOS values for the previous 5 hours are now saved out to the HEMCO restart file.
- Diagnostic collections are now organized in a linked list.
- Masks can now be treated as fractions (instead of binary values).
- Various updates to the HEMCO standalone code
- Modifications to on/off switches in `HEMCO_Config.rc`:
  - Extension names can be used as switches
  - Multiple switches can be combined with `.or`

### Changed
- HEMCO has now two run phases:
- Phase 1 reads the HEMCO list
- Phase 2 calculates emissions.
- Environmental fields used by HEMCO (stored in the `ExtState` object) can now be read directly from disk.
- Various updates to the HEMCO standalone code
- Modifications to on/off switches in the HEMCO configuration file

## [1.0] - 2014-11-07
Initial HEMCO release

### Added
- Bug fix: Prevent seg fault when emissions are turned off
- Bug fixes for the BIOGENIC_OCPI diagnostic
- Bug fixes in the computation of alkalinity
- PARANOx updates:
- Can now read the lookup table from netCDF or ASCII format
- Wind speed is now accounted for in the parameterization
- Dry deposition of N is included va loss of HNO3.
- Total tropospheric column mass is used to calculate dry deposition frequencies.
- Local times can now be calculated based on a time zone map (at 1x1 degree resolution).
- Non-emissions data may now be specified in `HEMCO_Config.rc` by setting the extension number to the wildcard (`*`) character.
- New MAP_A2A horizontal regridding algorithm
- The `R` time cycling option will cause HEMCO only to read data that is within the specified time range
- Entries in the `HEMCO_Config.rc` file may now be grouped into collections
- Minor updates in the HEMCO-to-GEOS-Chem interface
- UV albedo data is now read by HEMCO as a non-emissions data set.
- Included GFED4 biomass burning as an extension.
- Nested configuration files are now allowed
- Added the capability to exclude collections with `.not.`
- Added `hco_restart_mod.F90` to define and obtain HEMCO restart variables
- Vertical mapping between 72 and 47 level GEOS-Chem grids
- The MEGAN extension can now read initial data from a HEMCO restart file
- Index-sorted data can now be read from an ASCII file and mapped onto the simulation grid

### Changed
- Treat OCPI, OCPO, BCPI, and BCPO separately
- Stratospheric production and loss are read as non-emissions data
- Multiple emissions categories can now be assigned to each emissions field
- Extension switches has been moved to the beginning of `HEMCO_Config.rc`
- `Verbose` is no longer a logical switch but a number between 0 and 3
- Masks can now be applied to scale factors

### Removed
- Extension data has been removed from `HEMCO_Config.rc`.  This is now a subsection of `Base Emissions`.
