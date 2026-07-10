# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this repository is

**HEMCO** (Harmonized Emissions Component) is a Fortran component for computing atmospheric emissions from multiple data inventories, scale factors, and non-linear parameterizations ("extensions"). It is not run standalone in most contexts — this repo is normally checked out as a **git submodule** of a host model:

- `geoschem/GCClassic` — GEOS-Chem Classic (couples via the `gcclassic` interface)
- NASA GEOS / MAPL/ESMF-based models (couples via the `mapl`/ESMF interface)
- CESM2, WRF-GC, NOAA GEFS-Aerosol/UFS (couple via their own build systems, bypassing CMake)

It can also be built and run in **standalone mode** (`HEMCO_EXTERNAL_CONFIG` not set), driven entirely by `.rc` config files in `run/`, with no host atmospheric model.

If you arrived here via the GCClassic superproject, remember: `run`/`test` there are symlinks into the GEOS-Chem submodule, not this one. This repo's own `run/` directory holds the HEMCO **standalone** run-directory templates.

## Building

The build is CMake-based. In standalone mode it self-configures without a host model:

```console
cmake -S . -B build -D CMAKE_BUILD_TYPE=Release   # Debug also supported
cmake --build build --verbose
```

Key CMake options (see top-level `CMakeLists.txt`):
- `USE_REAL8` (ON/OFF, default ON) — HEMCO precision (`hp`) as 8-byte real
- `OMP` (TRUE/FALSE, default TRUE) — OpenMP threading
- `SANITIZE` (ON/OFF, GNU only, standalone builds) — address/leak/UB sanitizers
- `RUNDIR` — path to a HEMCO standalone run directory (must contain `HEMCO_sa_Spec.rc`); defaults to `..`
- `CMAKE_BUILD_TYPE` — `Release` (default), `Debug`, `RelWithDebInfo`

Only **Intel** and **GNU** Fortran compilers are supported (`HEMCO_SUPPORTED_COMPILER_IDS`); anything else is a hard CMake error.

When HEMCO is built as a submodule, the parent project sets `HEMCO_EXTERNAL_CONFIG` plus one of `GCCLASSIC_WRAPPER` or `MAPL_ESMF` (and optionally `MAPL3`), which determines `HEMCO_INTERFACE` (`standalone` / `gcclassic` / `mapl`) — see "Interface selection" below.

CI (`.github/workflows/ubuntu.yml`, `mac.yml`) builds the standalone target across a compiler/build-type matrix and runs `ctest`; there is currently no populated test suite (`ctest` finds nothing to run), so CI is effectively a compile check across Intel/GNU × Debug/Release combinations.

## Architecture

### Directory layout (`src/`)

- **`Core/`** — the HEMCO engine itself: config file parsing (`hco_config_mod`, `hco_extlist_mod`),   the emissions list and calculation engine (`hco_emislist_mod`, `hco_calc_mod`, `hco_readlist_mod`),   state objects (`hco_state_mod`, `hco_types_mod`), diagnostics (`hco_diagn_mod`, `hcoio_diagn_mod`),   and I/O backends (`hcoio_read_*_mod.F90` / `hcoio_write_*_mod.F90` — std/mapl/pio variants, selected   at CMake configure time based on `HEMCO_INTERFACE`). `hco_driver_mod.F90` is the INIT/RUN/FINAL driver for everything *not* handled by an extension.
- **`Extensions/`** — self-contained emission/parameterization modules (`hcox_*_mod.F90`): MEGAN (biogenic), GFED/FINN/GFAS (biomass burning), sea salt/seaflux/paranox, dust, lightning NOx, soil NOx, volcano, iodine, POPs, Rn-Pb-Be, TOMAS aerosol variants, etc. `hcox_driver_mod.F90` is the registry: `HCOX_Init`/`HCOX_Run`/`HCOX_Final` each contain one `IF (ExtState%<Ext>) CALL HCOX_<Ext>_{Init,Run,Final}(...)` block per extension, gated by whether that extension was enabled in `HEMCO_Config.rc`. **Adding a new extension means adding it here** (plus a CMakeLists.txt entry and a config-file section) — `hcox_template_mod.F90x` is the copy-paste skeleton for a new extension module, following the `InstGet`/`InstCreate`/`InstRemove` pattern for supporting multiple simultaneous instances of the same extension.
- **`Interfaces/`** — the boundary between HEMCO and each host model:
  - `Standalone/` — `hemco_standalone.F90` + `hcoi_standalone_mod.F90`, the standalone driver/executable.
  - `GEOS/` — ESMF `GridComp` wrapper (`HEMCO_GridCompMod.F90`) plus GEOS-specific `.rc` files, for
    NASA GEOS/GOCART/GMI coupling.
  - `MAPL_ESMF/` — `hcoi_esmf_mod.F90`, the MAPL/ESMF coupling layer (shared by GEOS and other
    MAPL-based hosts; behavior branches further on `MAPL3`).
  - `Shared/` — `hco_interface_common.F90`, code shared across interfaces regardless of host.
- **`Shared/`** — host-agnostic utility code: `GeosUtil/` (regridding, Julian day, Henry's law
  constants), `Headers/` (precision kinds in `hco_precision_mod.F90`, string parsing), `NcdfUtil/` (netCDF read/write wrappers, `hco_m_netcdf_io_*`).

### Interface selection (`HEMCO_INTERFACE`)

`src/Core/CMakeLists.txt` picks the I/O backend (`hcoio_read_std_mod`/`hcoio_read_mapl_mod`/etc.) based on `HEMCO_INTERFACE`, which is set once near the top of the top-level `CMakeLists.txt` depending on which of `HEMCO_EXTERNAL_CONFIG`/`GCCLASSIC_WRAPPER`/`MAPL_ESMF`/`MAPL3` the parent build defines. Non-CMake hosts (WRF-GC, CESM2-GC) bypass this and set the equivalent C-preprocessor switches themselves, so any change to an `hcoio_*` module or the switches guarding it must stay consistent with those external, non-CMake build systems too (see the discussion linked in `src/Core/CMakeLists.txt`, geoschem/HEMCO#87).

### Configuration-driven behavior

Most HEMCO behavior (which extensions run, which data files feed which species, scale factors, masks, time cycling) is controlled at runtime through `HEMCO_Config.rc` (template: `run/HEMCO_Config.rc.sample`) and `HEMCO_Diagn.rc`, not through recompilation. `hco_config_mod.F90`/`hco_extlist_mod.F90` parse these files at startup; extension-specific options are read via `GetExtOpt`. When changing emissions behavior, check whether the change belongs in Fortran code or is actually just a config-file/`run/` scripting concern.

## Versioning and changes

- Bump `PROJECT VERSION` in `CMakeLists.txt` **and** the matching version string in `src/Core/hco_error_mod.F90` together — there's an explicit reminder comment for this in `CMakeLists.txt`.
- `CHANGELOG.md` follows Keep a Changelog / SemVer; update it for any user-facing change (this is also called out explicitly in `CONTRIBUTING.md`).
- Changes affecting run directories/config files should be mirrored for GCHP and GEOS-Chem Classic where applicable, and use Fortran-90 free-format style consistent with surrounding code (this codebase has decades of contributions with inconsistent conventions — match nearby code rather than imposing a new style).
