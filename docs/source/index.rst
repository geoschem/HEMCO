.. _hco:

##########################################
The Harmonized Emissions Component (HEMCO)
##########################################

.. raw:: html

   <p>
     <a href="https://github.com/geoschem/hemco/releases"><img src="https://img.shields.io/github/v/release/geoschem/hemco?label=Latest%20Stable%20Release"></a>
     <a href="https://github.com/geoschem/hemco/releases/"><img src="https://img.shields.io/github/release-date/geoschem/hemco"></a>
     <a href="https://doi.org/10.5281/zenodo.4618253"><img src="https://img.shields.io/badge/DOI-doi.org%2F10.5281%2Fzenodo.4618253-blue" alt="DOI"></a><br />
     <a href="https://github.com/geoschem/hemco/blob/main/LICENSE.txt"><img src="https://img.shields.io/badge/License-MIT-blue.svg"></a>
     <a href="https://hemco.readthedocs.io/en/latest/"><img src="https://img.shields.io/readthedocs/geos-chem?label=ReadTheDocs"></a>
     <a href="https://github.com/geoschem/hemco/actions/workflows/ubuntu.yml"><img src="https://github.com/geoschem/hemco/actions/workflows/ubuntu.yml/badge.svg" alt="Ubuntu"></a>
     <a href="https://github.com/geoschem/hemco/actions/workflows/mac.yml"><img src="https://github.com/geoschem/hemco/actions/workflows/mac.yml/badge.svg" alt="Mac"></a>
   </p>

The **Harmonized Emissions Component (HEMCO)** is a software component
for computing atmospheric emissions from different sources, regions,
and species on a user-defined grid. It can combine, overlay, and
update a set of data inventories :ref:`base emissions <hco-cfg-base>`
and :ref:`scale factors <hco-cfg-scalefac>`, as specified by the user
through :ref:`the HEMCO configuration file <hco-cfg>`. Emissions that
depend on environmental variables and non-linear parameterizations are
calculated in separate :ref:`hco-ext`. HEMCO can be run in
:ref:`standalone mode <hco-sa-guide>` or :ref:`coupled to an
atmospheric model <hemco-coupling>`.  A more detailed description of
HEMCO is given in :cite:t:`Keller_et_al._2014` and
:cite:t:`Lin_et_al._2021`. 

.. toctree::
   :maxdepth: 2
   :caption: HEMCO Standalone User Guide

   hco-sa-guide/intro
   hco-sa-guide/hardware
   hco-sa-guide/software
   hco-sa-guide/login-env
   hco-sa-guide/download-code
   hco-sa-guide/create-rundir
   hco-sa-guide/compiling
   hco-sa-guide/config-sim
   hco-sa-guide/download-data
   hco-sa-guide/run-standalone

.. toctree::
   :maxdepth: 2
   :caption: HEMCO Reference Guide

   hco-ref-guide/intro
   hco-ref-guide/basic-examples
   hco-ref-guide/hemco-config
   hco-ref-guide/extensions
   hco-ref-guide/units
   hco-ref-guide/diagnostics
   hco-ref-guide/more-examples
   hco-ref-guide/under-the-hood
   hco-ref-guide/input-file-format
   coupling/intro
   hco-ref-guide/key-references

.. toctree::
   :maxdepth: 1
   :caption: Supplemental Guides

   geos-chem-shared-docs/supplemental-guides/load-libraries-guide
   geos-chem-shared-docs/supplemental-guides/spack-guide
   geos-chem-shared-docs/supplemental-guides/error-guide
   geos-chem-shared-docs/supplemental-guides/debug-guide
   geos-chem-shared-docs/doc/gcid-portal-overview
   geos-chem-shared-docs/doc/gcid-special-portals
   geos-chem-shared-docs/supplemental-guides/bashdatacatalog
   geos-chem-shared-docs/supplemental-guides/parallel-guide
   geos-chem-shared-docs/supplemental-guides/netcdf-guide
   geos-chem-shared-docs/supplemental-guides/coards-guide
   geos-chem-shared-docs/supplemental-guides/related-docs

.. toctree::
   :maxdepth: 1
   :caption: Help and Reference

   hco-ref-guide/version-history
   hco-ref-guide/known-bugs
   reference/CONTRIBUTING
   reference/SUPPORT
   geos-chem-shared-docs/editing_these_docs


.. toctree::
   :hidden:

   geos-chem-shared-docs/doc/geoschem-config
   geos-chem-shared-docs/doc/hemco-config
   geos-chem-shared-docs/doc/hemco-diagn
   geos-chem-shared-docs/doc/phot-chem
   geos-chem-shared-docs/doc/spec-db
   geos-chem-shared-docs/simulations/aerosol-only
   geos-chem-shared-docs/simulations/carbon
   geos-chem-shared-docs/simulations/fullchem
   geos-chem-shared-docs/simulations/hg
   geos-chem-shared-docs/simulations/metals
   geos-chem-shared-docs/simulations/tago3
   geos-chem-shared-docs/simulations/transport-tracers
   geos-chem-shared-docs/supplemental-guides/aerosols-guide
   geos-chem-shared-docs/supplemental-guides/apm-guide
   geos-chem-shared-docs/supplemental-guides/ate-guide
   geos-chem-shared-docs/supplemental-guides/cloud-conv-guide
   geos-chem-shared-docs/supplemental-guides/custom-emissions-guide
   geos-chem-shared-docs/supplemental-guides/customize-guide
   geos-chem-shared-docs/supplemental-guides/drydep-guide
   geos-chem-shared-docs/supplemental-guides/history-diag-guide
   geos-chem-shared-docs/supplemental-guides/pbl-mixing-guide
   geos-chem-shared-docs/supplemental-guides/photolysis-guide
   geos-chem-shared-docs/supplemental-guides/phys-consts-guide
   geos-chem-shared-docs/supplemental-guides/pm25-pm10-guide
   geos-chem-shared-docs/supplemental-guides/rrtmg-guide
   geos-chem-shared-docs/supplemental-guides/science-guides
   geos-chem-shared-docs/supplemental-guides/tomas-guide
   geos-chem-shared-docs/supplemental-guides/using-kpp-standalone
   geos-chem-shared-docs/supplemental-guides/wetdep-guide
