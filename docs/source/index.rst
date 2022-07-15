##########################################
The Harmonized Emissions Component (HEMCO)
##########################################

.. raw:: html

   <p>
   <a href="https://github.com/geoschem/HEMCO/releases"><img src="https://img.shields.io/github/v/release/geoschem/HEMCO?include_prereleases&label=Latest%20Pre-Release"></a>
   <a href="https://github.com/geoschem/HEMCO/releases"><img src="https://img.shields.io/github/v/release/geoschem/HEMCO?label=Latest%20Stable%20Release"></a>
   <a href="https://github.com/geoschem/HEMCO/releases/"><img src="https://img.shields.io/github/release-date/geoschem/HEMCO"></a>
   <a href="https://github.com/geoschem/HEMCO/blob/main/LICENSE.txt"><img src="https://img.shields.io/badge/License-MIT-blue.svg"></a>
   <a href="https://doi.org/10.5281/zenodo.4618253"><img src="https://zenodo.org/badge/DOI/10.5281/zenodo.4618253.svg"></a>
   <a href="https://hemco.readthedocs.io/en/latest/"><img src="https://img.shields.io/readthedocs/hemco?label=ReadTheDocs"></a>
   <a href="https://dev.azure.com/geoschem/HEMCO/_build?definitionId=11"><img src="https://dev.azure.com/geoschem/hemco/_apis/build/status/Quick%20Build?branchName=dev"></a>
   </p>

The **Harmonized Emissions Component (HEMCO)** is a software component
for computing atmospheric emissions from different sources, regions,
and species on a user-defined grid. It can combine, overlay, and
update a set of data inventories :ref:`base emissions <hco-cfg-base>`
and :ref:`scale factors <hco-cfg-scalefac>`, as specified by the user
through :ref:`the HEMCO configuration file <hco-cfg>`. Emissions that
depend on environmental variables and non-linear parameterizations are
calculated in separate :ref:`hco-ext`. HEMCO can be run in
:ref:`standalone mode <hco-running>` or :ref:`coupled to an
atmospheric model <hemco-coupling>`.  A more detailed description of
HEMCO is given in :cite:t:`Keller_et_al._2014` and :cite:t:`Lin_et_al._2021`.

.. toctree::
   :maxdepth: 1
   :caption: Getting Started

   getting-started/quick-start.rst
   getting-started/hardware-requirements.rst
   getting-started/software-requirements.rst

.. toctree::
   :maxdepth: 2
   :caption: User Guide (HEMCO Standalone)

   user-guide/downloading.rst
   user-guide/compiling.rst
   user-guide/rundir-init.rst
   user-guide/rundir-config.rst
   user-guide/running.rst

.. toctree::
   :maxdepth: 2
   :caption: Configuring HEMCO

   config/basic-examples.rst
   config/hemco-config.rst
   config/extensions.rst
   config/diagnostics.rst
   config/units.rst
   config/more-examples.rst

.. toctree::
   :maxdepth: 2
   :caption: Technical information

   tech-info/input-file-format.rst
   tech-info/under-the-hood.rst
   coupling/intro.rst

.. toctree::
   :maxdepth: 1
   :caption: HEMCO reference

   reference/key-references.rst
   reference/version-history.rst
   reference/known-bugs.rst

.. toctree::
   :maxdepth: 1
   :caption: Supplemental Guides

   geos-chem-shared-docs/supplemental-guides/libraries.rst
   geos-chem-shared-docs/supplemental-guides/spack.rst
   geos-chem-shared-docs/supplemental-guides/debug-guide.rst
   geos-chem-shared-docs/supplemental-guides/bashdatacatalog.rst
   geos-chem-shared-docs/supplemental-guides/netcdf-guide.rst
   geos-chem-shared-docs/supplemental-guides/coards-guide.rst
   geos-chem-shared-docs/supplemental-guides/related-docs.rst


.. toctree::
   :maxdepth: 1
   :caption: Additional information

   reference/CONTRIBUTING.md
   reference/SUPPORT.md
   geos-chem-shared-docs/editing_these_docs.rst
