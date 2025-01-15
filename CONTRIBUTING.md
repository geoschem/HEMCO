# Contributing Guidelines

Thank you for looking into contributing to HEMCO! HEMCO is a grass-roots model that relies on contributions from community members like you. Whether you're new to HEMCO or a longtime user, you're a valued member of the community, and we want you to feel empowered to contribute.

## We use GitHub and ReadTheDocs
We use GitHub to host the HEMCO source code, to track issues, user questions, and feature requests, and to accept pull requests: [https://github.com/geoschem/HEMCO](https://github.com/geoschem/HEMCO). Please help out as you can in response to issues and user questions.

HEMCO documentation can be found at [hemco.readthedocs.io](https://hemco.readthedocs.io).

## When should I submit updates?

Submit bug fixes right away, as these will be given the highest priority.  Please see **[Support Guidelines](https://hemco.readthedocs.io/en/stable/reference/SUPPORT.html)** for more information.

Submit updates (code and/or data) for mature model developments once you have submitted a paper on the topic.

## How can I submit updates?
We use **GitHub Flow**, so all changes happen through [pull requests](https://help.github.com/articles/creating-a-pull-request/). This workflow is [described here](https://docs.github.com/en/get-started/using-github/github-flow).

As the author you are responsible for:
- Testing your changes
- Updating the user documentation (if applicable)
- Supporting issues and questions related to your changes

### Coding conventions
The HEMCO codebase dates back several decades and includes contributions from many people and multiple organizations. Therefore, some inconsistent conventions are inevitable, but we ask that you do your best to be consistent with nearby
code.

### Checklist for submitting code updates

  1. Use Fortran-90 free format instead of Fortran-77 fixed format.
  2. Include thorough comments in all submitted code.
  3. Include full citations for references at the top of relevant source code modules.
  4. Check that you have updated the `CHANGELOG.md` file.
  5. Remove extraneous code updates (e.g. testing options, other science).
  6. Submit any related code or configuration files for [GCHP](https://gchp.readthedocs.io) along with code or configuration files for [GEOS-Chem Classic](https://geos-chem.readthedocs.io).

### Checklist for submitting data files

  1. Choose a final file naming convention before submitting data files for inclusion to GEOS-Chem.
  2. Make sure that all netCDF files [adhere to the COARDS conventions](https://geos-chem.readthedocs.io/en/latest/geos-chem-shared-docs/supplemental-guides/coards-guide.html).
  3. [Concatenate netCDF files](https://geos-chem.readthedocs.io/en/latest/geos-chem-shared-docs/supplemental-guides/netcdf-guide.html#concatenate-netcdf-files)  to reduce the number of files that need to be opened.  This results in more efficient I/O operations.
  4. [Chunk and deflate netCDF](https://geos-chem.readthedocs.io/en/latest/geos-chem-shared-docs/supplemental-guides/netcdf-guide.html#chunk-and-deflate-a-netcdf-file-to-improve-i-o) files in order to improve file I/O.
  5. Include an updated [HEMCO configuration file](https://hemco.readthedocs.io/en/latest/hco-ref-guide/hemco-config.html) corresponding to the new data.
  6. Include a README file detailing data source, contents, etc.
  7. Include script(s) used to process original data.
  8. Include a summary or description of the expected results (e.g. emission totals for each species).

Also follow these additional steps to ensure that your data can be read by GCHP:

  1. All netCDF data variables should be of type `float` (aka `REAL*4`) or `double` (aka `REAL*8`).
  2. Use a recent reference datetime (i.e. after `1900-01-01`) for the netCDF `time:units` attribute.
  3. The first time value in each file should be 0, corresponding with the reference datetime.

## How can I request a new feature?
We accept feature requests through issues on GitHub. To request a new feature, **[open a new issue](https://github.com/geoschem/HEMCO/issues/new/choose)** and select the feature request template. Please include all the information that migth be relevant, including the motivation for the feature.

## How can I report a bug?
Please see **[Support Guidelines](https://hemco.readthedocs.io/en/stable/reference/SUPPORT.html)**.

## Where can I ask for help?
Please see **[Support Guidelines](https://hemco.readthedocs.io/en/stable/reference/SUPPORT.html)**.
