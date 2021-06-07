[![License](https://img.shields.io/badge/License-MIT-blue.svg)](https://github.com/geoschem/geos-chem/blob/master/LICENSE.txt)

# README for the HEMCO source code repository

This repository (https://github.com/geoschem/HEMCO) contains the Harmonized Emissions Component 
(HEMCO) source code. HEMCO is a software component for computing (atmospheric) emissions from
different sources, regions, and species on a user-defined grid. It can combine, overlay, and
update a set of data inventories ('base emissions') and scale factors, as specified by the user
through the HEMCO configuration file. Emissions that depend on environmental variables and 
non-linear  parameterizations are calculated in separate HEMCO extensions. HEMCO can be run 
in standalone mode or coupled to an atmospheric model. A more detailed description of HEMCO 
is given in Keller et al. (2014).

## CI statuses

Pipeline | Status
:---|:---
Build Matrix (main) | [![Build Status](https://dev.azure.com/geoschem/hemco/_apis/build/status/Build%20Matrix?branchName=main)](https://dev.azure.com/geoschem/hemco/_build/latest?definitionId=7&branchName=main)


## Documentation

### Reference

C. A. Keller, M. S. Long, R. M. Yantosca, A. M. Da Silva, S. Pawson, D. J. Jacob, *HEMCO v1.0: a versatile,
ESMF-compliant component for calculation emissions in atmospheric models*, <u>Geosci. Model Dev.</u>, **7**, 1409-1417, 2014.

### Online user's manual

  * [The HEMCO User's Guide](http://hemco.readthedocs.io)


## Support
We encourage GEOS-Chem users to use [the Github issue tracker attached to this repository](https://github.com/geoschem/HEMCO/issues/new/choose) to report bugs or technical issues with the HEMCO code.

## License

HEMCO is distributed under the MIT license. Please see the license documents LICENSE.txt and
AUTHORS.txt in the root folder.


08 Jan 2021
GEOS-Chem Support Team
geos-chem-support@g.harvard.edu
