.. _hco-sa-download-data:

###################
Download input data
###################

Before starting a HEMCO standalone simulation, make sure that all of
the relevant emissions and meteorology that you will need for your
simulation are present on disk.

.. tip::

   If you are located at an institution where there are several other
   HEMCO and/or `GEOS-Chem <https://geos-chem.readthedocs.io>`_ users,
   then data for HEMCO standalone might already be located in a shared
   folder.  Ask your sysadmin or IT staff.

The :ref:`GEOS-Chem Input Data <gcid>` portal is the main source of
emissions and meteorology simulations. This data, which is curated by
the GEOS-Chem Support Team at Washington University in St. Louis, is stored an
Amazon Web Services S3 bucket named `s3://geos-chem
<https://geos-chem.s3.amazonaws.com/index.html>`_.  You can easily
download the data from there to your computer cluster or AWS EC2 cloud
instance.

You can use a couple of different methods to download data.  Click on
one of the links below for more information.

.. toctree::
   :maxdepth: 1

   hco-sa-dry-run.rst
   ../geos-chem-shared-docs/supplemental-guides/bashdatacatalog.rst
   hco-sa-globus.rst
