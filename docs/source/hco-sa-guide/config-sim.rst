.. _hco-sa-sim-config:

######################
Configure a simulation
######################

.. note::

   Another useful resource for instructions on configuring HEMCO run
   directories is our `YouTube tutorial
   <https://www.youtube.com/watch?v=6Bup9V0ts6U&t=69s>`_.

Navigate to your new directory, and examine the contents:

.. code-block:: console

   $ cd /path/to/hemco/run/dir
   $ ls
   build/                         download_data.yml               HEMCO_sa_Grid.4x5.rc  Restarts/
   cleanRunDir.sh*                HEMCO_Config.rc                 HEMCO_sa_Spec.rc      rundir.version
   CodeDir@                       HEMCO_Config.rc.gmao_metfields  HEMCO_sa_Time.rc      runHEMCO.sh*
   config_for_offline_emissions/  HEMCO_Diagn.rc                  OutputDir/
   download_data.py*              HEMCO_sa_Config.rc              README

.. _hco-sa-sim-config-rundir:

=================================
Run directory configuration files
=================================

The following files can be modified to set up your HEMCO standalone simulation.

.. _hco-sa-sim-config-rundir-sa:

HEMCO_sa_Config.rc
------------------

Main configuration file for the HEMCO standalone simulation. This
file points to the other configuration files used to set up your
simulation (e.g. :ref:`hco-sa-sim-config-rundir-sa-grid`,
:ref:`hco-sa-sim-config-rundir-sa-time`, etc):.

This file typically references a
:ref:`hco-sa-sim-config-rundir-sa-hcorc` file using

.. code-block:: none

   >>>include HEMCO_Config.rc

which contains the emissions settings. Settings in
:file:`HEMCO_sa_Config.rc` will always override any settings in
the included :ref:`hco-sa-sim-config-rundir-sa-hcorc`.

.. _hco-sa-sim-config-rundir-sa-hcorc:

HEMCO_Config.rc
---------------

Contains emissions settings. :file:`HEMCO_Config.rc` can be taken
from a another model (such as GEOS-Chem), or can be built from a
sample file.

For more information on editing :file:`HEMCO_Config.rc`, please
see the following chapters: :ref:`hco-cfg`, :ref:`edit-hco-cfg`,
and :ref:`cfg-ex`.

.. important::

   Make sure that the path to your data directory in the
   :file:`HEMCO_Config.rc` file is correct.  Otherwise, HEMCO
   standalone will not be able read data from disk.

.. _hco-sa-sim-config-rundir-sa-hcodg:

HEMCO_Diagn.rc
--------------

Specifies which fields to save out to the HEMCO diagnostics file
saved in :file:`OutputDir` by default. The frequency to save out
diagnostics is controlled by the :ref:`hco-cfg-set-diagnfreq`
setting in :ref:`hco-sa-sim-config-rundir-sa`.

For more information, please see the chapter entitled
:ref:`hco-diag-configfile`.

.. _hco-sa-sim-config-rundir-sa-grid:

HEMCO_sa_Grid.$RES.rc
---------------------

Defines the grid specification for resolution :literal:`$RES`. Sample
files for several horizontal resolutions (4.0 x 5.0, 2.0 x 2.5, 0.5 x
0.625, and 0.25 x 0.3125 global grids) are stored in the in
:file:`HEMCO/run/` folder.  These are are automatically copied to the
run directory based on options chosen when running
:file:`createRunDir.sh`.

If you choose to run with a custom grid or over a regional domain, you
will need to modify this file manually.

.. _hco-sa-sim-config-rundir-sa-spec:

HEMCO_sa_Spec.rc
----------------

Defines the species to include in the HEMCO standalone
simulation. By default, the species in a GEOS-Chem full-chemistry
"standard" simulation are included.

You may easily generate a :file:`HEMCO_sa_Spec.rc` corresponding to a
different GEOS-Chem simulation with the GCPy example script
:file:`make_hemco_sa_spec.py`.  For usage details, see the `Generate a
HEMCO_sa_Spec.rc for HEMCO Standalone
<https://gcpy.readthedocs.io/en/latest/Hemco-Examples.html#generate-a-hemco-sa-spec-rc-file-for-hemco-standalone>`_
documentation at gcpy.readthedocs.io.

.. _hco-sa-sim-config-rundir-sa-time:

HEMCO_sa_Time.rc
----------------

Defines the start and end times of the HEMCO standalone simulation
as well as the emissions timestep (s).

.. _hco-sa-sim-config-rundir-sa-run:

runHEMCO.sh
-----------

Sample run script for submitting a HEMCO standalone simulation via
SLURM.
