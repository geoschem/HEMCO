.. |br| raw:: html

   <br />

.. _hco-sa-dry-run:

#######################################
Download data with a dry-run simulation
#######################################

Follow the steps below to perform a HEMCO standalone dry-run simulation:

==========================
Complete preliminary setup
==========================

Make sure that you have done the following steps;

#. :ref:`Downloaded the HEMCO source code <hco-sa-download>`
#. :ref:`Compiled the HEMCO standalone code <hco-sa-compile>`
#. :ref:`Configured your simulation <hco-sa-sim-config>`

.. _dry-run-run-flag:

=============================================
Run the executable with the ``--dryrun`` flag
=============================================

Run the HEMCO standalone executable file at the command line with the
:command:`--dryrun` command-line argument as shown below:

.. code-block:: console

   $ ./hemco_standalone -c HEMCO_sa_Config.rc --dryrun | tee log.dryrun

The :program:`tee` command will send the output of the dryrun to the
screen as well as to a file named :file:`log.dryrun`.

The :file:`log.dryrun` file will look somewhat like a regular
HEMCO standalone log file but will also contain a list of data files and
whether each file was found on disk or not.  This information will be
used by the :file:`download_data.py` script in the next step.

You may use whatever name you like for the dry-run output
log file (but we prefer :file:`log.dryrun`).

==============================================================
Run the :file:`download_data.py` script on the dryrun log file
==============================================================

Once you have successfully executed a HEMCO standalone dry-run, you
can use the output from the dry-run (contained in the
:file:`log.dryrun` file) to download the data files that the HEMCO
standalone will need to perform the corresponding "production"
simulation. You will download data from the :ref:`GEOS-Chem Input Data
<gcid>` portal.

Initialize the GCPy Python environment
--------------------------------------

Before you use the :file:`download_data.py` script, you will need to
load a Python environment.  We recommend using the Python environment
for GCPy (aka :file:`gcpy_env`), which contains all of the relevant
packages. For more information about how to install this environment,
please see `gcpy.readthedocs.io <gcpy.readthedocs.io.>`_.

Initialize the Python environment for GCPy with this command:

.. code-block:: console

   $ conda activate gcpy_env

Run the download_data.py script
-------------------------------

Navigate to your HEMCO run directory where you executed the dry-run
simulation.  You will use the :file:`download_data.py` script to
transfer data to your machine.  The command you will use takes this
form:

.. code-block:: console

   (gcpy_env) $ ./download_data.py log.dryrun PORTAL-NAME

where:

- :file:`download_data.py` is the dry-run data download program
  (written in Python).  It is included in each :ref:`HEMCO standalone
  run directory <hco-sa-rundir>` that you create. |br|
  |br|

- :file:`log.dryrun` is the log file from your HEMCO standalone
  dry-run simulation. |br|
  |br|

- :literal:`PORTAL-NAME` specifies the data portal that you wish
  to download from.  Allowed values are:

  .. list-table:: Allowed values for the ``PORTAL-NAME`` argument
		  to ``download_data.py``
     :header-rows: 1
     :align: center

     * - Value
       - Downloads from portal
       - With this command
       - Via this method
     * - geoschem+aws
       - :ref:`GEOS-Chem Input Data <gcid-data>`
       - :command:`aws s3 cp`
       - AWS CLI
     * - geoschem+http
       - :ref:`GEOS-Chem Input Data <gcid-data>`
       - :command:`wget`
       - HTTP
     * - rochester
       - :ref:`GCAP 2.0 met data @ Rochester <gcid-special-portals-gcap2>`
       - :command:`wget`
       - HTTP
     * - skip-download
       - Skips data download altogether
       - N/A
       - N/A

For example, to download data from the :ref:`GEOS-Chem Input Data
<gcid-data>` portal, use this command:

.. code-block:: console

   (gcpy_env) $ ./download_data.py log.dryrun geoschem+http

But if you have `AWS CLI (command-line interface)
<https://aws.amazon.com/cli/>`_ set up on your machine, use
this command instead:

.. code-block:: console

   (gcpy_env) $ ./download_data.py log.dryrun geoschem+aws

This will result in a much faster data transfer than by HTTP.

(Optional) Examine the log of unique data files
-----------------------------------------------

The :file:`download_data.py` script will generate a **log of
unique data files** (i.e. with all duplicate listings removed), which
looks similar to this:

.. code-block:: text

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!! LIST OF (UNIQUE) FILES REQUIRED FOR THE SIMULATION
   !!! Start Date       : 20190701 000000
   !!! End Date         : 20190701 010000
   !!! Simulation       : fullchem
   !!! Meteorology      : MERRA2
   !!! Grid Resolution  : 4.0x5.0
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ./HEMCO_Config.rc
   ./HEMCO_Config.rc.gmao_metfields
   ./HEMCO_Diagn.rc
   ./HISTORY.rc
   ./Restarts/GEOSChem.Restart.20190701_0000z.nc4 --> /home/ubuntu/ExtData/GEOSCHEM_RESTARTS/GC_14.5.0/GEOSChem.Restart.fullchem.20190701_0000z.nc4
   ./Restarts/HEMCO_restart.201907010000.nc
   ./geoschem_config.yml
   /path/to/ExtData/CHEM_INPUTS/CLOUD_J/v2024-09/FJX_j2j.dat
   /path/to/ExtData/CHEM_INPUTS/CLOUD_J/v2024-09/FJX_scat-aer.dat
   /path/to/ExtData/CHEM_INPUTS/CLOUD_J/v2024-09/FJX_scat-cld.dat
   /path/to/ExtData/CHEM_INPUTS/CLOUD_J/v2024-09/FJX_scat-ssa.dat
   /path/to/ExtData/CHEM_INPUTS/CLOUD_J/v2024-09/FJX_spec.dat
   /path/to/ExtData/CHEM_INPUTS/FastJ_201204/fastj.jv_atms_dat.nc
   /path/to/ExtData/CHEM_INPUTS/Linoz_200910/Linoz_March2007.dat
   /path/to/ExtData/CHEM_INPUTS/Olson_Land_Map_201203/Olson_2001_Drydep_Inputs.nc
   /path/to/ExtData/CHEM_INPUTS/UCX_201403/NoonTime/Grid4x5/InitCFC_JN2O_01.dat

    ... etc ...

This name of this "unique" log file will be the same as the log file
with dryrun ouptut, with :file:`.unique` appended. In our above
example, we passed :file:`log.dryrun` to :file:`download_data.py`, so
the "unique" log file will be named :file:`log.dryrun.unique`. This
"unique" log file can be very useful for documentation purposes.

Deactivate the GCPy Python environment
--------------------------------------

You can then deactivate the GCPy Python environment after all data
files have been downloaded:

.. code-block:: console

   (gcpy_env) $ conda deactivate

=============================================
Skip download, but create log of unique files
=============================================

If you wish to only produce the **log of unique data files** without
downloading any data, then use :literal:`skip-download` in place of
the :literal:`PORTAL-NAME` when running :file:`donwload_data.py`:

.. code-block:: console

   (gcpy_env) $ ./download_data.py log.dryrun skip-download

You can also abbreviate the command to:

.. code-block:: console

   (gcpy_env) $ ./download_data.py log.dryrun skip

This can be useful if you already have the necessary data downloaded to
your system but wish to create the log of unique files for documentation
purposes.
