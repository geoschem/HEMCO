.. _hco-sa-login:

################################
Configure your login environment
################################

.. tip::

   You may :ref:`skip ahead <hco-sa-download>` if you will be using
   :program:`GEOS-Chem Classic` on an Amazon EC2 cloud instance.
   When you initialize the EC2 instance with one of the pre-configured Amazon
   Machine Images  (AMIs) all of the required software libraries will be
   automatically loaded.

In this chapter, you will learn how to load the software packages that
you have created into your computational environment.  This will need
to be done each time you log in to your computer system.

An environment file does the following:

  1. Loads software libraries into your login environment.  This is
     often done with a module manager such as :command:`lmod`,
     :command:`spack`, or  :command:`environment-modules`.

  2. Stores settings for HEMCO and its dependent libraries in
     shell variables called `environment variables
     <https://www.networkworld.com/article/3215965/all-you-need-to-know-about-unix-environment-variables.html>`_.

Environment files allow you to easily switch between different sets of
libraries.  For example, you can keep one environment file to load the
Intel Compilers for HEMCO standalone and another to load
the GNU Compilers.

For general information about how libraries are loaded, see our
:ref:`Library Guide <libguide>` in the Supplemental Guides section.

We recommend that you place module load commands into a separate
**environment file**  rather than directly into your :file:`~/.bashrc`
or :file:`~/.bash_aliases` startup scripts.

.. _hco-sa-login-gnu:

================================================
Sample environment file for GNU 12.2.0 compilers
================================================

Below is a sample environment file (based on an enviroment file for
the Harvard Cannon computer cluster).  This file will load software
libraries built with the `GNU 12.2.0 compilers
<https://gcc.gnu.org/onlinedocs/12.2.0/>`_.

.. note::

   This environment file shown below assumes that required software
   packages for :program:`GEOS-Chem Classic` are available as
   pre-built modules.  If your computer system does not have these
   packages pre-installed, you can build them with Spack.  Please see
   our :ref:`spackguide` supplemental guide for detailed instructions.

Save the code below (with any appropriate modifications for your own
computer system) to a file named :file:`~/gnu12.env`.

.. code-block:: bash

   ###############################################################################
   #
   # Environment file for HEMCO + GNU Compiler Collection 12.2.0
   #
   ###############################################################################

   # Display message (if we are in a terminal window)
   if [[ $- = *i* ]] ; then
     echo "Loading modules for GEOS-Chem Classic, please wait ..."
   fi

   #==============================================================================
   # Unload all previously-unloaded software
   #==============================================================================

   # Unload packages loaded with "module load"
   module purge

   #==============================================================================
   # Load software packages for GNU 12.2.0
   #==============================================================================
   if [[ $- = *i* ]] ; then
     echo "... Loading FASRC-built software, please wait ..."
   fi

   # Pre-built modules needed for HEMCO
   # (NOTE: These may be named differently on your system)
   module load gcc/12.2.0-fasrc01             # gcc / g++ / gfortran
   module load openmpi/4.1.4-fasrc01          # MPI
   module load netcdf-c/4.9.2-fasrc01         # netcdf-c
   module load netcdf-fortran/4.6.0-fasrc02   # netcdf-fortran
   module load flex/2.6.4-fasrc01             # Flex lexer (needed for KPP)
   module load cmake/3.25.2-fasrc01           # CMake (needed to compile)

   #==============================================================================
   # Environment variables and related settings
   # (NOTE: Lmod will define <module>_HOME variables for each loaded module)
   #==============================================================================

   # Make all files world-readable by default
   umask 022

   # Set number of threads for OpenMP.  If running in a SLURM environment,
   # use the number of requested cores.  Otherwise use 8 cores for OpenMP.
   if [[ "x${SLURM_CPUS_PER_TASK}" == "x" ]]; then
       export OMP_NUM_THREADS=8
   else
       export OMP_NUM_THREADS="${SLURM_CPUS_PER_TASK}"
   fi

   # Max out the stacksize memory limit
   export OMP_STACKSIZE="500m"

   # Compilers
   export CC="gcc"
   export CXX="g++"
   export FC="gfortran"
   export F77="${FC}"

   # netCDF
   if [[ "x${NETCDF_HOME}" == "x" ]]; then
       export NETCDF_HOME="${NETCDF_C_HOME}"
   fi
   export NETCDF_C_ROOT="${NETCDF_HOME}"
   export NETCDF_FORTRAN_ROOT=${NETCDF_FORTRAN_HOME}

   # KPP 3.0.0+
   export KPP_FLEX_LIB_DIR=${FLEX_HOME}/lib64

   #==============================================================================
   # Set limits
   #==============================================================================

   ulimit -c unlimited   # coredumpsize
   ulimit -u 50000       # maxproc
   ulimit -v unlimited   # vmemoryuse
   ulimit -s unlimited   # stacksize

   #==============================================================================
   # Print information
   #==============================================================================

   module list

   echo ""
   echo "Environment:"
   echo ""
   echo "CC                  : ${CC}"
   echo "CXX                 : ${CXX}"
   echo "FC                  : ${FC}"
   echo "KPP_FLEX_LIB_DIR    : ${KPP_FLEX_LIB_DIR}"
   echo "MPI_HOME            : ${MPI_HOME}"
   echo "NETCDF_HOME         : ${NETCDF_HOME}"
   echo "NETCDF_FORTRAN_HOME : ${NETCDF_FORTRAN_HOME}"
   echo "OMP_NUM_THREADS     : ${OMP_NUM_THREADS}"
   echo ""
   echo "Done sourcing ${BASH_SOURCE[0]}"

.. tip::

   Ask your sysadmin how to load software libraries.  If you are using
   your institution's computer cluster, then chances are there will
   be a software module system installed, with commands similar to
   those listed above.

You may also place the above command within your HEMCO standalone run
script, which will be discussed in a subsequent chapter.

To activate the settings contained in the environment file, type:

.. code-block:: console

   $ . ~/gnu12.env

.. _hco-sa-login-intel:

================================================
Sample environment file for Intel 2023 compilers
================================================

Below is a sample environment file from the Harvard Cannon computer
cluster.  This file will load software libraries built with the Intel
2023 compilers.

Add the code below (with the appropriate modifications for your
system) into a file named :file:`~/intel23.env`.

.. code-block:: bash

   ###############################################################################
   #
   # Environment file for HEMCO + GNU Compiler Collection 12.2.0
   #
   ###############################################################################

   # Unload all modules first
   module purge

   # Load modules
   module load intel/23.0.0-fasrc01           # icc / i++ / gfortran
   module load intelmpi/2021.8.0-fasrc01      # MPI
   module load netcdf-fortran/4.6.0-fasrc03   # netCDF-Fortran
   module load flex/2.6.4-fasrc01             # Flex lexer (needed for KPP)
   module load cmake/3.25.2-fasrc01           # CMake (needed to compile)

   #==============================================================================
   # Environment variables and related settings
   # (NOTE: Lmod will define <module>_HOME variables for each loaded module
   #==============================================================================

   # Make all files world-readable by default
   umask 022

   # Set number of threads for OpenMP.  If running in a SLURM environment,
   # use the number of requested cores.  Otherwise use 8 cores for OpenMP.
   if [[ "x${SLURM_CPUS_PER_TASK}" == "x" ]]; then
       export OMP_NUM_THREADS=8
   else
       export OMP_NUM_THREADS="${SLURM_CPUS_PER_TASK}"
   fi

   # Max out the stacksize memory limit
   export OMP_STACKSIZE="500m"

   # Compilers
   export CC="icx"
   export CXX="icx"
   export FC="ifort"
   export F77="${FC}"

   # netCDF
   if [[ "x${NETCDF_HOME}" == "x" ]]; then
      export NETCDF_HOME="${NETCDF_C_HOME}"
   fi
   export NETCDF_C_ROOT="${NETCDF_HOME}"
   export NETCDF_FORTRAN_ROOT="${NETCDF_FORTRAN_HOME}"

   # KPP 3.0.0+
   export KPP_FLEX_LIB_DIR="${FLEX_HOME}/lib64"

   #==============================================================================
   # Set limits
   #==============================================================================

   ulimit -c unlimited   # coredumpsize
   ulimit -u 50000       # maxproc
   ulimit -v unlimited   # vmemoryuse
   ulimit -s unlimited   # stacksize

   #==============================================================================
   # Print information
   #==============================================================================

   module list

   echo ""
   echo "Environment:"
   echo ""
   echo "CC                  : ${CC}"
   echo "CXX                 : ${CXX}"
   echo "FC                  : ${FC}"
   echo "KPP_FLEX_LIB_DIR    : ${KPP_FLEX_LIB_DIR}"
   echo "MPI_HOME            : ${MPI_HOME}"
   echo "NETCDF_HOME         : ${NETCDF_HOME}"
   echo "NETCDF_FORTRAN_HOME : ${NETCDF_FORTRAN_HOME}"
   echo "OMP_NUM_THREADS     : ${OMP_NUM_THREADS}"
   echo ""
   echo "Done sourcing ${BASH_SOURCE[0]}"

   
.. tip::

   Ask your sysadmin how to load software libraries.  If you
   are using your institution's computer cluster, then chances
   are there will be a software module system installed, with
   commands similar to those listed above.

To activate the settings contained in the environment file, type:

.. code-block:: console

   $ . intel23.env

.. tip::

   Keep a separate environment file for each combination of
   modules that you will load.

.. _hco-sa-envvar-compilers:

=======================================
Set environment variables for compilers
=======================================

Add the following environment variables to your environment file to
specify the compilers that you wish to use:

.. table:: Environment variables that specify the choice of compiler
   :align: center

   +---------------+------------------+--------------------+-----------------+
   | Variable      | Specifies the:   | GNU name           | Intel name      |
   +===============+==================+====================+=================+
   | :envvar:`CC`  | C compiler       | :envvar:`gcc`      | :envvar:`icx`   |
   +---------------+------------------+--------------------+-----------------+
   | :envvar:`CXX` | C++ compiler     | :envvar:`g++`      | :envvar:`icx`   |
   +---------------+------------------+--------------------+-----------------+
   | :envvar:`FC`  | Fortran compiler | :envvar:`gfortran` | :envvar:`ifort` |
   +---------------+------------------+--------------------+-----------------+

These environment variables should be defined in your
:ref:`environment file <hco-sa-login>`.

.. note::

   HEMCO only requires the Fortran compiler.  But you will
   also need the C and C++ compilers if you plan to build other
   software packages or :ref:`install libraries manually <spackguide>`.

   Also, older Intel compiler versions used :envvar:`icc` as the name
   for the C compiler and :envvar:`icpc` as the name of the C++ compiler.
   These names have been deprecated in Intel 2023 and will be removed
   from future Intel compiler releases.

.. _hco-sa-envvar-parallel:

=============================================
Set environment variables for parallelization
=============================================

The HEMCO standalone` uses `OpenMP parallelization
<Parallelizing_GEOS-Chem>`_, which is an implementation of
shared-memory (aka serial) parallelization.

.. important::

   OpenMP-parallelized programs cannot execute on more than 1
   computational node.  Most modern computational nodes typically
   contain  between 16 and 64 cores. Therefore, HEMCO standalone
   simulations will not be able to take advantage of more cores than
   these.

Add the following environment variables to your environment file to
control the OpenMP parallelization settings:

.. option:: OMP_NUM_THREADS

   The :envvar:`OMP_NUM_THREADS` environment variable sets the number of
   computational cores (aka threads) to use.

   For example, the command below will tell HEMCO standalone to use 8
   cores within parallel sections of code:

   .. code:: console

      $ export OMP_NUM_THREADS=8

.. option:: OMP_STACKSIZE

   In order to use HEMCO standalone with `OpenMP
   parallelization <Parallelizing_GEOS-Chem>`_, you must request the
   maximum amount of stack memory in your login environment. (The
   stack memory is where local automatic variables and temporary
   :envvar:`!$OMP PRIVATE` variables will be created.) Add the
   following lines to your system startup file and to your GEOS-Chem
   run scripts:

   .. code-block:: bash

      ulimit -s unlimited
      export OMP_STACKSIZE=500m

   The :command:`ulimit -s unlimited` command will tell the bash shell
   to use the maximum amount of stack memory that is available.

   The environment variable :envvar:`OMP_STACKSIZE` must also be set to a very
   large number. In this example, we are nominally requesting 500 MB of
   memory. But in practice, this will tell the GNU Fortran compiler to use
   the maximum amount of stack memory available on your system. The value
   **500m** is a good round number that is larger than the amount of stack
   memory on most computer clusters, but you can increase this if you wish.

.. _errors_caused_by_incorrect_settings:

=======================================
Fix errors caused by incorrect settings
=======================================

Be on the lookout for these errors:

  #. If :option:`OMP_NUM_THREADS` is set to 1, then your
     HEMCO standalone simulation will execute using only
     one computational core.  This will make your simulation take much
     longer than is necessary.

  #. If :option:`OMP_STACKSIZE` environment variable is not included
     in your environment file (or if it is set to a very low value),
     you might encounter a **segmentation fault**.  In this case,
     the HEMCO standalone "thinks" that it does not have
     enough memory to perform the simulation, even though sufficient
     memory may be present.
