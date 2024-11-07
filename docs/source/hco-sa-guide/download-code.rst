.. _hco-sa-download:

########################
Download the source code
########################

The :program:`HEMCO` source code may be downloaded (aka "cloned") with
Git.  By default the :command:`git clone` command will give you the
**main** branch by default:
default.

.. code-block:: console

   $ git clone --recurse-submodules https://github.com/geoschem/hemco.git HEMCO
   $ cd HEMCO

This will place you on the **main** branch, which contains the latest
stable release of HEMCO.

.. tip::

   To use an older HEMCO version (e.g. 3.0.0), follow
   these additional steps:

   .. code-block:: console

      $ git checkout tags/3.0.0                  # Points HEAD to the tag "3.0.0"
      $ git branch version_3.0.0                 # Creates a new branch at tag "3.0.0"
      $ git checkout version_3.0.0               # Checks out the version_3.0.0 branch
      $ git submodule update --init --recursive  # Reverts submodules to the "3.0.0" tag

   You can do this for any tag in the version history.   For a list of
   all tags, type:

   .. code-block:: console

      $ git tag

   If you have any unsaved changes, make sure you commit those to a
   branch prior to updating versions.
