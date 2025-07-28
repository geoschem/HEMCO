.. _hco-known-bugs:

#####################
Known bugs and issues
#####################

Please see our `Issue tracker on GitHub
<https://github.com/geoschem/HEMCO/issues>`_ for a list of recent
bugs and fixes.

===================
Current bug reports
===================

These `bug reports (on GitHub)
<https://github.com/geoschem/HEMCO/issues?q=is%3Aissue+is%3Aopen+label%3A%22category%3A+Bug%22>`_
are currently unresolved. We hope to fix these in future releases.

=======================================
Other issues that you should know about
=======================================

.. _gc-known-bugs-gcc12:

GCC 12.2.0 is discontinued in Spack v1.0.0
------------------------------------------

As of Spack v1.0, `spack-packages <https://packages.spack.io/>`_ has
been split off into its own separate repository. This change includes
the unfortunate deprecation of the :program:`GNU Compiler Collection
(GCC)` version 12.2.0. It appears that only the most recent minor
release in each major release is now treated as stable. These
deprecations are updated promptly for example, GCC 12.4.0 is already
marked as deprecated just 10 days after the release of GCC 12.5.0.

Deprecated GCC versions are no longer listed with the :command:`spack
info` command, so rather than warning users about deprecation, Spack
simply fails with an unhelpful error message about not being able to
satisfy the request.

For the time being, we recommend that you use `Spack release v0.23.1
<https://github.com/spack/spack/releases/tag/v0.23.1>`_ which still
supports GCC 12.2.0 and related libraries.  Please see our
:ref:`spackguide` Supplemental Guide for an updated Spack
installation workflow.

Masks cannot be applied to extensions
-------------------------------------

It is currently not possible to :ref:`geographically tag emissions
<cfg-ex-mask-tagged>` computed by :ref:`hco-ext` in the same way that
you would do for :ref:`base emissions <hco-cfg-base>`.  We hope to add
this feature into a future HEMCO release.

HEMCO may not recognize alternate spellings of units
----------------------------------------------------

If a unit string (e.g. :literal:`kg/m2/s`) read from a netCDF
file matches the unit string listed under the
:ref:`hco-cfg-base-srcunit` column of :ref:`the HEMCO configuration
file <hco-cfg>`, then no unit conversion will happen.

But if the unit string in the file is e.g. :literal:`kg m-2 s-1` and
the unit in the configuration file is :literal:`kg/m2/s`, then HEMCO
detects this as a difference in units, and will try to apply an
automatic conversion that is really unnecssary.

Therefore, we recommend not to rely on HEMCO's automatic unit
capability, and to specfiy all scale factors for unit conversions
explicitly in the configuration file.

============================
Bugs that have been resolved
============================

These `bugs (reported on GitHub) <https://github.com/geoschem/HEMCO/issues?q=+label%3A%22category%3A+Bug+Fix%22+>`_ have been resolved.
