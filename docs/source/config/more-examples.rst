.. |br| raw:: html

   <br />

.. _cfg-ex:

###########################
More configuration examples
###########################

.. _cfg-ex-scl:

=====================
Scale factor examples
=====================

.. _cfg-ex-scl-shapefile:

Scale/zero emissions using a shapefile country mask
---------------------------------------------------

HEMCO has the ability to define country-specific scale factors. To
utilize this feature, you must first specify a mask file in the
**NON-EMISSIONS DATA** section of :ref:`the HEMCO configuration file
<hco-cfg>`, such as:

.. code-block:: kconfig

   #==============================================================================
   # --- Country mask file ---
   #==============================================================================
   * COUNTRY_MASK /path/to/file/countrymask_0.1x0.1.nc CountryID 2000/1/1/0 C xy count * - 1 1

The mask file specified above was created from a shapefile obtained
from the `GADM database <http://www.gadm.org>`_. The country mask
netCDF file (:file:`countrymask_0.1x0.1.nc`, `follow this link
<http://geoschemdata.wustl.edu/ExtData/HEMCO/MASKS/v2014-07/countrymask_0.1x0.1.nc>`_
) identifies countries by their ISO 3166-1 numeric code. Countries and
their ISO3166-1-numeric codes are listed in the
:file:`country_codes.csv` file (`follow this link
<http://geoschemdata.wustl.edu/ExtData/HEMCO/MASKS/v2014-07/country_codes.csv>`_).

The country-specific scale factors can be specified in a separate
ASCII file ending with with the suffix :literal:`.txt.` The container
name of the mask file (e.g. :literal:`COUNTRY_MASK`) must be given in
the first line of the file. The following lines define the
country-specific scale factors. ID 0 is reserved for the default
values that are applied to all countries with no specific values
listed. An example :file:`scalefactor.txt` file is provided below:

.. code-block:: kconfig

   # Country mask field name
   COUNTRY_MASK

   # Country data
   # Name   | ID  | Scale factor
   DEFAULT    0     1.0
   CHINA      156   0.95
   INDIA      356   1.10
   KOREA      410   0.0

The scale factor(s) listed are interpreted by HEMCO the same way as
other scale factors. Multiple values separated by :literal:`/` are
interpreted as temporally changing values:

  - 7 values = Sun, Mon, ..., Sat;
  - 12 values = Jan, Feb, ..., Dec;
  - 24 values = 12am, 1am, ..., 11pm (local time!).

The country-specific scale factors would then be defined in the
:ref:`Scale Factors <hco-cfg-scalefac>` section of :ref:`the HEMCO
configuration file <hco-cfg>` as:

.. code-block:: kconfig

   501 SCALE_COUNTRY /path/to/file/scalefactor.txt  - - - xy count 1

The scale factors can the be applied to the emission field(s) that you
wish to scale. For example:

.. code-block:: kconfig

   0 MIX_NO_IND MIX_Asia_NO.generic.025x025.nc NO_INDUSTRY 2008-2010/1-12/1/0 C xy kg/m2/s NO  1/27/25/1006/ 501 1/2 45

These steps can also be used to scale emissions for different regions
(e.g. provinces, states) by providing HEMCO with a mask file
containing the regions to be scaled.


.. _cfg-ex-scl-rec-mask:

Scale/zero emissions using a rectangular mask
---------------------------------------------

.. important::

   If you are using HEMCO versions prior to 3.5.0, you may encounter a
   bug when trying to follow this example. See Github issue:
   https://github.com/geoschem/HEMCO/issues/153 for a workaround.

Another way to scale all emissions over a country (or set them to
zero) is to apply a rectangular mask.

For example, to set all emissions over Australia and surrounding
islands to zero, add this line to the :ref:`hco-cfg-masks` section of
:ref:`the HEMCO configuration file <hco-cfg>`:

.. code-block:: kconfig

    1010 AUS_MASK 105.0/-46.0/160.0/-10.0 - 2000/1/1/0 C xy 1 1 105.0/-46.0/160.0/–10.0``

Here you directly provide the lower left and upper right corner of the
mask region mask instead of a netCDF file:
:literal:`lon1/lat1/lon2/lat2` You can then combine this mask with
a scale factor of zero to eliminate any emissions over that area.

In :ref:`Base emissions <hco-cfg-base>`

.. code-block:: kconfig

    0 HTAP_NO_IND /path/to/HTAP_NO_INDUSTRY.generic.01x01.nc emi_no 2008-2010/1-12/1/0 C xy kg/m2/s NO 1/27/25/501 1/2 4

In :ref:`Scale Factors <hco-cfg-scalefac>`:

.. code-block:: kconfig

   501 SCALE_AUS 0.0 - - - xy unitless 1 1010

In :ref:`hco-cfg-masks`:

.. code-block:: kconfig

   # Defines a rectangular region that
   # should cover AUS + surrounding islands
   1010 AUS_MASK 105.0/-46.0/160.0/-10.0 – 2000/1/1/0 C xy 1 1 105.0/-46.0/160.0/-10.0

.. _cfg-ex-scl-spc:

Scale emissions by species
--------------------------

You may define uniform scale factors for single species that
apply across all emission inventories, sectors and extensions. These
scale factors can be set in the :ref:`Settings <hco-cfg-settings>`
section of :ref:`the HEMCO configuration file <hco-cfg>`, using the
:option:`EmissScale_<species-name>`, where :literal:`<species-name>`
denotes the name of a HEMCO species such as :literal:`CO`,
:literal:`CH4`, :literal:`NO`, etc.

For instance, to scale all NO emissions by 50% add the line
:literal:`EmisScale_NO` to the :ref:`Settings <hco-cfg-settings>`
section of the :ref:`the HEMCO configuration file <hco-cfg>`:

.. code-block:: kconfig

   ###############################################################################
   ### BEGIN SECTION SETTINGS
   ###############################################################################

   ROOT:                        /path/to/HEMCO/data/directory
   Logfile:                     HEMCO.log
   ... etc ...
   EmisScale_NO                 1.5

   ### END SECTION SETTINGS ###

.. _cfg-ex-scl-zero-spc:

Zero emissions of selected species
----------------------------------

To zero emissions of a given species (e.g. NO) from any inventory
listed under :ref:`Base Emissions <hco-cfg-base>`, do the following:

#. Create your own scale factor and assign value 0.0 to it. This must
   go into the :ref:`Scale Factors <hco-cfg-scalefac` section of
   :ref:`the HEMCO configuration file <hco-cfg>`:

   .. code-block:: kconfig

      400 ZERO 0.0 - - - xy 1 1

#. Apply this scale factor to all of the emissions entries in the
   HEMCO configuration file that you would like to zero out.  For
   example:

   .. code-block:: kconfig

      0 MIX_NO_IND  /path/to/MIX_Asia_NO.generic.025x025.nc NO_INDUSTRY  2008-2010/1-12/1/0 C xy kg/m2/s  NO  1/27/25/400/1006 1/2 45

This can be a useful way to set the emissions of some species to zero
for sensitivity study purposes.

.. note::

   All scale factors should be listed before masks.

.. _cfg-ex-ext-global:

Scale extension emissions globally by species
---------------------------------------------

You may pass a global scale factor to the :ref:`hco-ext`.  For
example, to double soil NO emissions everywhere, add the
:literal:`Scaling_NO` to the section for the :option:`SoilNOx`
extension.  This is located in the :option:`Extension Switches
<hco-cfg-ext-switches>` sectoin of :ref:`the HEMCO configuration file
<hco-cfg>`, as shown below:

.. code-block:: kconfig

   104     SoilNOx           : on    NO
       --> Use fertilizer NOx:       true
       --> Scaling_NO        :       2.0

.. _cfg-ex-summer-nox:

Scale summertime soil NOx emisions over the US
----------------------------------------------

It is possible to pass uniform and/or spatiotemporal scale factors to
some of the extensions, including :option:`SoilNOx`.

For instance, suppose you want to cut summertime soil NOx emissions
over the US by a factor of 2. This can be done by assigning a scale
field of the form :literal:`Scaling_<species-name>` in the
:ref:`Extension Switches <hco-cfg-ext-switches>` section of :ref:`the
HEMCO configuration file <hco-cfg>`:



| ``104 SOILNOX_ARID    $ROOT/SOILNOX/v2014-07/soilNOx.climate.generic.05x05.nc    ARID         2000/1/1/0        C xy unitless NO - 1 1``
| ``104 SOILNOX_NONARID $ROOT/SOILNOX/v2014-07/soilNOx.climate.generic.05x05.nc    NON_ARID     2000/1/1/0        C xy unitless NO - 1 1``
| \ ``104 SOILNOX_SCALE   1.0 - 2000/1/1/0 C xy unitless * 333 1 1``\

SOILNOX\_SCALE is just a dummy scale factor with a global, uniform value
of 1.0. The actual temporal scaling over the US is done via scale factor
333 assigned to this field. This approach ensures that all soil NOx
emissions outside of the US remain intact.

The next step is to define scale factor Nr. 333 (named
\`SOILNOX\_SCALE\`) in the scale factor section of the configuration
file:

| ``# Scale factor to scale US soil NOx emissions by a factor of 0.5 in month June-August.``
| \ ``333 SOILNOX_SCALE 1.0/1.0/1.0/1.0/1.0/0.5/0.5/0.5/1.0/1.0/1.0/1.0 - 2000/1-12/1/0 - xy 1 1 5000``\

Scale factor SOILNOX\_SCALE defines a monthly varying scale factor, with
all scale factors being 1.0 except for months June-August, where the
scale factor becomes 0.5. The last column of the SOILNOX\_SCALE entry
assigns mask Nr. 5000 to this scale factor. This ensures that the scale
factor will only be applied over the region spanned by mask 5000. This
musk mast be defined in section masks of the HEMCO configuration file:

| ``1005 USA_MASK      $ROOT/MASKS/v2014-07/usa.mask.nei2005.geos.1x1.nc    MASK 2000/1/1/0 C xy unitless 1 -165/10/-40/90``
| \ ``5000 SOILNOX_MASK  -106.3/37.0/-93.8/49.0 - - - xy 1 1 -106.3/37.0/-93.8/49.0``\

In this example, mask 5000 is defined as the region between 106.3 - 93.8
degrees west and 37.0 - 49.0 degrees north. If you want to apply the
soil NOx scaling over the entire US, you can also just refer to the
existing USA mask, e.g.:

| ``# Scale factor to scale US soil NOx emissions by a factor of 0.5 in month June-August.``
| \ ``333 SOILNOX_SCALE 1.0/1.0/1.0/1.0/1.0/0.5/0.5/0.5/1.0/1.0/1.0/1.0 - 2000/1-12/1/0 - xy 1 1 1005``\

.. _cfg-ex-scl-nonemis:

Applying scale factors to non-emissions data
--------------------------------------------

***`Jenny Fisher <User:jaf>`__ wrote:***

    We have a question about applying HEMCO scale factors to fields that
    are in the non-emissions data section of the ``HEMCO_Config.rc``.

    My student Neil (cc’d) is trying to perform two experiments with the
    Hg simulation:

    #. Zero soil emissions
    #. 3x bromine emissions

    For #1, the soil emissions are calculated online in
    land\_mercury\_mod.F, but there is a step that multiplies whatever
    is calculated by the ``HG0_SOILDIST`` read from HEMCO, so it seemed
    like the cleanest way to zero these emissions would be to multiply
    that field by a scaling factor of zero in HEMCO:

| ``     (((SOILDIST``
| ``     0 HG0_SOILDIST $ROOT/MERCURY/v2014-09/SOIL/soilHg.presentday.v11-01.geosfp.4x5.nc HG0_DIST 1985/1/1/0 C xy unitless * 55 1 1``
| ``     )))SOILDIST``
| ``     . . . ``
| ``     # --- Scale factors for anthropogenic, artisinal, and soil emissions ---``
| ``     55 ZEROSOIL       0.0   - - - xy 1 1``

    However, that doesn’t seem to have had any impact. Are we doing
    something wrong here, or are scaling factors not applied to this
    dataset for some reason? Can you suggest a better way to do this?

    We tried to do something similar for #2 by setting a scaling factor
    of 3.0 for the BrOx\_GC fields, but this also didn’t appear to work.

***`Christoph Keller <User:Christoph_Keller>`__ wrote:***

    Please call ``HCO_EvalFld`` instead of ``HCO_GetPtr``, as follows
    (subroutine ``MERCURY_READYR`` in ``GeosCore/mercury_mod.F``, line
    3236):

    Old:

| ``        ``\ \ ``! Soil distribution``
| ``        CALL HCO_GetPtr( am_I_Root, HcoState, 'HG0_SOILDIST', Ptr2D, RC )``
| ``        IF ( RC /= HCO_SUCCESS ) THEN``
| ``           CALL ERROR_STOP ( 'Cannot get pointer to HG0_SOILDIST', LOC )``
| ``        ENDIF``
| ``        EHg0_dist =  Ptr2D(:,:)``
| ``        Ptr2D     => NULL()``\

    New:

| ``        ``\ \ ``USE HCO_CALC_MOD,          ONLY : HCO_EvalFld``
| ``        ...``
| ``        ! Soil distribution``
| ``        CALL HCO_EvalFld( am_I_Root, HcoState, 'HG0_SOILDIST', EHg0_dist, RC )``
| ``        IF ( RC /= HCO_SUCCESS ) THEN``
| ``           CALL ERROR_STOP ( 'Cannot evaluate HG0_SOILDIST', LOC )``
| ``        ENDIF``\

    Note that you don't pass the pointer to ``HCO_EvalFld`` but rather
    the allocated array. The difference between ``HCO_GetPtr`` and
    ``HCO_EvalFld`` is that ``HCO_GetPtr`` returns an array that points
    to the 'naked' HEMCO field associated with the given name, i.e. it
    ignores any scale factors. HCO\_EvalFld evaluates and applies the
    scale factors associated with the given field name.

    The content of the data pointer returned by ``HCO_GetPtr`` is
    dynamic, i.e. it can change during runtime if the underlying HEMCO
    field is updated, e.g. on an hourly basis. The field returned by
    ``HCO_EvalFld`` is static and does not change over time. Because of
    this, it's good practice to call HCO\_EvalFld on every time step -
    in case that the underlying field and/or scale factors change over
    time.

.. _cfg-ex-mask:

==================
Mask file examples
==================

Exercise care in defining mask regions
--------------------------------------

In an effort to reduce I/O HEMCO ignores any emission entries that are
deemed ¨irrelevant¨ because there is another (global) emission entry
for the same species and emission category, but higher hierarchy.

For instance, suppose you have the following two fields defined under
:ref:`Base Emissions <hco-cfg-base>`:

.. code-block:: kconfig

    0 TEST_1 file.nc var 2000/1/1/0 C xy 1 1 CO - 1 1
    0 TEST_2 file.nc var 2000/1/1/0 C xy 1 1 CO - 1 2

In this case, during initialization HEMCO determines that
:literal:`TEST_1` is obsolete because it will always be overwritten by
:literal:`TEST_2` (because of the higher hierarchy). But if there is a
mask assigned to an emission inventory, HEMCO uses the provided mask domain to determine whether this inventory has to be
treated as ¨global¨ or not.

Going back to the example above, let's add a mask to the second field:

.. code-block:: kconfig

   0 TEST_1 file.nc var 2000/1/1/0 C xy 1 1 CO -    1 1
   0 TEST_2 file.nc var 2000/1/1/0 C xy 1 1 CO 1000 1 2

and let´s define the following :ref:`mask <hco-cfg-masks>`:

.. code-block:: kconfig

   1000 TEST_MASK mask.nc var 2000/1/1/0 C xy 1 1 -180/180/-90/90

HEMCO uses the highlighted mask range (to define the extension of this
mask. If that range covers the entire HEMCO grid domain, it considers
every emission inventory linked with this mask as ¨global¨. In our
example, :literal:`TEST_2` would still be considered global because
the mask extends over the entire globe, and :literal:`TEST_1` is thus
ignored by HEMCO.

However, changing the mask domain to something smaller will tell HEMCO
that :literal:`TEST_2` is not global, and that it cannot drop
:literal:`TEST_1` because of that:

.. code-block:: kconfig

   1000 TEST_MASK mask.nc var 2000/1/1/0 C xy 1 1 -90/180/-45/45

Long story short: if you set the mask range to a domain that is
somewhat smaller than your simulation window, things work just
fine. But if you set the range to something bigger, HEMCO will start
ignoring emission files.

.. _cfg-ex-frac-mask:

Preserving fractional values when masking emissions
---------------------------------------------------

Question from a HEMCO user:

    I see that when the mask files are regridded they are remapped to
    0 or 1 via regular rounding. Unfortunately, this method will not
    work well for my application, because the region I am trying to zero out is a
    small region inside the 4x5 grid cell and thus the current mask will
    not change the emissions on a 4x5 scale.

    I was wondering whether it would be possible/straightforward to
    modify the mask regridding method such that 4x5 emissions scale will
    scale with the fraction of the gird cell that is masked (e.g., if a
    quarter of the grid cells in one of the 4x5 grid are masked, the
    emissions will scale down by 25%).

For this application, it may better to may be able to achieve what you
want by defining your mask file in the :ref:`Scale Factors
<hco-cfg-scalefac>` section of :ref:`the HEMCO configuration file <hco-cfg>`.

By defining a mask in the :ref:`hco-cfg-masks` section, HEMCO
identifies the data container type as MASK and treats the data as
binary.  Long story short:

.. code-block:: kconfig

   ###############################################################################
   ### BEGIN SECTION MASKS
   ###############################################################################

   If your mask file is currently defined here ...

   ### END SECTION MASKS ###

If you instead move that line to the SECTION SCALE FACTORS then HEMCO
will treat the mask as type SCAL. I believe that would preserve the
regridded value (in your example 0.25) and apply that to the emissions
in a 4x5 grid box.

.. code-block:: kconfig

   ###############################################################################
   ### BEGIN SECTION SCALE FACTORS
   ###############################################################################

   ... put your mask file here instead ...

   ### END SECTION SCALE FACTORS ###



Using masks to create emissions for geographically tagged tracers
-----------------------------------------------------------------

***`Doug Finch <http://dougfinch.co.uk/>`__ wrote:***

    I was planning on making a sort of 'tagged' CH4 run. I know the
    model already allows for tagging of emissions from different
    emissions types (wetland etc) but is there a way to do this
    geographically? I was planning on doing something similar to the
    tagged CO run but after a quick look at the code it looks like it
    won't be that simple. Do you have any advice for setting this up?

***`Bob Yantosca <http://people.seas.harvard.edu/~yantosca/>`__
replied:***

    For the tagged CH4 run, it should be pretty easy to set up. You can
    just use masks that are defined in the HEMCO config file. If the
    masks are square regions you can just define them in the HEMCO file
    like this, in the ``MASKS`` section:

| ``   #==============================================================================``
| ``   # Country/region masks``
| ``   #==============================================================================``
| ``   1001 MASK_1     -30/30/45/70   - 2000/1/1/0 C xy 1 1 -30/30/45/70``
| ``   1002 MASK_2     -118/17/-95/33 - 2000/1/1/0 C xy 1 1 -118/17/-95/33``
| ``   . . . etc. for other regions . . .``

    If the masks are not square regions, you can create a netCDF file
    with the various areas.

    And then in the ``BASE EMISSIONS SECTION``, for each line of
    emissions, you need to add extra lines for the tagged tracers so
    that the masks will get applied. For example, in the
    HEMCO\_Config.rc for CH4, you would add:

| ``   #==============================================================================``
| ``   # --- EDGAR v4.2 emissions, various sectors ---``
| ``   #==============================================================================``
| ``   (((EDGAR``
| ``   ### Gas and oil ###``
| ``   0 CH4_GAS__1B2a    $ROOT/CH4/v2014-09/v42_CH4.0.1x0.1.nc  ch4_1B2a  2004-2008/1/1/0 C xy kg/m2/s CH4   -    1 1``
| ``   0 CH4_GAS__1b2a_1  -                                      -         -               - -  -       CH4_1 1001 1 1``
| ``   0 CH4_GAS__1b2a_2  -                                      -         -               - -  -       CH4_2 1002 1 1``
| ``   ... etc ... ``
| ``   ### Coal mines ###``
| ``   CH4_COAL__1B1      $ROOT/CH4/v2014-09/v42_CH4.0.1x0.1.nc  ch4_1B1   2004-2008/1/1/0 C xy kg/m2/s CH4   -    2 1``
| ``   0 CH4_COAL__1B1_1  -                                      -         -               - -  -       CH4_1 1001 2 1``
| ``   0 CH4_COAL__1B1_2  -                                      -         -               - -  -       CH4_2 1002 2 1``
| ``   ... etc ...``

    This will put the total emissions into your CH4 tracer (tracer #1).
    It will then also apply the regional masks to the total emissions
    and then store them into tagged tracers CH4\_1 and CH4\_2 (or
    whatever you have them called). These tagged tracers CH4\_1, CH4\_2,
    etc. have to be defined in your ``input.geos`` and also in your
    tracer restart file.

    Long story short: all of the complexity of putting emissions into
    tagged tracers is now handled in the ``HEMCO_Config.rc`` input file
    instead of having to hardwire it in the code.

***`Christoph Keller <http://www.ckeller.ch/>`__ replied:***

    Regarding your HEMCO configuration file, we need to be careful about
    the extensions. Those are much more hardcoded than the “base”
    emissions and they will only calculate emissions for the species you
    attach to each extension (in section settings). For instance, your
    setting for ``CH4_WETLANDS`` is:

``   121     CH4_WETLANDS      : on    CH4``

    This means that the ``CH4_WETLANDS`` extension will only calculate
    emissions for ``CH4``, but not for any of the tagged tracers. If you
    wanted to calculate emissions for those, you need to list them all
    as extension species:

``   121     CH4_WETLANDS      : on    CH4/CH4_1/CH4_2/...    ``\ **``<—>``\ `` ``\ ``WON’T``\ `` ``\ ``WORK!!!``**

    However, as I said the extensions are more hardcoded (because they
    scale fields by temperature, etc.), so the line above won’t work at
    the moment because CH4\_WETLANDS expect only one species. In the
    future we may try to extend these extensions to accept tagged
    tracers

--`Bob Y. <User:Bmy>`__ (`talk <User_talk:Bmy>`__) 15:26, 11 June 2015
(UTC)


.. _cfg-ex-ext:

=========================
HEMCO extensions examples
=========================

.. _cfg-ex-ext-fix-megan:

Fixing emissions to a current year in MEGAN
-------------------------------------------

Question submitted by a user:

   Is it possible to fix :option:`MEGAN` emissions in one year? I know
   this works for many other emission inventories, but MEGAN emissions
   are dependent on environmental variables.

The best option may be may be to run the HEMCO standalone and save out
MEGAN emissions for the desired year.  Then use the :ref:`HEMCO diagnostic
output <hco-diag>` saved to netCDF as input for your :option:`MEGAN` emissions
instead of using the :option:`MEGAN` extension.

Follow these steps.

#.  Run the HEMCO standalone model. Make sure the following entries
    to your :file:`HEMCO_Diagn.rc` file:

    .. code-block:: kconfig

       EmisISOP_Biogenic  ISOP   108    -1  -1   2   kg/m2/s  ISOP_emissions_from_biogenic_sources
       EmisISOP_Biogenic  ISOP   108    -1  -1   2   kg/m2/s  ISOP_emissions_from_biogenic_sources
       EmisALD2_Biogenic  ALD2   108    -1  -1   2   kg/m2/s  ALD2_emissions_from_biogenic_sources
       ... etc for other MEGAN species ...

    In the above entries, :literal:`108` tells HEMCO to get the
    emissions from the :option:`MEGAN` extension (HEMCO extension
    #108) for the specfied species. You can change the units the
    emissions are saved out as -- typically we save the biogenic
    emissions out in atomsC/cm2/s, but for this purpose it may make
    sense to save out the emissions in HEMCO's preferred unit
    kg/m2/s.

#. Add the following lines in the :ref:`Settings <hco-cfg-settings>`
   section of :ref:`HEMCO_Config.rc <hco-cfg>` configuration file:

    .. code-block:: kconfig

       DiagnFile:                   HEMCO_Diagn.rc
       DiagnPrefix:                 HEMCO_diagnostics
       DiagnFreq:                   Monthly

   For more information, see the sections on :option:`DiagnFile`,
   :option:`DiagnPrefix`, :option:`DiagnFreq`.

#. Turn off the MEGAN extension in the :ref:`Extension Switches
   <hco-cfg-ext-switches>:

   .. code-block:: kconfig

      108     MEGAN                  : off   ISOP/ACET/PRPE/...etc additional species...

#. Add entries for reading the fixed MEGAN emission that were archived
   in Step 1 under :ref:`Base Emissions <hco-cfg-base>`.  For example:

   .. code-block:: kconfig

      0 MEGAN_ISOP [PATH_TO_FILE]/HEMCO_diagnostic.2016$MM010000.nc EmisISOP_Biogenic 2016/1-12/1/1/0 C xy kg/m2/s ISOP - 4 1

   .. note::

      HEMCO :option:`Cat` :literal:` = 4` is reserved for biogenic emissions.

#. Run HEMCO in either standalone mode, or coupled to an external
   model, dependingon your application.

.. _cfg-ex-ext-emit-2d-levels:

Add 2D emissions into specific levels
-------------------------------------

HEMCO can emit emissions into a layer other than the surface layer.
For example:

.. code-block:: kconfig

   0 EMEP_CO EMEP.nc CO 2000-2014/1-12/1/0 C xyL5 kg/m2/s CO 1/1001 1 2

will release the :literal:`EMEP_CO` into level 5 instead of
level 1. Theoretically, you could create a separate HEMCO entry for
every emission level (under :ref:`Base Emissions <hco-cfg-base>`:

.. code-block:: kconfig

   0 EMEP_CO_L1 EMEP.nc CO 2000-2014/1-12/1/0 C xyL1 kg/m2/s CO 1 150/1001 1 2
   0 EMEP_CO_L2 EMEP.nc CO 2000-2014/1-12/1/0 C xyL2 kg/m2/s CO 1 151/1001 1 2
   0 EMEP_CO_L3 EMEP.nc CO 2000-2014/1-12/1/0 C xyL3 kg/m2/s CO 1 152/1001 1 2

and assign :ref:`Scale Factors <hco-cfg-scalefac>` (e.g. 150, 151,
152) to specify the fraction of EMEP emissions to be added into each level:

.. code-block:: kconfig

   151 EMEP_LEV1_FRAC 0.5 - - - xy 1 1
   152 EMEP_LEV2_FRAC 0.1 - - - xy 1 1
   153 EMEP_LEV3_FRAC 0.1 - - - xy 1 1``

But this approach is somewhat cumbersome. Also, this won’t give you
the possibility to specifically emit a fraction above the PBL given
that the PBL height is variable over time.

Use this notation (under :ref:`Base Emissions <hco-cfg-base>`) to tell
HEMCO that you would like EMEP emissins to be added into levels 1 through 3:

.. code-block:: kconfig

   0 EMEP_CO_L1 EMEP.nc CO 2000-2014/1-12/1/0 C xyL=1:3 kg/m2/s CO 1 1001 1 2

The emissions are then spread across the lowest 3 model levels based
upon the model level thicknesses.

Instead of specifying the model levels, you may also specify the
altitude in meters or use :literal:`PBL` for the planetary boundary
layer:

.. code-block:: kconfig

   # Emit from surface up to 2500 meters
   0 EMEP_CO_L1 EMEP.nc CO 2000-2014/1-12/1/0 C xyL=1:2500m kg/m2/s C 1001 1 2

   # Emit between 1000 and 5000 meters altitude
   0 EMEP_CO_L1 EMEP.nc CO 2000-2014/1-12/1/0 C xyL=1000m:5000m kg/m2/s CO 1 1001 1 2

   # Emit between 5000 meters altitude and model level 17
   0 EMEP_CO_L1 EMEP.nc CO 2000-2014/1-12/1/0 C xyL=500m:17 kg/m2/s CO 1 1001 1 2

   # Emit from the surface to the PBL top
   0 EMEP_CO_L1 EMEP.nc CO 2000-2014/1-12/1/0 C xyL=1:PBL kg/m2/s CO 1 1001 1 2

HEMCO can also read the emission levvel from an external source
(e.g. netCDF file) that is listed as a scale factor.  This field can
then be referred to using its scale factor ID.  As an example, let's
assume daily varying emission heights for 2009-2010 are archived in
:file:`emis_heights.nc` as variable :literal:`emish` in units of
:literal:`m`. available for years 2009 to 2010). You can then define a
:ref:`Scale Factor <hco-cfg-scalefac>` such as:

.. code-block:: kconfig

   300 EMIT_HEIGHT emis_heights.nc emish 2009-2010/1-12/1-31/0 C xy m 1

and refer to this scale factor as the upper bound of the injection
height under :ref:`Base Emissions`:

.. code-block:: kconfig

   0 GFAS_CO GFAS_201606.nc cofire 2009-2010/1-12/1-31/0 C xyL=1:scal300 kg/m2/s CO - 5 3

It should be noted that HEMCO always regrids the fields to the model
grid before doing any data operations. If the emission height file is
very spotty and contains a lot of zeros the averaged injection heights
may be too low. In this case it may be required to set all zeros to
missing values (which are ignored by HEMCO) to achieve the desired result.

--`Melissa Sulprizio <User:Melissa_Payer>`__
(`talk <User_talk:Melissa_Payer>`__) 18:40, 1 March 2017 (UTC)

Vertically distributing emissions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In HEMCO 3.0.0, the capability to vertically allocate emissions has been
added. To achieve this, HEMCO first copies emissions to all levels when
dimensions ``xyL*`` is specified in ``HEMCO_Config.rc``. Scale factors
can then be applied to determine distribute the emissions vertically.

For example, in ``HEMCO_Config.rc`` we define a file ``vert_alloc.nc``
that defines the ratio of emissions to apply to each level for CEDS
energy, industry, and ship emissions.

| `` #==============================================================================``
| `` # --- CEDS vertical partitioning ---``
| `` #==============================================================================``
| `` (((CEDS``
| `` ``\ \ ``315``\ \ `` ENERGY_LEVS   vert_alloc.nc g_energy   2017/1/1/0 C xyz 1 1``
| `` ``\ \ ``316``\ \ `` INDUSTRY_LEVS vert_alloc.nc g_industry 2017/1/1/0 C xyz 1 1``
| `` ``\ \ ``317``\ \ `` SHIP_LEVS     vert_alloc.nc cmv_c3     2017/1/1/0 C xyz 1 1``
| `` )))CEDS``

These scale factors are then applied to the ``CEDS_*_ENE``,
``CEDS_*_IND``, and ``CEDS_*_SHIP`` fields. These fields are 2D in the
CEDS files, but we now can specify dimension ``xyL*`` instead of ``xy``
to tell HEMCO to copy the field into each emissions level:

| `` 0 CEDS_CO_ENE $ROOT/CEDS/v2020-08/$YYYY/CO-em-total-anthro_CEDS_$YYYY.nc  CO_ene  1970-2017/1-12/1/0 C ``\ \ ``xyL*``\ \ `` kg/m2/s CO 26/37/35/``\ \ ``315``\ \ `` 1  5``
| `` 0 CEDS_CO_IND $ROOT/CEDS/v2020-08/$YYYY/CO-em-total-anthro_CEDS_$YYYY.nc  CO_ind  1970-2017/1-12/1/0 C ``\ \ ``xyL*``\ \ `` kg/m2/s CO 26/``\ \ ``316``\ \ ``       1  5``
| `` 0 CEDS_CO_SHP $ROOT/CEDS/v2020-08/$YYYY/CO-em-total-anthro_CEDS_$YYYY.nc  CO_shp  1970-2017/1-12/1/0 C ``\ \ ``xyL*``\ \ `` kg/m2/s CO 26/``\ \ ``317``\ \ ``       10 5``

--`Melissa Sulprizio <User:Melissa_Payer>`__
(`talk <User_talk:Melissa_Payer>`__) 19:33, 2 August 2021 (UTC)

Other examples
--------------

Passive species
~~~~~~~~~~~~~~~

The passive species module allows you to run a suite of passive species
alongside any simulation, i.e. it works with all simulation types. To
use the passive species follow these steps:

**1.** Include passive species in the advected species menu in
``input.geos``. For example, to add passive species to the
TransportTracers (formerly Rn-Pb-Be) simulation:

| ``%%% ADVECTED SPECIES MENU %%%:``
| ``Type of simulation      : 1``
| ``Species Entries ------->: Name``
| ``Species name            : Rn222``
| ``Species name            : Pb210``
| ``... etc ...``
| ``Species name            : PASV1``
| ``Species name            : PASV2``
| ``Species name            : PASV3``
| ``... etc ...``

**2**. Create a `PASSIVE SPECIES
MENU <GEOS-Chem_Input_Files#Passive_Species_Menu>`__ in ``input.geos``
where you define the basic properties of the passive species. The
passive species menu can be added anywhere in ``input.geos``.

For each passive species you must define the following quantities (each
of which is color-coded for reference below):

+---------------------------------------------+----------------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Quantity                                    | Units                | Description                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
+=============================================+======================+======================================================================================================================================================================================================================================================================================================================================================================================================================================================================+
| Name                                        | -                    | Name of the species. Each species name listed in the `PASSIVE SPECIES MENU <GEOS-Chem_Input_Files#PASSIVE_SPECIES_MENU>`__ must exactly match the corresponding species name in the `ADVECTED SPECIES MENU <GEOS-Chem_Input_Files#ADVECTED_SPECIES_MENU>`__. If the two menus aren't consistent, you will get a species database error.                                                                                                                              |
+---------------------------------------------+----------------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Molecular weight                            | g mol\ :sup:`-1`     | Molecular weight of the species                                                                                                                                                                                                                                                                                                                                                                                                                                      |
+---------------------------------------------+----------------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Atmospheric lifetime                        | s                    | Atmospheric lifetime of the species. A value of -1 denotes that the species never decays (i.e. it has an infinite lifetime).                                                                                                                                                                                                                                                                                                                                         |
+---------------------------------------------+----------------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Default initial atmospheric concentration   | mol mol\ :sup:`-1`   | The default initial background concentration. GEOS-Chem will use this value if the passive species concentration cannot be found in the GEOS-Chem restart file. If the passive species is included in the restart file, GEOS-Chem will use the concentrations from the restart file instead of the default initial concentrations. The passive species concentration field is automatically written to restart files created at the end of a GEOS-Chem simulation.   |
|                                             |                      |                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|                                             |                      | In other words, the default initial concentrations set in input.geos are only used to “coldstart” a passive species.                                                                                                                                                                                                                                                                                                                                                 |
+---------------------------------------------+----------------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Species long name                           | -                    | An optional string for the long name of the species. This will be used to define the ``long_name`` variable attribute when archiving the species to netCDF diagnostic output.                                                                                                                                                                                                                                                                                        |
|                                             |                      |                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|                                             |                      | NOTE: This field is only available in `GEOS-Chem 12.2.0 <GEOS-Chem_12#12.2.0>`__ and later versions.                                                                                                                                                                                                                                                                                                                                                                 |
+---------------------------------------------+----------------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+

The following PASSIVE SPECIES MENU example (for `GEOS-Chem
12.2.0 <GEOS-Chem_12#12.2.0>`__ and later versions) defines 3 species
with the following properties:

#. MW = 50 g/mol; atmospheric lifetime = 1 hour; default initial
   concentration = 1 ppt
#. MW = 250 g/mol; atmospheric lifetime = 3.8 days; default initial
   concentration = 1 ppb
#. Same as species #2, but with an infinite lifetime (never decays)

| ``------------------------+------------------------------------------------------``
| ``%%% PASSIVE SPECIES MENU %%%:``
| ``Passive species #1      : ``\ \ ``PASV1``\ \ `` ``\ \ ``50.0``\ \ ``  ``\ \ ``3600.0``\ \ ``   ``\ \ ``1.0e-12``\ \ `` ``\ \ ``Passive_Species_1``\
| ``Passive species #2      : ``\ \ ``PASV2``\ \ `` ``\ \ ``250.0``\ \ `` ``\ \ ``328320.0``\ \ `` ``\ \ ``1.0e-09``\ \ `` ``\ \ ``Passive_Species_2``\
| ``Passive species #3      : ``\ \ ``PASV3``\ \ `` ``\ \ ``250.0``\ \ `` ``\ \ ``-1``\ \ ``       ``\ \ ``1.0e-09``\ \ `` ``\ \ ``Passive_Species_3``\
| ``... etc ...``

***NOTE:*** If you are using a GEOS-Chem version prior to `GEOS-Chem
12.2.0 <GEOS-Chem_12#12.2.0>`__, then you must also specify the number
of passive species as the first line of the PASSIVE SPECIES MENU (e.g.
``Number of passive spec. : 3``). This value was formerly stored in
``Input_Opt%N_PASSIVE``. Also note that the optional species long name
field is not present. Thus for versions prior to 12.2.0, you would set
up the PASSIVE SPECIES MENU as follows:

| ``------------------------+------------------------------------------------------``
| ``%%% PASSIVE SPECIES MENU %%%:``
| ``Number of passive spec. : 3``
| ``Passive species #1      : ``\ \ ``PASV1``\ \ `` ``\ \ ``50.0``\ \ ``  ``\ \ ``3600.0``\ \ ``   ``\ \ ``1.0e-12``\
| ``Passive species #2      : ``\ \ ``PASV2``\ \ `` ``\ \ ``250.0``\ \ `` ``\ \ ``328320.0``\ \ `` ``\ \ ``1.0e-09``\
| ``Passive species #3      : ``\ \ ``PASV3``\ \ `` ``\ \ ``250.0``\ \ `` ``\ \ ``-1``\ \ ``       ``\ \ ``1.0e-09``\
| ``... etc ...``

**3.** Assign emissions to passive species in ``HEMCO_Config.rc``. You
can omit this step in which case there will be no sources for your
passive species. For instance, to define a uniform flux of 1e-3 and 1e-9
kg/m2/s for ``PASV1`` and ``PASV2``, respectively, add the following two
lines to the base emissions section (in ``HEMCO_Config.rc``):

| ``     0 PASV1_Flux 1.0e-3  - - - xy kg/m2/s PASV1 - 1 1``
| ``     0 PASV2_Flux 1.0e-9  - - - xy kg/m2/s PASV2 - 1 1``
| ``     ... etc ...``

    ***NOTE:*** If you would like to define passive species that are
    geographically tagged, then make sure to supply the number of the
    corresponding mask(s) in the third-to-last column. For example:

| ``     0 PASV1_Flux 1.0e-3  - - - xy kg/m2/s PASV1 ``\ \ ``1000``\ \ `` 1 1``
| ``     0 PASV2_Flux 1.0e-9  - - - xy kg/m2/s PASV2 ``\ \ ``1001``\ \ `` 1 1``
| ``     ... etc ...``

    Here, 1000 and 1001 refer to mask definitions in the `MASKS section
    of the HEMCO configuration file <The_HEMCO_User's_Guide#Masks>`__.
    `Please also see the following
    section <#Using_masks_to_create_emissions_for_geographically_tagged_tracers>`__.

**4.** Request HEMCO diagnostic output. There is no default diagnostics
for these emissions but you can easily define a HEMCO diagnostics for
each passive species by creating a corresponding entry in the HEMCO
diagnostics file (``HEMCO_diagn.rc``):

| ``# Name       Spec  ExtNr Cat Hier Dim Unit``
| ``PASV1_TOTAL  PASV1 -1    -1  -1   2   kg/m2/s``
| ``PASV2_TOTAL  PASV2 -1    -1  -1   2   kg/m2/s``
| ``PASV3_TOTAL  PASV3 -1    -1  -1   2   kg/m2/s``

To activate these diagnostics you need to link to the HEMCO diagnostics
file in ``HEMCO_Config.rc``, as well as define the diagnostics output
frequency (in the settings section of the HEMCO configuration file). For
example to write the diagnostics at 30-minute intervals:

| ``DiagnFile:                 HEMCO_Diagn.rc``
| ``DiagnFreq:                 00000000 003000``

--`Bob Yantosca <User:Bmy>`__ (`talk <User_talk:Bmy>`__) 17:56, 27
February 2019 (UTC)

Using the HEMCO built-in diagnostics framework
----------------------------------------------

Overview
~~~~~~~~

The `HEMCO diagnostics framework <The_HEMCO_User's_Guide#Diagnostics>`__
lets user define customized diagnostics. All diagnostic definitions need
be provided in an external diagnostics definition file, typically called
``DiagnFile.rc``. The diagnostics file name is defined in the settings
section of the HEMCO configuration file, together with the diagnostics
file name and the diagnostics output frequency. For instance, the
following entry tells HEMCO to read diagnostics definitions from file
``MyDiagnFile.rc`` and write hourly diagnostics into files
``Diagnostics.YYYYMMDDhhmmss.nc`` (the date will be appended to the
diagnostics prefix by HEMCO:

| ``      DiagnFile            : MyDiagnFile.rc``
| ``      DiagnPrefix          : Diagnostics``
| ``      DiagnFreq            : Hourly``

All emissions to be written into the diagnostics can now be defined in
file MyDiagnFile.rc. For example, the following entries would tell HEMCO
to write out total vertical integrated NO emissions (into field
``NO_TOTAL``), 3D field of lightning NO emissions (extension Nr 103,
into field ``NO_LIGHTNING``), 2D field of soil NO emissions in kg per
grid box (extension 104, into ``NO_SOIL``), total anthropogenic NO
emissions (emission category 1, into ``NO_ANTHRO``), and anthropogenic
NO emissions from NEI11 (category 1, hierarchy 50, into ``NO_NEI11``):

| ``# Name       Spec ExtNr Cat Hier Dim OutUnit``
| ``NO_TOTAL     NO   -1    -1  -1   2   kg/m2/s``
| ``NO_LIGHTNING NO   103   -1  -1   3   kg/m2/s``
| ``NO_SOIL      NO   104   -1  -1   2   kg``
| ``NO_ANTHRO    NO   0      1  -1   2   kg/m2/s``
| ``NO_NEI11     NO   0      1  50   2   kg/m2/s``

Compute emission totals
~~~~~~~~~~~~~~~~~~~~~~~

Create a ``DiagnFile.rc`` with the emission categories or inventories
for which you want to compute totals. For example, to compare total NO
emissions from different anthropogenic inventories:

| ``# Name          Spec ExtNr Cat Hier Dim OutUnit``
| ``EDGAR_NO        NO   0      1  2    2   kg``
| ``EMEP_NO         NO   0      1  10   2   kg``
| ``BRAVO_NO        NO   0      1  20   2   kg``
| ``CAC_NO          NO   0      1  30   2   kg``
| ``NEI11_NO        NO   0      1  50   2   kg``
| ``MIX_NO          NO   0      1  45   2   kg``

Make sure your HEMCO configuration points to ``DiagnFile.rc``:

| ``DiagnPrefix: HEMCO_Diagnostics``
| ``DiagnFreq: Monthly``
| ``DiagnFile: DiagnFile.rc``

If you run for one month, it makes sense to set the diagnostics output
frequency (``DiagnFreq``) to ``Monthly``, then you will get the monthly
mean value. You can set the output unit to ``kg`` instead of ``kg/m2/s``
to get the total flux per grid box.

Once you have the diagnostics, you can easily calculate the emission
totals using CDO, NCO, NCL, etc. For example, if you write out the
fluxes in kg instead of kg/m2/s, it’s as easy as:

``cdo fldsum HEMCO_Diagnostics.201308010000.nc totals.nc``

And then

| ``% ncdump totals.nc ``
| ``  netcdf totals {``
| ``  dimensions:``
| ``       lon = 1 ;``
| ``       lat = 1 ;``
| ``       time = UNLIMITED ; // (1 currently)``
| `` ...``
| `` MIX_NO =``
| ``  2.827249e+09 ;``
| `` NEI11_NO =``
| ``  8.113327e+08 ;``
| `` CAC_NO =``
| ``  7.514479e+07 ;``
| `` BRAVO_NO =``
| ``  5.152818e+07 ;``
| `` EMEP_NO =``
| ``  9.123655e+08 ``
| `` EDGAR_NO =``
| ``  4.196484e+09 ;``
| ``}``

--`Melissa Sulprizio <User:Melissa_Payer>`__
(`talk <User_talk:Melissa_Payer>`__) 16:41, 24 May 2017 (UTC)

Use of mathematical expressions
-------------------------------

HEMCO v2.1.001 supports the use of mathematical expressions in the HEMCO
configuration file. Similar to uniform values, these are placed in
column 'sourceFile'. All expressions are evaluated during run-time. They
can be used e.g. to model an oscillating emission source. All
mathematical expressions must contain at least one time-dependent
variable that is evaluated on-the-fly. Mathematical expressions are
specified by using the prefix ``MATH:``, followed by the mathematical
expression. The expression is a combination of variables, mathematical
operations, and constants, e.g. 5.0+2.5\*sin(HH).

Supported variables & operators
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following variable names and mathematical operations are currently
supported:

**Variable names**: YYYY (current year), MM (month), DD (day), HH
(hour), NN (minute), SS (second), DOY (day of year), DOM (# of days of
current month), WD (weekday: 0=Sun, 1=Mon, ..., 6=Sat), LWD (weekday in
local time), LH (hour in local time), PI (number PI).

**Basic mathematical operators**: + - \* / ^ ( )

**Advanced mathematical functions**: sin, cos, tan, asin, acos, atan,
sinh, cosh, tanh, sind, cosd, tand, log, log10, nint, anint, aint, exp,
sqrt, abs, floor. The names refer to the equivalent Fortran functions.

Examples
~~~~~~~~

1. To define a sine-wave emission source of NO with an oscillation
frequency of 24 hours, add the following line to section 'Base
Emissions' in ``HEMCO_Config.rc``:

| ``# ExtNr Name sourceFile sourceVar sourceTime C/R/E SrcDim SrcUnit Species ScalIDs Cat Hier``
| ``0 SINE_NO ``\ **``MATH:sin(HH/12*PI)``**\ `` - * C xy kg/m2/s NO - 1 500``

We assign an emission category of 1 and a hierarchy of 500 to this
field. No scale factors are applied. This emission source will produce
negative emissions, which may cause an HEMCO error (depending on your
settings, see remarks below). To avoid this, add an offset of 2.0:

``0 SINE_NO ``\ **``MATH:2.0+sin(HH/12*PI)``**\ `` - * C xy kg/m2/s NO - 1 500``

Remarks
~~~~~~~

1. For mathematical expressions it is recommended to set the sourceTime
attribute to '\*', in particular if the mathematical expression uses
some of the short-term variables (HH, NN, SS, LH). This ensures that the
expression is evaluated on every emission time step.

2. Mathematical expressions can produce negative emissions, which by
default cause HEMCO to stop with an error. Negative emissions can be
enabled by setting ``Negative values`` to 2 in the HEMCO configuration
file.
