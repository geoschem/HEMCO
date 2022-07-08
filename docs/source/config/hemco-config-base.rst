.. _hemco-cfg-base:

##############
Base emissions
##############

The base emission section lists all base emission fields and how they
are linked to scale factors. For each base emissions, the following
attributes need to be defined:

.. option:: ExtNr

   Extension number associated with this field. All base emissions
   should have extension number zero.  The :literal:`ExtNr` of the
   data listed in section Extensions data must match with the
   corresponding extension number.

   The extension number can be set to the wildcard character. In that
   case, the field is read by  HEMCO (if the assigned species name
   matches any of the HEMCO species, see 'Species' below) but not used
   for emission calculation. This is particularly useful if HEMCO is
   only used for data I/O but not for emission calculation.             

.. option:: Name

 Descriptive field identification name. Two         
 consecutive underscore characters (``__``) can be  
 used to attach a 'tag' to a name. This is only of  
 relevance if multiple base emission fields share   
 the same species, category, hierarchy, and scale   
 factors. In this case, emission calculation can be 
 optimized by assigning field names that only       
 differ by its tag to those fields (e.g.            
 :literal:`DATA__SECTOR1`, :literal:`DATA__SECTOR2`, etc.).
                                                    
 For fields assigned to extensions other than the   
 base extension (ExtNr = 0), the field names are    
 prescribed and must not be modified because the    
 data is identified by these extensions by name.    
+----------------+----------------------------------------------------+
 ``sourceFile``  Path and name of the input file. See `the Input    
                 File Format section <#Input_file_format>`__ for    
                 more details on the input file format              
                 requirements.                                      
                                                                    
                 Name tokens can be provided that become evaluated  
                 during runtime. For example, to use the root       
                 directory specified in the settings (see `the      
                 HEMCO Settings section <#HEMCO_Settings>`__), the  
                 token ``$ROOT`` can be used. Similarly, the token  
                 ``$CFDIR`` refers to the location of the           
                 configuration file. This allows users to reference 
                 data relative to the location of the configuration 
                 file. For instance, if the data is located in      
                 subfolder ``data`` of the same directory as the    
                 configuration file, the file name can be set to    
                 ``$CFDIR/data/filename.nc``.                       
                                                                    
                 Similarly, the **date tokens** ``$YYYY``, ``$MM``, 
                 ``$DD``, ``$HH``, and ``$MN`` can be used to refer 
                 to the current valid year, month, day, hour, and   
                 minute, respectively. These values are determined  
                 from the current simulation datetime and the       
                 ``SrcTime`` specification for this entry (see      
                 below). The tokens ``$MODEL`` and ``$RES`` refer   
                 to the meteorological model and resolution. These  
                 tokens can be set explicitly in the settings       
                 section. In GEOS-Chem, they are set to             
                 compiler-flag specific values if not set in the    
                 settings section. As of v1.1.010, any token        
                 defined in the settings section can be used to     
                 construct a part of the file name (see `the        
                 User-defined token                                 
                 section <#User-defined_token>`__).                 
                                                                    
                 As an alternative to an input file, **geospatial   
                 uniform values** can directly be specified in the  
                 configuration file (see e.g. scale factor          
                 ``SO2toSO4`` in `Getting                           
                 Started <#Getting_Started>`__). If multiple values 
                 are provided (separated by the separator           
                 character), they are interpreted as different time 
                 slices. In this case, the ``sourceTime`` attribute 
                 can be used to specify the times associated with   
                 the individual slices. If no time attribute is     
                 set, HEMCO attempts to determine the time slices   
                 from the number of data values: 7 values are       
                 interpreted as weekday (Sun, Mon, ..., Sat); 12    
                 values as month (Jan, ..., Dec); 24 values as      
                 hour-of-day (12am, 1am, ..., 11pm).                
                                                                    
                 As of v2.1.001 uniform values can be combined with 
                 **mathematical expressions**, e.g. to model a      
                 sine-wave emission source. Mathematical            
                 expressions must be labeled ``MATH:``, followed by 
                 the expression, e.g. 'MATH:2.0+sin(HH/12*PI)'. See 
                 `our HEMCO examples wiki page <HEMCO_examples>`__  
                 for more information and examples.                 
                                                                    
                 **Country-specific data** can be provided through  
                 an ASCII file (.txt). More details on this option  
                 are given in `the Input File Format                
                 section <#Input_file_format>`__.                   
                                                                    
                 If this entry is **left empty** (``-``), the       
                 filename from the preceding entry is taken, and    
                 the next 5 attributes will be ignored (see entry   
                 ``MACCITY_SO4`` in `Getting                        
                 Started <#Getting_Started>`__).                    
+----------------+----------------------------------------------------+
 ``sourceVar``   Source file variable of interest. Leave empty      
                 (``-``) if values are directly set through the     
                 ``sourceFile`` attribute or if ``sourceFile`` is   
                 empty.                                             
+----------------+----------------------------------------------------+
 ``sourceTime``  This attribute defines the time slices to be used  
                 and the data refresh frequency. The format is      
                 year/month/day/hour. Accepted are discrete dates   
                 for time-independent data (e.g. ``2000/1/1/0``)    
                 and time ranges for temporally changing fields     
                 (e.g. ``1980-2007/1-12/1-31/0-23``). Data will     
                 automatically become updated as soon as the        
                 simulation date enters a new time interval.        
                                                                    
                 The provided time attribute determines the data    
                 refresh frequency. It does not need to correspond  
                 to the datetimes of the input file. For example,   
                 if the input file contains daily data of year 2005 
                 and the time attribute is set to ``2005/1/1/0``,   
                 the file will be read just once (at the beginning  
                 of the simulation) and the data of Jan 1, 2005 is  
                 used throughout the simulation. If the time        
                 attribute is set to ``2005/1-12/1/0``, the data is 
                 updated on every month, using the first day data   
                 of the given month. For instance, if the           
                 simulation starts on July 15, the data of July 1,  
                 2005 are used until August 1, at which point the   
                 data will be refreshed to values from August 1,    
                 2005. Only a time attribute of                     
                 ``2005/1-12/1-31/0`` will make sure that the input 
                 data are refreshed daily to the current day's      
                 data. Finally, if the time attribute is set to     
                 ``2005/1-12/1-31/0-23``, the data file is read     
                 every simulation hour, but the same daily data is  
                 used throughout the day (since there are no hourly 
                 data in the file). Providing too high update       
                 frequencies is not recommended unless the data     
                 interpolation option is enabled (see below).       
                                                                    
                 If the provided time attributes do not match a     
                 datetime of the input file, the 'most likely' time 
                 slice is selected. The most likely time slice is   
                 determined based on the specified source time      
                 attribute, the datetimes available in the input    
                 file, and the current simulation date. In most     
                 cases, this is just the closest available time     
                 slice that lies in the past. For example, if a     
                 file contains annual data from 2005 to 2010 and    
                 the source time attribute is set to                
                 ``2005-2010/1-12/1/0``, the data of 2005 is used   
                 for all simulation months in 2005. More complex    
                 datetime selections occur for files with           
                 discontinuous time slices, e.g. a file with        
                 monthly data for year 2005, 2010, 2020, and 2050.  
                 In this case, if the time attribute is set to      
                 ``2005-2020/1-12/1/0``, the monthly values of 2005 
                 are (re-)used for all years between 2005 and 2010, 
                 the monthly values of 2010 are used for simulation 
                 years 2010 - 2020, etc.                            
                                                                    
                 It is possible to use tokens ``$YYYY``, ``$MM``,   
                 ``$DD``, and ``$HH``, which will automatically be  
                 replaced by the current simulation date. Weekly    
                 data (e.g. data changing by the day of the week)   
                 can be indicated by setting the day attribute to   
                 ``WD`` (the wildcard character will work, too, but 
                 is not recommended). Weekly data needs to consist  
                 of at least seven time slices - in increments of   
                 one day - representing data for every weekday      
                 starting on Sunday. It is possible to store        
                 multiple weekly data, e.g. for every month of a    
                 year: ``2000/1-12/WD/0``. These data must contain  
                 time slices for the first seven days of every      
                 month, with the first day per month representing   
                 Sunday data, then followed by Monday, etc.         
                 (irrespective of the real weekdays of the given    
                 month). If the wildcard character is used for the  
                 days, the data will be interpreted if (and only    
                 if) there are exactly seven time slices. See `the  
                 Input File Format section <#Input_file_format>`__  
                 for more details. Default behavior is to interpret 
                 weekly data as 'local time', i.e. token ``WD``     
                 assumes that the provided values are in local      
                 time. As of HEMCO v2.0.005, it is possible to use  
                 weekly data referenced to UTC time using token     
                 ``UTCWD``.                                         
                                                                    
                 Similar to the weekday option, there is an option  
                 to indicate hourly data that represents local      
                 time: ``LH``. If using this flag, all hourly data  
                 of a given time interval (day, month, year) are    
                 read into memory and the local hour is picked at   
                 every location. A downside of this is that all     
                 hourly time slices in memory are updated based on  
                 UTC time. For instance, if a file holds local      
                 hourly data for every day of the year, the source  
                 time attribute can be set to                       
                 ``2011/1-12/1-31/LH``. On every new day (according 
                 to UTC time), this will read all 24 hourly time    
                 slices of that UTC day and use those hourly data   
                 for the next 24 hours. For the US, for instance,   
                 this results in the wrong daily data being used    
                 for the last 6-9 hours of the day (when UTC time   
                 is one day ahead of local US time).                
                                                                    
                 There is a difference between source time          
                 attributes ``2005-2008/$MM/1/0`` and               
                 ``2005-2008/1-12/1/0``. In the first case, the     
                 file will be updated annually, while the update    
                 frequency is monthly in the second case. The token 
                 ``$MM`` simply indicates that the current          
                 simulation month shall be used whenever the file   
                 is updated, but it doesn’t imply a refresh         
                 interval. Thus, if the source time attribute is    
                 set to ``$YYYY/$MM/$DD/$HH``, the file will be     
                 read only once and the data of the simulation      
                 start date is taken (and used throughout the       
                 simulation). For uniform values directly set in    
                 the configuration file, all time attributes but    
                 one must be fixed, e.g. valid entries are          
                 ``1990-2007/1/1/0`` or ``2000/1-12/1/1``, but not  
                 ``1990-2007/1-12/1/1``.                            
                                                                    
                 All data read from netCDF file are assumed to be   
                 in UTC time, except for weekday data that are      
                 always assumed to be in local time. Data read from 
                 the configuration file and/or from ASCII are       
                 always assumed to be in local time.                
                                                                    
                 It is legal to keep different time slices in       
                 different files, e.g. monthly data of multiple     
                 years can be stored in files file_200501.nc,       
                 file_200502.nc, ..., file_200712.nc. By setting    
                 the source file attribute to ``file_$YYYY$MM.nc``  
                 and the source time attribute to                   
                 ``2005-2007/1-12/1/0``, data of file_200501.nc is  
                 used for simulation dates of January 2005 (or any  
                 January of a previous year), etc. The individual   
                 files can also contain only a subset of the        
                 provided data range, e.g. all monthly files of a   
                 year can be stored in one file: file_2005.nc,      
                 file_2006.nc, file_2007.nc. In this case, the      
                 source file name should be set to ``file_$YYYY``,  
                 but the source time attribute should still be      
                 ``2005-2007/1-12/1/0`` to indicate that the field  
                 shall be updated monthly.                          
                                                                    
                 This attribute can be set to the wildcard          
                 character (``*``), which will force the file to be 
                 updated on every HEMCO time step.                  
                                                                    
                 As of HEMCO version 2 the file reference time can  
                 be shifted by a fixed amount by adding an optional 
                 fifth element to the time stamp attribute. For     
                 instance, consider the case where 3-hourly         
                 averages are provided in individual files with     
                 centered time stamps, e.g.:                        
                 file.yyyymmdd_0130z.nc, file.yyyymmdd_0430z.nc,    
                 ..., file.yyymmdd_2230z.nc. To read these files    
                 ``at the beginning`` of their time intervals, the  
                 time stamp can be shifted by 90 minutes:           
                 ``2000-2016/1-12/1-31/0-23/``\ **``+90minutes``**. 
                 At time 00z, HEMCO will then read file 0130z and   
                 keep using this file until 03z, when it switches   
                 to file 0430z. Similarly, it is possible to shift  
                 the file reference time by any number of years,    
                 months, days, or hours. Time shifts can be forward 
                 or backward in time (use - sign to shift           
                 backwards).                                        
+----------------+----------------------------------------------------+
 ``CRE``         Controls the time slice selection if the           
                 simulation date is outside the range provided in   
                 attribute source time (see above). The following   
                 options are available:                             
                                                                    
                 -  **``C``** (cycling): Data are interpreted as    
                    climatology and recycled once the end of the    
                    last time slice is reached. For instance, if    
                    the input data contains monthly data of year    
                    2000, and the source time attribute is set to   
                    ``2000/1-12/1/0 C``, the same monthly data will 
                    be re-used every year. If the input data spans  
                    multiple years (e.g. monthly data from          
                    2000-2003), the closest available year will be  
                    used outside of the available range (e.g. the   
                    monthly data of 2003 is used for all simulation 
                    years after 2003).                              
                 -  **``CS``** (cycling, skip): Data are            
                    interpreted as climatology and recycled once    
                    the end of the last time slice is reached. Data 
                    that aren't found are skipped. This is useful   
                    when certain fields aren't found in a restart   
                    file and, in that case, those fields will be    
                    initialized to default values.                  
                    **(**\ `GEOS-Chem                               
                    12.1.0 <GEOS-Chem_12#12.1.0>`__\ **and          
                    higher)**                                       
                 -  **``CY``** (cycling, use simulation year): Same 
                    as ``C``, except don't allow ``Emission year``  
                    setting to override year value.                 
                 -  **``CYS``** (cycling, skip, use simulation      
                    year): Same as ``CS``, except don't allow       
                    ``Emission year`` setting to override year      
                    value.                                          
                 -  **``R``** (range): Data are only considered as  
                    long as the simulation time is within the time  
                    range specified in attribute ``sourceTime``.    
                    The provided range does not necessarily need to 
                    match the time stamps of the input file. If it  
                    is outside of the range of the netCDF time      
                    stamps, the closest available date will be      
                    used. For instance, if a file contains data for 
                    years 2003 to 2010 and the provided range is    
                    set to ``2006-2010/1/1/0 R``, the file will     
                    only be considered between simulation years     
                    2006-2010. For simulation years 2006 through    
                    2009, the corresponding field on the file is    
                    used. For all years beyond 2009, data of year   
                    2010 is used. If the simulation date is outside 
                    the provided time range, the data is ignored    
                    but HEMCO does not return an error - the field  
                    is simply treated as empty (a corresponding     
                    warning is issued in the HEMCO log file). For   
                    example, if the source time attribute is set to 
                    ``2000-2002/1-12/1/0 R``, the data will be used 
                    for simulation years 2000 to 2002 and ignored   
                    for all other years.                            
                 -  **``RA``** (range, averaging otherwise):        
                    combination of flags ``R`` and ``A``. As long   
                    as the simulation year is within the specified  
                    year range, HEMCO will use just the data from   
                    that particular year. As soon as the simulation 
                    year is outside the specified year range, HEMCO 
                    will use the data averaged over the specified   
                    years. For example, consider the case where the 
                    emission file contains monthly data for years   
                    2005-2010. Setting the time attribute to        
                    ``2005-2010/1-12/1/0 R`` will ensure that this  
                    data is only used within simulation years 2005  
                    to 2010 and ignored outside of it. When setting 
                    the time attribute to ``2005-2010/1-12/1/0 A``, 
                    HEMCO will always use the 2005-2010 averaged    
                    monthly values, even for simulation years 2005  
                    to 2010. A time attribute of                    
                    ``2005-2010/1-12/1/0 RA`` will make sure that   
                    HEMCO uses the monthly data of the current year 
                    if the simulation year is between 2005 and      
                    2010, and the 2005-2010 average for simulation  
                    years before and after 2005 and 2010,           
                    respectively. **(v1.1.010 and higher)**         
                 -  **``RF``** (range, forced): same as ``R``, but  
                    HEMCO stops with an error if the simulation     
                    date is outside the provided range. **(v1.1.011 
                    and higher)**                                   
                 -  **``RY``** (range, use simulation year): Same   
                    as ``R``, except don't allow ``Emission year``  
                    setting to override year value.                 
                 -  **``E``** (exact): Fields are only used if the  
                    time stamp on the field exactly matches the     
                    current simulation datetime. In all other       
                    cases, data is ignored but HEMCO does not       
                    return an error. For example, if the source     
                    time attribute is set to                        
                    ``2000-2013/1-12/1-31/0 E``, every time the     
                    simulation enters a new day HEMCO will attempt  
                    to find a data field for the current simulation 
                    date. If no such field can be found on the      
                    file, the data is ignored (and a warning is     
                    prompted). This setting is particularly useful  
                    for data that is highly sensitive to date and   
                    time, e.g. restart variables.                   
                 -  **``EF``** (exact, forced): same as ``E``, but  
                    HEMCO stops with an error if no data field can  
                    be found for the current simulation date and    
                    time. **(v1.1.011 and higher)**                 
                 -  **``EC``** (exact, read/query continuously)     
                 -  **``ECF``** (exact, forced, read/query          
                    continuously)                                   
                 -  **``EY``** (exact, use simulation year): Same   
                    as ``E``, except don't allow ``Emission year``  
                    setting to override year value.                 
                 -  **``A``** (averaging): tells HEMCO to average   
                    the data over the specified range of years. For 
                    instance, setting the time attribute to         
                    ``1990-2010/1-12/1/0 A`` will cause HEMCO to    
                    calculate monthly means between 1990 to 2010    
                    and use those irrespective of the current       
                    simulation date **(v1.1.010 and higher)**. The  
                    data from the different years can be spread out 
                    over multiple files. For example, it is legal   
                    to use the averaging flag in combination with   
                    files that use year tokens such as              
                    ``file_$YYYY.nc`` **(only v1.1.014 and          
                    higher)**.                                      
                 -  **``I``** (interpolation): data fields are      
                    interpolated in time. As an example, let's      
                    assume a file contains annual data for years    
                    2005, 2010, 2020, and 2050. If the source time  
                    attribute is set to ``2005-2050/1/1/0 I``, data 
                    becomes interpolated between the two closest    
                    years every time we enter a new simulation      
                    year. If the simulation starts on January 2004, 
                    the value of 2005 is used for years 2004 and    
                    2005. At the beginning of 2006, the used data   
                    is calculated as a weighted mean for the 2005   
                    and 2010 data, with 0.8 weight given to 2005    
                    and 0.2 weight given to 2010 values. Once the   
                    simulation year changes to 2007, the weights    
                    change to 0.6 for 2005 and 0.4 for 2010, etc.   
                    The interpolation frequency is determined by    
                    the source time attribute. In the given         
                    example, setting the source time attribute to   
                    ``2005-2050/1-12/1/0 I`` would result in a      
                    recalculation of the weights on every new       
                    simulation month. Interpolation works in a very 
                    similar manner for discontinuous monthly,       
                    daily, and hourly data. For instance if a file  
                    contains monthly data of 2005, 2010, 2020, and  
                    2050 and the source time attribute is set to    
                    ``2005-2050/1-12/1/0 I``, the field is          
                    recalculated every month using the two          
                    bracketing fields of the given month: July 2007 
                    values are calculated from July 2005 and July   
                    2010 data (with weights of 0.6 and 0.4,         
                    respectively), etc. Data interpolation also     
                    works between multiple files (**only v1.1.014   
                    and higher**). For instance, if monthly data    
                    are stored in files file_200501.nc,             
                    file_200502.nc, etc., a combination of source   
                    file name ``file_$YYYY$MM.nc`` and source time  
                    attribute ``2005-2007/1-12/1-31/0 I`` will      
                    result in daily data interpolation between the  
                    two bracketing files, e.g. if the simulation    
                    day is July 15, 2005, the fields current values 
                    are calculated from files file_200507.nc and    
                    file_200508.nc, respectively. Data              
                    interpolation across multiple files also works  
                    if there are file 'gaps', for example if there  
                    is a file only every three hours:               
                    file_20120101_0000.nc, file_20120101_0300.nc,   
                    etc. Hourly data interpolation between those    
                    files can be achieved by setting source file to 
                    ``file_$YYYY$MM$DD_$HH00.nc``, and source time  
                    to ``2000-2015/1-12/1-31/0-23 I`` (or whatever  
                    the covered year range is).                     
+----------------+----------------------------------------------------+
 ``SrcDim``      Spatial dimension of input data (``xy`` for        
                 horizontal data; ``xyz`` for 3-dimensional data).  
                                                                    
                 As of HEMCO v1.1.004, the ``SrcDim`` attribute     
                 accepts an integer number as vertical coordinate   
                 to indicate the number of vertical levels to be    
                 read, as well as the direction of the vertical     
                 axis. For example, to use the lowest 5 levels of   
                 the input data only, set ``SrcDim`` to ``xy5``.    
                 This will place the lowest 5 levels of the input   
                 data into HEMCO levels 1 to 5. To use the topmost  
                 5 levels of the input data, set ``SrcDim`` to      
                 ``xy-5``. The minus sign will force the vertical   
                 axis to be flipped, i.e. the 5 topmost levels will 
                 be placed into HEMCO levels 1 to 5 (in reversed    
                 order, so that the topmost level of the input data 
                 will be placed in HEMCO level 1, etc.).            
                                                                    
                 In HEMCO v1.1.005 and higher, the ``SrcDim``       
                 attribute can also be used to indicate the level   
                 into which 2D data shall be released by setting    
                 the vertical coordinate to ``LX``, with X being    
                 the release level. For instance, to emit a 2D      
                 field into level 5, set ``srcDim`` to ``xyL5``.    
                                                                    
                 HEMCO v2 has two new options to specify the        
                 emission injection height: (a) the vertical height 
                 can be given as model level (default) or in        
                 meters, e.g. to emit a source at 2000m:            
                 ``xyL=2000m``; (b) for 2D fields it is legal to    
                 define a range of levels, in which case the        
                 emissions are uniformly distributed across these   
                 levels (maintaining the original total emissions). 
                 Examples for this are ``xyL=1:5`` (emit into       
                 levels 1-5) or ``xyL=2:5000m`` (emit between model 
                 level 2 and 5000m); (c) the vertical layer can     
                 also be set to the PBL height by using the         
                 character string ``PBL``. For example, to emit a   
                 source uniformly between the surface and the PBL   
                 top, use ``xyL=1:PBL``.                            
                                                                    
                 HEMCO v2.1.005 includes the option to use          
                 injection height information from an external      
                 source (i.e. netCDF file). For now, these heights  
                 are expected to be in meters. The injection height 
                 data must be listed as a scale factor and can then 
                 be referenced in the SrcDim setting. For an        
                 example see `our HEMCO examples wiki               
                 page <HEMCO_examples>`__.                          
                                                                    
                 As of v1.1.010, it is legal to read netCDF files   
                 with an arbitrary additional dimension. For these  
                 files, the name of the additional dimension and    
                 the desired dimension index must be specified as   
                 part of the ``SrcDim`` attribute. For example, to  
                 read a file that contains 3D ensemble data (with   
                 the individual ensemble runs as additional         
                 dimension 'ensemble'), set ``srcDim`` to           
                 ``xyz+"ensemble=3"`` to indicate that you wish to  
                 read the third ensemble member. It is also legal   
                 to use a user-defined token for the dimension      
                 index to be used, e.g. ``xyz+"ensemble=$ENS"``.    
                                                                    
                 Note that arbitrary additional dimensions are      
                 currently not supported in a high-performance      
                 environment that uses the ESMF/MAPL input/output   
                 libraries.                                         
+----------------+----------------------------------------------------+
 ``SrcUnit``     Data units. See `the Units in HEMCO                
                 section <#Units_in_HEMCO>`__.                      
+----------------+----------------------------------------------------+
 ``Species``     HEMCO emission species name. Emissions will be     
                 added to this species. All HEMCO emission species  
                 are defined at the beginning of the simulation     
                 (see `the Interfaces section <#Interfaces>`__). If 
                 the species name does not match any of the HEMCO   
                 species, the field is ignored altogether.          
                                                                    
                 The species name can be set to the wildcard        
                 character, in which case the field is always read  
                 by HEMCO but no species is assigned to it. This    
                 can be useful for extensions that import some      
                 (species-independent) fields by name.              
+----------------+----------------------------------------------------+
                 The three entries below only take effect for       
                 fields that are assigned to the base extension     
                 (ExtNr = 0), e.g. that are used for automatic      
                 emission calculation. They are used by HEMCO to    
                 determine the priority of the emission fields,     
                 i.e. how the final emission fields are assembled   
                 from all provided data fields.                     
+----------------+----------------------------------------------------+
 ``ScalIDs``     Identification numbers of all scale factors and    
                 masks that shall be applied to this base emission  
                 field. Multiple entries must be separated by the   
                 separator character. The ``ScalIDs`` must          
                 correspond to the numbers provided in the `Scale   
                 Factors <#Scale_factors>`__ and `Masks <#Masks>`__ 
                 sections.                                          
+----------------+----------------------------------------------------+
 ``Cat``         Emission category. Used to distinguish different,  
                 independent emission sources. Emissions of         
                 different categories are always added to each      
                 other.                                             
                                                                    
                 Up to three emission categories can be assigned to 
                 each entry (separated by the separator character). 
                 Emissions are always entirely written into the     
                 first listed category, while emissions of zero are 
                 used for any other assigned category.              
                                                                    
                 -  In practice, the only time when more than one   
                    emissions category needs to be specified is     
                    when an inventory does not separate between     
                    anthropogenic, biofuels, and/or trash           
                    emissions.                                      
                 -  For example, the CEDS inventory uses categories 
                    1/2/12 because CEDS lumps both biofuel          
                    emissions and trash emissions with              
                    anthropogenic emissions. Because it is not      
                    possible to separate out biofuels and trash,    
                    the 1/2/12 category designation means "Put      
                    everything into the first listed category       
                    (1=anthropogenic), and set the other listed     
                    categories (2=biofuels, 12=trash) to zero.      
+----------------+----------------------------------------------------+
 ``Hier``        Emission hierarchy. Used to prioritize emission    
                 fields within the same emission category.          
                 Emissions of higher hierarchy overwrite lower      
                 hierarchy data. Fields are only considered within  
                 their defined domain, i.e. regional inventories    
                 are only considered within their mask boundaries.  
+----------------+----------------------------------------------------+
