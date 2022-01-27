#!/bin/bash

# createRunDir.sh: Create HEMCO standalone run directory
#
# Optional argument: run directory name
#
# If optional run directory name argument is not passed then the user
# will be prompted to enter a name interactively, or choose to use the
# default name gchp_{simulation}/
#
# Usage: ./createRunDir.sh [rundirname]
#
# Initial version: E. Lundgren,10/5/2018

srcrundir=$(pwd)
cd ..
hemcodir=$(pwd)
cd ${srcrundir}

# Define separator lines
thickline="\n===========================================================\n"
thinline="\n-----------------------------------------------------------\n"

printf "${thickline}HEMCO STANDALONE RUN DIRECTORY CREATION${thickline}"

#-----------------------------------------------------------------
# Export data root path in ~/.geoschem/config if file exists
#-----------------------------------------------------------------
if [[ -f ${HOME}/.geoschem/config ]]; then
    source ${HOME}/.geoschem/config
    if [[ ! -d ${GC_DATA_ROOT} ]]; then
	printf "\nWarning: Default root data directory does not exist!"
        printf "\nSet new path below or manually edit ${HOME}/.geoschem/config.\n"
    fi
else
    printf "\nDefine path to ExtData."
    printf "\nThis will be stored in ${HOME}/.geoschem/config for future automatic use.\n"
    mkdir -p ${HOME}/.geoschem
fi

#-----------------------------------------------------------------
# One-time configuration of data root path in ~/.geoschem/config
#-----------------------------------------------------------------
if [[ -z "${GC_DATA_ROOT}" ]]; then
    printf "${thinline}Enter path for ExtData:${thinline}"
    valid_path=0
    while [ "$valid_path" -eq 0 ]; do
	read -e extdata
	if [[ ${extdata} = "q" ]]; then
	    printf "\nExiting.\n"
	    exit 1
	elif [[ ! -d ${extdata} ]]; then
            printf "\nError: ${extdata} does not exist. Enter a new path or hit q to quit.\n"
	else
	    valid_path=1
	    echo "export GC_DATA_ROOT=${extdata}" >> ${HOME}/.geoschem/config
            source ${HOME}/.geoschem/config
	fi
    done
fi

#-----------------------------------------------------------------
# Ask user to select meteorology source
#-----------------------------------------------------------------
printf "${thinline}Choose meteorology source:${thinline}"
printf "  1. MERRA-2 (Recommended)\n"
printf "  2. GEOS-FP\n"
printf "  3. GISS ModelE2.1 (GCAP 2.0)\n"
valid_met=0
while [ "${valid_met}" -eq 0 ]; do
    read met_num
    valid_met=1
    if [[ ${met_num} = "1" ]]; then
	met_name='MERRA2'
	met_name_lc="merra2"
	met_dir='MERRA2'
	met_native='0.5x0.625'
	met_latres='05'
	met_lonres='0625'
	met_extension='nc4'
	met_cn_year='2015'
	pressure_unit='Pa '
	pressure_scale='0.01'
	offline_dust_sf='3.86e-4'
    elif [[ ${met_num} = "2" ]]; then
	met_name='GEOSFP'
	met_name_lc="geosfp"
	met_dir='GEOS_FP'
	met_native='0.25x0.3125'
	met_latres='025'
	met_lonres='03125'
	met_extension='nc'
	met_cn_year='2011'
	pressure_unit='hPa'
	pressure_scale='1.0 '
	offline_dust_sf='6.42e-5'
    elif [[ ${met_num} = "3" ]]; then
	met_name='ModelE2.1'
	met_name_lc='modele2.1'
	met_dir='E21'
	met_resolution='2x2.5'
	met_native='2x2.5'
	met_latres='20'
	met_lonres='25'
	met_extension='nc4'
	met_cn_year='1950'
	pressure_unit='Pa '
	pressure_scale='0.01'
	offline_dust_sf='1.0'
    else
	printf "Invalid meteorology option. Try again.\n"
    fi
done

if [[ ${met_name} = "ModelE2.1" ]]; then
    printf "${thinline}Choose scenario (presently available years in parentheses):${thinline}"
    printf "  1. Historical (1851-1860; 2001-2014)\n"
    printf "  2. Historical nudged to MERRA-2 (2001-2014)\n"
    printf "  3. SSP1-1.9 (2040-2049; 2090-2099)\n"
    printf "  4. SSP1-2.6 (2040-2049; 2090-2099)\n"
    printf "  5. SSP4-3.4 (2040-2049; 2090-2099)\n"
    printf "  6. SSP2-4.5 (2040-2049; 2090-2099)\n"
    printf "  7. SSP4-6.0 (2040-2049; 2090-2099)\n"
    printf "  8. SSP3-7.0 (2040-2049; 2090-2099)\n"
    printf "  9. SSP5-8.5 (2040-2049; 2090-2099)\n"

    valid_scen=0
    while [ "${valid_scen}" -eq 0 ]; do
	read scen_num
	valid_scen=1
	if [[ ${scen_num} = "1" ]]; then
	    scenario="HIST"
            runid="E213f10aF40oQ40"
	    met_avail="# 1851-1860; 2001-2014"
	elif [[ ${scen_num} = "2" ]]; then
	    scenario="HIST"
            runid="E213f10aF40oQ40nudge"
	    met_avail="# 2001-2014"
	elif [[ ${scen_num} = "3" ]]; then
	    scenario="SSP119"
            runid="E213SSP119aF40oQ40"
	    met_avail="# 2040-2049; 2090-2099"
	elif [[ ${scen_num} = "4" ]]; then
	    scenario="SSP119"
            runid="E213SSP119aF40oQ40"
	    met_avail="# 2040-2049; 2090-2099"
	elif [[ ${scen_num} = "5" ]]; then
	    scenario="SSP119"
            runid="E213SSP119aF40oQ40"
	    met_avail="# 2040-2049; 2090-2099"
	elif [[ ${scen_num} = "6" ]]; then
	    scenario="SSP119"
            runid="E213SSP119aF40oQ40"
	    met_avail="# 2040-2049; 2090-2099"
	elif [[ ${scen_num} = "7" ]]; then
	    scenario="SSP119"
            runid="E213SSP119aF40oQ40"
	    met_avail="# 2040-2049; 2090-2099"
	elif [[ ${scen_num} = "8" ]]; then
	    scenario="SSP119"
            runid="E213SSP119aF40oQ40"
	    met_avail="# 2040-2049; 2090-2099"
	elif [[ ${scen_num} = "9" ]]; then
	    scenario="SSP119"
            runid="E213SSP119aF40oQ40"
	    met_avail="# 2040-2049; 2090-2099"
	else
  	    valid_scen=0
	    printf "Invalid GCAP 2.0 scenario. Try again.\n"
	fi
    done

    printf "${thinline}Choose a fixed year for volcanic emissions (1978-2020)\n or -1 for year of simulation (assuming year exists):${thinline}"
    valid_volc=0
    while [ "${valid_volc}" -eq 0 ]; do
	read volc_year
	valid_volc=1
	echo $volc_year
	if [[ $volc_year -ge 1978 ]] && [[ $volc_year -le 2020 ]]; then
	    volc_year=$volc_year
        elif [[ $volc_year -eq -1 ]]; then
	    volc_year='$YYYY'
	else
  	    valid_volc=0
	    printf "Invalid volcano year. Try again.\n"
        fi
    done

else
    scenario=""
    runid=""
    volc_year='$YYYY'
    met_avail="# 1980-2021"
fi

#-----------------------------------------------------------------
# Ask user to select horizontal resolution
#-----------------------------------------------------------------
printf "${thinline}Choose horizontal resolution:${thinline}"
if [[ ${met_name} = "ModelE2.1" ]] || [[ ${met_name} = "ModelE2.2" ]]; then
    printf "  1. 4.0  x 5.0 *\n"
    printf "  2. 2.0  x 2.5\n"
    printf "  3. 0.5  x 0.625 *\n"
    printf "  4. 0.25 x 0.3125 *${thinline}"
    printf "             * Will be interpolated online via FlexGrid from native 2.0 x 2.5 resolution\n"
else
    printf "  1. 4.0 x 5.0\n"
    printf "  2. 2.0 x 2.5\n"
    printf "  3. 0.5 x 0.625\n"
    printf "  4. 0.25 x 0.3125\n"
    printf "  5. Custom\n"
fi

valid_res=0
while [ "${valid_res}" -eq 0 ]; do
    read res_num
    valid_res=1
    if [[ ${res_num} = "1" ]]; then
	grid_res='4x5'
	grid_res_long='4.0x5.0'
	grid_dir=$grid_res
	grid_file='HEMCO_sa_Grid.4x5.rc'
    elif [[ ${res_num} = "2" ]]; then
	grid_res='2x25'
	grid_res_long='2.0x2.5'
	grid_dir='2x2.5'
	grid_file='HEMCO_sa_Grid.2x25.rc'
    elif [[ ${res_num} = "3" ]]; then
	grid_res='05x0625'
	grid_res_long='0.5x0.625'
	grid_dir=$grid_res_long
	grid_file='HEMCO_sa_Grid.05x0625.rc'
    elif [[ ${res_num} = "4" ]]; then
	grid_res='025x03125'
	grid_res_long='0.25x0.3125'
	grid_dir=$grid_res_long
	grid_file='HEMCO_sa_Grid.025x03125.rc'
    elif [[ ${res_num} = "5" ]]; then
	printf "You will need to provide your own HEMCO_sa_Grid.rc file.\n"
	printf "See the HEMCO standalone guide for more information:\n"
	printf "http://wiki.seas.harvard.edu/geos-chem/index.php/HEMCO_standalone\n"
	valid_res=1
    else
	printf "Invalid resolution option. Try again.\n"
    fi
done

#-----------------------------------------------------------------
# Horizontal resolution-dependent settings
#-----------------------------------------------------------------
dead_tf="-999.0e0" # Default DEAD-dust scaling factor for online emissions
if [[ ${met_name} = "ModelE2.1" ]]; then
    if [[ "$runid" == "E213f10aF40oQ40nudge" ]]; then
        case "$grid_res" in
            "4x5" ) dead_tf="0.00474046"; giss_res="F40"  ;;
            "2x25" ) dead_tf="0.00243979"; giss_res="F40"  ;;
            "05x0625" ) dead_tf="0.00276896"; giss_res="F40"  ;;
            "025x03125" ) dead_tf="0.00254319"; giss_res="F40"  ;;
  	esac
    else
        case "$grid_res" in
            "4x5" ) dead_tf="0.03564873"; giss_res="F40"  ;;
            "2x25" ) dead_tf="0.01050036"; giss_res="F40"  ;;
            "05x0625" ) dead_tf="0.01340854"; giss_res="F40"  ;;
            "025x03125" ) dead_tf="0.01066495"; giss_res="F40"  ;;
	esac
    fi
fi

#-----------------------------------------------------------------
# Ask user to provide path to HEMCO_Config.template file
#-----------------------------------------------------------------
printf "${thinline}Enter file path to the HEMCO_Config.rc with your emissions settings.\n\n"
printf "NOTE: This may be a HEMCO_Config.rc file from a GEOS-Chem run directory\n"
printf "or a HEMCO_Config.template file from the GEOS-Chem source code repository.${thinline}"

valid_path=0
while [ "$valid_path" -eq 0 ]; do
    read -e hco_config_file

    # Test for quitting
    if [[ "x${hco_config_file}" == "xq" ]]; then
	printf "\nExiting.\n"
	exit 1
    fi

    # Replace ~ with the user's home directory
    # NOTE: This is a safe algorithm.
    if [[ "${hco_config_file}" =~ '~' ]]; then
       hco_config_file="${hco_config_file/#\~/$HOME}"
       echo "Expanding to: ${hco_config_file}"
    fi

    if [[ ! -f ${hco_config_file} ]]; then
        printf "\nError: ${hco_config_file} does not exist. Enter a new file path or hit q to quit.\n"
    else
	valid_path=1
    fi

    if [[ "$hco_config_file" == *"template"* ]]; then
	hco_config_dir=$(dirname $hco_config_file)
    fi
    
done

#-----------------------------------------------------------------
# Ask user to define path where directoy will be created
#-----------------------------------------------------------------
printf "${thinline}Enter path where the run directory will be created:${thinline}"

valid_path=0
while [ "$valid_path" -eq 0 ]; do
    read -e rundir_path

    # Test for quitting
    if [[ "x${rundir_path}" == "xq" ]]; then
	printf "\nExiting.\n"
	exit 1
    fi

    # Replace ~ with the user's home directory
    # NOTE: This is a safe algorithm.
    if [[ "${rundir_path}" =~ '~' ]]; then
       rundir_path="${rundir_path/#\~/$HOME}"
       echo "Expanding to: ${rundir_path}"
    fi

    # If this is just a new directory within an existing one,
    # give the user the option to proceed
    if [[ ! -d ${rundir_path} ]]; then
        if [[ -d $(dirname ${rundir_path} ) ]]; then
            printf "\nWarning: ${rundir_path} does not exist,\nbut the parent directory does.\nWould you like to make this directory? (y/n/q)\n"
            read mk_rundir
            if [[ "x${mk_rundir}" == "xy" ]]; then
                mkdir $rundir_path
	    elif [[ "x${mk_rundir}" == "xq" ]]; then
		printf "\nExiting.\n"
		exit 1
            fi
        fi
    fi

    # Ask user to supply a new path again
    if [[ ! -d ${rundir_path} ]]; then
        printf "\nERROR: ${rundir_path} does not exist. Enter a new path or hit q to quit.\n"
    else
	valid_path=1
    fi
done

#-----------------------------------------------------------------
# Ask user to define run directoy name if not passed as argument
#-----------------------------------------------------------------
if [ -z "$1" ]; then
    printf "${thinline}Enter run directory name, or press return to use default:\n\n"
    printf "NOTE: This will be a subfolder of the path you entered above.${thinline}"
    
    read -e rundir_name
    if [[ -z "${rundir_name}" ]]; then
	rundir_name=hemco_${grid_res}_"${met_name,,}"
	printf "  -- Using default directory name ${rundir_name}\n"
    fi
else
    rundir_name=$1
fi

#-----------------------------------------------------------------
# Ask user for a new run directory name if specified one exists
#-----------------------------------------------------------------
rundir=${rundir_path}/${rundir_name}

valid_rundir=0
while [ "${valid_rundir}" -eq 0 ]; do
    if [[ -d ${rundir} ]]; then
	printf "\nWarning: ${rundir} already exists.\n"
        printf "Enter a different run directory name, or q to quit:\n"
	read -e new_rundir
	if [[ ${new_rundir} = "q" ]]; then
	    printf "Exiting.\n"
	    exit 1
	else
	    rundir=${rundir_path}/${new_rundir}
	fi
    else
        valid_rundir=1
    fi
done

#-----------------------------------------------------------------
# Create run directory
#-----------------------------------------------------------------
mkdir -p ${rundir}

# Copy run directory files and subdirectories
cp -r ./OutputDir ${rundir}
cp ${hco_config_file}          ${rundir}/HEMCO_Config.rc
cp ./HEMCO_sa_Config.template  ${rundir}/HEMCO_sa_Config.rc
cp ./HEMCO_sa_Time.rc          ${rundir}
cp ./HEMCO_sa_Spec.rc          ${rundir}
cp ./${grid_file}              ${rundir}
cp ./HEMCO_Diagn.rc            ${rundir}
cp ./runHEMCO.sh               ${rundir}
cp ./README                    ${rundir}

# Create symbolic link to code directory
ln -s ${hemcodir} ${rundir}/CodeDir

# Create build directory
mkdir ${rundir}/build
printf "To build HEMCO type:\n   cmake ../CodeDir\n   make -j\n   make install\n" >> ${rundir}/build/README

#=============================================================================
#### Replacement for `sed -i -e` that works on both MacOS and Linux
#=============================================================================
function sed_ie() {
    REGEX=${1}
    FILE=${2}
    if [[ "x$(uname -s)" == "xDarwin" ]]; then
	sed -i '' -e "${REGEX}" "${FILE}"          # MacOS/Darwin
    else
	sed -i -e "${REGEX}" "${FILE}"             # GNU/Linux
    fi
}

#=============================================================================
#### Define function to replace values in config files
#=============================================================================
function replace_colon_sep_val() {
    KEY=${1}
    VALUE=${2}
    FILE=${3}

    # Debug print (leave commented out)
    # printf '%-30s : %-20s %-20s\n' "${KEY//\\}" "${VALUE}" "${FILE}"

    # Replace value in line starting with 'whitespace + key + whitespace + : +
    # whitespace + value' where whitespace is variable length including none
    #
    # MacOS sed does not allow you to use \t for tab.  The quick fix is
    # to use printf to save a tab character to a variable, and to use
    # that in the regular expression everywhere you would have used \t.
    # See the Github issue geoschem/geos-chem #617. (bmy, 2/23/21)
    TAB=$(printf "\t")
    REGEX="s|^\([${TAB} ]*${KEY}[${TAB} ]*:[${TAB} ]*\).*|\1${VALUE}|"
    sed_ie "${REGEX}" "${FILE}"
}

#--------------------------------------------------------------------
# Navigate to run directory and set up input files
#--------------------------------------------------------------------

cd ${rundir}

# Specify meteorology if using a template HEMCO_Config.rc 
if [[ "$hco_config_file" == *"template"* ]]; then
    if [[ ${met_name} = "ModelE2.1" ]]; then
	sed_ie "/### BEGIN SECTION SETTINGS/r ${hco_config_dir}/header.gcap2" HEMCO_Config.rc
	sed_ie "/# --- Meteorology fields for FlexGrid ---/r ${hco_config_dir}/met_fields.gcap2" HEMCO_Config.rc
    else
	sed_ie "/### BEGIN SECTION SETTINGS/r ${hco_config_dir}/header.gmao" HEMCO_Config.rc
	sed_ie "/# --- Meteorology fields for FlexGrid ---/r ${hco_config_dir}/met_fields.gmao" HEMCO_Config.rc
    fi
fi

# Replace token strings in certain files
sed_ie "s|{DATA_ROOT}|${GC_DATA_ROOT}|"   HEMCO_sa_Config.rc
sed_ie "s|{GRID_FILE}|${grid_file}|"      HEMCO_sa_Config.rc
sed_ie "s|{MET_NAME}|${met_name}|"        HEMCO_sa_Config.rc
sed_ie "s|{GRID_RES}|${grid_res}|"        HEMCO_sa_Config.rc
sed_ie "s|{DATA_ROOT}|${GC_DATA_ROOT}|"   HEMCO_Config.rc
sed_ie "s|{GRID_DIR}|${grid_dir}|"        HEMCO_Config.rc
sed_ie "s|{MET_DIR}|${met_dir}|"          HEMCO_Config.rc
sed_ie "s|{NATIVE_RES}|${met_native}|"    HEMCO_Config.rc
sed_ie "s|{LATRES}|${met_latres}|"        HEMCO_Config.rc
sed_ie "s|{LONRES}|${met_lonres}|"        HEMCO_Config.rc
sed_ie "s|{DUST_SF}|${offline_dust_sf}|"  HEMCO_Config.rc
sed_ie "s|{DEAD_TF}|${dead_tf}|"          HEMCO_Config.rc
sed_ie "s|{MET_AVAIL}|${met_avail}|"      HEMCO_Config.rc

# Assign appropriate vertical resolution files for a given simulation
if [[ ${met_name} = "ModelE2.1" ]]; then
    sed_ie          's|{Br_GC}|* Br_GC          $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.SpeciesConc.2010-2019.$MM.{VERTRES}L.nc4 SpeciesConc_Br         2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie         's|{BrO_GC}|* BrO_GC         $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.SpeciesConc.2010-2019.$MM.{VERTRES}L.nc4 SpeciesConc_BrO        2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie    's|{GLOBAL_ACTA}|* GLOBAL_ACTA    $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.SpeciesConc.2010-2019.$MM.{VERTRES}L.nc4 SpeciesConc_ACTA       2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie      's|{GLOBAL_Cl}|* GLOBAL_Cl      $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.SpeciesConc.2010-2019.$MM.{VERTRES}L.nc4 SpeciesConc_Cl         2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie     's|{GLOBAL_ClO}|* GLOBAL_ClO     $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.SpeciesConc.2010-2019.$MM.{VERTRES}L.nc4 SpeciesConc_ClO        2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie     's|{GLOBAL_HCl}|* GLOBAL_HCl     $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.SpeciesConc.2010-2019.$MM.{VERTRES}L.nc4 SpeciesConc_HCl        2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie   's|{GLOBAL_HCOOH}|* GLOBAL_HCOOH   $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.SpeciesConc.2010-2019.$MM.{VERTRES}L.nc4 SpeciesConc_HCOOH      2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie    's|{GLOBAL_HNO3}|* GLOBAL_HNO3    $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.SpeciesConc.2010-2019.$MM.{VERTRES}L.nc4 SpeciesConc_HNO3       2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie     's|{GLOBAL_HO2}|* GLOBAL_HO2     $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.SpeciesConc.2010-2019.$MM.{VERTRES}L.nc4 SpeciesConc_HO2        2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie    's|{GLOBAL_HOCl}|* GLOBAL_HOCl    $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.SpeciesConc.2010-2019.$MM.{VERTRES}L.nc4 SpeciesConc_HOCl       2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie     's|{GLOBAL_NIT}|* GLOBAL_NIT     $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.SpeciesConc.2010-2019.$MM.{VERTRES}L.nc4 SpeciesConc_NIT        2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie      's|{GLOBAL_NO}|* GLOBAL_NO      $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.SpeciesConc.2010-2019.$MM.{VERTRES}L.nc4 SpeciesConc_NO         2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie     's|{GLOBAL_NO2}|* GLOBAL_NO2     $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.SpeciesConc.2010-2019.$MM.{VERTRES}L.nc4 SpeciesConc_NO2        2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie     's|{GLOBAL_NO3}|* GLOBAL_NO3     $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.SpeciesConc.2010-2019.$MM.{VERTRES}L.nc4 SpeciesConc_NO3        2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie      's|{GLOBAL_O3}|* GLOBAL_O3      $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.SpeciesConc.2010-2019.$MM.{VERTRES}L.nc4 SpeciesConc_O3         2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie      's|{GLOBAL_OH}|* GLOBAL_OH      $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.SpeciesConc.2010-2019.$MM.{VERTRES}L.nc4 SpeciesConc_OH         2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie       's|{AERO_SO4}|* AERO_SO4       $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.SpeciesConc.2010-2019.$MM.{VERTRES}L.nc4 SpeciesConc_SO4        2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie       's|{AERO_NH4}|* AERO_NH4       $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.SpeciesConc.2010-2019.$MM.{VERTRES}L.nc4 SpeciesConc_NH4        2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie       's|{AERO_NIT}|* AERO_NIT       $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.SpeciesConc.2010-2019.$MM.{VERTRES}L.nc4 SpeciesConc_NIT        2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie      's|{AERO_BCPI}|* AERO_BCPI      $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.SpeciesConc.2010-2019.$MM.{VERTRES}L.nc4 SpeciesConc_BCPI       2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie      's|{AERO_BCPO}|* AERO_BCPO      $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.SpeciesConc.2010-2019.$MM.{VERTRES}L.nc4 SpeciesConc_BCPO       2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie      's|{AERO_OCPI}|* AERO_OCPI      $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.SpeciesConc.2010-2019.$MM.{VERTRES}L.nc4 SpeciesConc_OCPI       2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie      's|{AERO_OCPO}|* AERO_OCPO      $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.SpeciesConc.2010-2019.$MM.{VERTRES}L.nc4 SpeciesConc_OCPO       2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie      's|{AERO_DST1}|* AERO_DST1      $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.SpeciesConc.2010-2019.$MM.{VERTRES}L.nc4 SpeciesConc_DST1       2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie      's|{GLOBAL_OA}|* GLOBAL_OA      $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.AerosolMass.2010-2019.$MM.{VERTRES}L.nc4 TotalOA                2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie        's|{PCO_CH4}|* PCO_CH4        $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.ProdLoss.2010-2019.$MM.{VERTRES}L.nc4    ProdCOfromCH4          2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie      's|{PCO_NMVOC}|* PCO_NMVOC      $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.ProdLoss.2010-2019.$MM.{VERTRES}L.nc4    ProdCOfromNMVOC        2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie          's|{PH2O2}|* PH2O2          $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.ProdLoss.2010-2019.$MM.{VERTRES}L.nc4    Prod_H2O2              2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie        's|{O3_PROD}|* O3_PROD        $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.ProdLoss.2010-2019.$MM.{VERTRES}L.nc4    Prod_Ox                2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie        's|{O3_LOSS}|* O3_LOSS        $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.ProdLoss.2010-2019.$MM.{VERTRES}L.nc4    Loss_Ox                2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie           's|{JBrO}|* JBrO           $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.JValues.2010-2019.$MM.{VERTRES}L.nc4     Jval_BrO               2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie          's|{JH2O2}|* JH2O2          $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.JValues.2010-2019.$MM.{VERTRES}L.nc4     Jval_H2O2              2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie           's|{JNO2}|* JNO2           $ROOT/GCAP2/OFFLINE_FIELDS/13.0.0/GEOSChem.JValues.2010-2019.$MM.{VERTRES}L.nc4     Jval_NO2               2015/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie       's|{CH4_LOSS}|* CH4_LOSS       $ROOT/GCAP2/GMI/v2015-02/{VERTRES}L/gmi.clim.CH4.geos5.2x25.nc                      loss                   2005/1-12/1/0 C xyz s-1      * - 1 1|' HEMCO_Config.rc
    sed_ie     's|{CO2_COPROD}|* CO2_COPROD     $ROOT/GCAP2/CO2/v2019-02/CHEM/CO2_prod_rates.2x25.{VERTRES}L.nc                     LCO               2004-2009/1-12/1/0 C xyz kgC/m3/s * - 1 1|' HEMCO_Config.rc
    sed_ie      's|{Br_TOMCAT}|* Br_TOMCAT      $ROOT/GCAP2/MERCURY/v2014-09/BrOx/BrOx.GMI.geos5.2x25.{VERTRES}L.nc4                LBRO2N                 1985/1-12/1/0 C xyz pptv     * - 1 1|' HEMCO_Config.rc
    sed_ie     's|{BrO_TOMCAT}|* BrO_TOMCAT     $ROOT/GCAP2/MERCURY/v2014-09/BrOx/BrOx.GMI.geos5.2x25.{VERTRES}L.nc4                LBRO2H                 1985/1-12/1/0 C xyz pptv     * - 1 1|' HEMCO_Config.rc
    sed_ie      's|{GLOBAL_OC}|1002 GLOBAL_OC   $ROOT/GCAP2/POPs/v2015-08/OCPO.4x5.{VERTRES}L.nc4                                   OCPO              2005-2009/1-12/1/0 C xyz kg       * - 1 1|' HEMCO_Config.rc
    sed_ie      's|{GLOBAL_BC}|1002 GLOBAL_BC   $ROOT/GCAP2/POPs/v2015-08/BCPO.4x5.{VERTRES}L.nc4                                   BCPO              2005-2009/1-12/1/0 C xyz kg       * - 1 1|' HEMCO_Config.rc
    sed_ie  's|{TES_CLIM_CCL4}|* TES_CLIM_CCL4  $ROOT/GCAP2/RRTMG/v2018-11/species_clim_profiles.2x25.{VERTRES}L.nc4                CCl4                   2000/1/1/0    C xyz ppbv     * - 1 1|' HEMCO_Config.rc
    sed_ie 's|{TES_CLIM_CFC11}|* TES_CLIM_CFC11 $ROOT/GCAP2/RRTMG/v2018-11/species_clim_profiles.2x25.{VERTRES}L.nc4                CFC11                  2000/1/1/0    C xyz ppbv     * - 1 1|' HEMCO_Config.rc
    sed_ie 's|{TES_CLIM_CFC12}|* TES_CLIM_CFC12 $ROOT/GCAP2/RRTMG/v2018-11/species_clim_profiles.2x25.{VERTRES}L.nc4                CFC12                  2000/1/1/0    C xyz ppbv     * - 1 1|' HEMCO_Config.rc
    sed_ie 's|{TES_CLIM_CFC22}|* TES_CLIM_CFC22 $ROOT/GCAP2/RRTMG/v2018-11/species_clim_profiles.2x25.{VERTRES}L.nc4                CFC22                  2000/1/1/0    C xyz ppbv     * - 1 1|' HEMCO_Config.rc
    sed_ie   's|{TES_CLIM_CH4}|* TES_CLIM_CH4   $ROOT/GCAP2/RRTMG/v2018-11/species_clim_profiles.2x25.{VERTRES}L.nc4                CH4                    2000/1/1/0    C xyz ppbv     * - 1 1|' HEMCO_Config.rc
    sed_ie   's|{TES_CLIM_N2O}|* TES_CLIM_N2O   $ROOT/GCAP2/RRTMG/v2018-11/species_clim_profiles.2x25.{VERTRES}L.nc4                N2O                    2000/1/1/0    C xyz ppbv     * - 1 1|' HEMCO_Config.rc
    sed_ie    's|{GMI_LOSS_CO}|* GMI_LOSS_CO    $ROOT/GCAP2/GMI/v2015-02/{VERTRES}L/gmi.clim.CO.geos5.2x25.nc                       loss                   2005/1-12/1/0 C xyz s-1     CO - 1 1|' HEMCO_Config.rc
    sed_ie    's|{GMI_PROD_CO}|* GMI_PROD_CO    $ROOT/GCAP2/GMI/v2015-02/{VERTRES}L/gmi.clim.CO.geos5.2x25.nc                       prod                   2005/1-12/1/0 C xyz v/v/s   CO - 1 1|' HEMCO_Config.rc
    sed_ie     's|{OCEAN_MASK}|1000 OCEAN_MASK  $METDIR/TOPO                                                                        focean                    */1/1/0 C xy 1 1  -180/-90/180/90|' HEMCO_Config.rc
    sed_ie        's|{Bry_DIR}|GCAP2/Bry/v2015-01/{VERTRES}L|'                                                                                                                                    HEMCO_Config.rc
    sed_ie        's|{GMI_DIR}|GCAP2/GMI/v2015-02/{VERTRES}L|'                                                                                                                                    HEMCO_Config.rc
    sed_ie        's|{UCX_DIR}|GCAP2/UCX/v2018-02|'                                                                                                                                               HEMCO_Config.rc
    sed_ie         "s|{GFEDON}|off|"                                                                                                                                                              HEMCO_Config.rc
    sed_ie         "s|{ONLINE}|on |"                                                                                                                                                              HEMCO_Config.rc
    sed_ie       "s|{UCX_LEVS}|${grid_lev}|"                                                                                                                                                      HEMCO_Config.rc
    sed_ie       "s|{SCENARIO}|${scenario}|"                                                                                                                                                      HEMCO_Config.rc
    sed_ie          "s|{RUNID}|${runid}|"                                                                                                                                                         HEMCO_Config.rc
    sed_ie      "s|{VOLC_YEAR}|${volc_year}|g"                                                                                                                                                    HEMCO_Config.rc
    sed_ie        "s|{DEAD_TF}|${dead_tf}|"                                                                                                                                                       HEMCO_Config.rc
    sed_ie        "s|{GISSRES}|${giss_res}|g"                                                                                                                                                     HEMCO_Config.rc
    sed_ie        "s|{VERTRES}|${grid_lev}|g"                                                                                                                                                     HEMCO_Config.rc
else
    sed_ie    's|{GLOBAL_ACTA}|* GLOBAL_ACTA    $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.SpeciesConc.$YYYY$MM$DD_0000z.nc4      SpeciesConc_ACTA  2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie          's|{Br_GC}|* Br_GC          $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.SpeciesConc.$YYYY$MM$DD_0000z.nc4      SpeciesConc_Br    2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie         's|{BrO_GC}|* BrO_GC         $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.SpeciesConc.$YYYY$MM$DD_0000z.nc4      SpeciesConc_BrO   2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie      's|{GLOBAL_Cl}|* GLOBAL_Cl      $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.SpeciesConc.$YYYY$MM$DD_0000z.nc4      SpeciesConc_Cl    2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie     's|{GLOBAL_ClO}|* GLOBAL_ClO     $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.SpeciesConc.$YYYY$MM$DD_0000z.nc4      SpeciesConc_ClO   2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie     's|{GLOBAL_HCl}|* GLOBAL_HCl     $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.SpeciesConc.$YYYY$MM$DD_0000z.nc4      SpeciesConc_HCl   2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie   's|{GLOBAL_HCOOH}|* GLOBAL_HCOOH   $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.SpeciesConc.$YYYY$MM$DD_0000z.nc4      SpeciesConc_HCOOH 2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie    's|{GLOBAL_HNO3}|* GLOBAL_HNO3    $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.SpeciesConc.$YYYY$MM$DD_0000z.nc4      SpeciesConc_HNO3  2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie     's|{GLOBAL_HO2}|* GLOBAL_HO2     $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.SpeciesConc.$YYYY$MM$DD_0000z.nc4      SpeciesConc_HO2   2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie    's|{GLOBAL_HOCl}|* GLOBAL_HOCl    $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.SpeciesConc.$YYYY$MM$DD_0000z.nc4      SpeciesConc_HOCl  2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie     's|{GLOBAL_NIT}|* GLOBAL_NIT     $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.SpeciesConc.$YYYY$MM$DD_0000z.nc4      SpeciesConc_NIT   2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie      's|{GLOBAL_NO}|* GLOBAL_NO      $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.SpeciesConc.$YYYY$MM$DD_0000z.nc4      SpeciesConc_NO    2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie     's|{GLOBAL_NO2}|* GLOBAL_NO2     $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.SpeciesConc.$YYYY$MM$DD_0000z.nc4      SpeciesConc_NO2   2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie     's|{GLOBAL_NO3}|* GLOBAL_NO3     $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.SpeciesConc.$YYYY$MM$DD_0000z.nc4      SpeciesConc_NO3   2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie      's|{GLOBAL_O3}|* GLOBAL_O3      $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.SpeciesConc.$YYYY$MM$DD_0000z.nc4      SpeciesConc_O3    2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie      's|{GLOBAL_OH}|* GLOBAL_OH      $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.SpeciesConc.$YYYY$MM$DD_0000z.nc4      SpeciesConc_OH    2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie       's|{AERO_SO4}|* AERO_SO4       $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.SpeciesConc.$YYYY$MM$DD_0000z.nc4      SpeciesConc_SO4   2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie       's|{AERO_NH4}|* AERO_NH4       $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.SpeciesConc.$YYYY$MM$DD_0000z.nc4      SpeciesConc_NH4   2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie       's|{AERO_NIT}|* AERO_NIT       $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.SpeciesConc.$YYYY$MM$DD_0000z.nc4      SpeciesConc_NIT   2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie      's|{AERO_BCPI}|* AERO_BCPI      $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.SpeciesConc.$YYYY$MM$DD_0000z.nc4      SpeciesConc_BCPI  2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie      's|{AERO_BCPO}|* AERO_BCPO      $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.SpeciesConc.$YYYY$MM$DD_0000z.nc4      SpeciesConc_BCPO  2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie      's|{AERO_OCPI}|* AERO_OCPI      $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.SpeciesConc.$YYYY$MM$DD_0000z.nc4      SpeciesConc_OCPI  2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie      's|{AERO_OCPO}|* AERO_OCPO      $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.SpeciesConc.$YYYY$MM$DD_0000z.nc4      SpeciesConc_OCPO  2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie      's|{AERO_DST1}|* AERO_DST1      $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.SpeciesConc.$YYYY$MM$DD_0000z.nc4      SpeciesConc_DST1  2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie      's|{GLOBAL_OA}|* GLOBAL_OA      $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.AerosolMass.$YYYY$MM$DD_0000z.nc4      TotalOA           2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie        's|{PCO_CH4}|* PCO_CH4        $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.ProdLoss.$YYYY$MM$DD_0000z.nc4         ProdCOfromCH4     2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie      's|{PCO_NMVOC}|* PCO_NMVOC      $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.ProdLoss.$YYYY$MM$DD_0000z.nc4         ProdCOfromNMVOC   2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie          's|{PH2O2}|* PH2O2          $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.ProdLoss.$YYYY$MM$DD_0000z.nc4         Prod_H2O2         2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie        's|{O3_PROD}|* O3_PROD        $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.ProdLoss.$YYYY$MM$DD_0000z.nc4         Prod_Ox           2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie        's|{O3_LOSS}|* O3_LOSS        $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.ProdLoss.$YYYY$MM$DD_0000z.nc4         Loss_Ox           2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie           's|{JBrO}|* JBrO           $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.JValues.$YYYY$MM$DD_0000z.nc4          Jval_BrO          2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie          's|{JH2O2}|* JH2O2          $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.JValues.$YYYY$MM$DD_0000z.nc4          Jval_H2O2         2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie           's|{JNO2}|* JNO2           $ROOT/GCClassic_Output/13.0.0/$YYYY/GEOSChem.JValues.$YYYY$MM$DD_0000z.nc4          Jval_NO2          2010-2019/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie       's|{CH4_LOSS}|* CH4_LOSS       $ROOT/CH4/v2014-09/4x5/gmi.ch4loss.geos5_47L.4x5.nc                                 CH4loss           1985/1-12/1/0      C xyz s-1      * - 1 1|' HEMCO_Config.rc
    sed_ie     's|{CO2_COPROD}|* CO2_COPROD     $ROOT/CO2/v2019-02/CHEM/CO2_prod_rates.GEOS5.2x25.47L.nc                            LCO               2004-2009/1-12/1/0 C xyz kgC/m3/s * - 1 1|' HEMCO_Config.rc
    sed_ie      's|{Br_TOMCAT}|* Br_TOMCAT      $ROOT/MERCURY/v2014-09/BrOx/BrOx.GMI.geos5.2x25.nc                                  LBRO2N                 1985/1-12/1/0 C xyz pptv     * - 1 1|' HEMCO_Config.rc
    sed_ie     's|{BrO_TOMCAT}|* BrO_TOMCAT     $ROOT/MERCURY/v2014-09/BrOx/BrOx.GMI.geos5.2x25.nc                                  LBRO2H                 1985/1-12/1/0 C xyz pptv     * - 1 1|' HEMCO_Config.rc
    sed_ie      's|{GLOBAL_OC}|1002 GLOBAL_OC   $ROOT/POPs/v2015-08/OCPO.$met.$RES.nc                                               OCPO              2005-2009/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie      's|{GLOBAL_BC}|1002 GLOBAL_BC   $ROOT/POPs/v2015-08/BCPO.$met.$RES.nc                                               BCPO              2005-2009/1-12/1/0 C xyz 1        * - 1 1|' HEMCO_Config.rc
    sed_ie  's|{TES_CLIM_CCL4}|* TES_CLIM_CCL4  $ROOT/RRTMG/v2018-11/species_clim_profiles.2x25.nc                                  CCl4                   2000/1/1/0    C xyz ppbv     * - 1 1|' HEMCO_Config.rc
    sed_ie 's|{TES_CLIM_CFC11}|* TES_CLIM_CFC11 $ROOT/RRTMG/v2018-11/species_clim_profiles.2x25.nc                                  CFC11                  2000/1/1/0    C xyz ppbv     * - 1 1|' HEMCO_Config.rc
    sed_ie 's|{TES_CLIM_CFC12}|* TES_CLIM_CFC12 $ROOT/RRTMG/v2018-11/species_clim_profiles.2x25.nc                                  CFC12                  2000/1/1/0    C xyz ppbv     * - 1 1|' HEMCO_Config.rc
    sed_ie 's|{TES_CLIM_CFC22}|* TES_CLIM_CFC22 $ROOT/RRTMG/v2018-11/species_clim_profiles.2x25.nc                                  CFC22                  2000/1/1/0    C xyz ppbv     * - 1 1|' HEMCO_Config.rc
    sed_ie   's|{TES_CLIM_CH4}|* TES_CLIM_CH4   $ROOT/RRTMG/v2018-11/species_clim_profiles.2x25.nc                                  CH4                    2000/1/1/0    C xyz ppbv     * - 1 1|' HEMCO_Config.rc
    sed_ie   's|{TES_CLIM_N2O}|* TES_CLIM_N2O   $ROOT/RRTMG/v2018-11/species_clim_profiles.2x25.nc                                  N2O                    2000/1/1/0    C xyz ppbv     * - 1 1|' HEMCO_Config.rc
    sed_ie    's|{GMI_LOSS_CO}|* GMI_LOSS_CO    $ROOT/GMI/v2015-02/gmi.clim.CO.geos5.2x25.nc                                        loss                   2005/1-12/1/0 C xyz s-1     CO - 1 1|' HEMCO_Config.rc
    sed_ie    's|{GMI_PROD_CO}|* GMI_PROD_CO    $ROOT/GMI/v2015-02/gmi.clim.CO.geos5.2x25.nc                                        prod                   2005/1-12/1/0 C xyz v/v/s   CO - 1 1|' HEMCO_Config.rc
    sed_ie     's|{OCEAN_MASK}|1000 OCEAN_MASK  $METDIR/$CNYR/01/$MET.$CNYR0101.CN.$RES.$NC                                         FROCEAN           2000/1/1/0 C xy 1 1       -180/-90/180/90|' HEMCO_Config.rc
    sed_ie        's|{Bry_DIR}|STRAT/v2015-01/Bry|'                                                                                                                                               HEMCO_Config.rc
    sed_ie        's|{GMI_DIR}|GMI/v2015-02|'                                                                                                                                                     HEMCO_Config.rc
    sed_ie        's|{UCX_DIR}|UCX/v2018-02|'                                                                                                                                                     HEMCO_Config.rc
    sed_ie         "s|{GFEDON}|on |"                                                                                                                                                              HEMCO_Config.rc
    sed_ie         "s|{ONLINE}|off|"                                                                                                                                                              HEMCO_Config.rc
    sed_ie       's|{UCX_LEVS}|72|'                                                                                                                                                               HEMCO_Config.rc
    sed_ie      "s|{VOLC_YEAR}|${volc_year}|g"                                                                                                                                                    HEMCO_Config.rc
    sed_ie        's|{VERTRES}|47|'                                                                                                                                                               HEMCO_Config.rc
fi

# Set online dust emission mass tuning factor according to met field
# and resolution. These values were obtained from hcox_dustdead_mod.F90.
if [[ "x${met_name}" == "xGEOSFP" && "x${grid_res}" == "x4x5" ]]; then
    replace_colon_sep_val "--> Mass tuning factor" 8.3286e-4 HEMCO_Config.rc
fi
if [[ "x${met_name}" == "xGEOSFP" && "x${grid_res}" == "x2x25" ]]; then
    replace_colon_sep_val "--> Mass tuning factor" 5.0416e-4 HEMCO_Config.rc
fi
if [[ "x${met_name}" == "xMERRA2" && "x${grid_res}" == "x4x5" ]]; then
    replace_colon_sep_val "--> Mass tuning factor" 7.8533e-4 HEMCO_Config.rc
fi
if [[ "x${met_name}" == "xMERRA2" && "x${grid_res}" == "x2x25" ]]; then
    replace_colon_sep_val "--> Mass tuning factor" 4.7586e-4 HEMCO_Config.rc
fi

# Modify default settings for GCAP simulations
if [[ ${met_name} = "ModelE2.1" ]]; then
    replace_colon_sep_val "--> CEDS"                  false HEMCO_Config.rc
    replace_colon_sep_val "--> POET_EOH"              false HEMCO_Config.rc
    replace_colon_sep_val "--> TZOMPASOSA_C2H6"       false HEMCO_Config.rc
    replace_colon_sep_val "--> XIAO_C3H8"             false HEMCO_Config.rc
    replace_colon_sep_val "--> AEIC"                  false HEMCO_Config.rc
    replace_colon_sep_val "--> CEDS_SHIP"             false HEMCO_Config.rc
    replace_colon_sep_val "--> OFFLINE_DUST"          false HEMCO_Config.rc
    replace_colon_sep_val "--> OFFLINE_BIOGENICVOC"   false HEMCO_Config.rc
    replace_colon_sep_val "--> OFFLINE_SEASALT"       false HEMCO_Config.rc
    replace_colon_sep_val "--> OFFLINE_SOILNOX"       false HEMCO_Config.rc
    replace_colon_sep_val "--> GMD_SFC_CH4"           false HEMCO_Config.rc
    replace_colon_sep_val "--> QFED2"                 false HEMCO_Config.rc
    replace_colon_sep_val "--> SfcVMR"                false HEMCO_Config.rc
    replace_colon_sep_val "--> CMIP6_SFC_BC"          true  HEMCO_Config.rc
    replace_colon_sep_val "--> CMIP6_SFC_LAND_ANTHRO" true  HEMCO_Config.rc
    replace_colon_sep_val "--> CMIP6_AIRCRAFT"        true  HEMCO_Config.rc
    replace_colon_sep_val "--> CMIP6_SHIP"            true  HEMCO_Config.rc
    replace_colon_sep_val "--> BB4MIPS"               true  HEMCO_Config.rc
fi

#--------------------------------------------------------------------
# Navigate back to source code directory
#--------------------------------------------------------------------
cd ${srcrundir}

#----------------------------------------------------------------------
# Archive repository version in run directory file rundir.version
#----------------------------------------------------------------------
version_log=${rundir}/rundir.version
echo "This run directory was created with ${srcrundir}/createRunDir.sh." > ${version_log}
echo " " >> ${version_log}
echo "HEMCO repository version information:" >> ${version_log}
cd ${hemcodir}
remote_url=$(git config --get remote.origin.url)
code_branch=$(git rev-parse --abbrev-ref HEAD)
last_commit=$(git log -n 1 --pretty=format:"%s")
commit_date=$(git log -n 1 --pretty=format:"%cd")
commit_user=$(git log -n 1 --pretty=format:"%cn")
commit_hash=$(git log -n 1 --pretty=format:"%h")
cd ${srcrundir}
printf "\n  Remote URL: ${remote_url}" >> ${version_log}
printf "\n  Branch: ${code_branch}"    >> ${version_log}
printf "\n  Commit: ${last_commit}"    >> ${version_log}
printf "\n  Date: ${commit_date}"      >> ${version_log}
printf "\n  User: ${commit_user}"      >> ${version_log}
printf "\n  Hash: ${commit_hash}"      >> ${version_log}

#-----------------------------------------------------------------
# Ask user whether to track run directory changes with git
#-----------------------------------------------------------------
printf "${thinline}Do you want to track run directory changes with git? (y/n)${thinline}"
valid_response=0
while [ "$valid_response" -eq 0 ]; do
    read enable_git
    if [[ ${enable_git} = "y" ]]; then
	cd ${rundir}
	printf "\n\nChanges to the following run directory files are tracked by git:\n\n" >> ${version_log}
	git init
	git add *.rc *.sh
	printf " " >> ${version_log}
	git commit -m "Initial run directory" >> ${version_log}
	cd ${curdir}
	valid_response=1
    elif [[ ${enable_git} = "n" ]]; then
	valid_response=1
    else
	printf "Input not recognized. Try again.\n"
    fi
done

#-----------------------------------------------------------------
# Done!
#-----------------------------------------------------------------
printf "\nCreated ${rundir}\n"

exit 0
