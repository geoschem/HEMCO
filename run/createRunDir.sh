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

curdir=$(pwd)
cd ..
hemcodir=$(pwd)
cd ${curdir}

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
valid_met=0
while [ "${valid_met}" -eq 0 ]; do
    read met_num
    valid_met=1
    if [[ ${met_num} = "1" ]]; then
	met_name='MERRA2'
	met_dir='MERRA2'
	met_native='0.5x0.625'
	met_latres='05'
	met_lonres='0625'
	met_extension='nc4'
	met_cn_year='2015'
	pressure_unit='Pa '
	pressure_scale='0.01'
	dust_sf='3.86e-4'
    elif [[ ${met_num} = "2" ]]; then
	met_name='GEOSFP'
	met_dir='GEOS_FP'
	met_native='0.25x0.3125'
	met_latres='025'
	met_lonres='03125'
	met_extension='nc'
	met_cn_year='2011'
	pressure_unit='hPa'
	pressure_scale='1.0 '
	dust_sf='6.42e-5'
    else
	printf "Invalid meteorology option. Try again.\n"
    fi
done

#-----------------------------------------------------------------
# Ask user to select meteorology source
#-----------------------------------------------------------------
printf "${thinline}Choose horizontal resolution:${thinline}"
printf "  1. 4.0 x 5.0\n"
printf "  2. 2.0 x 2.5\n"
printf "  3. 0.5 x 0.625\n"
printf "  4. 0.25 x 0.3125\n"
printf "  5. Custom\n"

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
# Ask user to provide path to HEMCO_Config.template file
#-----------------------------------------------------------------
printf "${thinline}Enter path to the HEMCO_Config.rc file with your emissions settings.\n\n"
printf "NOTE: This may be a HEMCO_Config.rc file from a GEOS-Chem run directory\n"
printf "or a HEMCO_Config.template file from the GEOS-Chem source code repository.${thinline}"

valid_path=0
while [ "$valid_path" -eq 0 ]; do
    read -e hco_config_path

    # Test for quitting
    if [[ "x${hco_config_path}" == "xq" ]]; then
	printf "\nExiting.\n"
	exit 1
    fi

    # Replace ~ with the user's home directory
    # NOTE: This is a safe algorithm.
    if [[ "${hco_config_path}" =~ '~' ]]; then
       hco_config_path="${hco_config_path/#\~/$HOME}"
       echo "Expanding to: ${hco_config_path}"
    fi

    if [[ ! -f ${hco_config_path} ]]; then
        printf "\nError: ${hco_config_path} does not exist. Enter a new path or hit q to quit.\n"
    else
	valid_path=1
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
	rundir_name=hemco_${grid_name}_"${met_name,,}"
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
cp ${hco_config_path}          ${rundir}/HEMCO_Config.rc
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

#-----------------------------------------------------------------
# Replace token strings in certain files
#-----------------------------------------------------------------
# HEMCO_sa_Config.rc
sed -i -e "s|{DATA_ROOT}|${GC_DATA_ROOT}|"   ${rundir}/HEMCO_sa_Config.rc
sed -i -e "s|{GRID_FILE}|${grid_file}|"      ${rundir}/HEMCO_sa_Config.rc
sed -i -e "s|{MET_NAME}|${met_name}|"        ${rundir}/HEMCO_sa_Config.rc
sed -i -e "s|{GRID_RES}|${grid_res}|"        ${rundir}/HEMCO_sa_Config.rc

# HEMCO_Config.rc (copied from GEOS-Chem)
sed -i -e "s|{DATA_ROOT}|${GC_DATA_ROOT}|"   ${rundir}/HEMCO_Config.rc
sed -i -e "s|{GRID_DIR}|${grid_dir}|"        ${rundir}/HEMCO_Config.rc
sed -i -e "s|{MET_DIR}|${met_dir}|"          ${rundir}/HEMCO_Config.rc
sed -i -e "s|{NATIVE_RES}|${met_native}|"    ${rundir}/HEMCO_Config.rc
sed -i -e "s|{LATRES}|${met_latres}|"        ${rundir}/HEMCO_Config.rc
sed -i -e "s|{LONRES}|${met_lonres}|"        ${rundir}/HEMCO_Config.rc
sed -i -e "s|{DUST_SF}|${dust_sf}|"          ${rundir}/HEMCO_Config.rc

#----------------------------------------------------------------------
# Archive repository version in run directory file rundir.version
#----------------------------------------------------------------------
version_log=${rundir}/rundir.version
echo "This run directory was created with HEMCO/run/createRunDir.sh." > ${version_log}
echo " " >> ${version_log}
echo "HEMCO repository version information:" >> ${version_log}
cd ${hemcodir}
remote_url=$(git config --get remote.origin.url)
code_branch=$(git rev-parse --abbrev-ref HEAD)
last_commit=$(git log -n 1 --pretty=format:"%s")
commit_date=$(git log -n 1 --pretty=format:"%cd")
commit_user=$(git log -n 1 --pretty=format:"%cn")
commit_hash=$(git log -n 1 --pretty=format:"%h")
cd ${curdir}
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
