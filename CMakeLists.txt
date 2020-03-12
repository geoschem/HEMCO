# HEMCO/CMakeLists.txt

cmake_minimum_required(VERSION 3.5)
project(HEMCO VERSION 3.0.0 LANGUAGES Fortran)

# Set policies
cmake_policy(SET CMP0054 NEW)
cmake_policy(SET CMP0057 NEW)
if(POLICY CMP0074)
       		 cmake_policy(SET CMP0074 NEW)
endif()
if(POLICY CMP0079)
    cmake_policy(SET CMP0079 NEW)
endif()

# Add CMakeScripts/ to the module path and import helper functions
list(INSERT CMAKE_MODULE_PATH 0 ${CMAKE_CURRENT_SOURCE_DIR}/CMakeScripts)
include(HEMCO-Helpers)

# Must define HEMCO_EXTERNAL_CONFIG for all models except standalone
if(NOT HEMCO_EXTERNAL_CONFIG)
   set(STANDALONE TRUE)
endif()

if(STANDALONE)
   # Declare the BaseTarget. All HEMCO targets depend on BaseTarget.
   # This is used to control the compiler options and definitions for HEMCO
   # targets (via inheritance).
   add_library(BaseTarget INTERFACE)
else()
   # Not sure if this is needed. Can each project have its own BaseTarget?
   add_library(HEMCOBuildProperties INTERFACE)
endif()

# Put all of HEMCO's mod files in a subdirectory of the build
# directory called mod
set(CMAKE_Fortran_MODULE_DIRECTORY ${HEMCO_BINARY_DIR}/mod)

if(STANDALONE)
   target_include_directories(BaseTarget
   	INTERFACE ${HEMCO_BINARY_DIR}/mod
   )
else()
   target_include_directories(HEMCOBuildProperties
    	INTERFACE ${HEMCO_BINARY_DIR}/mod
   )
endif()

# Append HEMCO's environment variables to CMAKE_PREFIX_PATH
list(APPEND CMAKE_PREFIX_PATH
	# Possible NetCDF environment variables
	$ENV{NetCDF_F_ROOT}         $ENV{NetCDF_C_ROOT}     $ENV{NetCDF_ROOT}
	$ENV{NETCDF_F_ROOT}         $ENV{NETCDF_C_ROOT}     $ENV{NETCDF_ROOT}
	$ENV{NetCDF_Fortran_ROOT}
	$ENV{NETCDF_FORTRAN_ROOT}

	# Possible HEMCO environmnet variables (still needed? -ewl)
	$ENV{GC_F_BIN} 		$ENV{GC_BIN}
	$ENV{GC_F_INCLUDE} 	$ENV{GC_INCLUDE}
	$ENV{GC_F_LIB} 		$ENV{GC_LIB}
)

# Link NetCDF-F to BaseTarget
find_package(NetCDF REQUIRED)
if(STANDALONE)
   target_include_directories(BaseTarget INTERFACE ${NETCDF_INCLUDE_DIRS})
   target_link_libraries(BaseTarget INTERFACE ${NETCDF_LIBRARIES})
else()
   target_include_directories(HEMCOBuildProperties INTERFACE ${NETCDF_INCLUDE_DIRS})
   target_link_libraries(HEMCOBuildProperties INTERFACE ${NETCDF_LIBRARIES})
endif()   

# not sure if we want this for non-standalone
if(STANDALONE)
   # Use the NC_HAS_COMPRESSION definition if nf_def_var_deflate is in netcdf.inc
   if(EXISTS ${NETCDF_F77_INCLUDE_DIR}/netcdf.inc)
   	file(READ ${NETCDF_F77_INCLUDE_DIR}/netcdf.inc NCINC)
   	if("${NCINC}" MATCHES ".*nf_def_var_deflate.*")
   		target_compile_definitions(BaseTarget INTERFACE "NC_HAS_COMPRESSION")
   	endif()
   endif()
endif()

# Print a description of the source code repo's version
# NOTE: this currently is blank so I'm commenting it out
#get_repo_version(HEMCO_REPO_VERSION ${CMAKE_CURRENT_SOURCE_DIR})
#message(STATUS "HEMCO @ ${HEMCO_REPO_VERSION}")

if(NOT HEMCO_EXTERNAL_CONFIG)
	# This conditional block configures the HEMCO build for HEMCO
        # standalone. It sets HEMCO_EXE_TARGETS and it configures
        # the BaseTarget.

	# Set CMAKE_BUILD_TYPE to Release by default
	if(NOT CMAKE_BUILD_TYPE)
		set(CMAKE_BUILD_TYPE "Release"
			CACHE STRING
			"Set the build type"
			FORCE
		)
	endif()

	# Display CMAKE_PREFIX_PATH and CMAKE_BUILD_TYPE
	hemco_pretty_print(SECTION "Useful CMake variables")
	hemco_pretty_print(VARIABLE CMAKE_PREFIX_PATH)
	hemco_pretty_print(VARIABLE CMAKE_BUILD_TYPE)

	# Get the run directory
	hemco_pretty_print(SECTION "Run directory setup")
	set(RUNDIR ".." CACHE PATH "Path to your run directory")
	hemco_pretty_print(VARIABLE RUNDIR)
	message(STATUS "Bootstrapping  ${RUNDIR}")
	get_filename_component(RUNDIR "${RUNDIR}" ABSOLUTE BASE_DIR "${CMAKE_BINARY_DIR}") # Make RUNDIR an absolute path

	## Configure the GEOS-Chem build for GCHP or GC-Classic
	#set(BUILD_WITHOUT_RUNDIR FALSE)
	#if(EXISTS ${RUNDIR}/HEMCO_sa_Spec.rc)
	#	file(STRINGS ${RUNDIR}/HEMCO_sa_Spec.rc )
	#elseif(EXISTS ${RUNDIR}/getRunInfo)
	#	set(GCHP FALSE) # getRunInfo is only in GC-Classic run direcotries
	#elseif((NOT EXISTS RUNDIR) AND (DEFINED RUNDIR_SIM))
	#	set(GCHP FALSE) # special case for building without a run directory
	#	set(RUNDIR ${CMAKE_BINARY_DIR})
	#	set(BUILD_WITHOUT_RUNDIR TRUE)
	#else()
	#	message(FATAL_ERROR "Your run directory doesn't have an input.geos or getRunInfo! Set RUNDIR to a valid run directory.")
	#endif()

	if(STANDALONE)
		# Configure for HEMCO standalone
		include(HEMCO-ConfigureStandalone)
		configureHEMCOStandalone()
	endif()

	# Use the default compiler options (do this for non-standalone too?)
	include(HEMCO-DefaultCompilerOptions)
endif()

# Write BaseTarget's configuration to a file
get_target_property(BT_DEFINITIONS  BaseTarget INTERFACE_COMPILE_DEFINITIONS)
get_target_property(BT_OPTIONS      BaseTarget INTERFACE_COMPILE_OPTIONS)
get_target_property(BT_LIBRARIES    BaseTarget INTERFACE_LINK_LIBRARIES)
get_target_property(BT_INCLUDES     BaseTarget INTERFACE_INCLUDE_DIRECTORIES)
file(WRITE ${CMAKE_BINARY_DIR}/BaseTarget.txt
    "# This file shows the BaseTarget's configuration.\n"
    "\n"
    "BaseTarget::INTERFACE_COMPILE_DEFINITIONS:${BT_DEFINITIONS}\n"
    "BaseTarget::INTERFACE_COMPILE_OPTIONS:${BT_OPTIONS}\n"
    "BaseTarget::INTERFACE_LINK_LIBRARIES:${BT_LIBRARIES}\n"
    "BaseTarget::INTERFACE_INCLUDE_DIRECTORIES:${BT_INCLUDES}\n"
    "CMAKE_Fortran_FLAGS_RELEASE:${CMAKE_Fortran_FLAGS_RELEASE}\n"
    "CMAKE_Fortran_FLAGS_DEBUG:${CMAKE_Fortran_FLAGS_DEBUG}\n"
    "CMAKE_Fortran_FLAGS:${CMAKE_Fortran_FLAGS}\n\n"
)


# Try to compile a simple program that uses NetCDF-Fortran and OpenMP
if(NOT HEMCO_EXTERNAL_CONFIG AND NOT HEMCO_TRY_RUN_PASSED)
	# Format definitions with -D prefix
	set(TRY_RUN_DEFINITIONS "")
	foreach(DEF ${BT_DEFINITIONS})
		list(APPEND TRY_RUN_DEFINITIONS "-D${DEF}")
	endforeach()

	# Try to compile and run try_compile.F90
	try_run(RUN_FAILED COMPILED_OK
		${CMAKE_CURRENT_BINARY_DIR}/try_compile 					# binary dir
		${CMAKE_CURRENT_SOURCE_DIR}/CMakeScripts/try_compile.F90	# test source file
		COMPILE_DEFINITIONS ${TRY_RUN_DEFINITIONS} ${BT_OPTIONS}	# compiler flags
		LINK_LIBRARIES ${BT_LIBRARIES}								# link flags
		CMAKE_FLAGS "-DINCLUDE_DIRECTORIES=${BT_INCLUDES}"			# include directories
		COMPILE_OUTPUT_VARIABLE COMPILE_OUTPUT
		RUN_OUTPUT_VARIABLE RUN_OUTPUT
	)

	# Display a warning if its compilation failed
	if(NOT COMPILED_OK)
		if(OMP)
			set(CONDITIONAL_AND_OMP " and OpenMP")
		endif()
		message(WARNING
			"Failed to compile a simple program that uses NetCDF-Fortran"
			"${CONDITIONAL_AND_OMP}! Could your NetCDF installation "
			"be broken?\nSee \"FailedCompile.txt\" for more info."
		)
		file(WRITE ${CMAKE_BINARY_DIR}/FailedCompile.txt "${COMPILE_OUTPUT}")
	else()
		file(REMOVE ${CMAKE_BINARY_DIR}/FailedCompile.txt)
	endif()

	# Display a warning if its execution failed
	if(RUN_FAILED)
		if(OMP)
			set(CONDITIONAL_AND_OMP "and OpenMP ")
		endif()
		message(WARNING
			"A simple program that uses NetCDF-Fortran "
			"${CONDITIONAL_AND_OMP}compiled successfully, but its execution "
			"failed!\n\nSee \"FailedExecution.txt\" for more info."
		)
		file(WRITE ${CMAKE_BINARY_DIR}/FailedEasyRun.txt "${COMPILE_OUTPUT}\n${RUN_OUTPUT}")
	else()
		file(REMOVE ${CMAKE_BINARY_DIR}/FailedEasyRun.txt ${CMAKE_BINARY_DIR}/simple_xy.nc)
		set(HEMCO_TRY_RUN_PASSED TRUE CACHE INTERNAL "try_run passed" FORCE)
	endif()
endif()

if(NOT GEOS AND NOT STANDALONE)
    add_subdirectory(src)
endif()


