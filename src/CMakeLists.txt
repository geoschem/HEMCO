add_subdirectory(Core)
add_subdirectory(Extensions)
add_subdirectory(Interfaces)

#############################################################
# If using in GEOSgcm, comment out above and uncomment below
#############################################################
#esma_set_this ()
#
#set (src_directories
#   ${CMAKE_CURRENT_SOURCE_DIR}
#   ${CMAKE_CURRENT_SOURCE_DIR}/Shared
#   ${CMAKE_CURRENT_SOURCE_DIR}/Core
#   ${CMAKE_CURRENT_SOURCE_DIR}/Extensions
#   ${CMAKE_CURRENT_SOURCE_DIR}/Interfaces
#   )
#
#set (srcs)
#foreach (dir ${src_directories})
#  file (GLOB tmpsrcs ${dir}/*.[fF] ${dir}/*.[fF]90 ${dir}/*.c)
#  list (APPEND srcs ${tmpsrcs})
#endforeach()
#
## These files contain Fortran MODULE statements that are
## deactivated by CPP statements.  CMake incorrectly determines
## that the file produces the corresponding .mod file, but they do not.
## Essentially a clash between using CPP to deactivate a file vs
## the config system.  The latter is preferable.  TLC
#list (REMOVE_ITEM srcs
#   ${CMAKE_CURRENT_SOURCE_DIR}/Extensions/hcox_tomas_dustdead_mod.F
#   ${CMAKE_CURRENT_SOURCE_DIR}/Extensions/hcox_tomas_seasalt_mod.F90
#   ${CMAKE_CURRENT_SOURCE_DIR}/Extensions/hcox_tomas_jeagle_mod.F90
#   )
#
#
#esma_add_library (${this} 
#   SRCS ${srcs}
#   DEPENDENCIES MAPL_Base
#   )
#
#target_include_directories (${this} PUBLIC ${INC_ESMF} ${INC_NETCDF})
#
#target_compile_definitions (${this} PRIVATE ESMF_ DEVEL NO_BPCH UCX 
#   EXTERNAL_GRID EXTERNAL_FORCING GEOS_FP USE_REAL8 USE_TEND NC_DIAG DISCOVER MODEL_GEOS)
#set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})
