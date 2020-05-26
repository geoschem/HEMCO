# HEMCO/src/Interfaces/GCHPctm/CMakeLists.txt

# Add Registry subdirectory
add_subdirectory(Registry)

# Add new library
add_library(HCOI_GCHP STATIC EXCLUDE_FROM_ALL
        HEMCO_GridCompMod.F90
)

# Libraries that must be built before this library
target_link_libraries(HCOI_GCHP
	PUBLIC HCOI_MAPL_ESMF HCOI_Standalone
)

# Where to store mod files
target_include_directories(HCOI_GCHP
	INTERFACE ${HEMCO_BINARY_DIR}/mod
)

# Compile options
target_compile_options(HCOI_GCHP
	PRIVATE
		""
		$<$<STREQUAL:"${CMAKE_Fortran_COMPILER_ID}","GNU">:-ffree-line-length-none>
)

# Add registry as a dependency
add_dependencies(HCOI_GCHP hco_registry)
target_include_directories(HCOI_GCHP PUBLIC ${CMAKE_CURRENT_BINARY_DIR}/Registry)
