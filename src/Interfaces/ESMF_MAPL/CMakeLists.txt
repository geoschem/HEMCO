# HEMCO/src/Interfaces/ESMF_MAPL/CMakeLists.txt

add_library(HCOI_ESMF_MAPL STATIC EXCLUDE_FROM_ALL
        hcoi_esmf_mod.F90
)
target_link_libraries(HCOI_ESMF_MAPL
	PUBLIC HCOI_Shared
)
target_include_directories(HCOI_ESMF_MAPL
	INTERFACE ${HEMCO_BINARY_DIR}/mod
)
target_compile_options(HCOI_ESMF_MAPL
	PRIVATE
		""
		$<$<STREQUAL:"${CMAKE_Fortran_COMPILER_ID}","GNU">:-ffree-line-length-none>
)
