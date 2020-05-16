add_library(JulDayHco STATIC EXCLUDE_FROM_ALL
        hco_julday_mod.F90
)
target_link_libraries(JulDayHco
        PUBLIC HeadersHco
)
target_include_directories(JulDayHco
	INTERFACE ${HEMCO_BINARY_DIR}/mod
)

add_library(GeosUtilHco STATIC EXCLUDE_FROM_ALL
	hco_regrid_a2a_mod.F90
	hco_henry_mod.F90
)
target_link_libraries(GeosUtilHco
	PUBLIC NcdfUtilHco
)
target_include_directories(GeosUtilHco
	INTERFACE ${HEMCO_BINARY_DIR}/mod
)
