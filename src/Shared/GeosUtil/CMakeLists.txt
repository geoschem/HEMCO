add_library(JulDayHco STATIC EXCLUDE_FROM_ALL
        julday_mod.F
)
target_link_libraries(JulDayHco
        PUBLIC HeadersHco
)

add_library(GeosUtilHco STATIC EXCLUDE_FROM_ALL
	regrid_a2a_mod.F90
	henry_mod.F
)
target_link_libraries(GeosUtilHco
	PUBLIC NcdfUtilHco
)
