add_library(JulDay STATIC EXCLUDE_FROM_ALL
        julday_mod.F
)
target_link_libraries(JulDay
        PUBLIC Headers
)

add_library(GeosUtil STATIC EXCLUDE_FROM_ALL
	regrid_a2a_mod.F90
	henry_mod.F
)
target_link_libraries(GeosUtil
	PUBLIC NcdfUtil
)
