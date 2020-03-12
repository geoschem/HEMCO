function(configureHEMCOGCHPctm)

    # Find OpenMP if we're building a multithreaded executable
    hemco_pretty_print(SECTION "Threading")
    set(OMP "OFF" CACHE STRING "Switch to enable/disable OpenMP threading in HEMCO")
    hemco_pretty_print(VARIABLE OMP IS_BOOLEAN)
    if("${OMP}")
       find_package(OpenMP REQUIRED)
       target_compile_options(HEMCOBuildProperties
		INTERFACE ${OpenMP_Fortran_FLAGS}
       )
       target_link_libraries(HEMCOBuildProperties
		INTERFACE ${OpenMP_Fortran_FLAGS}
       )
    else()
        target_compile_definitions(HEMCOBuildProperties
		INTERFACE "NO_OMP"
	)
    endif()

    #hemco_pretty_print(SECTION "General settings")

    # Always set USE_REAL8. See https://github.com/geoschem/geos-chem/issues/43.
    target_compile_definitions(HEMCOBuildProperties
	INTERFACE "USE_REAL8"
    )

    # Always set ESMF_
    target_compile_definitions(HEMCOBuildProperties
	INTERFACE "ESMF_"
    )

    #hemco_pretty_print(SECTION "Components")

endfunction()
