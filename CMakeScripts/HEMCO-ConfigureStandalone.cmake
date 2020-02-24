function(configureHEMCOStandalone)

    # Find OpenMP if we're building a multithreaded executable
    hemco_pretty_print(SECTION "Threading")
    set(OMP "ON" CACHE STRING "Switch to enable/disable OpenMP threading in HEMCO")
    hemco_pretty_print(VARIABLE OMP IS_BOOLEAN)
    if("${OMP}")
       find_package(OpenMP REQUIRED)
       target_compile_options(BaseTarget INTERFACE ${OpenMP_Fortran_FLAGS})
       target_link_libraries(BaseTarget INTERFACE ${OpenMP_Fortran_FLAGS})
    else()
        target_compile_definitions(BaseTarget INTERFACE "NO_OMP")
    endif()

    #hemco_pretty_print(SECTION "General settings")

    # Always set USE_REAL8. See https://github.com/geoschem/geos-chem/issues/43.
    target_compile_definitions(BaseTarget INTERFACE "USE_REAL8")

    #hemco_pretty_print(SECTION "Components")

    # Determine which executables should be built
    set(HEMCO_EXE_TARGETS "hemco_standalone" CACHE STRING "Executable targets that get built as a part of \"all\"")

    # Export the following variables to GEOS-Chem's directory's scope
    set(HEMCO_EXE_TARGETS       ${HEMCO_EXE_TARGETS}        PARENT_SCOPE)
    set(RUNDIR                  ${RUNDIR}                   PARENT_SCOPE)

    ## CPP flags used in HEMCO
    #ESMF_
    #USE_REAL8
    #MODEL_GEOS
    #TOMAS
    #TOMAS12
    #DEVEL
    #MPI
    #MESSY
    #__SX__
    #LINUX_IFORT

endfunction()
