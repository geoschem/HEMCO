# HEMCO/src/Interfaces/CMakeLists.txt

# Always build shared interface files
add_subdirectory(Shared)

# For now, always build standalone interface files
add_subdirectory(Standalone)

# if GCHP CTM:
if(HEMCO_EXTERNAL_CONFIG AND MAPL_ESMF)
    add_subdirectory(MAPL_ESMF)
    add_subdirectory(GCHPctm)
endif()
