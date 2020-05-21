# HEMCO/src/Interfaces/GCHP/Registry/CMakeLists.txt

# Build HEMCO registry
add_custom_command(OUTPUT 
		${CMAKE_CURRENT_BINARY_DIR}/HEMCO_DeclarePointer___.h  
		${CMAKE_CURRENT_BINARY_DIR}/HEMCO_GetPointer___.h  
		${CMAKE_CURRENT_BINARY_DIR}/HEMCO_InternalSpec___.h
		hco_registry.stamp
	COMMAND ${MAPL_ACG} -v ${CMAKE_CURRENT_SOURCE_DIR}/HEMCO_Registry.rc
	COMMAND cmake -E touch hco_registry.stamp
	COMMENT "Building HEMCO registry..."
	MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/HEMCO_Registry.rc
	WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}	
)
add_custom_target(hco_registry DEPENDS hco_registry.stamp)
