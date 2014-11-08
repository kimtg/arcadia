# Building with cmake:
# Create and enter a build directory
# If compiling with GNU readline run 'cmake .. && make'
# If compiling without GNU readline run 'cmake -DREAD_LINE=0 .. && make'

project(arcadia)
cmake_minimum_required(VERSION 2.8)

# Source files
set(SOURCES arcadia.c arc.c)

# If it's not specified, set the READ_LINE macro to the default
if (NOT DEFINED READ_LINE)
	set(READ_LINE 1)
endif()
ADD_DEFINITIONS(-DREAD_LINE=${READ_LINE})

# The target executable
add_executable(arcadia ${SOURCES})

# Always link stdmath
target_link_libraries(arcadia m)

# Only link GNU readline if we're compiling using it
if (READ_LINE)
	target_link_libraries(arcadia readline)
endif()
