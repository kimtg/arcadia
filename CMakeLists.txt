# Building with cmake:
# Create and enter a build directory
# If compiling without GNU readline run 'cmake .. && make'
# If compiling with GNU readline run 'cmake -DREADLINE=1 .. && make'

project(arcadia)
cmake_minimum_required(VERSION 2.8)

# Source files
set(SOURCES arcadia.c arc.c)

# The target executable
add_executable(arcadia ${SOURCES})

# Always link stdmath
target_link_libraries(arcadia m)

# Only link GNU readline if we're compiling using it
if (READLINE)
	target_link_libraries(arcadia m readline)
endif()
