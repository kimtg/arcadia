project(arcadia)
cmake_minimum_required(VERSION 2.8)

# no headers
set(HEADERS )

# there is a single source file
set(SOURCES arcadia.c)

add_executable(arcadia ${HEADERS} ${SOURCES})

