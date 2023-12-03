cmake_minimum_required(VERSION 3.5)

project(nastran_bd Fortran)

set(CMAKE_Fortran_FLAGS "-Wall -ffree-line-length-none -w -fno-range-check -fno-automatic -fcray-pointer -std=legacy -fallow-invalid-boz")

file(GLOB_RECURSE SRCS "*.f90")
add_library(${PROJECT_NAME} STATIC ${SRCS})
set_property(TARGET ${PROJECT_NAME} PROPERTY POSITION_INDEPENDENT_CODE ON)

