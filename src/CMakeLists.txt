cmake_minimum_required(VERSION 3.9)

project(LLVMBootCamp)

# for LLVM
find_package(LLVM REQUIRED CONFIG)

# for threading library
find_package(Threads REQUIRED)

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

execute_process(
    COMMAND ${LLVM_TOOLS_BINARY_DIR}/llvm-config-13 --cxxflags
    COMMAND tr -d \n
    OUTPUT_VARIABLE LLVM_CXXFLAGS
)

execute_process(
    COMMAND ${LLVM_TOOLS_BINARY_DIR}/llvm-config-13 --ldflags
    COMMAND tr -d \n
    OUTPUT_VARIABLE LLVM_LDFLAGS
)

execute_process(
    COMMAND ${LLVM_TOOLS_BINARY_DIR}/llvm-config-13 --libfiles
    COMMAND tr -d \n
    OUTPUT_VARIABLE LLVM_LIBFILES
)

# build options
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "release")
endif()

#set(CMAKE_CXX_FLAGS "-Wno-unused-command-line-argument -Wno-unknown-warning-option ${LLVM_CXXFLAGS} -Wno-gnu-zero-variadic-macro-arguments -fno-rtti -fPIC")
set(CMAKE_CXX_FLAGS "${LLVM_CXXFLAGS}")
set(CMAKE_CXX_FLAGS_DEBUG "-g -O0")
set(CMAKE_CXX_FLAGS_RELEASE "-O3 -DNDEBUG")
set(CMAKE_EXE_LINKER_FLAGS "${LLVM_LDFLAGS} ${LLVM_LIBFILES}")

# print status
message(STATUS "Build type: -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}")
message(STATUS "Using LLVMConfig.cmake in: -DLLVM_DIR=${LLVM_DIR}")
message(STATUS "LLVM version: ${LLVM_PACKAGE_VERSION}")
message(STATUS "LLVM C++ compiler flags: ${LLVM_CXXFLAGS}")
message(STATUS "LLVM linker flags: ${LLVM_LDFLAGS}")

# build targets
FILE(GLOB CPPSources *.cpp)
FILE(GLOB CPPHeaders *.hpp)
FILE(GLOB CPPMain main.cpp)

list(REMOVE_ITEM CPPSources ${CPPMain} ${CPPGreen})

if(CMAKE_THREAD_LIBS_INIT)
    set(LIBS ${CMAKE_THREAD_LIBS_INIT} LLVM)
else()
    set(LIBS LLVM)
endif()

add_executable(llvm_boot_camp ${CPPMain})
target_link_libraries(llvm_boot_camp ${LIBS})
