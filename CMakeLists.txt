cmake_minimum_required(VERSION 3.0)
project(svase)

set(CMAKE_CXX_STANDARD 20)

option(CMAKE_BUILD_SHARED_LIBS "Build shared libraries" OFF)
option(CMAKE_BUILD_PORTABLE "Build fully portable (no shared libc), not recommended with glibc" OFF)
# used on alpine builder, see: https://build-your-own.org/blog/20221229_alpine/

# Handle local prefix (TODO: parameterize)
set(DEP_INSTALL_DIR deps/install)
set(CMAKE_PREFIX_PATH ${DEP_INSTALL_DIR})
link_directories(${DEP_INSTALL_DIR}/lib64)
link_directories(${DEP_INSTALL_DIR}/lib)
include_directories(${DEP_INSTALL_DIR}/include)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Werror")
set(CMAKE_BUILD_TYPE Debug)

# Dependencies
find_package(slang REQUIRED)
find_package(fmt REQUIRED)
find_package(cxxopts REQUIRED)

# Build and link
add_executable(svase
        src/design.cpp
        src/rewrite.cpp
        src/driver.cpp
        src/diag.cpp)
target_include_directories(svase PRIVATE include)

if (CMAKE_BUILD_SHARED_LIBS)
    target_link_libraries(svase PRIVATE svlang fmt)
else()
    if (CMAKE_BUILD_PORTABLE)
        target_link_libraries(svase PRIVATE svlang fmt -static)
    else()
        target_link_libraries(svase PRIVATE svlang fmt -static-libgcc -static-libstdc++)
    endif()
endif()


