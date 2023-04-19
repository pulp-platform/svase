cmake_minimum_required(VERSION 3.0)
project(svase)

set(CMAKE_CXX_STANDARD 20)

# Handle local prefix (TODO: parameterize)
set(DEP_INSTALL_DIR deps/install)
set(CMAKE_PREFIX_PATH ${DEP_INSTALL_DIR})
link_directories(${DEP_INSTALL_DIR}/lib64)
include_directories(${DEP_INSTALL_DIR}/include)

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
target_link_libraries(svase PRIVATE slang fmt)



