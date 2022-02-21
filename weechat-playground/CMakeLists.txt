# cmake -G"Unix Makefiles" -DCMAKE_BUILD_TYPE=Debug ..
# make VERBOSE=1
cmake_minimum_required(VERSION 3.10)

project(bs-weechat-playground VERSION 0.1 DESCRIPTION "Banshee-Sympatico Weechat Playground")

add_library(bs-weechat-playground SHARED
  playground.cpp)

target_compile_options(bs-weechat-playground PRIVATE
  -Wall -Wextra -Wpedantic -Werror)

# TODO: not working
string(APPEND CMAKE_CXX_FLAGS_DEBUG " -ggdb3 -O0")
