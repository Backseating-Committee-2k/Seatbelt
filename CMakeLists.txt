cmake_minimum_required(VERSION 3.21)
project(Seatbelt)
include(FetchContent)

set(CMAKE_CXX_STANDARD 23)

FetchContent_Declare(
        fmt
        GIT_REPOSITORY https://github.com/fmtlib/fmt.git
        GIT_TAG c4ee726532178e556d923372f29163bd206d7732
)

FetchContent_Declare(
        ctre
        GIT_REPOSITORY https://github.com/hanickadot/compile-time-regular-expressions.git
        GIT_TAG 331aebc79d715d1d0036d2d32021584eefecbcc9
)

FetchContent_Declare(
        Arguably
        GIT_REPOSITORY https://github.com/mgerhold/arguably
        GIT_TAG v0.1.8
)

FetchContent_Declare(
        googletest
        GIT_REPOSITORY https://github.com/google/googletest.git
        GIT_TAG 58d77fa8070e8cec2dc1ed015d66b454c8d78850
)

FetchContent_MakeAvailable(fmt ctre Arguably googletest)

if (MSVC)
    # enable code analysis
    set_property(GLOBAL PROPERTY VS_GLOBAL_EnableCppCoreCheck true)
    set_property(GLOBAL PROPERTY VS_GLOBAL_CodeAnalysisRuleSet CppCoreCheckRules.ruleset)
    set_property(GLOBAL PROPERTY VS_GLOBAL_RunCodeAnalysis true)

    # remove /W3 from defaults
    string(REGEX REPLACE "/W3" "" CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS})
    string(REGEX REPLACE "-W3" "" CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS})
endif ()

set(TARGET_LIST Seatbelt fmt gmock gmock_main gtest gtest_main)

add_executable(Seatbelt
        src/main.cpp
        src/lexer.cpp
        src/lexer.hpp
        src/types.hpp
        src/parser.cpp
        src/parser.hpp
        src/location.hpp
        src/source_code.hpp
        src/emitter.cpp
        src/emitter.hpp
        src/error.cpp
        src/error.hpp
        src/type_checker.cpp
        src/type_checker.hpp
        src/scope_generator.hpp
        src/scope.hpp
        src/scope_generator.cpp
        src/data_type.hpp
        src/type_container.cpp
        src/type_container.hpp
        src/parameter_list.hpp
        src/scope.cpp
        src/namespace.hpp
        src/namespace.cpp
        src/return_type_checker.hpp
        src/return_type_checker.cpp
        src/mutability.hpp
        src/bssembly.hpp
        src/overloaded.hpp)

target_link_libraries(Seatbelt fmt ctre Arguably)

foreach (target ${TARGET_LIST})
    # set warning levels
    if (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
        message("MSVC build")
        target_compile_options(${target} PUBLIC /W4 /WX /permissive- /fsanitize=address)
    elseif (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
        message("GCC build")
        target_compile_options(${target} PUBLIC -Wall -Wextra -Werror -pedantic -Wconversion -pthread -fsanitize=address,undefined)
        target_link_options(${target} PUBLIC -fsanitize=address,undefined)
    endif ()

    # define DEBUG_BUILD
    target_compile_definitions(${target} PUBLIC "$<$<CONFIG:DEBUG>:DEBUG_BUILD>")

    # static runtime library
    set_property(TARGET ${target} PROPERTY
            MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")

    if (CMAKE_BUILD_TYPE STREQUAL "Release")
        message("Enabling LTO for target ${target}")
        set_property(TARGET ${target} PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
    else ()
        message("Not enabling LTO for target ${target} (not a release build)")
    endif ()
endforeach ()

enable_testing()
add_subdirectory(test)
