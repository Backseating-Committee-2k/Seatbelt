add_executable(
        Tests
        tests.cpp
)

target_link_libraries(Tests PRIVATE GTest::gtest GTest::gtest_main GTest::gmock GTest::gmock_main)
message(STATUS "gtest include directory: ${gtest_SOURCE_DIR}")
add_dependencies(Tests Seatbelt)

target_include_directories(Tests PUBLIC
        ${PROJECT_SOURCE_DIR}/src
        )

# set warning levels
if (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    message("MSVC build")
    target_compile_options(Tests PUBLIC /W4 /permissive- /fsanitize=address)
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    message("GCC build")
    target_compile_options(Tests PUBLIC -Wall -Wextra -pedantic -Wconversion -pthread)
    target_link_libraries(Tests PRIVATE TBB::tbb)
endif ()

# define DEBUG_BUILD
target_compile_definitions(Tests PUBLIC "$<$<CONFIG:DEBUG>:DEBUG_BUILD>")

# static runtime library
set_property(TARGET Tests PROPERTY
        MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")

# set binary filenames
set_target_properties(Tests PROPERTIES OUTPUT_NAME_DEBUG Tests-debug)
set_target_properties(Tests PROPERTIES OUTPUT_NAME_RELWITHDEBINFO Tests-relwithdebinfo)
set_target_properties(Tests PROPERTIES OUTPUT_NAME_RELEASE Tests-release)
set_target_properties(Tests PROPERTIES OUTPUT_NAME_MINSIZEREL Tests-minsizerel)

if (CMAKE_BUILD_TYPE STREQUAL "Release")
    message("Enabling LTO for target Tests")
    set_property(TARGET Tests PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
else ()
    message("Not enabling LTO for target Tests (not a release build)")
endif ()

add_test(
        NAME
        unit
        COMMAND
        Tests
)
