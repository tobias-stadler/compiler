add_executable(tools_cli CLI.cpp)
target_link_libraries(tools_cli PRIVATE core)

add_subdirectory(dsl)
