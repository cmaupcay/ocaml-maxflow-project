Complete Ocaml graph project for functional programming course.

Authors :
 - Raphaël Bocquel
 - Clément Maupuy

This project implements both the classic Ford-Fulkerson algorithm to find the maximum flow graph from a capacity graph, 
and a version that uses the Bellman-Ford pathfinding algorithm to find the minimum cost, maximum flow graph from a capacity and cost graph.
The latter uses randomness to ensure non-determinism.

It also implements a simple, custom input file format that lists university and students. This format lisst universities and the number of students they can accomodate,
as well as students and their universities of choice (see graph/student_format/students_format1.txt for an exemple).

The minimum cost maximum flow algorithm may be tested by running `make build`, then `./target/attribute_univ.exe inputpath outputpath`. 
All directories in the input and output paths provided must exist. The input file must adhere to the custom format. 
The output will be a Graphviz format text file and a SVG format dot file representing the resulting flow graph.

All modules and function, including both main algorithms, can be tested by running `make test`.

A [`Makefile`](Makefile) provides some useful commands:

 - `make build` to compile. This creates an `ftest.exe` executable
 - `make demo` to run the `ftest` program with some arguments
 - `make format` to indent the entire project
 - `make edit` to open the project in VSCode
 - `make clean` to remove build artifacts
 - `make test` to test all modules