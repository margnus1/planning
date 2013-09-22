planning
========

A simple planning problem solver

It uses an arbitrary and ambigous encoding of the PDDL language in JSON,
however, it should be straight-forward to add a parser for proper PDDL to it.

compiling
---------
It is compiled using the [MLton](http://mlton.org) whole-program optimising Standard-ML compiler

    mlton planning.mlb

usage
-----
To solve a problem using breadth-first search

    ./planning bfs [problem.json]

To generate the entire state graph of a problem as a GraphViz graph

    ./planning stategraph [problem.json] > [result.dot]
