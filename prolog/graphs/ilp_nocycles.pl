:- use_module('../interpreter.pl').

vertex(a, foo).
vertex(b, bar).
vertex(c, baz).
vertex(d, hello).

edge(a, b).
edge(b, c).
edge(c, d).

findVertexLabeled(StartVertex, What, StartVertex) :-
        vertex(StartVertex, What).
findVertexLabeled(StartVertex, What, ResultVertex) :-
        edge(StartVertex, NextVertex),
        findVertexLabeled(NextVertex, What, ResultVertex).
