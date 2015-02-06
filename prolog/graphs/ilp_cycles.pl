:- use_module('../interpreter.pl').

vertex(a, foo).
vertex(b, bar).
vertex(c, baz).
vertex(d, hello).

edge(a, a).
edge(a, b).
edge(b, c).
edge(c, d).
edge(b, a).

findVertexLabeled(StartVertex, What, StartVertex) :-
        vertex(StartVertex, What).
findVertexLabeled(StartVertex, What, ResultVertex) :-
        edge(StartVertex, NextVertex),
        not(seen(NextVertex)),
        =>(seen(NextVertex),
           findVertexLabeled(NextVertex, What, ResultVertex)).

findVertexLabeledFromStart(StartVertex, What, ResultVertex) :-
        =>(seen(StartVertex),
           findVertexLabeled(StartVertex, What, ResultVertex)).
