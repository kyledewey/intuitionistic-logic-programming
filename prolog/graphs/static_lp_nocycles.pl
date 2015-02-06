vertex(a, foo).
vertex(b, bar).
vertex(c, baz).
vertex(d, hello).

edge(a, b).
edge(b, c).
edge(c, d).

% -StartVertex: Vertex
% -What: Label
% -VertexWithLabel: Vertex
findVertexLabeled(StartVertex, What, StartVertex) :-
        % Our starting vertex is labeled with what we want
        vertex(StartVertex, What).
findVertexLabeled(StartVertex, What, ResultVertex) :-
        % fan out to children
        edge(StartVertex, NextVertex),
        findVertexLabeled(NextVertex, What, ResultVertex).

