startingVertices(
        [vertex(a, foo),
         vertex(b, bar),
         vertex(c, baz),
         vertex(d, hello)]).

startingEdges(
        [edge(a, b),
         edge(b, c),
         edge(c, d)]).

% -StartVertex: Vertex
% -What: Label
% -Vertices: [vertex(Name, Label)]
% -Edges: [edge(Name, Name)]
% -VertexWithLabel: Vertex
findVertexLabeled(Vertex, What, Vertices, _, Vertex) :-
        member(vertex(Vertex, What), Vertices).
findVertexLabeled(StartVertex, What, Vertices,
                  Edges, ResultVertex) :-
        member(edge(StartVertex, NextVertex), Edges),
        findVertexLabeled(NextVertex, What, Vertices,
                          Edges, ResultVertex).

% -StartVertex: Vertex
% -What: Label
% -HypEdge: edge(Name, Name)
% -VertexWithLabel: Vertex
findVertexLabeledHypEdge(
    StartVertex, What, HypEdge, VertexWithLabel) :-
        startingVertices(Vertices),
        startingEdges(Edges),
        findVertexLabeled(
            StartVertex, What, Vertices,
            [HypEdge|Edges], VertexWithLabel).
