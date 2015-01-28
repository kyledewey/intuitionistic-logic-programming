startingVertices(
        [vertex(a, foo),
         vertex(b, bar),
         vertex(c, baz),
         vertex(d, hello)]).

startingEdges(
        [edge(a, a),
         edge(a, b),
         edge(b, c),
         edge(c, d),
         edge(b, a)]).

% -StartVertex: Vertex
% -What: Label
% -Vertices: [vertex(Name, Label)]
% -Edges: [edge(Name, Name)]
% -Seen: [Name]
% -VertexWithLabel: Vertex
findVertexLabeled(Vertex, What, Vertices, _, _, Vertex) :-
        member(vertex(Vertex, What), Vertices).
findVertexLabeled(StartVertex, What, Vertices,
                  Edges, Seen, ResultVertex) :-
        member(edge(StartVertex, NextVertex), Edges),
        \+ member(NextVertex, Seen),
        findVertexLabeled(NextVertex, What, Vertices,
                          Edges, [NextVertex|Seen],
                          ResultVertex).

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
            [HypEdge|Edges], [StartVertex],
            VertexWithLabel).
