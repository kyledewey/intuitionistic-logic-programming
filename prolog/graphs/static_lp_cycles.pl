vertex(a, foo).
vertex(b, bar).
vertex(c, baz).
vertex(d, hello).

edge(a, a).
edge(a, b).
edge(b, c).
edge(c, d).
edge(b, a).

% -StartVertex: Vertex
% -What: Label
% -VertexWithLabel: Vertex
%
% We get infinite looping with cycles
findVertexLabeled(StartVertex, What, StartVertex) :-
        % Our starting vertex is labeled with what we want
        vertex(StartVertex, What).
findVertexLabeled(StartVertex, What, ResultVertex) :-
        % fan out to children
        edge(StartVertex, NextVertex),
        findVertexLabeled(NextVertex, What, ResultVertex).

% -StartVertex: Vertex
% -What: Label
% -Seen: [Vertex]
% -VertexWithLabel: Vertex
%
% Like `findVertexLabeled`, but it works correctly with respect
% to cycles.
%
findVertexLabeledHandlesCycles(StartVertex, What, _, StartVertex) :-
        vertex(StartVertex, What).
findVertexLabeledHandlesCycles(StartVertex, What, Seen, ResultVertex) :-
        edge(StartVertex, NextVertex),
        \+ member(NextVertex, Seen),
        findVertexLabeledHandlesCycles(
            NextVertex, What,
            [NextVertex|Seen], ResultVertex).

% -StartVertex: Vertex
% -What: Label
% -VertexWithLabel: Vertex
findVertexLabeledHandlesCycles(StartVertex, What, VertexWithLabel) :-
        findVertexLabeledHandlesCycles(
            StartVertex, What, [StartVertex], VertexWithLabel).
