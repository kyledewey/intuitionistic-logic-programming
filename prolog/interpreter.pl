:- module('interpreter', [interpret/1]).

varOrAtom(X) :-
        var(X); atom(X); number(X).

structShape(Struct) :-
        Struct =.. [Name|Params],
        atom(Name),
        maplist(varOrAtom, Params).

% Syntax:
%
% a \in Atom
% x \in Variable
% 
% p \in Param ::= a | x | s
% s \in Structure ::= a(\vec{p})
% c \in Clause ::= s :- q.
% q \in Query ::= true | false | q_1, q_2 | q_1 ; q_2 | s | q <= s

% -Query: Query
% -Facts: [HypotheticalFact]
%
interpret(true, _) :- !.
interpret(false, _) :- fail.
interpret((Q1, Q2), Facts) :-
        !,
        interpret(Q1, Facts),
        interpret(Q2, Facts).
interpret((Q1; Q2), Facts) :-
        !, % don't try to treat it as a fact or call
        (interpret(Q1, Facts); interpret(Q2, Facts)).
interpret(<=(Q, S), Facts) :-
        !, % don't try to treat it as a fact or call
        structShape(S),
        interpret(Q, [S|Facts]).
interpret(Q, Facts) :-
        structShape(Q),
        member(Q, Facts).
interpret(Call, Facts) :-
        structShape(Call),
        clause(Call, Body),
        interpret(Body, Facts).

% -Query: Query
interpret(Query) :-
        interpret(Query, []).
