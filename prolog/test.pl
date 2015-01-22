:- use_module('interpreter.pl').

unify(X, X).

take(tony, his101).

grad(S) :-
        take(S, his101), take(S, eng201).

%% ?- interpret((<=(grad(S), take(S, eng201)))).
%% S = tony ;
