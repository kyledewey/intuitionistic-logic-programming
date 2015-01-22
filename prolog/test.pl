:- use_module('interpreter.pl').

unify(X, X).

class(his101).
class(eng201).

student(jill).
student(tony).

take(tony, his101).

take(jill, eng201).
take(jill, his101).

grad(S) :-
        take(S, his101), take(S, eng201).

%% ?- interpret((<=(grad(S), take(S, eng201)))).
%% S = tony ;
