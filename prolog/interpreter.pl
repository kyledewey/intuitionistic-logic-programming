:- module('interpreter', [interpret/1]).

% Behaves like an assert in a non-logical language.
% -Clause to call
% -Error message
ensureSuccess(Clause, _, _) :-
        call(Clause), !.
ensureSuccess(_, Err, Params) :-
        write('ERROR: '),
        format(Err, Params),
        nl, nl,

        % while the documentation says this can be passed as an option to
        % `get_prolog_backtrace`, `get_prolog_backtrace` doesn't appear to
        % honor it.
        set_prolog_flag(backtrace_goal_depth, 50),
        get_prolog_backtrace(50, Backtrace),
        print_prolog_backtrace(user_error, Backtrace, [subgoal_positions(true)]),
        halt.

% Cuts are used here as a way of getting the usual nondeterministic
% Prolog semantics into more of a deterministic pattern-matching semantics.

% -Query: Query
%
% Fails if what we have is not a valid query.
query(true) :- !.
query(false) :- !.
query((Q1, Q2)) :-
        !,
        query(Q1),
        query(Q2).
query((Q1; Q2)) :-
        !,
        query(Q1),
        query(Q2).
query(<=(Q, _)) :-
        !,
        % trying to check the structure is pointless, because it will
        % always be a structure in Prolog anyway.  Here it's all about
        % context.  For example, I could have the struct `,(foo, bar)`,
        % which will be treated differently from conjunction as it is
        % in the position of an intuitionistic fact.
        query(Q).
query(Call) :-
        % Make sure such a clause exists.  We cannot pass the call as-is
        % to clause, since it may not unify with the head of a clause in
        % the database.
        Call =.. [Name|Params],
        length(Params, NumParams),
        length(EmptyParams, NumParams),
        Probe =.. [Name|EmptyParams],

        % we only need to know there is at least one such clause
        once(clause(Probe, _)).

ensureQuery(Query) :-
        ensureSuccess(query(Query),
                      'Syntactically ill-formed query: ~w~n',
                      [Query]).

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
interpret(false, _) :- !, fail.
interpret((Q1, Q2), Facts) :-
        !,
        interpret(Q1, Facts),
        interpret(Q2, Facts).
interpret((Q1; Q2), Facts) :-
        !,
        (interpret(Q1, Facts); interpret(Q2, Facts)).
interpret(<=(Q, S), Facts) :-
        !,
        interpret(Q, [S|Facts]).
interpret(Q, Facts) :-
        member(Q, Facts).
interpret(Call, Facts) :-
        clause(Call, Body),
        ensureQuery(Body),
        interpret(Body, Facts).

% -Query: Query
interpret(Query) :-
        ensureQuery(Query),
        interpret(Query, []).
