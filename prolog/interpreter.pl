:- module('interpreter', [interpret/1]).

% These rules behave according to those in
% "A Logic for Hypothetical Reasoning", by Anthony Bonner.
%
% There are some important differences from what's exactly shown
% in the paper:
%
% 1. We have a separation of horn clauses from intuitionistic facts.
%    The paper shows these as part of the same ruleset `R`, but if
%    this were true, then we would be allowed to have intuitionistic
%    rules (not just facts) added, which is disallowed in the rules.
%    Additionally, the rules are completely ambiguous as to what happens
%    with unifications involving intuitionistic facts and horn clauses.
%    With horn clauses, unification should be treated much like beta
%    reduction in the lambda calculus, BUT with intuitionistic facts,
%    the variables in a fact need to be in sync with the facts added.
%    That is, while unification with horn clauses can add new variables,
%    adding intuitionistic facts or unifying against intuitionistic facts
%    must NOT add new variables for correctness.
%
% 2. Both the horn clause knowledgebase and the intuitionistic facts are
%    treated as lists as opposed to sets.  This might have just been
%    notational puns in the paper, as semantically both of these should
%    clearly be lists (i.e., if I have the same fact or rule multiple times,
%    then I need to nondeterministically execute both, which is consistent
%    with Prolog semantics).


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
interpret(true, _) :- !. % standard
interpret(false, _) :- !, fail. % standard
interpret((Q1, Q2), Facts) :- % standard
        !,
        interpret(Q1, Facts),
        interpret(Q2, Facts).
interpret((Q1; Q2), Facts) :- % standard
        !,
        (interpret(Q1, Facts); interpret(Q2, Facts)).
interpret(<=(Q, S), Facts) :- % ILP only
        !,

        % Interpret Q under our current ruleset with the additional
        % intuitionistic fact S.  Facts are considered in reverse
        % chronological order from when they were added, with the most
        % recent facts being considered before earlier facts.
        interpret(Q, [S|Facts]).
interpret(Q, Facts) :- % ILP only
        % Nondeterministically pick a fact that Q matches on.
        % We intentionally don't use cut here, because it
        % is possible (and very likely) that an intuitionistic
        % fact bears the same name and arity as the head of
        % a horn clause, and so we want to permit backtracking
        % to calls.  We consider intuitionistic facts before calls.
        member(Q, Facts).
interpret(Call, Facts) :- % standard
        % Nondeterministically select a clause in the knowledge base
        % whoose head unifies with the given call.  The body returned
        % gives us what's left to execute, after introducing new
        % variables.  This closely corresponds to beta reduction in
        % the lambda calculus.
        clause(Call, Body),
        ensureQuery(Body),
        interpret(Body, Facts).

% -Query: Query
interpret(Query) :-
        ensureQuery(Query),
        interpret(Query, []).
