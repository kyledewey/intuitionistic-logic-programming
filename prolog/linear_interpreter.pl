:- module('linear_interpreter', [interpret/1]).

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

% Below are some key implementation notes:
%
% 1. I'm piggybacking off of Prolog for quite a bit, specifically for:
%    - The syntax
%    - The rulebase
%    - Code for handling the rulebase and performing calls
%    - Unification
%    - Nondeterminism
%    - Conjunction
%    - Disjunction
%    - Negation-as-failure
%
%    Observe that ILP is just classical logic programming with intuitionistic
%    implication, which is why most rules are handled simply by deferring
%    to Prolog.
%
% 2. I handle only intuitionistic facts.  I'm not sure how intuitionistic
%    rules would work, though it is my guess that if we had these, we would
%    need some way to say that a variable should be universally quantified.
%    That is, we need to differentiate between variables that are introduced
%    in the rule being added, and variables which are used in the surrounding
%    scope.

% -[Fact]
% -Fact
%
% Succeeds if the given fact is present in the series of facts.
% Uses == instead of unification.
factPresent([H|_], Fact) :-
        H == Fact.
factPresent([_|T], Fact) :-
        factPresent(T, Fact).

% Syntax:
%
% a \in Atom
% x \in Variable
% 
% p \in Param ::= a | x | s
% s \in Structure ::= a(\vec{p})
% c \in Clause ::= s :- q.
% q \in Query ::= true         % always succeeds
%                 | false      % always fails
%                 | q_1, q_2   % conjunction
%                 | q_1 ; q_2  % disjunction
%                 | s          % calls
%                 | q <= s     % intuitionistic implication
%                 | s => q     % intuitionistic implication
%
% -Query: Query
% -InputFacts: [HypotheticalFact]
% -OutputFacts: [HypotheticalFact]
%
interpret(true, Facts, Facts) :- !. % standard
interpret(false, Facts, Facts) :- !, fail. % standard
interpret((Q1, Q2), InputFacts, OutputFacts) :- % standard
        !,
        interpret(Q1, InputFacts, TempFacts),
        interpret(Q2, TempFacts, OutputFacts).
interpret((Q1; Q2), InputFacts, OutputFacts) :- % standard
        !,
        (interpret(Q1, InputFacts, OutputFacts);
         interpret(Q2, InputFacts, OutputFacts)).
interpret(<=(Q, S), InputFacts, OutputFacts) :- % ILP only
        !,
        interpretHypothetical(Q, S, InputFacts, OutputFacts).
interpret(=>(S, Q), InputFacts, OutputFacts) :- % ILP only
        !,
        interpretHypothetical(Q, S, InputFacts, OutputFacts).
interpret(not(Q), Facts, Facts) :- % standard
        !,
        \+ interpret(Q, Facts, _).
interpret(Q, InputFacts, OutputFacts) :- % ILP only
        % Nondeterministically pick a fact that Q matches on.
        % We intentionally don't use cut here, because it
        % is possible (and very likely) that an intuitionistic
        % fact bears the same name and arity as the head of
        % a horn clause, and so we want to permit backtracking
        % to calls.  We consider intuitionistic facts before calls.
        select(Q, InputFacts, OutputFacts).
interpret(Call, InputFacts, OutputFacts) :- % standard
        % Nondeterministically select a clause in the knowledge base
        % whoose head unifies with the given call.  The body returned
        % gives us what's left to execute, after introducing new
        % variables.  This closely corresponds to beta reduction in
        % the lambda calculus.
        clause(Call, Body),
        interpret(Body, InputFacts, OutputFacts).

% -Query
% -Fact
% -InputFacts: [Fact]
% -OutputFacts: [Fact]
interpretHypothetical(Query, Fact, InputFacts, OutputFacts) :-
        % Interpret Q under our current ruleset with the additional
        % intuitionistic fact S.  Facts are considered in reverse
        % chronological order from when they were added, with the most
        % recent facts being considered before earlier facts.

        interpret(Query, [Fact|InputFacts], OutputFacts),

        % Make sure that we used S (this is linear, not affine)
        \+ factPresent(OutputFacts, Fact).

% -Query: Query
interpret(Query) :-
        interpret(Query, [], OutputFacts),
        assertion(OutputFacts == []).
