:- use_module('interpreter.pl').

unify(X, X).

class(his101).
class(eng201).

student(jill).
student(tony).
student(tom).

take(tony, his101).

take(jill, eng201).
take(jill, his101).

grad(S) :-
        take(S, his101), take(S, eng201).

% All students who have graduated:
%
% grad(S)

% All students who are one course away from graduation,
% along with the course: 
% 
% (student(S), not(grad(S)), <=(grad(S), take(S, C)))

% All students who are two courses away from graduation,
% along with the courses:
%
% (student(S), not(grad(S)), <=((not(grad(S)), <=(grad(S), take(S, C1))), take(S, C2)))

% Interesting points:
%
% -Without the extra `not(grad(S))` parts, we appear to get extra
%  students.  The reason why is that the query basically is saying
%  something like "hypothetically, if a student were to retake a class
%  they have already taken, would they graduate?", without the not.
%
% -With the two course away from graduation case, we end up getting all
%  course orderings.  This is expected, as fixing this requires second-order
%  properties (we need to reason about past values which have been assigned).
%
