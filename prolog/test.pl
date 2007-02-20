% test.pl

% num_between(+Lo, +Hi, ?N)
num_between(Lo, Hi, Lo) :-
	Lo =< Hi.
num_between(Lo, Hi, N) :-
	Lo < Hi,
	Lo1 is Lo + 1, 
	num_between(Lo1, Hi, N).

% 17X + 19Y = N
solve_eq1(X, Y, N) :-
	num_between(1, 50, X),
	num_between(1, 50, Y), 
	num_between(1, 100, N),
	X < Y,
	N is 17 * X + 19 * Y,
	Y < N.

pyth_trip(X, Y, Z, N) :-
	num_between(1, N, X),
	num_between(1, N, Y),
	num_between(1, N, Z),
	X < Y, Y < Z,
	S is X*X + Y*Y,
	ZZ is Z*Z,
	S = ZZ.


% practice
my_last(X, [X]).
my_last(X, [_|Xs]) :- my_last(X, Xs). 

count(_, [], 0) :- !.
count(X, [X|Xs], N) :- count(X, Xs, N1), N is N1 + 1, !.
count(X, [_|Xs], N) :- count(X, Xs, N), !.


% prod(A, B, SetA, SetB)
prod(X, Y, Xs, Ys) :- member(X, Xs), member(Y, Ys).

subset([], _) :- !.
subset([X|Xs], Y) :- member(X, Y), subset(Xs, Y), !.

remove_all(_, [], []) :- !.
remove_all(X, [X|Xs], R) :-
	remove_all(X, Xs, R), !.
remove_all(X, [H|Xs], [H|R]) :-
	remove_all(X, Xs, R).


%%%%%%%%%%%%%%%%%%%%%%%%

get_all_asn([], []).
get_all_asn([[asn, X] | As], [[asn, X] | R]) :-
	get_all_asn(As, R).
get_all_asn([[exam, X] | As], R) :-
	get_all_asn(As, R).

get_all_exam([], []).
get_all_exam([[exam, X] | As], [[exam, X] | R]) :-
	get_all_exam(As, R).
get_all_exam([[asn, X] | As], R) :-
	get_all_exam(As, R).

act_sum([], 0).
act_sum([[_, Weight] | As], Sum) :-
	act_sum(As, Sum2),
	Sum is Weight + Sum2.

scheme(A) :-
	Act = [[asn, 10], [asn, 10], [asn, 10], [asn, 10], [asn, 10], [exam, 25], [exam, 25]],
	get_all_asn(Act, A),
	get_all_exam(Act, E),
	act_sum(A, Aweight),
	act_sum(E, Eweight),
	Total is Aweight + Eweight,
	write(A), write(' '), write(Aweight), nl,
	write(E), write(' '), write(Eweight), nl,
	write('Total = '), write(Total).
