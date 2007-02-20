% stest.pl
	
% num_between(+Lo, +Hi, ?N)
num_between(Lo, Hi, Lo) :-
	Lo =< Hi.
num_between(Lo, Hi, N) :-
	Lo < Hi,
	Lo1 is Lo + 1, 
	num_between(Lo1, Hi, N).

set_all_diff([X], Okay, Taboo) :-
       member(X, Okay),
       \+ member(X, Taboo).

set_all_diff([X|Xs], Okay, Taboo) :-
       member(X, Okay),
       \+ member(X, Taboo),
       set_all(Xs, Okay, [X|Taboo]).

cell(R, C, Val, Cand).

check(R, C, Val, Cand) :-
	num_between(1, 9, R),
	num_between(1, 9, C),
	member(Val, Cand).
	