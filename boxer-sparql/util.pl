:- module(util, [map/3, filter/3, is_triple/1, is_filter/1, is_var/1]).

map(_, [], []).
map(Pred, [A|R1], [B|R2]) :-
	call(Pred, A, B),
	map(Pred, R1, R2).

filter(_, [], []).
filter(Pred, [A|R1], [A|R2]) :-
	call(Pred, A),
	filter(Pred, R1, R2).
filter(Pred, [A|R1], R2) :-
	\+ call(Pred, A),
	filter(Pred, R1, R2).

is_triple(triple(_, _, _)).

is_filter(filter(_, _, _)).

is_var(var(_)).

double(X, Y) :- Y is X * X.