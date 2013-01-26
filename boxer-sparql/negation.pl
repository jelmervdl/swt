:- module(negation, [s_negate/3]).
:- use_module(library(lists), [intersection/3, member/2, append/3]).

s_negate(Y1, NY, Y) :-
	find_vars(Y1, V1),
	find_vars(NY, NV),
	negate_terms(NY, Y2),
	append(Y1, Y2, Y3),
	% add filter for union of V1 and NV
	intersection(V1, NV, UnequalVars),
	write(UnequalVars),
	generate_filters(UnequalVars, Filters),
	append(Filters, Y3, Y).

find_vars(Hints, SetOfVars) :-
	find_vars(Hints, [], SetOfVars).

find_vars([], Vars, SetOfVars) :-
	list_to_set(Vars, SetOfVars).

find_vars([Hint|Hints], VarsAcc, SetOfVars) :-
	Hint =.. [_|Args],
	findall(var(X), member(var(X), Args), Vars),
	append(Vars, VarsAcc, VarsAcc1),
	find_vars(Hints, VarsAcc1, SetOfVars).

negate_terms([], []).
negate_terms([var(X)|In], [var(N)|Out]) :-
	!,
	atom_chars(X, Chars),
	atom_chars(N, [n|Chars]),
	negate_terms(In, Out).

negate_terms([X|In], [N|Out]) :-
	\+ X = var(_),
	X =.. [Pred|Args],
	negate_terms(Args, NegatedArgs),
	N =.. [Pred|NegatedArgs],
	negate_terms(In, Out).

generate_filters([], []).
generate_filters([X|Vars], [filter(X, '!=', Y)|Filters]) :-
	negate_terms([X], [Y]),
	generate_filters(Vars, Filters).
	
	