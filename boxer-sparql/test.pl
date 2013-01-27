:- use_module(library(lists),[member/2,select/3]).
:- use_module(hints, [known_type/2, known_relation/2, known_action/2]).
:- use_module(rules, [rule/2]).
:- use_module(sparql, [sparql_write/2]).
:- use_module(negation, [s_negate/3]).
:- use_module(util, [filter/3]).

sublist(A, B) :-
	append(First, _, B),
	append(_, A, First).

% s-rules are rules to rewrite the drs/2 structure into a list of triples and hints.
s(drs(_Ref, Drs), Y) :-
	sl(Drs, Y).

s(_:whq(_AnswerType, Drs1, Ref, Drs2), [topic(var(Ref)) | Y]) :-
	s(Drs1, Y1),
	s(Drs2, Y2),
	append(Y1, Y2, Y).

% Negation is a *i***.
s(alfa(_AlfaType, Drs1, drs([], [_:not(NDrs)])), Y) :-
	s(Drs1, Y1),
	s(NDrs, NY),
	s_negate(Y1, NY, Y).

s(alfa(_AlfaType, Drs1, Drs2), Y) :-
	s(Drs1, Y1),
	s(Drs2, Y2),
	append(Y1, Y2, Y).

s(Tokens:named(Ref, _Sym, _NeType, _), [triple(var(Ref), rdf(rdfs:label), nameref(Tokens))]).

%s(_:rel(Ref1, Ref2, Sym, _), [rel(Ref1, Sym, Ref2)]) :-
%	member(Sym, [agent, patient]).

s(_:rel(Ref1, Ref2, Keyword, _), [rel(var(Ref1), Keyword, var(Ref2))]).

%s(_:rel(Ref1, Ref2, Sym, _), [triple(Ref1, Rel, Ref2), rel(Ref1, Sym, Ref2)]) :-
%	\+ member(Sym, [agent, patient, of]),
%	known_relation(Sym, Rel).

%s(_:pred(Ref, unit_of_time, n, _), [filter(datatype, Ref, rdf(xsd:date))]) :- !.

s(_:pred(Ref1, Sym, n, _), [type(var(Ref1), Sym)]).

s(_:pred(Ref1, Sym, v, _), [pred(var(Ref1), Sym)]).

s(_:pred(Ref1, topic, a, _), [topic(var(Ref1))]) :- !.

s(_:pred(Ref1, Sym, a, _), [adverb(var(Ref1), Sym)]).

s(_:eq(Ref1, Ref2), [eq(var(Ref1), var(Ref2))]).

% also ignore the meaning of prop.
s(_:prop(_, Drs), Y) :-
	s(Drs, Y).

s(_:imp(Drs1, Drs2), Y) :-
	s(Drs1, Y1),
	s(Drs2, Y2),
	append(Y1, Y2, Y).

% sl/2 is s/2 but for lists of drs/2's.
sl([], []).
sl([X|R], M) :-
	s(X, Yx),
	sl(R, Yr),
	append(Yx, Yr, M).

% Apply all possible rule/2's
postprocess(Pre, Post) :-
	rule(Pre, Result), postprocess(Result, Post)%, ! % be careful with this cut here.
	; Pre = Post.

% A bit of a wrapper/hack around find_names. If the literal before the first
% searched-for token is 'The', add it to the name.
find_names(Literals, [T|Tokens], Names) :-
	(
		sublist([_:TheLiteral, T:_], Literals),
		member(tok:'The', TheLiteral), !,
		['The'|Rest] = Names
		;
		Rest = Names
	),
	find_names_(Literals, [T|Tokens], Rest).

% Find names by their tokens in Literals (second part of sem/3)
find_names_(_, [], []).
find_names_(Literals, [T|Tokens], [N|Names]) :-
	nonvar(Tokens),
	member(T:Data, Literals),
	member(tok:N, Data),
	find_names_(Literals, Tokens, Names).

% Replace nameref/1 with a string from Literals (second part of sem/3)
fill_in_names(_, [], []).
fill_in_names(Literals, [X | Triples], [Filled | TriplesWithNames]) :-
	X =.. [Pred|Args],
	fill_in_names_(Literals, Args, ArgsWithNames),
	Filled =.. [Pred|ArgsWithNames],
	fill_in_names(Literals, Triples, TriplesWithNames).

fill_in_names_(_, [], []).
fill_in_names_(Literals, [X|Rest], [Y|RestFilled]) :-
	(
		X = nameref(Tokens),
		Y = lit(Name),
		find_names(Literals, Tokens, Names),
		atomic_list_concat(Names, ' ', Name)
		;
		\+ X = nameref(_),
		X = Y
	),
	fill_in_names_(Literals, Rest, RestFilled).


% Remove everything that is not a triple/3
filter_triples(In, Filtered) :-
	filter(is_triple, In, Filtered).

test(N) :-
	sem(N, _, Y),
	s(Y, Z),
	postprocess(Z, ZZ),
	filter_triples(ZZ, D),
	write(D),
	nl.

go(N) :-
	sem(N, Literals, Drs),
	s(Drs, Hints),
	%write(Hints),nl,
	postprocess(Hints, EnrichedHints),
	filter_triples(EnrichedHints, Triples),
	fill_in_names(Literals, Triples, TriplesWithNames),
	member(topic(Topic), EnrichedHints),
	sparql_write(Topic, TriplesWithNames), nl,
	fail ; !.