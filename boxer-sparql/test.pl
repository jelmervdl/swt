:- use_module(library(lists),[member/2,select/3]).
:- use_module(hints, [known_type/2, known_relation/2, known_action/2, known_type_relation/2]).
:- use_module(rules, [rule/2]).
:- use_module(sparql, [sparql_write/1]).

sublist(A, B) :-
	append(First, _, B),
	append(_, A, First).

% s-rules are rules to rewrite the drs/2 structure into a list of triples and hints.
s(drs(_Ref, Drs), Y) :-
	sl(Drs, Y).

s(_:whq(_AnswerType, Drs1, _Referent, Drs2), Y) :-
	s(Drs1, Y1),
	s(Drs2, Y2),
	append(Y1, Y2, Y).

s(alfa(_AlfaType, Drs1, Drs2), Y) :-
	s(Drs1, Y1),
	s(Drs2, Y2),
	append(Y1, Y2, Y).

s(Tokens:named(Ref, _Sym, _NeType, _), [triple(Ref, rdf(rdfs:label), nameref(Tokens))]).

s(_:rel(Ref1, Ref2, Sym, _), [rel(Ref1, Sym, Ref2)]) :-
	member(Sym, [agent, patient]).

s(_:rel(Ref1, Ref2, of, _), [of(Ref1, Ref2)]).

s(_:rel(Ref1, Ref2, Sym, _), [triple(Ref1, Rel, Ref2), rel(Ref1, Sym, Ref2)]) :-
	\+ member(Sym, [agent, patient, of]),
	known_relation(Sym, Rel).

s(_:pred(Ref1, Sym, n, _), [type(Ref1, Sym) | Triples]) :-
	known_type(Sym, Resource), Triples = [triple(Ref1, rdf(rdf:type), Resource)], ! % behavior-altering cut warning
	; Triples = [].

% for testing: ignore eq.
%s(X:eq(Ref1, Ref1), []).
s(_:eq(Ref1, Ref2), [eq(Ref1, Ref2)]).

% also ignore the meaning of prop.
s(_:prop(_, Drs), Y) :-
	s(Drs, Y).

s(_:pred(Ref1, Sym, v, _), [pred(Ref1, Sym)]).

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
fill_in_names(Literals, [triple(A, B, nameref(Tokens)) | Triples], [triple(A, B, lit(Name)) | TriplesWithNames]) :-
	!,
	find_names(Literals, Tokens, Names),
	atomic_list_concat(Names, ' ', Name),
	fill_in_names(Literals, Triples, TriplesWithNames).
fill_in_names(Literals, [triple(nameref(Tokens), B, C) | Triples], [triple(lit(Name), B, C) | TriplesWithNames]) :-
	!,
	find_names(Literals, Tokens, Names),
	atomic_list_concat(Names, ' ', Name),
	fill_in_names(Literals, Triples, TriplesWithNames).
fill_in_names(Literals, [triple(A, B, C) | Triples], [triple(A, B, C) | TriplesWithNames]) :-
	\+ A = nameref(_),
	\+ C = nameref(_),
	fill_in_names(Literals, Triples, TriplesWithNames).


% Remove everything that is not a triple/3
filter_triples([],[]).
filter_triples([X|R], [X|R2]) :-
	X = triple(_, _, _), !,
	filter_triples(R, R2).
filter_triples([X|R], R2) :-
	\+ X = triple(_, _, _),
	filter_triples(R, R2).

test(N) :-
	sem(N, _, Y),
	s(Y, Z),
	postprocess(Z, ZZ),
	filter_triples(ZZ, D),
	write(D),
	nl.

go(N) :-
	sem(N, X, Y),
	s(Y, Z),
	postprocess(Z, ZZ),
	filter_triples(ZZ, D),
	fill_in_names(X, D, TriplesWithNames),
	sparql_write(TriplesWithNames), nl,
	fail ; !.