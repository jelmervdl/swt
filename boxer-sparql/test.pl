:- use_module(library(lists),[member/2,select/3]).

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

s(Tokens:named(Ref, _Sym, _NeType, _), [triple(Ref, rdfs:label, nameref(Tokens))]).

s(_:rel(Ref1, Ref2, Sym, _), [rel(Ref1, Sym, Ref2)]) :-
	member(Sym, [agent, patient]).

s(_:rel(Ref1, Ref2, of, _), [of(Ref1, Ref2)]).

s(_:rel(Ref1, Ref2, Sym, _), [triple(Ref1, Rel, Ref2), rel(Ref1, Sym, Ref2)]) :-
	\+ member(Sym, [agent, patient, of]),
	known_relation(Sym, Rel).

s(_:pred(Ref1, Sym, n, _), [type(Ref1, Sym) | Triples]) :-
	known_type(Sym, Resource), Triples = [triple(Ref1, rdf:type, Resource)], ! % behavior-altering cut warning
	; Triples = [].

% for testing: ignore eq.
%s(X:eq(Ref1, Ref1), []).
s(_:eq(Ref1, Ref2), [eq(Ref1, Ref2)]).

% also ignore the meaning of prop.
s(_:prop(_, Drs), Y) :-
	s(Drs, Y).

s(_:pred(Ref1, Sym, v, _), [pred(Ref1, Sym)]).

sl([], []).
sl([X|R], M) :-
	s(X, Yx),
	sl(R, Yr),
	append(Yx, Yr, M).

known_type(person, rdf(foaf:'Person')).
%% known_type(X, lit(X)). % fallback for now.

known_relation(X, lit(X)). % fallback for now.

known_action(write, rdf(movie:writer)).
known_action(write, rdf(dbpprop:author)).
known_action(direct, rdf(movie:director)).

known_type_relation(X, dbpedia:X). %wild guess

rdf_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').
rdf_namespace(movie, 'http://data.linkedmdb.org/resource/movie/').

% Find agent-patient-verb relations.
rule(Pre, [triple(A, Relation, B)|Pre]) :-
	member(rel(C, patient, A), Pre), 	
	member(rel(C, agent, B), Pre),
	member(pred(C, Action), Pre),
	known_action(Action, Relation),
	\+ member(triple(A, Relation, B), Pre). % only succeed if not already applied.

% Find 'of' relations
rule(Pre, [triple(A, Rel, B) | Pre1]) :-
	select(of(A, B), Pre, Pre1),
	member(type(A, Type), Pre1),
	known_type_relation(Type, Rel).

% Find x1 = x2 and replace x2 with x1.
% Please apply this rule as last :/
rule(Pre, Post) :-
	select(eq(A, B), Pre, Pre1), % removes eq/2
	rename_instances_in_triples(B, A, Pre1, Post).

rename_instances_in_triples(_, _, [], []).
rename_instances_in_triples(A, B, [triple(A, X, Y)|Rest], [triple(B, X, Y)|Renamed]) :-
	!,
	rename_instances_in_triples(A, B, Rest, Renamed).
rename_instances_in_triples(A, B, [T|Rest], [T|Renamed]) :-
	\+ T = triple(A, _, _),
	rename_instances_in_triples(A, B, Rest, Renamed).

% Apply all possible rule/2's
postprocess(Pre, Post) :-
	rule(Pre, Result), postprocess(Result, Post), ! % be careful with this cut here.
	; Pre = Post.

% Find names by their tokens in Literals (second part of sem/3)
find_names(_, [], []).
find_names(Literals, [T|Tokens], [N|Names]) :-
	nonvar(Tokens),
	member(T:Data, Literals),
	member(tok:N, Data),
	find_names(Literals, Tokens, Names).

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

sparql_write(Triples) :-
	sparql_write_namespaces,
	write('SELECT ?x0 WHERE {'),nl,
	(
		member(Triple, Triples),
		write('  '), sparql_write_triple(Triple), nl,
		fail ; !
	),
	write('}'),nl.

sparql_write_namespaces :-
	rdf_namespace(X, Y),
	write('PREFIX '), write(X), write(': <'), write(Y), write('>'), nl,
	fail; !.

sparql_write_triple(triple(A, Ref, B)) :-
	sparql_write_atom(A), write(' '),
	sparql_write_atom(Ref), write(' '),
	sparql_write_atom(B), write('.').

%% sparql_write_atom(X) :-
%% 	var(X), !,
%% 	random_varname(X),
%% 	write('?'), write(X).

sparql_write_atom(lit(X)) :-
	write('"'), write(X), write('"').

sparql_write_atom(rdf(X)) :-
	write(X).

% also a bit of a fallback.
sparql_write_atom(X) :-
	\+ X = lit(_),
	\+ X = rdf(_),
	write(X).

%% random_varname(X) :-
%% 	varname(X),
%% 	retract(varname(X)).

random_varname(a).

varname(a).
varname(b).
varname(c).
varname(d).
varname(e).

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