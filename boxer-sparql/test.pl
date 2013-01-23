:- use_module(library(lists),[member/2,select/3]).

s(drs(_Ref, Drs), Y) :-
	sl(Drs, Y).

s(X:whq(AnswerType, Drs1, Referent, Drs2), Y) :-
	s(Drs1, Y1),
	s(Drs2, Y2),
	append(Y1, Y2, Y).

s(alfa(AlfaType, Drs1, Drs2), Y) :-
	s(Drs1, Y1),
	s(Drs2, Y2),
	append(Y1, Y2, Y).

s(Tokens:named(Ref, Sym, NeType, _), [triple(Ref, rdfs:label, nameref(Tokens))]).

s(X:rel(Ref1, Ref2, Sym, _), [rel(Ref1, Sym, Ref2)]) :-
	member(Sym, [agent, patient]).

s(X:rel(Ref1, Ref2, Sym, _), [triple(Ref1, Rel, Ref2), rel(Ref1, Sym, Ref2)]) :-
	\+ member(Sym, [agent, patient]),
	known_relation(Sym, Rel).

s(X:pred(Ref1, Sym, n, _), [triple(Ref1, rdf:type, Resource)]) :-
	known_type(Sym, Resource).

% for testing: ignore eq.
%s(X:eq(Ref1, Ref1), []).
s(X:eq(Ref1, Ref2), [eq(Ref1, Ref2)]).

% also ignore the meaning of prop.
s(X:prop(Ref, Drs), Y) :-
	s(Drs, Y).

s(X:pred(Ref1, Sym, v, _), [pred(Ref1, Sym)]).

sl([], []).
sl([X|R], M) :-
	s(X, Yx),
	sl(R, Yr),
	append(Yx, Yr, M).

known_type(person, rdf(foaf:'Person')).
known_type(X, lit(X)). % fallback for now.

known_relation(X, lit(X)). % fallback for now.

known_action(write, rdf(movie:writer)).
known_action(write, rdf(dbpprop:author)).
known_action(direct, rdf(movie:director)).

rdf_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').
rdf_namespace(movie, 'http://data.linkedmdb.org/resource/movie/').

% Find agent-patient-verb relations.
rule(Pre, [triple(A, Relation, B)|Pre]) :-
	member(rel(C, patient, A), Pre), 	
	member(rel(C, agent, B), Pre),
	member(pred(C, Action), Pre),
	known_action(Action, Relation),
	\+ member(triple(A, Relation, B), Pre). % only succeed if not already applied.

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


postprocess(Pre, Post) :-
	rule(Pre, Result), postprocess(Result, Post), ! % be careful with this cut here.
	; Pre = Post.

find_names(Sen, [], []).
find_names(Sen, [T|Tokens], [N|Names]) :-
	nonvar(Tokens),
	member(T:Data, Sen),
	member(tok:N, Data),
	find_names(Sen, Tokens, Names).

filter_triples([],[]).
filter_triples([X|R], [X|R2]) :-
	X = triple(_, _, _), !, %red cut, ooh!
	filter_triples(R, R2).

filter_triples([X|R], R2) :-
	\+ X = triple(_, _, _),
	filter_triples(R, R2).

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