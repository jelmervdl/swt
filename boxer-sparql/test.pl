:- use_module(library(lists),[member/2,select/3]).

sublist(A, B) :-
	append(First, _, B),
	append(_, A, First).


% All the used namespaces in the resulting SPARQL query
rdf_namespace(owl, 'http://www.w3.org/2002/07/owl#').
rdf_namespace(xsd, 'http://www.w3.org/2001/XMLSchema#').
rdf_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').
rdf_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
rdf_namespace(foaf, 'http://xmlns.com/foaf/0.1/').
rdf_namespace(dc, 'http://purl.org/dc/elements/1.1/').
rdf_namespace(dbpedia2, 'http://dbpedia.org/property/').
rdf_namespace(dbpprop, 'http://dbpedia.org/property/').
rdf_namespace(dbpedia, 'http://dbpedia.org/').
rdf_namespace(dbpedia-owl, 'http://dbpedia.org/ontology/').
rdf_namespace(skos, 'http://www.w3.org/2004/02/skos/core#').
%rdf_namespace(, 'http://dbpedia.org/resource/').

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

% typename to type resource mapping
known_type(person, rdf(foaf:'Person')).
%% known_type(X, lit(X)). % fallback for now.

known_relation(X, lit(X)). % fallback for now.

% Verb to relation mapping
known_action(write, rdf(dbpprop:author)).
known_action(direct, rdf(dbpprop:director)).
%known_action(write, rdf(movie:writer)).
%known_action(direct, rdf(movie:director)).

% Type (pred) to relation mapping
known_type_relation(X, rdf(dbpprop:X)). %wild guess

% Find agent-patient-verb relations.
rule(Pre, [triple(A, Relation, B)|Pre]) :-
	member(rel(C, patient, A), Pre), 	
	member(rel(C, agent, B), Pre),
	\+ member(triple(A, _, B), Pre), % only continue if not already a relation between a and b
	member(pred(C, Action), Pre),
	known_action(Action, Relation).

% Find 'of' relations
rule(Pre, [triple(B, Rel, A) | Pre1]) :-
	select(of(A, B), Pre, Pre1),
	member(type(A, Type), Pre1),
	known_type_relation(Type, Rel).

% Find x1 = x2 and replace x2 with x1.
% Please apply this rule as last :/
rule(Pre, Post) :-
	select(eq(A, B), Pre, Pre1), % removes eq/2
	rename_instances_in_triples(B, A, Pre1, Post).

% For equality, we rename all the equal symbols to the same symbol.
rename_instances_in_triples(_, _, [], []).
rename_instances_in_triples(A, B, [triple(A, X, Y)|Rest], [triple(B, X, Y)|Renamed]) :-
	!,
	rename_instances_in_triples(A, B, Rest, Renamed).
rename_instances_in_triples(A, B, [triple(Y, X, A)|Rest], [triple(Y, X, B)|Renamed]) :-
	!,
	rename_instances_in_triples(A, B, Rest, Renamed).
rename_instances_in_triples(A, B, [T|Rest], [T|Renamed]) :-
	\+ T = triple(A, _, _),
	rename_instances_in_triples(A, B, Rest, Renamed).

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

sparql_write(Triples) :-
	sparql_write_namespaces,
	write('SELECT * WHERE {'),nl,
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
	write('"'), write(X), write('"@en').

sparql_write_atom(rdf(X)) :-
	write(X).

% also a bit of a fallback.
% also, if it isn't a rdf-atom nor a literal, it is probably a variable.
sparql_write_atom(X) :-
	\+ X = lit(_),
	\+ X = rdf(_),
	write('?'),
	write(X).

%% random_varname(X) :-
%% 	varname(X),
%% 	retract(varname(X)).

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