
s(Sen, drs(_Ref, Drs), Y) :-
	sl(Sen, Drs, Y).

s(Sen, X:whq(AnswerType, Drs1, Referent, Drs2), Y) :-
	s(Sen, Drs1, Y1),
	s(Sen, Drs2, Y2),
	append(Y1, Y2, Y).

s(Sen, alfa(AlfaType, Drs1, Drs2), Y) :-
	s(Sen, Drs1, Y1),
	s(Sen, Drs2, Y2),
	append(Y1, Y2, Y).

s(Sen, Tokens:named(Ref, Sym, NeType, _), [triple(Ref, rdfs:label, lit(Name))]) :-
	find_names(Sen, Tokens, Names),
	atomic_list_concat(Names, ' ', Name).

s(Sen, X:rel(Ref1, Ref2, Sym, _), [rel(Ref1, Sym, Ref2)]) :-
	member(Sym, [agent, patient]).

s(Sen, X:rel(Ref1, Ref2, Sym, _), [triple(Ref1, Rel, Ref2), rel(Ref1, Sym, Ref2)]) :-
	\+ member(Sym, [agent, patient]),
	known_relation(Sym, Rel).

s(Sen, X:pred(Ref1, Sym, n, _), [triple(Ref1, rdf:type, Resource)]) :-
	known_type(Sym, Resource).

s(Sen, X:pred(Ref1, Sym, v, _), [pred(Ref1, Sym)]).

sl(Sen, [], []).
sl(Sen, [X|R], M) :-
	s(Sen, X, Yx),
	sl(Sen, R, Yr),
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
postprocess(Pre, [triple(A, Relation, B)|Pre]) :-
	member(rel(C, patient, A), Pre), 	
	member(rel(C, agent, B), Pre),
	member(pred(C, Action), Pre),
	known_action(Action, Relation).

find_names(Sen, [], []).
find_names(Sen, [T|Tokens], [N|Names]) :-
	member(T:Data, Sen),
	member(tok:N, Data),
	find_names(Sen, Tokens, Names).

filter_triples([],[]).
filter_triples([X|R], [X|R2]) :-
	X = triple(_, _, _), !, %red cut, ooh!
	filter_triples(R, R2).

filter_triples([_|R], R2) :-
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

sparql_write_atom(lit(X)) :-
	write('"'), write(X), write('"').

sparql_write_atom(rdf(X)) :-
	write(X).

% also a bit of a fallback.
sparql_write_atom(X) :-
	\+ X = lit(_),
	\+ X = rdf(_),
	write(X).

test :-
	sem(1, X, Y),
	s(X, Y, Z),
	postprocess(Z, ZZ),
	filter_triples(ZZ, D),
	write(D),
	nl.

go :-
	sem(1, X, Y),
	s(X, Y, Z),
	postprocess(Z, ZZ),
	filter_triples(ZZ, D),
	sparql_write(D), nl,
	fail ; !.