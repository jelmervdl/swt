:- module(sparql, [sparql_write/2]).
:- use_module(rdf, [rdf_namespace/2]).

sparql_write(Topic, Triples) :-
	sparql_write_namespaces,
	write('SELECT DISTINCT '), sparql_write_atom(Topic), write(' WHERE {'),nl,
	(
		member(Triple, Triples),
		Triple = triple(_, _, _),
		write('  '), sparql_write_triple(Triple), nl,
		fail ; !
	),
	% Write filters after triples.
	(
		member(Filter, Triples),
		Filter = filter(_, _, _),
		write('  '), sparql_write_triple(Filter), nl,
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

sparql_write_triple(filter(Lhs, Op, Rhs)) :-
	write('FILTER ( '),
		sparql_write_atom(Lhs), write(' '),
		write(Op), write(' '),
		sparql_write_atom(Rhs), write(' '),
	write(')').

sparql_write_atom(lit(X)) :-
	write('"'), write(X), write('"@en').

sparql_write_atom(rdf(X)) :-
	write(X).

sparql_write_atom(var(X)) :-
	write('?'),
	write(X).
