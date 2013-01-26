:- module(sparql, [sparql_write/2]).
:- use_module(rdf, [rdf_namespace/2]).

sparql_write(Topic, Triples) :-
	sparql_write_namespaces,
	write('SELECT '), sparql_write_atom(Topic), write(' WHERE {'),nl,
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

sparql_write_triple(filter(What, Ref, Is)) :-
	write('FILTER ('), write(What),
	write('('), sparql_write_atom(Ref), write(')'),
	write(' = '), sparql_write_atom(Is),
	write(')').

%% sparql_write_atom(X) :-
%% 	var(X), !,
%% 	random_varname(X),
%% 	write('?'), write(X).

sparql_write_atom(lit(X)) :-
	write('"'), write(X), write('"@en').

sparql_write_atom(rdf(X)) :-
	write(X).

sparql_write_atom(var(X)) :-
	write('?'),
	write(X).

%% random_varname(X) :-
%% 	varname(X),
%% 	retract(varname(X)).