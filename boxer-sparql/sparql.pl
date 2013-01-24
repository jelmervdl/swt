:- module(sparql, [sparql_write/1]).
:- use_module(rdf, [rdf_namespace/2]).

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