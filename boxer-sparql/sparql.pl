:- module(sparql, [sparql_write/1]).
:- use_module(rdf, [rdf_namespace/2]).
:- use_module(util, [map/3, filter/3, is_triple/1, is_filter/1, is_topic/1]).

sparql_write(RevTriples) :-
	sparql_write_namespaces,
	reverse(RevTriples, Triples), % often yields a better order.
	% Format topics
	filter(util:is_topic, Triples, Topics),
	map(sparql:sparql_format, Topics, TopicExpressions),
	atomic_list_concat(TopicExpressions, ', ', TopicExpression),
	% Print query
	write('SELECT '), write(TopicExpression), write(' WHERE {'),nl,
	(
		member(Triple, Triples),
		Triple = triple(_, _, _),
		write('  '), sparql_write_triple(Triple), nl,
		fail ; !
	),
	% Combine and write filters after triples.
	(
		filter(util:is_filter, Triples, Filters),
		Filters = [_|_], % not empty
		map(sparql:sparql_format_filter, Filters, FilterExpressions),
		atomic_list_concat(FilterExpressions, ' && ', FilterExpression),
		write('  FILTER ( '), write(FilterExpression), write(' )'), nl , !
		; write('')
	),
	write('}'),nl.

sparql_write_namespaces :-
	rdf_namespace(X, Y),
	write('PREFIX '), write(X), write(': <'), write(Y), write('>'), nl,
	fail; !.

sparql_format(topic(X), Expr) :-
	sparql_format_atom(X, Expr).

sparql_format_filter(filter(Lhs, Op, Rhs), Expr) :-
	sparql_format_atom(Lhs, LhsExpr),
	sparql_format_atom(Rhs, RhsExpr),
	atomic_list_concat([LhsExpr, ' ', Op, ' ', RhsExpr], Expr).

sparql_format_atom(lit(X), Expr) :-
	atomic_list_concat(['"', X, '"@en'], Expr).

sparql_format_atom(rdf(X), X).

sparql_format_atom(var(X), Expr) :-
	atomic_list_concat(['?', X], Expr).


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
