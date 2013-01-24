:- module(rules, [rule/2]).
:- use_module(hints, [known_action/2, known_type_relation/2]).

% Find agent-patient-verb relations.
rule(Pre, [triple(A, Relation, B)|Pre]) :-
	member(rel(C, patient, A), Pre), 	
	member(rel(C, agent, B), Pre),
	\+ member(triple(A, _, B), Pre), % only continue if not already a relation between a and b
	member(pred(C, Action), Pre),
	known_action(Action, Relation).

% Find 'in' and 'of' relations
rule(Pre, [triple(B, Rel, A) | Pre1]) :-
	member(Keyword, [in, of]),
	select(rel(A, Keyword, B), Pre, Pre1),
	member(type(A, Type), Pre1),
	known_type_relation(Type, Rel).

% Find x1 = x2 and replace x2 with x1.
% Please apply this rule as last :/
rule(Pre, Post) :-
	select(eq(A, B), Pre, Pre1), % removes eq/2
	rename_instances_in_triples(B, A, Pre1, Post).

% If the topic has been given a name that is the first word of
% the sentence, drop it. It is most likely something like "Show"
% or "List".
rule(Pre, Post) :-
	member(topic(Ref), Pre),
	select(triple(Ref, rdf(rdfs:label), nameref([1001])), Pre, Post).

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
