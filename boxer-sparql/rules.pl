:- module(rules, [rule/2]).
:- use_module(hints, [known_action/2, known_type_relation/2]).

% If we are looking for a date, add it to the property name (founded -> foundingDate)
rule(Pre, [triple(PatientRel, rdf(dbpediaowl:Property), TimeRel) | Post]) :-
	select(pred(EventRel, Sym), Pre, Pre1),
	select(rel(EventRel, temp_rel, TimeRel), Pre1, Pre2),
	select(rel(EventRel, patient, PatientRel), Pre2, Pre3),
	select(type(TimeRel, unit_of_time), Pre3, Post),
	atomic_list_concat([Sym, 'ing', 'Date'], Property).

% Find agent-patient-verb relations.
rule(Pre, [triple(A, Relation, B)|Post]) :-
	select(rel(C, patient, A), Pre, Pre1), 	
	select(rel(C, _, B), Pre1, Post),
	\+ member(triple(A, _, B), Post), % only continue if not already a relation between a and b
	member(pred(C, Action), Post),
	known_action(Action, Relation).

% Turn prime & minister into 'prime minister' 
rule(Pre, [type(Rel, Extended) | Post]) :-
	select(type(Rel, Sym), Pre, Pre1),
	select(adverb(Rel, Adv), Pre1, Post),
	atomic_list_concat([Adv, Sym], ' ', Extended).

% Find 'in' and 'of' relations
rule(Pre, [triple(B, Rel, A) | Pre1]) :-
	member(Keyword, [in, of]),
	select(rel(A, Keyword, B), Pre, Pre1),
	member(type(A, Type), Pre1),
	known_type_relation(Type, Rel).

% Or maybe they are the other way around: A of B, therefore B something of A.
% But most of the time we have no idea what the actual relation name is
% therefore just use a variable.
rule(Pre, [triple(A, var(_), B) | Pre1]) :-
	member(Keyword, [in, of]),
	select(rel(A, Keyword, B), Pre, Pre1),
	member(type(A, _Type), Pre1).

% There is some sort of relation (e.g. flowing through) that is impossible
% to guess the word for.
rule(Pre, [triple(A, var(_), B) | Post]) :-
	member(pred(E, _Verb), Pre), % some event
	select(rel(E, agent, A), Pre, Post), % .. that is done by A
	member(rel(E, _, B), Post),
	\+ member(triple(A, _, B), Pre).


% If the topic has been given a name that is the first word of
% the sentence, drop it. It is most likely something like "Show"
% or "List".
rule(Pre, Post) :-
	member(topic(Ref), Pre),
	select(triple(Ref, rdf(rdfs:label), nameref([1001])), Pre, Post).

% Translate type hints into rdf:type triples.
rule(Pre, [triple(Ref, rdf(rdf:type), Type) | Pre]) :-
	member(type(Ref, Sym), Pre),
	\+ member(triple(Ref, rdf(rdf:type), _), Pre),
	known_type(Sym, Type).

% Find x1 = x2 and replace x2 with x1.
% Please apply this rule as last :/
rule(Pre, Post) :-
	select(eq(A, B), Pre, Pre1), % removes eq/2
	rename_instances_in_triples(B, A, Pre1, Post).

% For equality, we rename all the equal symbols to the same symbol.
rename_instances_in_triples(_, _, [], []).
rename_instances_in_triples(A, B, [filter(X, A, Y)|Rest], [filter(X, B, Y)|Renamed]) :-
	rename_instances_in_triples(A, B, Rest, Renamed).

rename_instances_in_triples(A, B, [triple(A, X, Y)|Rest], [triple(B, X, Y)|Renamed]) :-
	rename_instances_in_triples(A, B, Rest, Renamed).

rename_instances_in_triples(A, B, [triple(Y, X, A)|Rest], [triple(Y, X, B)|Renamed]) :-
	rename_instances_in_triples(A, B, Rest, Renamed).

rename_instances_in_triples(A, B, [topic(A)|Rest], [topic(B)|Renamed]) :-
	rename_instances_in_triples(A, B, Rest, Renamed).

rename_instances_in_triples(A, B, [T|Rest], [T|Renamed]) :-
	\+ T = triple(A, _, _),
	rename_instances_in_triples(A, B, Rest, Renamed).
