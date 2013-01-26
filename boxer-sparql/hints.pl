:- module(hints, [known_type/2, known_relation/2, known_action/2, known_type_relation/2]).

% typename to type resource mapping
% TODO: maybe it is helpful to look at foaf and dbpediaowl rdf and
% see which relations they define. If one matches with X, let that
% then be a known type with the correct prefix.
known_type(person, rdf(foaf:'Person')).
known_type(X, rdf(dbpediaowl:Y)) :-
	ucfirst(Y, X).

known_relation(X, lit(X)). % fallback for now.

% Verb to relation mapping
known_action(write, rdf(dbpprop:author)).
known_action(direct, rdf(dbpprop:director)).
%known_action(write, rdf(movie:writer)).
%known_action(direct, rdf(movie:director)).

% These are the wrong way around, being born
% is passive. To direct is active.
%known_action(born, rdf(dbpprop:birthPlace)).
%known_action(born, rdf(dbpprop:placeOfBirth)).
known_action(bear, rdf(dbpprop:placeOfBirth)).
known_action(found, rdf(dbpprop:founded)).

% Type (pred) to relation mapping
known_type_relation(X, rdf(dbpprop:X)). %wild guess

% Turns first letter of the atom into uppercase:
% e.g. ucfirst('Chat', chat).
ucfirst(Uc, Lc) :-
	atom_chars(Lc, [Lcf|Lcc]),
	upcase_atom(Lcf, Ucf),
	atom_chars(Uc, [Ucf|Lcc]).