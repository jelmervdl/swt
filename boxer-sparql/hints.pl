:- module(hints, [known_type/2, known_relation/2, known_action/2, known_type_relation/2]).

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