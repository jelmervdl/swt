relations(drs(Referent, Conditions), Triplets) :-
	relations_list(Conditions, Triplets).

relations(X:prop(Referent, Drs), Triplets) :-
	relations(Drs, Triplets).

% The alfa-type specifies the kind of anaphoric information: anaphoric pronoun, definite description, proper name, reflexive pronoun, or deictic pronoun.
relations(alfa(Type, Drs1, Drs2), Triplets) :-
	relations(Drs1, Triplets1),
	relations(Drs2, Triplets2),
	append(Triplets1, Triplets2, Triplets).

relations(X:pred(Referent, Symbol, n, Sense), [triple(C, isa, symbol)]).
relations(X:pred(Referent, Symbol, a, Sense), [triple(C, is, symbol)]).

reqlations(X:rel(A, C), )

relations_list([X|Rest], Merged) :-
	relations(X, Triplets),
	relations(Rest, RestTriplets),
	append(Triplets, RestTriplets, Merged).

