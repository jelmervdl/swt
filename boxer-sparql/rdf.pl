:- module(rdf, [rdf_namespace/2]).

% All the used namespaces in the resulting SPARQL query
rdf_namespace(owl, 'http://www.w3.org/2002/07/owl#').
rdf_namespace(xsd, 'http://www.w3.org/2001/XMLSchema#').
rdf_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').
rdf_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
rdf_namespace(foaf, 'http://xmlns.com/foaf/0.1/').
rdf_namespace(dc, 'http://purl.org/dc/elements/1.1/').
rdf_namespace(dbpedia2, 'http://dbpedia.org/property/').
rdf_namespace(dbpprop, 'http://dbpedia.org/property/').
rdf_namespace(dbpedia, 'http://dbpedia.org/').
rdf_namespace(dbpediaowl, 'http://dbpedia.org/ontology/').
rdf_namespace(skos, 'http://www.w3.org/2004/02/skos/core#').
%rdf_namespace(, 'http://dbpedia.org/resource/').
