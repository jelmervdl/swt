#!/usr/bin/python
import sparql
import sys

#endpoint = "http://data.linkedmdb.org/sparql"
endpoint = "http://dbpedia.org/sparql"
service = sparql.Service(endpoint)
result = service.query(sys.stdin.read())

for row in result.fetchone():
    print row
