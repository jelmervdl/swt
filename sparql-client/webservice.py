from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
import cgi
import subprocess
import sparql


class QuestionableService:

    def __init__(self):
        endpoint = "http://dbpedia.org/sparql"
        self.sparql = sparql.Service(endpoint)

    def query(self, query):
        answers = []
        sparql_queries = self.__parse(query)

        for sparql_query in sparql_queries:
            answers = self.sparql.query(sparql_query).fetchall()
            if len(answers) > 0:
                break

        return answers

    def __parse(self, query):
        cmd = ['./ask', str(query)]
        return subprocess.check_output(cmd).split("\n\n")


class QuestionableHandler(BaseHTTPRequestHandler):

    service = QuestionableService()

    def do_GET(self):
        self.send_response(200)
        self.send_header('Content-type', 'text/html')
        self.end_headers()

        with open('sparql-client/www/index.html') as f:
            self.wfile.write(f.read())

        return

    def do_POST(self):
        form = cgi.FieldStorage(
            fp=self.rfile,
            headers=self.headers,
            environ={
                'REQUEST_METHOD': 'POST',
                'CONTENT_TYPE': self.headers['Content-Type'],
            })

        self.send_response(200)
        self.send_header('Content-Type', 'application/json')
        self.end_headers()

        result = self.service.query(form['query'].value)
        self.wfile.write(result)
        return


server = HTTPServer(('', 8080), QuestionableHandler)
server.serve_forever()
