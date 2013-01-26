from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
from urllib2 import HTTPError
import cgi
import subprocess
import sparql
import re


class Template:

    def __init__(self, file):
        self.data = dict()

        with open(file) as f:
            self.template = f.read()

    def set(self, name, value):
        self.data[name] = value

    def render(self):
        return re.sub(r"\{(.*?)\{(.+?)\}(.*?)\}", self.__replace, self.template)

    def __replace(self, match):
        if match.group(2) in self.data:
            #escaped = cgi.escape(self.data[match.group(2)])
            escaped = self.data[match.group(2)]
            return '%s%s%s' % (match.group(1), escaped, match.group(3))
        else:
            return ''


class QuestionableService:

    def __init__(self):
        endpoint = "http://dbpedia.org/sparql"
        self.sparql = sparql.Service(endpoint)

    def query(self, query):
        answers = []
        sparql_queries = self.__parse(query)

        for sparql_query in sparql_queries:
            try:
                print "%s\n" % (sparql_query,)
                answers = self.sparql.query(sparql_query).fetchall()
                if len(answers) > 0:
                    break
            except HTTPError, e:
                print e

        return answers

    def __parse(self, query):
        cmd = ['./ask', str(query)]
        return subprocess.check_output(cmd).split("\n\n")


class QuestionableView:

    def render(self, result):
        if isinstance(result[0], sparql.Literal):
            return '<p>%s</p>' % cgi.escape(result[0].value)


class QuestionableHandler(BaseHTTPRequestHandler):

    service = QuestionableService()

    view = QuestionableView()

    def do_GET(self):
        self.send_response(200)
        self.send_header('Content-type', 'text/html')
        self.end_headers()

        template = Template('sparql-client/www/index.html')
        self.wfile.write(template.render())

        return

    def do_POST(self):
        form = cgi.FieldStorage(
            fp=self.rfile,
            headers=self.headers,
            environ={
                'REQUEST_METHOD': 'POST',
                'CONTENT_TYPE': self.headers['Content-Type'],
            })

        result = self.service.query(form['query'].value)

        if len(result) == 1 and isinstance(result[0][0], sparql.IRI):
            self.send_response(301)
            self.send_header('Location', result[0][0].value)
            self.end_headers()
        else:
            self.send_response(200)
            self.send_header('Content-Type', 'text/html')
            self.end_headers()

            response = map(self.view.render, result)

            template = Template('sparql-client/www/index.html')
            template.set('query', form['query'].value)
            template.set('results', '\n'.join(response))

            self.wfile.write(template.render().encode('ascii', 'xmlcharrefreplace'))


server = HTTPServer(('', 8080), QuestionableHandler)
server.serve_forever()
