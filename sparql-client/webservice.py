from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
from urllib2 import HTTPError
from cgi import escape, FieldStorage
import subprocess
import sparql
import re
from codecs import open


def escapeattr(data):
    return data.replace('"', '&quot;')


class Template:

    def __init__(self, file):
        self.data = dict()

        with open(file, 'r', 'utf-8') as f:
            self.template = f.read()

    def set(self, name, value):
        self.data[name] = value

    def render(self):
        template = re.sub(r"\{\?(\w+?)\s+(.+?)\}", self.__remove, self.template)
        return re.sub(r"\{(.*?)\{(.+?)\}(.*?)\}", self.__replace, template)

    def __remove(self, match):
        if match.group(1) in self.data:
            return match.group(2)
        else:
            return ''

    def __replace(self, match):
        if match.group(2) in self.data:
            return '%s%s%s' % (match.group(1), self.data[match.group(2)], match.group(3))
        else:
            return ''


class QuestionableService:

    def __init__(self):
        self.tries = 20

        endpoint = "http://dbpedia.org/sparql"
        self.sparql = sparql.Service(endpoint)

    def query(self, query):
        tries = 0
        sparql_queries = self.__parse(query)

        for sparql_query in sparql_queries:
            tries += 1
            if tries >= self.tries:
                break

            try:
                print "Try %d: %s\n" % (tries, sparql_query)
                answers = self.sparql.query(sparql_query).fetchall()
                if len(answers) > 0:
                    return answers
            except HTTPError, e:
                print e

        return []

    def __parse(self, query):
        cmd = ['./ask', str(query)]
        return subprocess.check_output(cmd).split("\n\n")


class QuestionableView:

    def render(self, result):
        # Is there also a count, show it as well.
        if len(result) == 3:
            url = escapeattr(result[0].value)
            label = escape(result[1].value)
            count = int(result[2].value)
            html = '<a href="%s">%s</a> (%d)' % (url, label, count)
        # Labels: resources that had a label attribute
        elif len(result) == 2:
            url = escapeattr(result[0].value)
            label = escape(result[1].value)
            html = '<a href="%s">%s</a>' % (url, label)
        # Single values, like a date or a placename.
        elif isinstance(result[0], sparql.Literal):
            html = escape(result[0].value)
        # I just don't know.
        else:
            html = escape(str(result))

        return '<p>%s</p>' % html


class QuestionableHandler(BaseHTTPRequestHandler):

    service = QuestionableService()

    view = QuestionableView()

    def do_GET(self):
        self.send_response(200)
        self.send_header('Content-type', 'text/html')
        self.end_headers()

        template = Template('sparql-client/www/index.html')
        self.wfile.write(template.render().encode("utf-8"))

        return

    def do_POST(self):
        form = FieldStorage(
            fp=self.rfile,
            headers=self.headers,
            environ={
                'REQUEST_METHOD': 'POST',
                'CONTENT_TYPE': self.headers['Content-Type'],
            })

        query = form.getfirst('query', '').decode('utf-8')
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
            template.set('query', escapeattr(query))
            template.set('results', '\n'.join(response))

            self.wfile.write(template.render().encode('utf-8'))


server = HTTPServer(('', 8080), QuestionableHandler)
server.serve_forever()
