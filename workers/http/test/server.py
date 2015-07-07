#!/usr/bin/env python

import cgi
from contextlib import contextmanager
import json
from multiprocessing import Process
import SimpleHTTPServer
import SocketServer
import sys
import random

port = random.randint(8777, 8888)

class ServerHandler(SimpleHTTPServer.SimpleHTTPRequestHandler):

    def do_GET(self):
        self.send_response(200)
        self.send_header('Content-Type', 'text/plain')
        self.end_headers()
        self.wfile.write("welcome")

    def do_POST(self):
        length = int(self.headers['Content-Length'])
        body = json.loads(self.rfile.read(length))

        if 'color' in body and body['color'] == 'red':
            self.send_response(200)
        else:
            self.send_response(404)

        self.send_header('Content-Type', 'text/plain')
        self.end_headers()
        self.wfile.write("OHAI")


def serve():
    handler = ServerHandler
    httpd = SocketServer.TCPServer(("", port), handler)
    httpd.serve_forever()


@contextmanager
def background_server():
    server = Process(target=serve)
    server.start()
    try:
        yield
    finally:
        server.terminate()

if __name__ == '__main__':
    serve()