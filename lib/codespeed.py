from contextlib import contextmanager
import re
import time
import urllib
import urllib2

SERVER_URL = 'http://saas-1-61.plat.dev.dal2.mz-inc.com/'

@contextmanager
def run_timer(timer, test_name):
    try:
        if timer:
            timer.start(test_name)
        yield
    finally:
        if timer:
            timer.stop_and_post()


class CodeSpeedMetric:
    def __init__(self, project, revision, benchmark):
        self.project = project
        self.revision = revision
        self.benchmark = benchmark

    def post(self, test_name, value):
        post_to_server(self.project, self.revision, test_name, self.benchmark, value)


class CodeSpeedLoopMetric:
    def __init__(self, project, revision, benchmark, loop_duration):
        self.project = project
        self.revision = revision
        self.benchmark = benchmark
        self.loop_duration = loop_duration

    def post(self, test_name, values):
        values = [int(v) / self.loop_duration for v in values]
        post_to_server(self.project, self.revision, test_name, self.benchmark, sum(values))


class CodeSpeedTimer:
    def __init__(self, project, revision):
        self.project = project
        self.revision = revision

    def start(self, test_name):
        self.started = time.time() 
        self.test_name = test_name

    def stop_and_post(self):
        time_spent = time.time() - self.started
        post_to_server(self.project, self.revision, self.test_name, "Time elapsed", time_spent)


def post_to_server(project, revision, test_name, benchmark, value, **kwargs):
    data = {
        "commitid" : revision.strip(), 
        "project": project,
        "branch": "default",
        "executable": test_name,
        "benchmark": benchmark,
        "environment": "GO.CD",
        "result_value": value
    }
    # std_dev, min, max, etc.
    data.update(kwargs)

    params = urllib.urlencode(data)
    try:
        urllib2.urlopen(SERVER_URL + 'result/add/', params)
    except urllib2.HTTPError as e:
        print e
        if e.getcode() == 500:
            html = e.read()
            print "Summary:", re.search(r"<title>([^<]*)</title>", html).group(1)
            print "HTML:", html
        raise RuntimeError("Failed to post data to codespeed server")

