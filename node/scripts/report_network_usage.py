#!/usr/bin/env python

"""
Prints network usage in erlang-friendly format
"""

import os
import re
import subprocess

def main():
    uname = os.uname()[0]

    if uname == 'Darwin':
        p = subprocess.Popen(['netstat', '-ib'], stdout=subprocess.PIPE)
        (netstat_output, err) = p.communicate()
        lines = netstat_output.split('\n')

        column_titles = re.split(r'\s+', lines[0])
        name_column_index = column_titles.index('Name')
        rx_column_index = column_titles.index('Ibytes')
        tx_column_index = column_titles.index('Obytes')

        def data():
            yielded_names = set()
            for line in lines[1:]:
                if not line:
                    continue
                fields = re.split(r'\s+', line)
                name = fields[name_column_index]
                if name not in yielded_names:
                    yield {'name': name,
                        'rx_bytes': fields[rx_column_index],
                        'tx_bytes': fields[tx_column_index]}
                    yielded_names.add(name)

        stats = list(data())
    elif uname == 'Linux':
        p = subprocess.Popen(['netstat', '-ine'], stdout=subprocess.PIPE)
        (netstat_output, err) = p.communicate()
        (_header, _newline, body) = netstat_output.partition('\n')
        netstat_sections = body.split('\n\n')
        stats = [parse_netstat_section(section)
            for section in netstat_sections
            if section]
    else:
        raise RuntimeError("Getting network usage on {0} is not supported.".format(uname))

    print format_value(stats) + '.'


def parse_netstat_section(s):
    if not s:
        return None

    try:
        name = s.split(' ')[0]
        rx_bytes = re.search(r'RX bytes:(\d+)', s).group(1)
        tx_bytes = re.search(r'TX bytes:(\d+)', s).group(1)
        return {'name': name,
            'rx_bytes': int(rx_bytes),
            'tx_bytes': int(tx_bytes)}
    except:
        # Some interface sections don't have transmitted bytes info
        # so we ignore them.
        # Example from a travis-ci vm:
        #
        # venet0:0  Link encap:UNSPEC  HWaddr 00-00-00-00-00-00-00-00-00-00-00-00-00-00-00-00
        #           inet addr:1.2.3.4  P-t-P:5.6.7.8  Bcast:0.0.0.0  Mask:255.255.255.255
        #           UP BROADCAST POINTOPOINT RUNNING NOARP  MTU:1500  Metric:1
        return None


def format_value(value):
    if isinstance(value, dict):
        body = ', '.join('{0} => {1}'.format(k, format_value(v))
            for k, v in value.iteritems())
        return '#{' + body + '}'
    elif isinstance(value, list):
        body = ', '.join((format_value(v) for v in value))
        return '[' + body + ']'
    elif isinstance(value, int):
        return str(value)
    else:
        return '"{0}"'.format(value)


if __name__ == '__main__':
    main()