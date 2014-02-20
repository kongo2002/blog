---
tags: programming, erlang, ejabberd, python, authentication, script
author: Gregor Uhlenheuer
title: External ejabberd authentication script
summary: Example on how to use a python script for the external ejabberd
         authentication mechanism
---

Once again I am fiddling with [ejabberd][ejabberd] - this time it is the
**external authentication** method using a [python][python] script.


## Authentication in ejabberd

The ejabberd XMPP server supports several authentication mechanisms
out-of-the-box:

- **internal**: the default authentication using the [Mnesia][mnesia] database
  backend

- **ldap**: authentication against a LDAP server/directory

- **pam**: user authentication using PAM (pluggable authentication modules)
  that is currently supported by FreeBSD, Linux, Mac OSX, NetBSD, Solaris ...

- **odbc**: ODBC connection using either the PostgreSQL or MySQL interfaces

- **external**: using an external authentication script

- **anonymous**

For this post we will focus on the **external** authentication method of
ejabberd. This mechanism allows the use of an authentication script that may be
written in practically any language.

Ejabberd will invoke a configurable number of instances of the script and pass
the authentication requests via `stdin` and expects the result on `stdout`.
This method may seem somewhat awkward but is very flexible on the other hand.

Moreover this approach allows you to profit from authentication methods of
existing services. Say you already have a user administration interface in your
business layer this may be your preferred way to reuse that logic.


## Configuration

To use the external authentication script you have to edit the ejabberd
configuration in your `ejabberd.cfg`.


### General authentication

To enable external authentication for the whole ejabberd instance use something
like this:

~~~ {.erlang}
% toggle external authentication
{auth_method, external}.

% specify the full path to the script
{extauth_program, "python /usr/bin/auth.py http://localhost:8000/auth/"}

% number of instances to start per virtual host
{extauth_instances, 1}.

% whether to cache authentication for a specified amount of seconds
% requires the `mod_last` module
{extauth_cache, false}.
~~~


### External authentication per virtual host

Alternatively you may activate external authentication on a 'per virtual host'
base:

~~~ {.erlang}
% use default internal authentication for "foo.domain"
{host_config, "foo.domain",
    [
     {auth_method, [internal]}
    ]}.

% use the external authentication script for "bar.domain"
{host_config, "bar.domain",
    [
     {auth_method, [external]},
     {extauth_program, "python /usr/bin/auth.py http://localhost:8000/auth/"}
    ]}.
~~~


## Python script

In my case I chose [python][python] as my favorite scripting language for this
task. [Python][python] is platform independent and comes with a lot of existing
high-level libraries to make such a problem easy to implement.  You may find
the script I came up with below or fetch the latest version from
[github][github].

~~~ {.python}
#!/usr/bin/env python

# Copyright 2014 Gregor Uhlenheuer
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


import argparse
import json
import logging
import os
import struct
import sys
import urllib2


#
# DEFAULTS AND CONSTANTS
#

DEFAULT_LOG_DIR = '/var/log/ejabberd'
FALLBACK_URL = 'http://localhost:8000/auth/'
HEADERS = {
    'Content-Type': 'application/json',
    'Accept': 'application/json' }


#
# CLASS DEFINITIONS
#

class EjabberdError(Exception):
    '''Exception class that holds ejabberd related errors.'''

    def __init__(self, ex):
        self.ex = ex

    def __str__(self):
        return repr(self.ex)

class ApiHandler:
    '''
    Class to execute HTTP requests to process
    the authentication orders from ejabberd.
    '''

    def __init__(self, url, headers):
        '''Initialize an ApiHandler instance.'''

        self.url = url
        self.headers = headers

    def call(self, call, data):
        '''
        Call the specified authentication API using the
        urllib2 library functions.
        '''

        url = '%s/%s' % (self.url, call)
        req = urllib2.Request(url, data, self.headers)
        res = urllib2.urlopen(req)

        return json.load(res)

class EjabberdAuth:
    '''
    Class that encapsulates the ejabberd authentication logic.
    '''

    def __init__(self, url, headers, handler=None):
        '''
        Initialize a new EjabberdAuth instance.
        '''
        self.url = url
        self.headers = headers

        if handler is None:
            self.handler = ApiHandler(url, headers)
        else:
            self.handler = handler

    @staticmethod
    def make_jid(user, host):
        '''Build a JID using the given user and host'''

        return '%s@%s' % (user, host)

    def __from_ejabberd(self):
        '''
        Listen on stdin and read input data sent from the
        connected ejabberd instance.
        '''

        try:
            input_length = sys.stdin.read(2)

            if len(input_length) is not 2:
                logging.warn('ejabberd called with invalid input')
                return None

            (size,) = struct.unpack('>h', input_length)
            result = sys.stdin.read(size)

            logging.debug('Read %d bytes: %s', size, result)

            return result.split(':')
        except IOError:
            raise EjabberdError('Failed to read from ejabberd via stdin')

    def __to_ejabberd(self, success):
        '''
        Convert the input data into an ejabberd compatible
        format and send it to stdout.
        '''

        answer = 1 if success else 0
        token = struct.pack('>hh', 2, answer)

        sys.stdout.write(token)
        sys.stdout.flush()

        logging.debug('Returned %s success', 'with' if success else 'without')

    def __call_api(self, call, data):
        '''
        Call the JSON compatible API handler with the specified data
        and parse the response for a success.
        '''

        body = json.dumps(data)
        result = self.handler.call(call, body)

        success = result['success']

        if not success:
            msg = result['message']
            logging.warn('Call to API returned without success: ' + msg)

        return success

    def __auth(self, username, server, password):
        '''Try to authenticate the user with the specified password.'''

        logging.debug('Processing "auth"')

        jid = make_jid(username, server)
        data = {'username': jid, 'password': password}

        return self.__call_api('login', data)

    def __isuser(self, username, server):
        '''Try to find the specified user.'''

        logging.debug('Processing "isuser"')

        jid = make_jid(username, server)
        data = {'username': jid}

        return self.__call_api('exists', data)

    def __setpass(self, username, server, password):
        '''Try to set the user's password.'''

        logging.debug('Processing "setpass"')

        # TODO
        return False

    def loop(self):
        '''
        Start the endless loop that reads on stdin and passes
        the authentication results to stdout towards the
        connected ejabberd instance.
        '''

        while True:
            try:
                data = self.__from_ejabberd()
            except KeyboardInterrupt:
                logging.info('Terminating by user input')
                break
            except EjabberdError, err:
                logging.warn('Input error: ' + err)
                break

            success = False
            cmd = data[0]

            if cmd == 'auth':
                success = self.__auth(data[1], data[2], data[3])
            elif cmd == 'isuser':
                success = self.__isuser(data[1], data[2])
            elif cmd == 'setpass':
                success = self.__setpass(data[1], data[2], data[3])
            else:
                logging.warn('Unhandled ejabberd cmd "%s"', cmd)

            self.__to_ejabberd(success)


def get_args():
    '''
    Parse some basic configuration from command line arguments.
    '''

    # build command line argument parser
    desc = 'ejabberd authentication script'
    parser = argparse.ArgumentParser(description=desc)

    # base url
    parser.add_argument('url',
            nargs='?',
            metavar='URL',
            default=FALLBACK_URL,
            help='base URL (default: %(default)s)')

    # log file location
    parser.add_argument('-l', '--log',
            default=DEFAULT_LOG_DIR,
            help='log directory (default: %(default)s)')

    # debug log level
    parser.add_argument('-d', '--debug',
            action='store_const', const=True,
            help='toggle debug mode')

    args = vars(parser.parse_args())

    return args['url'], args['debug'], args['log']


if __name__ == '__main__':
    URL, DEBUG, LOG = get_args()

    LOGFILE = LOG + '/extauth.log'
    LEVEL = logging.DEBUG if DEBUG else logging.INFO
    PID = str(os.getpid())
    FMT = '[%(asctime)s] ['+PID+'] [%(levelname)s] %(message)s'

    # redirect stderr
    ERRFILE = LOG + '/extauth.err'
    sys.stderr = open(ERRFILE, 'a+')

    # configure logging
    logging.basicConfig(level=LEVEL, format=FMT, filename=LOGFILE)

    logging.info('Starting ejabberd auth script')
    logging.info('Using %s as base URL', URL)
    logging.info('Running in %s mode', 'debug' if DEBUG else 'release')

    EJABBERD = EjabberdAuth(URL, HEADERS)
    EJABBERD.loop()

    logging.warn('Terminating ejabberd auth script')
~~~


## References

- Python script on [github][github]
- External authentication scripts on [ejabberd.im][extauth]
- ejabberd [documentation][doc]


[ejabberd]: http://ejabberd.im/
[github]: https://github.com/kongo2002/ejabberd-python-extauth/
[python]: http://python.org/
[mnesia]: http://www.erlang.org/doc/man/mnesia.html
[extauth]: http://www.ejabberd.im/extauth/
[doc]: http://www.process-one.net/docs/ejabberd/guide_en.html#extauth
