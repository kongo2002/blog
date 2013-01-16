---
title: Writing an ejabberd HTTP module
tags: programming, erlang, ejabberd
author: Gregor Uhlenheuer
summary: Small introduction on how to write a custom HTTP module for ejabberd
---

Recently at work we were thinking about how we could retrieve some meta
information from our [ejabberd][1] server that is not accessible via the web
interface or a predefined command of `ejabberdctl`. For those of you that do
not know what *ejabberd* is: it's a very popular jabber/XMPP server or daemon
written in [Erlang][2]. After maybe half an hour of googling around and not
finding some *ready-to-use* solution we pretty much discarded the idea and
moved on to other problems.

Nevertheless today at home I got interested again and did some more research on
extending the basic ejabberd functionality. The good part is that ejabberd
comes with a built-in module system that allows you to add your own erlang
modules into ejabberd and even hook into some predefined events (though I did
not get to that part). The downside is the fact that ejabberd cannot be
described as being well documented. So many links, guides or further related
information found on the [message board][3] or FAQ's are broken or horribly
outdated.

## Basic module structure

Anyways, in the following parts I will shortly describe what I came up with so
far. The described module does not contain any helpful functionality but the
structure on how to built such a module is more important here than the actual
implementation.

In order to add a new module into ejabberd you have to implement the OTP
behavior `gen_mod` which expects two functions to be implemented:

* `start/2`: module initialization
* `stop/1`: module termination

In our case we want to build a HTTP module so we want to additionally implement
the `process/2` function that handles all HTTP requests that are routed to the
module.

The rough outline of our HTTP module will look like this:

~~~{.erlang}
%% Module name (has to match with the filename)
-module(mod_custom).

%% Module author
-author('Gregor Uhlenheuer').

%% Module version
-vsn('1.0').

%% Debug flag
-define(EJABBERD_DEBUG, true).

%% Implement the OTP gen_mod behavior
-behavior(gen_mod).

%% Module exports
-export([start/2, stop/1, process/2]).

%%
%% INCLUDES
%%

%% base ejabberd headers
-include("ejabberd.hrl").

%% ejabberd compatibility functions
-include("jlib.hrl").

%% ejabberd HTTP headers
-include("web/ejabberd_http.hrl").

%% initialization function
start(_Host, _Opts) ->
    ok.

%% function on module unload
stop(_Host) ->
    ok.

%% process any request to "/sockets"
process(["sockets"], _Request) ->
    % FIXME: implementation goes here
    "Not implemented yet";

%% process all remaining requests
process(_Page, _Request) ->
    % FIXME: implementation goes here
    "Fallback result".
~~~

So this is basically the whole module structure you need to get started with
the actual implementation.

## Compiling the module

Next we have to compile the module itself and adjust the ejabberd configuration
in order to integrate our newly built module.

    # move into your source directory
    $ cd mod_custom/src

You have to pass the file paths to your erlang/ejabberd header files referenced
in your module file (`ejabberd.hrl`, `jlib.hrl` and `ejabberd_http.hrl`):

    # compile using erlc
    $ erlc -I ../ejabberd/src \
         -I /lib64/ejabberd/include \
         -pa ../ejabberd/src \
         mod_custom.erl

## Configuring ejabberd

Before starting the ejabberd server we have to add the module to the main
configration file `ejabberd.cfg`. Somewhere in your config file you will find the part of the `ejabberd_http` setting:

~~~{.erlang}
% the part will probably look something like this
{5280, ejabberd_http, [http_poll, web_admin]}
~~~

You add a new request handler to the `ejabberd_http` part and you are good to go:

~~~{.erlang}
% this will probably look like this
{5280, ejabberd_http, [http_poll, web_admin,
        {request_handlers, [
            % your request handler will respond to anything like:
            % http://example.com:5280/custom/
            {["custom"], mod_custom}
        ]}
    ]}
~~~


Now you can copy the compiled beam file `mod_custom.beam` into your ejabberd
`ebin` directory and (re)start the ejabberd service:

    $ cp mod_custom.beam /lib64/ejabberd/ebin
    $ ejabberdctl restart

## Testing the module

Now you should be able to request your new module function via HTTP:

    $ curl -v localhost:5280/custom/sockets
    * About to connect() to localhost port 5280 (#0)
    *   Trying 127.0.0.1...
    * connected
    * Connected to localhost (127.0.0.1) port 5280 (#0)
    > GET /custom/sockets HTTP/1.1
    > User-Agent: curl/7.26.0
    > Host: localhost:5280
    > Accept: */*
    >
    < HTTP/1.1 200 OK
    < Content-Type: text/html; charset=utf-8
    < Content-Length: 19
    <
    Not implemented yet
    * Closing connection #0

## Tipps

While experimenting and searching for ways to get going with the ejabberd
module I stumbled upon a great way to modiy, compile and test your changes.

### Reload your module in a remote erlang shell

Instead of manually recompiling your erlang module, copying into your `ebin`
folder and restarting your ejabberd server you can just remotely connect to
your running ejabberd node and inspect your service during execution.

You can either start your ejabberd in `debug` mode and execute your commands from there:

    $ ejabberdctl debug

Or you can remotely attach to an already running ejabberd node:

    $ erl -sname node1 -remsh ejabberd@someserver

In case you get an error like the following:

    *** ERROR: Shell process terminated! (^G to start new job) ***

You have to pass your *erlang cookie* along with your `erl` command:

    $ erl -sname node1 -remsh ejabberd@someserver -setcookie *****

### Compile and reload modules without restarting

Now you can easily compile and reload your module from within your remote shell
without restarting the ejabberd service:

~~~{.erlang}
% compile
c(mod_custom).

% or just reload
l(mod_custom).
~~~

[1]: http://www.ejabberd.im
[2]: http://erlang.org
[3]: http://www.ejabberd.im/support
