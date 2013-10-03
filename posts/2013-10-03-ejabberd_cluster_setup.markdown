---
title: ejabberd cluster setup
author: Gregor Uhlenheuer
tags: ejabberd, cluster, erlang, mnesia
summary: Short walkthrough on building a ejabberd clustered setup
---

Like I mentioned already in an [older post][post] on [ejabberd][ejabberd] - it
is a really great piece of software but the biggest problem when using it or
especially when you are troubleshooting is the lack of proper documentation or
help resources.

You may often just need to install ejabberd as it is on your server and have it
run like *forever* without the need of touching it. But problems arise if you
have to go off the main route of using the out-of-the-box setup of
[ejabberd][ejabberd] - i.e. you want to migrate your existing ejabberd into a
clustered setup or create a clustered ejabberd from the very beginning. I
didn't find much help on this topic so I will describe a small walkthrough on
how to setup some ejabberds in a cluster. The way I approach this task may not
be the best one but this is the way I found to be the most stable and
reproducable way I came up with after trying *a lot* of things on the way.


# Building ejabberd

The first step will be the building and installation of the ejabberd nodes
itself.  These steps will be repeated for every node you want to participate in
the cluster.


## Get the sources

At first we will fetch the latest sources from ProcessOne's repository on
[github][github]. As recommended by ProcessOne the version `2.x` is still the
stable branch for production mode. So we will checkout the appropriate branch
and compile from the sources. I have gone trough the described process for the
latest commits on the so called "community edition" of ejabberd as well - and
it works the same.

~~~ {.bash}
git clone git://github.com/processone/ejabberd.git
git checkout origin/2.1.x -b 2.1.x
cd ejabberd/src
~~~


## Building and installing

Next we will compile the sources with the familiar `configure`, `make`, `make
install` procedure. Probably you have to create the configure script with
`autoreconf`.

~~~ {.bash}
autoreconf -vif
./configure
make
sudo make install
~~~

Now your ejabberd is successfully compiled and installed on your (first) node
and is ready to be configured. In a non-clustered setup you would be almost
finished - after adjusting our `ejabberd.cfg` you could already start the
service by running `ejabberdctl start`.


## Configuration

This configuration part is actually the most important step of the setup. You
will probably have to edit the three following files:

* `ejabberd.cfg`: the main configuration file of ejabberd
* `ejabberdctl.cfg`: the configuration of the `ejabberdctl` control script
* `ejabberdctl`: the control script itself


### ejabberd.cfg

The ejabberd configuration can be found at `/etc/ejabberd/ejabberd.cfg`. There
are a few settings you will probably want to edit.

    $ vim /etc/ejabberd/ejabberd.cfg

~~~ {.erlang}
% adjust the logging level if you like
{loglevel, 3}.

% set the ejabberd domain(s)
{hosts, ["your.net"]}.

% set the admin user(s)
{acl, admin, {user, "admin", "your.net"}}.
~~~

If you have specific needs for ejabberd **modules** you can search for the
`modules` section in the configuration file and (un)comment the appropriate
modules:

~~~ {.erlang}
{modules, [
    {mod_adhoc, []},
    % {mod_irc, []},

    % ...
    % ...
]}.
~~~

At work we have the need to adjust the **shaping** settings as we are using the
XMPP messaging for internal communication between different services that may
exceed the default shaper limits.

~~~ {.erlang}
% normal shaper rule - the unit is B/s
{shaper, normal, {maxrate, 100000}}.

% fast shaper rule
{shaper, fast, {maxrate, 5000000}}.
~~~


### ejabberdctl.cfg

Basically you have to adjust two settings in your `ejabberdctl.cfg` file - the
configuration file of the ejabberd control script.

    $ vim /etc/ejabberd/ejabberdctl.cfg

~~~ {.bash}
# the listening address of ejabberd
#
# the default is set to 127.0.0.1 where the different ejabberd nodes
# would not be able to see each other
INET_DIST_INTERFACE={0.0.0.0}

# the ejabberd node name
#
# this setting is crucial and has to match with DNS and hostname
ERLANG_NODE=ejabberd@node1
~~~


### ejabberdctl

The last file we have to edit before we can start the first node is the control
script `ejabberdctl` itself. I don't really like to edit this file because
updates of ejabberd and subsequent runs of `make install` would override your
current settings. But sadly there is no way to set the ejabberd hostname in the
`ejabberdctl.cfg` apart from being passed as an environment variable.

    $ vim /sbin/ejabberdctl

~~~ {.bash}
# the ejabberd host name
#
# the host name defaults to 'localhost' but this has to match
# with the 'ERLANG_NODE' setting in your ejabberdctl.cfg
HOST=node1
~~~

The big advantage of editing the ejabberdctl script like this is that you can
later start, stop, restart the ejabberd just by running `ejabberdctl ...` in
your shell without caring about the correct ejabberd node name you are talking
with. At work we found this to be the safest way especially to people not 100%
aware of the ejabberd setup to execute basic control commands.


## Starting the first node

Now we are ready to fire up the first ejabberd node. After starting the service
you can register the root account you specified in your `ejabberd.cfg`.

    $ ejabberdctl start
    $ ejabberdctl register admin my.net ***

You should now be able to login into the web administration interface of
ejabberd using your accout: <http://node1:5280/admin>

By the way, the web interface is configured in `ejabberd.cfg` in the `listen`
section. You could for example modify the listening port like this:

~~~ {.erlang}
{listen, [
    {5999, ejabberd_http, [
        web_admin,
        % ...
        % ...
    ]},
    % ...
    % ...
]}.
~~~


# Second and subsequent nodes

After the first node is up and running we can proceed with building the next
nodes and joining those to the cluster.


## Building

The building and configuation of the other nodes can simply be repeated from
the first node steps with modifying host names.


## Join the cluster

After ejabberd is successfully built and installed when can proceed with the
joining to the running first node.


### Erlang cookie

First we have to exchange/synchronize the erlang cookie files. You could easily
copy the erlang cookie file from the first node like this:

~~~ {.bash}
scp -avz root@node1:/var/lib/ejabberd/.erlang.cookie /var/lib/ejabberd
~~~


### Connect mnesia

Now we are ready to connect the mnesia of the current node with the running
first node's mnesia database. We simply start an erlang shell and start a
mnesia in the specified directory `/var/lib/ejabberd`:

~~~ {.bash}
# kill any running erlang/epmd instances
killall epmd

# remove any existing mnesia files
rm -f /var/lib/ejabberd/*

# set HOME to ejabberd mnesia directory
export HOME=/var/lib/ejabberd

# start erlang shell with mnesia
erl -sname ejabberd@node2 -mnesia dir '"/var/lib/ejabberd"' -s mnesia
~~~

Now as we are in the erlang shell we can interactively connect the two mnesia
databases with each other. In case you are not sure what you are doing you can
exit the erlang shell every time with `<ctrl-c><ctrl-c>`.

~~~ {.erlang}
% check mnesia state for the current node:
% running db nodes = [node2]
mnesia:info().

% connect with first node
mnesia:change_config(extra_db_nodes, ["ejabberd@node1"]).

% now you should see two running nodes
% running db nodes = [node2, node1]
mnesia:info().

% copy schema table type
mnesia:change_table_copy_type(schema, node(), disc_copies).

% check mnesia state for
% disc_copies = [schema]
mnesia:info().

% copy tables from first node
% depending on the amount of data in your first node's database
% this may take a while
Tables = mnesia:system_info(tables).
[mnesia:add_table_copy(Tb, node(), Type) ||
    {Tb, [{'ejabberd@node1', Type}]} <- [ {T, mnesia:table_info(T, where_to_commit)} ||
        T <- Tables]].

% you should see output like the following:
[{atomic,ok},{atomic,ok},{atomic,ok},{atomic,ok},...]
~~~


## Join ejabberd to cluster

Now that the mnesia databases are connected you can start the second ejabberd.

    $ ejabberdctl start

You can use the web interface on both hosts to check for all running nodes:
<http://node1:5280/admin/nodes/>


# More nodes...

The described procedure can now be repeated for as many ejabberd nodes you have
and would like to join to the clustered setup.

When all ejabberds are up and running you can simply add `ejabberdctl
start/stop` to your distribution's init scripts and you have a **reboot
consistent ejabberd cluster**!


[ejabberd]: http://ejabberd.im
[github]: http://github.com/processone/ejabberd
[post]: 2013-01-16-writing_an_ejabberd_module.html
