---
title: ejabberd: message archive management (XEP-0313)
author: Gregor Uhlenheuer
tags: programming, erlang, ejabberd, archive, module
summary: I have started implementing an ejabberd module that targets the
         experimental 'Message Archive Management' (XEP-0313) specification.
---

Lately programming with [erlang][erlang] fascinates me a lot. Specifically
hacking the XMPP server [ejabberd][ejabberd] draws my interest the most. So I
started implementing the *message archive management* XMPP extension described
in the experimental [XEP-0313][xep313] specification.


# ejabberd message archiving

ejabberd already supports some message archiving *out-of-the-box* by the
`mod_archive` module that is part of the ejabberd [community scripts][scripts]
repository.  That module implements the [XEP-0136][xep136] specification and
supports storage via mnesia, PostgreSQL, MySQL or sqlite.

For my approach I chose to implement the [XEP-0313][xep313] standard instead
because it is known to be less complicated and therefore easier to implement by
clients, as stated by the specification:

> "This specification aims to define a much simpler and modular protocol for
> working with a server-side message store. Through this it is hoped to boost
> implementation and deployment of archiving in XMPP."

Moreover I plan to use the [MongoDB][mongo] database as storage engine.


# ejabberd-mod-mam

You can find the latest version of **mod-mam** on [github][github]. The module
targets the current master branch of *ejabberd* which is the so called
"community edition".

The latest version of **mod-mam** supports:

- basic message archiving
- parts of the [RSM][rsm] (result set management) extension

The following points are on my TODO list:

- fully implement RSM ([XEP-0059][rsm])
- ejabberd fork/branch with integrated mod\_mam
- support for MongoDB replica sets
- archiving preferences
- unit tests


## Configuration

In order to use mod-mam you have to add it to the modules section in your
`ejabberd.cfg`. This could look like this:

~~~ {.erlang}
{modules,
  [
    {mod_mam,
      [
        % use the default localhost:27017
        % or define a specific host
        {mongo, {localhost, 27017}},

        % define a database to use
        % (default: test)
        {mongo_database, test},

        % specify a collection to use
        % (default: ejabberd_mam)
        {mongo_collection, ejabberd_mam}
      ]
    },

    % ...
  ]
}.
~~~


## MongoDB content

Just to give a short impression on how the content is stored, this is what an
archived message looks like inside the MongoDB collection:

~~~ {.javascript}
{
    "_id" : ObjectId("52f7fc9ecdbb08255f000002"),
    "user" : "test2",
    "server" : "localhost",
    "jid" : {
        "user" : "test",
        "server" : "localhost",
        "resource" : "sendxmpp"
    },
    "body" : "foo",
    "direction" : "to",
    "ts" : ISODate("2014-02-09T22:09:34.282Z"),
    "raw" : "<message xml:lang='en' to='test@localhost' type='chat'><body>foo</body><subject/></message>"
}
~~~


## Example archive query

An examplary archive query conversion between client and server would look like
the following. At first the client queries for its last two messages (limited by
a [RSM][rsm] instruction):

~~~ {.xml}
<iq type='get'
    id='query1'>
  <query xmlns='urn:xmpp:mam:tmp'
         queryid='x01'>
    <set xmlns='http://jabber.org/protocol/rsm'>
      <max>2</max>
    </set>
  </query>
</iq>
~~~

The server responds with the two requested messages:

~~~ {.xml}
<message xmlns='jabber:client'
         to='test@localhost/37367024071393189531836643'>
  <result queryid='x01'
          xmlns='urn:xmpp:mam:tmp'
          id='52F7FC8DCDBB08255F000001'>
    <forwarded xmlns='urn:xmpp:forward:0'>
      <delay xmlns='urn:xmpp:delay'
             stamp='2014-02-09T22:09:17Z'/>
      <message from='test2@localhost/sendxmpp'
               to='test@localhost'
               xml:lang='en'
               type='chat'>
        <body>
          foo
        </body>
      </message>
    </forwarded>
  </result>
</message>
<message xmlns='jabber:client'
         to='test@localhost/37367024071393189531836643'>
  <result queryid='x01'
          xmlns='urn:xmpp:mam:tmp'
          id='52F7FC9ECDBB08255F000003'>
    <forwarded xmlns='urn:xmpp:forward:0'>
      <delay xmlns='urn:xmpp:delay'
             stamp='2014-02-09T22:09:34Z'/>
      <message from='test2@localhost/sendxmpp'
               to='test@localhost'
               xml:lang='en'
               type='chat'>
        <body>
          bar
        </body>
      </message>
    </forwarded>
  </result>
</message>
~~~

Finally the server finishes the interaction with the closing `<id>` stanza:

~~~ {.xml}
<iq xmlns='jabber:client'
    to='test@localhost/37367024071393189531836643'
    id='query1'
    type='result'/>
~~~


# Feedback

As **mod-mam** is still in early beta phase any feedback, bug reports or
contributions are very much appreciated. Either contact me
[directly](mailto:kongo2002@gmail.com) or better head over to the
[issues][issues] on github and open a bug report or pull request.


# References

- mod-mam repository on [github][github]
- [XEP-0313][xep313]: message archive management
- [XEP-0136][xep136]: message archiving
- [XEP-0059][rsm]: result set management


[ejabberd]: http://ejabberd.im/
[xep136]: http://xmpp.org/extensions/xep-0136.html
[xep313]: http://xmpp.org/extensions/xep-0313.html
[scripts]: https://github.com/processone/ejabberd-contrib/
[mongo]: http://mongodb.org/
[erlang]: http://erlang.org/
[github]: https://github.com/kongo2002/ejabberd-mod-mam/
[rsm]: http://xmpp.org/extensions/xep-0059.html
[issues]: https://github.com/kongo2002/ejabberd-mod-mam/issues/
