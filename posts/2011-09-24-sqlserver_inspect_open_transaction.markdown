---
tags: sql, programming, sqlserver
author: Gregor Uhlenheuer
date: 2011-09-24
title: Inspect an open transaction in SQLServer
summary: There is a handy command to inspect an open transaction running in a
         Microsoft SQLServer.
---
In case you are currently processing a huge amount of data in a transaction in
your MS SQLServer you are usually not able to look into the already processed
data. Enter the following command in your Management Studio in order to enable
a sort of `live-inspection`:

~~~ {.sql}
SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED
~~~

I found this really helpful tip on [inpad.de][1]

[1]: http://www.inpad.de/?p=383
