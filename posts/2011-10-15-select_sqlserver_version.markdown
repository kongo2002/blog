---
tags: tsql, sql, sqlserver
author: Gregor Uhlenheuer
title: Determine SQLServer version via query
summary: Here is how you can find out the version of your SQLServer with a
         query.
---
The very simple way to find out the version of your SQL Server that is
currently in use is:

~~~ {.sql}
SELECT @@version
~~~

The disadvantage of this command is that the whole version string is returned
from this query. In order to get more fine grained information about your SQL
Server you will want to use ``SERVERPROPERTY`` instead:

~~~ {.sql}
-- get the product version like '10.50.1617.0'
SELECT SERVERPROPERTY('productversion')

-- get the SQL Server edition like 'Express Edition'
SELECT SERVERPROPERTY('edition')
~~~

You can find further information about the SQL Server versions right here:
<http://sqlserverbuilds.blogspot.com/>
