---
tags: sql, tsql, programming
author: Gregor Uhlenheuer
date: 2011-09-24
title: CSV export in T-SQL
summary: Because I always forget how to export data from the SQLServer into a
         CSV file I will post it up here.
---
Because I always forget how to export data from the SQLServer into a CSV [^1]
file I will post it up here:

~~~ {.sql}
EXEC master..xp_cmdshell'bcp "SELECT * FROM table" queryout "c:\tmp\export.txt" -c -T -x'
~~~

Find more useful tips concerning `T-SQL` on [inpad.de][1]

[1]: http://www.inpad.de/?cat=67

[^1]: Comma separated file
