---
title: Clean your system with udept
author: Gregor Uhlenheuer
tags: linux, vim, programming
date: 2007-12-20
summary: In order to clean up your portage-installed packages there is a great
         tool called udept
---

From time to time it can be very useful to check all of the installed packages
on the system. Often emerged packages pull a whole bunch of dependencies on the
system. The problem is that these dependencies are not removed when the package
that pulled in the other packages is unmerged.

Officially there is a portage tool named `depclean` (`emerge -–depclean`) that
searches for unused packages that are not necessary for other packages. But
everyone who once tried this tool can say that it’s not working that great.
Often really important packages are found to be unmerged and would badly damage
the system.

A useful alternative for this problem is a tool named *udept* (which can be
found in the portage tree in *app-portage/udept*) in my experience it’s working
nearly failure-free and relatively fast on the top. So just give it a try –
it’s worth testing

Some useful commands:
---------------------

    dep -dp                (depclean-mode with pretend-flag)
    dep -L <package-name>  (reverse dependencies from <package-name>)
    dep -l <package-name>  (dependencies from <package-name>)
    dep -Ln <package-name> (reverse dependencies (+uninstalled) from <package-name>)
