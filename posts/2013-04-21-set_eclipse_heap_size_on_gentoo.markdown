---
tags: gentoo, eclipse, java, scala
title: Eclipse: Maximum heap size on gentoo
author: Gregor Uhlenheuer
summary: I had some troubles to increase the maximum heap size used by eclipse
         on my gentoo installation. Read on to see how to solve it.
---

Yesterday I started looking into programming an [Android][1] app in [Scala][2]
and finally ended up using [eclipse][3] as the IDE. Right during my first
compile I ran into an `OutOfMemoryException`. The reason was because *eclipse*
was started with a maximum of *256 MB* of heap space which is obviously not
enough for all the used plugins to build an *android* app.

Actually the hard part was now to increase this maximum heap space. The eclipse
I am using is the gentoo package `dev-util/eclipse-sdk-bin` of the
[java-overlay][4]. At the end I discovered there are *at least* three
possibilities you can try:


# Change eclipse's INI file

The first thing to try is to change eclipse's .INI file. On my gentoo
installation the file can be found in the `/opt` folder:

    $ vim /opt/eclipse-sdk-bin-*/eclipse.ini

Now you can search for lines looking similar to this:

    -showsplash
    org.eclipse.platform
    --launcher.XXMaxPermSize
    256m
    -vmargs
    -Xms40m
    -Xmx256m

The interesting lines are the last two:

* `-Xms`: the initial heap size
* `-Xmx`: the maximum used heap size


# Pass VM arguments to your JRE

The next possibility is to adjust your VM arguments passed to your JRE used by
eclipse. In eclipse you can navigate to `Window → Preferences → Java →
Installed JREs` and selecting `Edit...` on the JRE being used. In the following
dialog you can set the *Default VM Arguments* to something like this:

    -Xms512m -Xmx1024m


# Edit the gentoo specific launcher script

Editing the gentoo launcher script for eclipse was actually the one that
finally solved my problems. I ended up editing the gentoo specific
configuration file that is used by the launcher script. I discovered that by
looking into:

    $ vim `which eclipse-bin-4.2`

The two important lines are right at the top:

~~~ { .bash }
[ -f "/etc/eclipserc-bin-${SLOT}" ] && . "/etc/eclipserc-bin-${SLOT}"
[ -f "$HOME/gentoo/.eclipserc" ] && . "$HOME/gentoo/.eclipserc"
~~~

This means to either use the system-wide configuration file
`/etc/eclipse-bin-4.2` (replace `4.2` with the `$SLOT` of your eclipse version)
or the file `gentoo/.eclipserc` in your home directory. In my opinion the
folder `gentoo` inside the home directory is not the ideal place to store
config files, so I use the system-wide setting instead.

Now all you have to do is to create/edit the file of your liking and put the
following lines in it:

~~~ { .bash }
ECLIPSE_XMS=512m
ECLIPSE_XMX=2048m

ECLIPSE_PERMSIZE=512m
ECLIPSE_MAX_PERMSIZE=2048m
~~~


## Check your configuration

In order to check your configuration changes you can easily look on your
running processes (i.e. use `htop`) and verify your parameters are passed the
way you want them to:

![Screenshot of htop](/images/htop_eclipse.png)

[1]: http://android.com
[2]: http://scala-lang.com
[3]: http://eclipse.org
[4]: https://overlays.gentoo.org/svn/proj/java/java-overlay/
