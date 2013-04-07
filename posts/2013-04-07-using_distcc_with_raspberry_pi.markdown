---
title: Using distcc with your Raspberry Pi
tags: gentoo, linux, raspberrypi, distcc
author: Gregor Uhlenheuer
summary: Using gentoo on your Raspberry Pi may sound like a suboptimal choice
         considering the awful long compile times. To speed things up you can
         setup your main workstation to serve as a distcc build server.
---

Compiling and building of portage packages takes quite a while when using the
Raspberry Pi. Fortunately there is a nice way to use your main workstation's
(probably much greater) computation power to your Pi's good: [distcc][2]

**Diclaimer** : This article expects your build servers to run gentoo linux as
well and to be configured to cross compile Raspberry Pi compatible ARM
(arm6j-hardfloat-linux-gnueabi) binaries. Until I find some time to write a
small howto for that you can read the respective [gentoo documentation][3] on
*distcc* cross-compiling.


# Preparing your build server(s)

At first we are going to prepare all your build servers that should assist your
Raspberry Pi during the compilation process.


## Install distcc

First you have to install [sys-devel/distcc][1]:

    $ emerge -av sys-devel/distcc


## Configuration

After you have successfully installed *distcc* on your build server(s) you can
adjust the configuration to your likings. The configuration file usually can be
found at `/etc/conf.d/distcc`.

~~~ {.bash}
# set the access rights for your distcc daemon to the right network subnet
# you can also list single IP addresses
DISTCCD_OPTS="${DISTCCD_OPTS} --allow 192.168.1.0/24"

# especially during the setup phase I found increasing the log level very helpful
DISTCCD_OPTS="${DISTCCD_OPTS} --log-file /var/log/distccd"
DISTCCD_OPTS="${DISTCCD_OPTS} --log-level info"
~~~


## Start the daemon

Now you can start your *distcc* daemon using the init scripts:

    $ /etc/init.d/distccd start

Additionally you may want to add the *distcc* service to your default runlevel:

    $ rc-update add distccd default


# Prepare your Raspberry Pi

After you prepared your build server(s) you can move on to setup your Raspberry Pi.


## Install distcc

You have to install *distcc* on your Raspberry Pi as well:

    $ emerge -av sys-devel/distcc

Additionally you have to add *distcc* to your portage features. Edit your
`make.conf` appropriately:

    FEATURES="distcc"


## List build servers

Now you have to specify which build servers should be taken into account when
using *distcc*. In the `/etc/distcc/hosts` file you can list all server
addresses. The order defines the priorities:

~~~ {.bash}
#!/bin/sh

192.168.1.101
192.168.1.102
192.168.1.103
~~~


## Prepare GCC

Finally you have to tell distcc which compiler has to be used instead of `gcc`
- you can use a wrapper script like this for this purpose:

~~~ {.bash}
#!/bin/sh

exec /usr/bin/distcc/arm6j-hardfloat-linux-gnueabi-g${0:$[-2]} "$@"
~~~

Now you just have to replace the existing symbolic links like this:

~~~ {.bash}
# move in your distcc folder
$ cd /usr/lib/distcc/bin

# set the executable flag on the wrapper script
$ chmod +x wrapper

# remove the old symlinks
$ rm cc c++ gcc g++

# link to the wrapper script
$ ln -s wrapper cc
$ ln -s wrapper c++
$ ln -s wrapper gcc
$ ln -s wrapper g++
~~~


# Test your setup

Now that all necessary steps are taken you can test the *distcc* setup when
emerging a cross-compile compatible package.

    $ emerge -va htop

You can observe the *distcc* daemon log on one of your build servers in order
to check if your build servers are utilized during the compilation phase:

    $ tail -f /var/log/distccd

[1]: http://packages.gentoo.org/package/sys-devel/distcc/
[2]: https://code.google.com/p/distcc/
[3]: http://www.gentoo.org/doc/en/cross-compiling-distcc.xml
