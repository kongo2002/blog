---
tags: linux, x11, gentoo, console
author: Gregor Uhlenheuer
date: 2010-05-20
title: Changing X11 resolution on-the-fly
---

Since I always seem to forget how to change the X11 resolution without
restarting X11 I will post it here.  The tool to use is called *Xrandr* [^1].
You can check if the extension is loaded with some like:

    $ grep -i randr /var/log/Xorg.0.log

That should output something similar to:

    (==) RandR enabled
    (II) Initializing built-in extension RANDR

Once you know the Xrandr extension is loaded you can change the resolution via:

    $ xrandr --size 800x600

All available resolutions can be print to console with a simple call of
*Xrandr* without arguments:

    $ xrandr

[^1]: X11 resize and rotation extension
