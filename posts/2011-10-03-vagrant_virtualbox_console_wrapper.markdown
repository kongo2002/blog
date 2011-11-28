---
tags: linux, gentoo, console, virtualization
author: Gregor Uhlenheuer
date: 2011-10-03
title: Vagrant - console wrapper for VirtualBox
summary: Vagrant is a nice and convenient tool to dynamically build
         configurable, lightweight and portable virtual machines using Oracle's
         VirtualBox.
---
[Vagrant][1] is a nice and convenient tool to dynamically build configurable,
lightweight and portable virtual machines using [Oracle's VirtualBox][2]. Using
this tool it is possible to quickly establish, configure and initialize new
development environments. It is even possible to package already configured
environments for deployment so other developers can recreate the virtual
machine with just a few commands.

## Installation

To get you going as quickly as possible I am shortly describing with steps you
have to do to install Vagrant on your system.

### Ruby and RubyGems

Vagrant is written in Ruby and published as a RubyGem – so you have to install
Ruby and RubyGems_ at first. In my case on Gentoo Linux this is nothing more
than running:

    $ emerge -av ruby rubygems

### VirtualBox

Since Vagrant is utilizing VirtualBox you have to install that one of course.
VirtualBox is an open-source full virtualizer for x86 hardware and runs on
Windows, Linux, Mac OSX and Solaris. In order to install VirtualBox you either
use your distribution's package manager or go the [download page][3] and install
it manually. On Gentoo Linux you can use `emerge` of course.

    $ emerge -av ">=virtualbox-4.1"

It is important to note that only the versions 4.1.x of VirtualBox are
compatible with Vagrant. If you are running on a stable gentoo profile you
currently have to unmask the version 4.1.2 of VirtualBox by adding the
following lines to your `package.keywords` file:

    $ echo "app-emulation/virtualbox
            app-emulation/virtualbox-modules
            app-emulation/virtualbox-additions
            dev-util/kbuild" >> /etc/portage/package.keywords

Moreover it might be necessary to add the `qt4` USE flag in order to build
the `VirtualBox` executable:

    $ echo "app-emulation/virtualbox qt4" >> /etc/portage/package.use

Now all there is to do is to add your user to the `vboxusers` group and start
the necessary virtualbox kernel modules:

    $ usermod -a -G vboxusers <username>
    $ modprobe vboxdrv vboxnetflt vboxnetadp

Instead of manually starting the virtualbox kernel modules every time you can
also autoload them by modifying the `/etc/conf.d/modules` file accordingly.

### Vagrant

Once this is done all requirements are set you can go ahead and install Vagrant
itself:

    $ gem install vagrant

On my gentoo machine I had some trouble using [gem][4] via [RVM][5] [^1] because the
gentoo developers set the `RUBYOPT` environment variable to `-rauto_gem` by
default. In this case you would have to unset the variable beforehand:

    $ unset RUBYOPT
    $ gem install vagrant

### Virtual environment box

Now that Vagrant is installed you are able to fetch a prebuilt virtual machine
image (called "box") and build a new development environment based on that:

    $ vagrant box add newbox http://files.vagrantup.com/lucid32.box
    $ vagrant init newbox
    $ vagrant up

The default box named "lucid32" which you usually use is a bare bone
installation of *32-bit Ubuntu Lucid (10.04)*. The name "newbox" is just an
arbitrary name for the fresh box image – you can choose whatever name you like.

In case you encounter problems with ssh'ing into your new virtual environment
on executing `vagrant up` or `vagrant ssh` you should check if you define an
alias of *localhost* in your `/etc/hosts` file like:

    127.0.0.1 localhost name alias

After I removed the alias Vagrant worked like it is supposed to.

[^1]: RVM: Ruby Version Manager

[1]: http://vagrantup.com
[2]: http://virtualbox.org
[3]: http://virtualbox.org/wiki/Downloads
[4]: http://rubygems.org
[5]: http://beginrescueend.com
