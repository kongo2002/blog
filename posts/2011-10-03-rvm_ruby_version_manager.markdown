---
tags: gentoo, linux, ruby
title: RVM - ruby version manager
author: Gregor Uhlenheuer
summary: RVM is a pretty convenient ruby version manager that enables easy
         management of parallel ruby environments.
---
During my attempts to install [Vagrant][1] [^1] on my gentoo machine I stumpled
upon [RVM][2] – the ruby version manager. Using RVM you are able to maintain
multiple ruby environments in parallel. So you can have i.e. *ruby-1.8.7* and
*ruby-1.9.2* installed next to each other and switch with only a couple of
commands.

Installation
------------

The RVM installation process is explained in detail on the RVM website but I
just want to sum up the steps I did on my machine.

~~~ {.bash}
# fetch rvm sources
$ bash < <(curl -sk https://rvm.beginrescueend.com/install/rvm)
# source rvm scripts on shell login
$ echo '[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"' >> ~/.zshrc
# emerge required packages
$ emerge -va libiconv readline zlib openssl curl git libyaml sqlite libxslt libtool bison
~~~

Now you are ready to install and load a ruby instance of your liking:

~~~ {.bash}
$ rvm install 1.9.2
$ rvm use 1.9.2
~~~

Gemsets
-------

RVM allows you to have multiple sets of RubyGems even for one single ruby
version – these sets are called *gemsets*. A typical workflow borrowed from the
[RVM website][3] looks like this:

~~~ {.bash}
$ rvm 1.9.2
$ gem install rails -v 2.3.3

$ rvm gemset create rails222 rails126

$ rvm 1.9.2@rails222
$ gem install rails -v 2.2.2

$ rvm 1.9.2@rails126
$ gem install rails -v 1.2.6

$ rvm 1.8.7
$ gem install rails -v 1.2.3
~~~


[^1]: see my article on [Vagrant][4]

[1]: http://vagrantup.com
[2]: http://beginrescueend.com
[3]: http://beginrescueend.com/gemsets/basics/
[4]: /2011-10-03-vagrant_virtualbox_console_wrapper/
