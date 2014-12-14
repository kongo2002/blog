---
title: Running ejabberd in docker using god
author: Gregor Uhlenheuer
tags: ejabberd, docker, god, container, linux
summary: See how you can run a god controlled ejabberd instance inside a docker container
---

Lately I have been doing some experiments using [ejabberd][ejabberd]
with [docker][docker]. In order to control the running ejabberd instance inside
the container it is good to have some kind of supervising daemon. Possible
candidates I tried so far are [supervisord][supervisord] and [god][god].


# god

The latter is my preferred choice as it supports very nice process
supervisioning using a linux kernel userspace interface (you will need your
kernel to be compiled with `CONFIG_CONNECTOR` for this mechanism to work). That
way there is no need to poll the process if it is still running. Moreover
[god][god] comes with a lot of reasonable default preferences resulting in very
simple configuration files. A basic god configuration file might look something
like this:

~~~ {.ruby}
God.watch do |w|
  w.name = "ssh"
  w.start = "/usr/sbin/sshd -D"
  w.keepalive
end
~~~


# ejabberd

Sadly using ejabberd using god is not as easy as the example shown above.
[god][god] expects the process to supervise to run in the foreground whereas
[ejabberd][ejabberd] which is usually controlled by the `ejabberdctl` script
starts a background daemon process.

I extracted some of the logic in the `ejabberdctl` script into a god
configuration file that works with god as expected:

~~~ {.ruby}
HOST         = 'localhost'
NAME         = "ejabberd@#{HOST}"
SPOOL_DIR    = '/var/lib/ejabberd'
EJABBERD_DIR = '/lib/ejabberd'
LOG_DIR      = '/var/log/ejabberd'
LOG_FILE     = "#{LOG_DIR}/ejabberd.log"

OPTS   = '+K true ' +    # kernel polling
         '-smp auto ' +  # automatic SMP detection
         '+P 250000'     # 250,000 ports

KERNEL = '-kernel inet_dist_use_interface \{0,0,0,0\}'
SASL   = "-sasl sasl_error_logger \\{file,\\\"#{LOG_FILE}\\\"\\}"

ERL    = "erl -noinput " +
         "-sname #{NAME} " +
         "-pa #{EJABBERD_DIR}/ebin " +
         "-mnesia dir \"'#{SPOOL_DIR}'\" " +
         "#{KERNEL} " +
         "-s ejabberd #{OPTS} #{SASL}"

CMD    = "erl -noinput -hidden " +
         "-sname ctl_#{HOST} " +
         "-pa #{EJABBERD_DIR}/ebin " +
         "#{KERNEL} " +
         "-s ejabberd_ctl -extra #{NAME}"

#
# EJABBERD
#
God.watch do |w|
    w.name = 'ejabberd'
    w.dir = SPOOL_DIR
    w.env = {
        'EJABBERD_CONFIG_PATH' => '/etc/ejabberd/ejabberd.yml',
        'EJABBERD_LOG_PATH'    => LOG_FILE
    }

    w.start   = "#{ERL} start"
    w.stop    = "#{CMD} stop"
    w.restart = "#{CMD} restart"

    w.grace = 20.seconds
    w.keepalive
end
~~~


# docker

Now you just have to add the god configuration to your *Dockerfile* like this:

~~~ {.bash}
RUN gem install god

ADD ./config.god /config.god

CMD ["god", "-c", "/config.god", "-D"]
~~~

[god]: http://godrb.com/
[ejabberd]: https://www.ejabberd.im/
[supervisord]: http://supervisord.org/
[docker]: https://www.docker.com/
