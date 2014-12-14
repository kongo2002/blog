---
title: god: watch file descriptors
author: Gregor Uhlenheuer
tags: ruby, god, linux
summary: I wrote a tiny extension for god in order to watch the number of open
         file descriptors of a running process.
---

A while ago at work we had some trouble with the C# driver for [MySQL][mysql]
leaking file descriptors when connecting via HTTPS. Every request of the service
using the MySQL driver leaked one file descriptor resulting in rapidly exceeding
the maximum allowed file descriptors of the process. Afterwards the web service
was effectively broken as it wasn't able to accept any connections/requests at
all.

After hours of investigating we finally found and reported the bug in the MySQL
driver itself. Nevertheless this behavior inspired me to write a tiny extension
for the process monitoring tool [god][god].


# god watch

The custom watch extends the basic `PollCondition` behavior which basically
polls the number of open file descriptors of the watched process and restarts
the service when a configured threshold is exceeded.

This is what the extension looks like:

~~~ {.ruby}
module God
    module Conditions

        # Condition Symbol :file_descriptors
        # Type: Poll
        #
        # Trigger when the process own more than a specified amount of
        # open file descriptors.
        #
        # Parameters
        #   Required
        #     +pid_file+ is the pid file of the process in question. Automatically
        #                populated for Watches.
        #     +above+    is the amount of maximum allowed open file descriptors
        #
        #   Optional
        #     +times+    number of checks that have to fail to be triggered
        #
        # Examples
        #
        #   Trigger if the process owns more than 256 file descriptors in
        #   at least 3 of the last 5 checks (from a Watch):
        #
        #   on.condition(:file_descriptors) do |c|
        #     c.above = 256
        #     c.times = [3, 5]
        #   end
        #
        #   Non-Watch Tasks must specify a PID file:
        #
        #   on.condition(:file_descriptors) do |c|
        #     c.above = 512
        #     c.pid_file = "/var/run/service.3000.pid"
        #   end
        class FileDescriptors < PollCondition
            attr_accessor :above, :pid_file, :times

            def initialize
                super
                self.above = nil
                self.times = [1, 1]
            end

            def prepare
                if self.times.kind_of?(Integer)
                    self.times = [self.times, self.times]
                end

                @timeline = Timeline.new(self.times[1])
            end

            def reset
                @timeline.clear
            end

            def pid
                self.pid_file ? File.read(self.pid_file).strip.to_i : self.watch.pid
            end

            def valid?
                valid = true
                valid &= complain("Attribute 'pid_file' must be specified", self) if self.pid_file.nil? && self.watch.pid_file.nil?
                valid &= complain("Attribute 'above' must be specified", self) if self.above.nil?
                valid
            end

            def test
                fds = Dir["/proc/#{self.pid}/fd/*"].size
                @timeline.push(fds)

                if @timeline.select { |x| x > self.above }.size >= self.times.first
                    output = @timeline.map { |x| "#{x > self.above ? '*' : ''}#{x}" }.join(", ")
                    self.info = "max file descriptors reached: [#{output}]"

                    return true
                else
                    return false
                end
            end

        end
    end
end
~~~

As described in the documentation comments you would can easily integrate the
watch in your god configuration using the `file_descriptors` condition:

~~~ {.ruby}
# load the custom extension
God.load "file_descriptors.god"

# your watch
God.watch do |w|

    # your watch configuration
    # ...

    # restart condition(s)
    w.restart_if do |restart|

        restart.condition(:file_descriptors) do |c|
          c.above = 512
        end

    end
end
~~~

The above setup would cause the process to be restarted as soon as it exceeds
512 open file descriptors.

[god]: http://godrb.com/
[mysql]: http://www.mysql.com/
