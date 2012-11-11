---
title: Raspberry Pi running gentoo
author: Gregor Uhlenheuer
tags: linux, gentoo, raspberrypi
summary: A short installation walkthrough on how to get the Raspberry pi to run
         gentoo linux
---

After having tried the ubuntu and archlinux distributions on my Raspberry Pi I
came back to my favorite linux distribution *gentoo*.  You can find a short
walkthrough of my installation steps below.

# Get files

First you have to download the files necessary for the installation:

~~~ {.bash}
$ cd /tmp

# arm stage3 autobuild
$ wget http://distfiles.gentoo.org/releases/arm/autobuilds/current-stage3-armv6j/stage3-armv6j-20121107.tar.bz2

# latest portage snapshot
$ wget http://distfiles.gentoo.org/snapshots/portage-latest.tar.bz2
~~~

You can get the latest Raspberry Pi kernel from github:

~~~ {.bash}
$ git clone --depth 1 git://github.com/raspberrypi/firmware
~~~

# Preparing the SD card

Now that we have all necessary files you can insert your SD card. In the
following steps I am using `/dev/mmcblk0` to identify the SD card. This
identifier may vary on other systems - you can check with `dmesg` after
inserting your card.

## Create the partitions

I chose to create a FAT32 boot partition of 32 MB, a swap partition with 512 MB
and the rest for the root EXT4 partition.

~~~ {.bash}
$ fdisk /dev/mmcblk0
~~~

## Create file systems

~~~ {.bash}
$ mkfs.vfat -F 16 /dev/mmcblk0p1
$ mkswap /dev/mmcblk0p2
$ mkfs.ext4 /dev/mmcblk0p3
~~~

# Installing gentoo

The SD card is formatted and ready to be used for gentoo installation.

## Mounting partitions

I am using the directory `/tmp/mnt/gentoo` for the installation directory. You
are free to substitute this to your liking.

~~~ {.bash}
$ mkdir /tmp/mnt/gentoo
$ mount /dev/mmcblk0p3 /tmp/mnt/gentoo
$ mkdir /tmp/mnt/gentoo/boot
$ mount /dev/mmcblk0p1 /tmp/mnt/gentoo/boot
~~~

## Extract files

Next we can extract portage and the stage3 image on the mounted SD card:

~~~ {.bash}
# extract stage3 files
$ tar xvf stage3-armv6j*.tar.bz2 -C /tmp/mnt/gentoo

# extract portage image
$ tar xvf portage-latest.tar.bz2 -C /tmp/mnt/gentoo/usr
~~~

## Install kernel and modules

Next we have to copy the kernel and its modules from the cloned github
repository:

~~~ {.bash}
$ cd /tmp/firmware/boot
$ cp * /tmp/mnt/gentoo/boot/
$ cp -r ../modules /tmp/mnt/gentoo/lib/
~~~

# Configuration

Before being able to use the new installation we have to adjust a few
configuration files.

## Edit fstab

Next you have to edit your `fstab` to match your partition scheme:

~~~ {.bash}
$ vim /tmp/mnt/gentoo/etc/fstab
~~~

My `fstab` looks like this:

    /dev/mmcblk0p1	/boot	auto	noauto,noatime	1 2
    /dev/mmcblk0p2	none	swap	sw		0 0
    /dev/mmcblk0p3	/	ext4	noatime		0 1

## Set boot options

After that you have to create a `cmdline.txt` file to pass the required boot
parameters:

~~~ {.bash}
$ echo 'root=/dev/mmcblk0p3 rootdelay=5' > /tmp/mnt/gentoo/boot/cmdline.txt
~~~

## Edit make.conf

After that you may want to edit your `make.conf` file to set your desired make
parameters like `CFLAGS` and set some default USE flags.

~~~ {.bash}
$ vim /tmp/mnt/gentoo/etc/portage/make.conf
~~~

## Set timezone

Next you will want to set your current timezone. Find a list of available timezones like this:

~~~ {.bash}
$ ls /tmp/mnt/gentoo/usr/share/zoneinfo
~~~

Set your desired timezone by copying the zoneinfo to the new file
`/etc/localtime`. In my case I chose the *Europe/Berlin* timezone:

~~~ {.bash}
$ cp /tmp/mnt/gentoo/usr/share/zoneinfo/Europe/Berlin /tmp/mnt/gentoo/etc/localtime
$ echo "Europe/Berlin" > /tmp/mnt/gentoo/etc/timezone
~~~

## Reset root password

As we don't want to chroot into the newly created gentoo installation we just
reset the root password by editing the `/tmp/mnt/gentoo/etc/shadow` file to the following:

    root::10770:0:::::

## Boot your Raspberry Pi

Before booting your Raspberry Pi you first have to unmount the SD card:

~~~ {.bash}
$ umount /tmp/mnt/gentoo/boot
$ umount /tmp/mnt/gentoo
~~~

# Post boot installation steps

After inserting your SD card into your Raspberry Pi and turning on the power
you should see a gentoo startup sequence and a login prompt.

## Set root password

After logging into root without a password you should immediately set a new password for root:

~~~ {.bash}
$ passwd
~~~

## Networking

In order to activate networking on boot you can add an entry via `rc-update`:

~~~ {.bash}
$ nano -w /etc/conf.d/net
$ cd /etc/init.d
$ ln -s net.lo net.eth0
$ rc-update add net.eth0 default
$ /etc/init.d/net.eth0 start
~~~

## Configuring inittab

In case you get error messages like `INIT Id "s0" respawning too fast` on boot
you may want to comment the first two serial console entries in `/etc/inittab`:

~~~ {.bash}
$ nano -w /etc/inittab
~~~

After editing the mentioned entries should look like this:

    # SERIAL CONSOLES
    #s0:12345:respawn:/sbin/agetty 9600 ttyS0 vt100
    #s1:12345:respawn:/sbin/agetty 9600 ttyS1 vt100

## Clock

The Raspberry Pi does not have a hardware clock so you need to disable the
`hwclock` service and enable `swclock` instead:

~~~ {.bash}
$ rc-update add swclock boot
$ rc-update del hwclock boot
~~~

Optionally you may want to emerge `ntp` and synchronize the clock on startup:

~~~ {.bash}
$ emerge -av ntp
$ rc-update add ntp-client default
$ /etc/init.d/ntp-client start
~~~

## SSH

You probably want to ssh into your Raspberry Pi from time to time:

~~~ {.bash}
$ rc-update add sshd default
$ /etc/init.d/sshd start
~~~

## Update system

After all necessary installation steps are passed you can update your system
and start using gentoo on your Raspberry Pi:

~~~ {.bash}
# update portage
$ emerge --sync
$ emerge -vauDN world

# install a decent editor
$ emerge -av vim
~~~


# References

- [Gentoo handbook][1]
- [Raspberry Pi][2]
- [Raspberry Pi on github][3]

[1]: http://www.gentoo.org/doc/en/handbook/handbook-arm.xml?full=1
[2]: http://raspberrypi.org
[3]: https://github.com/raspberrypi
