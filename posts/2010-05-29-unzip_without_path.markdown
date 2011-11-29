---
tags: linux, console, bash
author: Gregor Uhlenheuer
title: Unzipping without path structure
summary: Ever wanted to unzip files from a zip archive without wanting to
         extract the directory structure?
---
From time to time I need to unzip just a specific file archived inside a .zip
file. It’s very unpleasant to unzip the whole archive or the single file with
all the path structure inside the archive being recreated. The solution for
this problem is the argument `-j` to junk all paths.  A typical workflow then
would look like this:

    $ unzip -l archive.zip
    $ unzip -j archive.zip subdir/specific_file.c

To extract the file into a specific directory just append `-d {directory}` to
the command:

    $ unzip -j archive.zip subdir/specific_file.c -d target_dir
