---
author: Gregor Uhlenheuer
tags: programming, git, vcs
title: Git: Delete a remote tag
summary: It does not happen that often but in case you want to get rid of a git
         tag on a remote...
---
I don't need this command that often but when I do, I can't recall how it's
done. For that reason I am quickly posting how you can delete an already pushed
tag on a remote:

~~~ {.bash}
# first remove the tag in your local repository
git tag -d v0.0.3

# delete the tag on the remote
git push origin :refs/tags/v0.0.3
~~~

Not that difficult, isn't it?!
