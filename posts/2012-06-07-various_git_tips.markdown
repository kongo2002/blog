---
author: Gregor Uhlenheuer
title: Git: various tips
tags: git, dvcs
summary: Find a few tips for git I picked up lately/over time.
---
This post is a fairly rough collection of things on git I consider worth saying
and noting for myself (as a reference). Hopefully I will keep on updating this
with new tips and improvements.

## Settings

### Git aliases

These are my default git aliases I configured on every box I work on:

    br      = branch
    ci      = commit
    co      = checkout
    df      = diff
    lg      = log --graph --oneline --decorate --all
    st      = status --branch --short
    unadd   = reset HEAD
    ffmerge = merge --ff-only
    fixup   = commit --amend -C HEAD

You can define your aliases via `git config`:

~~~{.bash}
git config --global alias.lg 'log --graph --oneline --decorate --all'
~~~

### Various settings

Use color where possible:

~~~{.bash}
git config --global color.ui true
~~~

Use a mergetool for diffing and merging:

~~~{.bash}
git config --global merge.tool vimdiff
git config --global diff.tool vimdiff
~~~

## Basic workflow

### Changing, adding, committing

Stage parts/hunks of your changes interactively:

~~~{.bash}
git add --patch     # git add -p
~~~

Stage files interactively:

    git add -i

Checkout/discard parts of the current changes in your working directory:

~~~{.bash}
# this works like 'add --patch' in reverse
git checkout -p
~~~~

### Branching

This is how you create a new branch for changes you already did and switch
those on a new branch (this is a command I use *all the time*):

~~~{.bash}
# take my current working directory changes
# create a new branch called 'fix_issue21'
# and immediately switch to the created branch
git checkout -b fix_issue21
~~~

Show branched that are branches that are completely merged into the current branch:

    git branch --merged

This would be the opposite - branches that do have unique commits in it:

    git branch --no-merged

Find a branch that have a particular commit in it:

    git branch --contains 7e830ac7

### Mergin, rebasing

There is actually one pretty standard use case of rebasing which is to rebase
your topic branch off the latest version of the master branch. The usual way is
probably something like this:

~~~{.bash}
# we are currently on the topic branch
git checkout master
git pull
git checkout topic_branch
git rebase master
~~~

But there is actually a pretty nice way to speed up this workflow without
actually touching your local master branch:

    git fetch && git rebase origin/master

Use the interactive rebasing:

~~~{.bash}
git rebase --interactive    # git rebase -i
~~~

### Inspecting the history/changes

Show all commits reachable by branchA that are *not* reachable by branchB:

    git log branchA ^branchB

A typical example might be: "which commits are in my topic branch that are not
yet merged into master?":

    git log feature ^master

Another one: "which commits I just fetched from origin that are not yet merged
into master":

    git log origin/master ^master

I don't get tired of this: "which new commits will be pushed to
origin?":

    git log master ^origin/master

Important log options you will use from time to time:

~~~{.bash}
git log -p      # patch view
git log --stat  # see diff stats
git log --graph # see graph view
~~~

Who modified what changes in the specified file?

~~~{.bash}
git blame changefile.c

# ignore file moving/copying
git blame -C changefile.c
~~~

### Git remotes

This is how you would remove a branch on a remote:

~~~{.bash}
# remove the branch 'old_branch' on your remote called 'origin'
git push origin :old_branch
~~~

You can easily check out a specific branch and track its remote:

~~~{.bash}
git checkout -b feature origin/feature

# these two would do exactly the same
git checkout -t origin/feature
git checkout feature
~~~

## Various tips

Get more human-readable names for a specific commit:

    git describe HEAD
    git describe HEAD@{1.month.ago}

Get more verbose output of `curl` when communicating over http like cloning:

~~~{.bash}
export GIT_CURL_VERBOSE=1
git clone http://foo.github.com/project.git
~~~

Show the last commit that contains a specific string (regular expression match):

    git show :/fixed
    git show :/^Merged

### Git bundle

Create a bundle file:

    git bundle create repo.bundle master

Now you can send the binary file via email, copy it on a usb drive and the like
and treat it like a remote:

~~~{.bash}
# show branches in the bundle
git ls-remote repo.bundle

# clone from the bundle file
git clone repo.bundle -b master localrepo
cd localrepo
~~~

## References

Some really great sites or talks on stuff about git:

- [Git tips by Scott Chacon][1]

[1]: http://blip.tv/scott-chacon/git-tips-4232122
