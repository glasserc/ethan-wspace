ethan-wspace
============

Presenting, with tongue firmly in cheek: ethan-wspace, the definitive
emacs customizations for people who are OCD about whitespace.

You may have some opinions about whitespace in your source code. They
may even amount to preferences. However, it takes a seriously twisted
person to think about whitespace obsessively. I have. Unless you
demonstrate that you are OCD enough to think about these things for
hours, I will conclude that I know more about these things than you
do. And I will be right.

No matter how many opinions you have, I have more. And that makes mine
more correct.

So what?
========

You're probably thinking, "Who cares?" And you'd be right. I sincerely
doubt using these customizations will make your life as a programmer
even 1% more productive. 1% is nothing. You'd do better to buy a
bigger monitor.

So then, why be OCD about whitespace? It boils down to one essential
thing: diffs like these.

::

    $ git diff
    diff --git a/foo.c b/foo.c
    index 7be6eb3..8ba98ba 100644
    --- a/foo.c
    +++ b/foo.c
    @@ -1,6 +1,6 @@
     #include <stdio.h>

    -int main(){
    +int main(){  
       printf("hello world\n");
       return 0;
     }

What's going on in this diff? Take a few minutes to study it and figure out what's going on. What did I change? It doesn't look like I changed anything! But in fact there's a diff, so there must be some change, right?

The answer is that I either introduced or removed whitespace at the end of the line. ``git diff`` will highlight, in red, trailing whitespace like this that you added, but not that you took away. (P.S. ReStructuredText is removing the trailing whitespace from the example, but if you look at the rst source you will see that it is there.)

Reviewing diffs with whitespace changes is annoying. Trying to guess what the whitespace changes were, so you can undo the whitespace changes, is downright tedious. Ideally you should never have to do these things.

ethan-wspace can help.

What does it do?
================

ethan-wspace takes the approach of *do no harm*. Specifically, when you open a file:

- If the whitespace is already clean, ethan-wspace inserts hooks to
  clean the whitespace before every save.

- If the whitespace is not already clean, ethan-wspace highlights
  errors. It doesn't clean the whitespace without you asking for it,
  so you don't get spurious whitespace changes in your diffs. It
  doesn't prevent you from introducing new errors, but hopefully you
  will be cognizant of the errors in the file.

ethan-wspace recognizes the following categories of whitespace errors:

1. trailing whitespace at end of line (``eol``).

2. no trailing newline (``no-nl-eof``).

3. more than one trailing newline (``many-nls-eof``).

4. tabs, at all (``tabs``).

It recognizes these categories independently, and treats each category
as clean or not-clean. The goal is always to make your diffs
unambiguous. Laudable goal, right?

My tabs! Get your hands off my tabs!
====================================

It is my opinion (and remember, my opinions are right) that you should
never, ever have tabs in your source code, at all. This was once a
holy war, but in my experience, pretty much everybody today
understands this point and the reasoning behind it. If you disagree,
please see `Tabs Are Evil
<http://www.emacswiki.org/emacs/TabsAreEvil>`_ on the EmacsWiki.

Perhaps you are one of those bizarre creatures who uses `Smart Tabs
<http://www.emacswiki.org/emacs/SmartTabs>`_. In that case, you are
even more OCD about whitespace than I am, and in a twisted way I
salute you. However, ``ethan-wspace`` by default treats tabs as
errors, which you might find distracting. In that case, I recommend
something like the following::

    (set-default 'ethan-wspace-errors (remove 'tabs ethan-wspace-errors))

How to use it
=============

For a full emacs config that uses ethan-wspace, please see
http://github.com/glasserc/etc. I'd suggest you start keeping your
dot-rc files in source control too. If you're using git, you can make
a submodule that points to this repository, and in this way keep on
top of changes.

The essential aspects here are to add the ``lisp`` directory to your
``load-path``, and then ``(require 'ethan-wspace)``. In other words,
add to your ``init.el`` something like the following::

    (add-to-list 'load-path (expand-file-name "~/.emacs.d/upstream/ethan-wspace.git/lisp"))
    (require 'ethan-wspace)
    (global-ethan-wspace-mode 1)

(I keep all my git-based upstreams in a ``contrib`` directory, and
symlink the directories with lisp source code into
``~/.emacs.d/packages``, but your mileage may vary.)

You should also remove any customizations you have made to turn on
either ``show-trailing-whitespace`` or ``require-final-newline``; we
handle those for you.

When you open files, bad whitespace will be highlit and clean
whitespace will be maintained. You can switch from one to the other
using ``M-x ethan-wspace-highlight-FOO-mode`` or ``M-x
ethan-wspace-clean-FOO-mode`` (each mode disables the other).  If you
want to begin cleaning all whitespace, you can use ``M-x
ethan-wspace-clean-all-modes``.

You might also want to customize the face used to highlight erroneous
whitespace. This is configurable by ``ethan-wspace-face``. A default
face is computed based on the background of your frame when
``ethan-wspace`` was ``require``\ d (so you might want to make your
calls to ``color-theme`` first).

Relationship to other emacs things
==================================

Most other emacs whitespace customizations (and there are many: see
`ShowWhiteSpace on the EmacsWiki
<http://www.emacswiki.org/emacs/ShowWhiteSpace>`_) focus on showing
problematic whitespace. There are also some customizations out there
focused on `Deleting Whitespace
<http://www.emacswiki.org/emacs/DeletingWhitespace>`_. But there are
many and they all have extremely similar names. (``ethan-wspace`` aims
to be the most egotistically-named package.) ``ethan-wspace`` subsumes most of them, except for ``whitespace.el`` to show all whitespace in non-programming contexts, and ``ws-trim.el`` which I had never heard of before just now.

* `whitespace.el <http://www.emacswiki.org/emacs/WhiteSpace>`_ and the
  family of related code that includes ``visws.el``,
  ``whitespace-mode.el``, ``show-whitespace-mode.el``, and
  ``blank-mode.el`` has many options for making whitespace characters
  visible, both by faces and by changing their representations in the
  display table. That seems very useful for editing binary files or
  other circumstances where you care exactly what whitespace you're
  looking at, but it isn't really useful for editing source code,
  where you typically want whitespace to be as clean as possible. I
  have no idea which of those files is most recent or "best", as I
  have never used them.

* `ws-trim.el <ftp://ftp.lysator.liu.se/pub/emacs/ws-trim.el>`_
  automatically trims whitespace on edited lines. With a low
  ``ws-trim-level`` it is complementary to ``ethan-wspace``, and may
  be useful to encourage you to delete whitespace organically. I'd
  never heard about this package and hopefully ``ethan-wspace`` will
  grow similar functionality soon.

* Putting ``delete-trailing-whitespace`` or
  ``nuke-trailing-whitespace`` in your ``before-save-hook`` is now
  obsolete; these functions are too aggressive and will cause you many
  spurious whitespace commits.

* Standard emacs variables ``show-trailing-whitespace`` and
  ``require-final-newline`` are "subsumed" by this mode --
  ``require-final-newline`` is reimplemented in a more general way,
  and ``show-trailing-whitespace`` is triggered per-buffer by this
  mode. (``show-trailing-whitespace`` is built into emacs core and
  seems to be the fastest/most elegant way to highlight trailing whitespace.)

* ``next-line-add-newlines``, to add newlines when you move forward
  lines, still exists and is unchanged. I recommend you set this to
  nil (if it isn't already -- I think it is nil in all versions since
  21.1), but ``ethan-wspace`` will still trim unnecessary newlines on each
  save if there were fewer than two when the buffer was opened.

* `redspace.el <http://www.emacswiki.org/emacs/redspace.el>`_ is a
  small library meant only to highlight trailing whitespace. This is
  already done by the variable ``show-trailing-whitespace``, which is
  used internally by ``ethan-wspace``. ``show-trailing-whitespace``
  has the nice effect that it doesn't highlight trailing whitespace
  when your cursor is after it -- so you don't see little blinking
  lights as you type a line of text.

* `show-wspace.el <http://www.emacswiki.org/emacs/show-wspace.el>`_ is
  a library that has lots of faces to show tabs, trailing whitespace,
  and "hard spaces". ``ethan-wspace`` obsoletes this mode too.

More ranting about Tabs Are Evil
================================

Required reading for this discussion is JWZ's "famous" `tabs versus
spaces <http://www.jwz.org/gruntle/tabs-versus-spaces.html>`_ post. He
sets out three categories of effect that tabs have, and how to defuse
the whole situation.

I have encountered people who prefer tabs because they prefer being
able to press backspace and go exactly one level of indentation
back. These people are obviously wrong because if you're using a
halfway decent editor, it should be capable of indenting CORRECTLY for
you automatically (i.e. emacs's ``TAB`` behavior), as well as
backspacing a whole level in languages where that's useful
(i.e. emacs's ``python-backspace``). So this argument just boils down
to "I have a crappy text editor."

You may encounter people who say things like, "Tabs are better because
they let everybody set their own indentation width." And this is true
to a point. If you are one of those people, pop quiz: let's say you
use tabs, and prefer them to be four spaces wide. How do you indent
the last line of this code?

::

    if __name__ == '__main__':
        main.Application(config, sys.argv, time.time(),
                         docutils.parsers.rst.directives.images.Image)

If you said "five tabs, one space" -- you lose. Because then when you move to Jan's machine, where tabs are two spaces, you find::

    if __name__ == '__main__':
        main.Application(config, sys.argv, time.time(),
               docutils.parsers.rst.directives.images.Image)

And on Johann's machine, where tabs are eight spaces, you see::

    if __name__ == '__main__':
        main.Application(config, sys.argv, time.time(),
                                   docutils.parsers.rst.directives.images.Image)

Your beautifully-indented source code has been scattered to the
winds. You've just demonstrated that you aren't crazy enough to think
about whitespace issues obsessively enough. Rejoice! There is a place
for you in normal society.

It's due to code above that truly demented people will suggest using
tabs for *blocks only* and *spaces within blocks*. I've seen this rule
propounded on Reddit, for example. In the above code, that gives you
"one tab, seventeen spaces". I've never tried this approach on a
real project, for the simple fact that people are lazy and source-code
editors are imperfect, and somewhere, somehow, I am certain to come
across spaces where there should be tabs, or tabs where there should
be spaces. And then I will be furious.

(If I worked on a project with a team of sharpshooter programmers who
all agreed on the tabs-for-scope-plus-spaces-for-alignment rule, I'd
investigate configuring emacs to do this. But until then I rely on the
far easier expedient of just outlawing tabs in source code entirely
and consigning them to the dustbin of history.)
