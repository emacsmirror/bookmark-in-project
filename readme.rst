###################
Bookmark in Project
###################

By default Emacs built-in bookmark are global (shared across all projects),
This package exposes functions that limit bookmarks to the current project.

Available via `melpa <https://melpa.org/#/bookmark-in-project>`__.


Motivation
==========

When working on multiple projects at once, it's not always desirable to use a global list of bookmarks,
especially for features such as jumping to the next/previous bookmarks.

This package aims to make working on a single project convenient without interfering
the functionality of existing built-in commands.


Usage
=====

This package doesn't make any and changes to Emacs bookmarking functionality.
Instead it exposes commands which you may bind to keys

The key features provided are:

- Limit bookmark switching to other bookmarks in the same project.
- Support for toggling a bookmark (with automatic naming based on the context).
- Jump next/previous bookmark within the project.

All other built-in bookmarking functions can be used too,
however all other projects bookmarks will be accessible too.


Commands
--------

``bookmark-in-project-jump-next``, ``bookmark-in-project-jump-previous``
   Jump to the next/previous bookmark in the current project.

``bookmark-in-project-jump``, ``bookmark-in-project-jump-other-frame``, ``bookmark-in-project-jump-other-window``
   Jump to another bookmark in the project (equivalent to ``bookmark-jump``, ``bookmark-jump-other-frame`` .. etc).

``bookmark-in-project-toggle``
   Create/delete a bookmark on the current line, named using ``bookmark-in-project-name``.

   Equivalent to ``bookmark-set``, ``bookmark-delete``.

``bookmark-in-project-delete-all``
   Delete all bookmarks in the project.


Customization
-------------

Note that in general these settings can be left as-is,
but may be extended to adjust default behavior.

``bookmark-in-project-project-root`` (``'bookmark-in-project-project-root-default``)
   Function that finds the projects root-directory from the current buffer.
   This callback takes no arguments and must return a string or nil.

   The default function uses ``find-file-in-project``, ``projectile``, ``project`` when available,
   falling back to version-control and finally ``default-directory`` if all other methods fail.

``bookmark-in-project-name`` (``'bookmark-in-project-name-default-with-context``)
   Function that uses the current buffer and point to create a bookmark name.
   This function takes no arguments and returns a string or nil.

   The default function uses the project relative file name and the
   context at or before the cursor-location (calculated using ``imenu``).

   A simpler callback is also provided, called: ``'bookmark-in-project-name-default-with-line``.

``bookmark-in-project-name-fontify`` (``'bookmark-in-project-name-default-fontify``)
   Function that applies highlighting to the bookmarks.
   This function takes and returns a string representing the bookmark name.

   To disable highlighting set the value to ``'identity``.

``bookmark-in-project-verbose-toggle``
   Show a message when toggling bookmarks.

``bookmark-in-project-verbose-jump``
   Show a message when jumping to next/previous bookmarks.

``bookmark-in-project-use-completion-ivy``
   Use the IVY completion framework (when available).


Details
=======

- Jumping to bookmarks uses ``ivy`` when available,
  since it supports switching bookmarks without closing the completing read
  (see ``ivy-next-line-and-call`` & ``ivy-previous-line-and-call``).

  Otherwise the default completing read is used.

- Bookmarks are skipped when obscured by narrowing.


Installation
============

The package is available in melpa as ``bookmark-in-project``, here is an example with ``use-package``:

.. code-block:: elisp

   (use-package bookmark-in-project
     :commands (bookmark-in-project-jump
                bookmark-in-project-jump-next
                bookmark-in-project-jump-previous
                bookmark-in-project-delete-all)

     ;; Example key bindings.
     :bind (("M-n" . bookmark-in-project-jump-next)
            ("M-p" . bookmark-in-project-jump-previous)
            ("M-*" . bookmark-in-project-toggle)
            ("M-o" . bookmark-in-project-jump)))
