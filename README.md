# My Emacs Configuration

As is apparently tradition, I'm sharing my configuration of Emacs. I strongly suggest you use this as a guide, and don't
just replace your init.el file. I'm loading a number of packages that I find personally useful. It's unlikely you want
them all.

If you're an Emacs beginner I especially recommend starting with "stock" Emacs and adding packages, if you want, one at
a time. I hope this repository helps you solve some of the issues you'll run into as you start addding them.

Inclusion of a package in the configuration does *not* mean it's bug-free, of course, but it does mean I find the
package useful. I've got a comment section in the `init.el` file listing some packages I tried, and why I didn't keep
them.

## (Semi-)Literate Programming In Emacs

Rather than put startup code directly into emacs startup files ~init.el~ and ~early-init.el~, I've set up files with 
org-mode, which has a subsytem called babel. This allows me to intermix text, with formatting and editor support, and source
code, also with formatting and editor support.

I'm finding this to be a great way to set the code up for several reasons:

* It makes for a very clean way to explain the code as I go, for other programmers
* it results in more consistent and complete docs for my own understanding, next month or next year
* It improves organization of code, and reduces the number of comments actually needed in the code.

### How This Works

As mentioned above, this system takes advantage of Emacs' Org mode. This is a complete personal information 
manager that works directly with text files &emdash; no back-end database, no online system to keep your
information locked in its system.

Org mode is built into Emacs. However, there's usually a more recent version that can be installed with more
features, bug fixes, etc. I want to use this latest version, but it means the two config files need to be produced
differently.

The code in the ~early-init.el~ file gets executed before packages are loaded, including org-mode. So it would have
to use the built-in version of org mode, but bad things can happen if you initialize the mode and then try to load
a newer package. So, the process for that file is distinctly old-school: you write code in the ~early-config.org~ file,
then execute the emacs command ~org-babel-tangle~ to produce an early-init.el file with only the elisp code, which runs
normally.

In contrast, in the ~init.el~ file we can initialize the package manager, load the latest version of org-mode, then
produce source code from config.org directly and run that, with no manual step needed.

## File System Organization

As you add packages to Emacs, your Emacs config directory ends up with a lot of extra directories and files.[^1]

To keep this repository clean, it's designed to be added as a subdirectory of the emacs config directory, named
"my_emacs". My actual "init.el" and early-init.el files in my emacs home directory are just links to the same filename
in the my_emacs/ directory. (Windows users will probably need to create a separate file, which does nothing but load the versions
in this directory.)

## Code Sections

The code is divided into sections, which unfortunately are rather haphazard. TODO: clean it up.

I'm using the `use-package` macro to combine Emacs downloading and loading the package with configuration, key bindings,
and other setup stuff. _Much_ better than having it spread all through the file.

## Git Submodules

This repository includes several elisp packages as submodules, so if you clone it you'll need to do

```sh
git submodule init
git submodule update
```

## Notes on Specific Packages

## Contributing

I would love to hear your comments and suggestions on this repository. I intend to set up a semi-formal contribution
procedure soon. Right now, go ahead and create an issue or submit a pull request. And if you do, thank you!

[^1]: And, unfortunately, it's not just the config directory. See project
    [no-littering](https://github.com/emacscollective/no-littering) for an effort to make misbehaving packages use the
    emacs config directory rather than the arbitrary default locations.
