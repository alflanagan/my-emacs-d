# My Emacs Configuration

As is apparently tradition, I'm sharing my configuration of Emacs. I strongly suggest you use this as a guide, and don't
just replace your init.el file. I'm loading a number of packages that I find personally useful. It's unlikely you want
them all.

If you're an Emacs beginner I especially recommend starting with "stock" Emacs and adding packages, if you want, one at
a time. I hope this repository helps you solve some of the issues you'll run into as you start addding them.

Inclusion of a package in the configuration does *not* mean it's bug-free, of course, but it does mean I find the
package useful. I've got a comment section in the `init.el` file listing some packages I tried, and why I didn't keep
them.

## File System Organization

As you add packages to Emacs, your Emacs config directory ends up with a lot of extra directories and files.[^1]

To keep this repository clean, it's designed to be added as a subdirectory of the emacs config directory, named
"my_emacs". My actual "init.el" file that gets loaded initially looks like this:

```elisp
(load "~/.config/emacs/my_emacs/init")
```

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
