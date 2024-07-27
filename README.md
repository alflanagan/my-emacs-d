# My Emacs Configuration

As is apparently tradition, I'm sharing my configuration of Emacs. I strongly suggest you use this as a guide, and don't
just replace your init.el file. In particular, I'm loading a number of packages that I find personally useful. It's
unlikely you want them all, and if you're an Emacs beginner I especially recommend starting with "stock" Emacs and
adding packages, if you want, one at a time.

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

[^1]: And, unfortunately, it's not just the config directory. See project
    [no-littering](https://github.com/emacscollective/no-littering) for an effort to make misbehaving packages use the
    emacs config directory rather than whatever arbitrary location the developer picked.
