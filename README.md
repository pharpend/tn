# tn

This is a command-line program for keeping your own journals, written in
[Haskell][hs] and licensed under the [ISC license][iscl]. You can find a
copy of the ISC license in the [LICENSE file][license-file]

## Installation and Usage

First, install [Stack][hs-stack].

    $ stack setup
    $ stack install tn

Versions of tn less than `4` are just awful, so run

    $ tn --version

to make sure you have a decent version. else, try this:

You'll need [git][git] and [The Haskell Stack][hs-stack].

    $ git clone https://github.com/pharpend/tn.git
    $ cd tn
    $ stack setup
    $ stack install

To add an entry, simply run

    $ tn "entry text"

The journal is stored in `~/.local/share/tn/`.

## Contributing

I welcome any contributions anyone wants to make. If you find a bug or a
feature request, use the [GitHub bug tracker][issues]. If you want to
contribute, use the normal method of contributing to projects on GitHub.

You can contact me at `peter@harpending.org`, or `pharpend` on FreeNode.

[colemak]: http://colemak.com/
[git]: https://git-scm.com/book/en/v2/Getting-Started-Installing-Git
[gpl-gnu]: https://gnu.org/licenses/
[iscl]: https://www.haskell.org/
[issues]: https://github.com/pharpend/tn/issues
[hs]: https://www.haskell.org/
[hs-stack]: http://docs.haskellstack.org/en/stable/README.html
[mkdn]: https://en.wikipedia.org/wiki/Markdown
[license-file]: LICENSE
[pandoc]: http://pandoc.org/
