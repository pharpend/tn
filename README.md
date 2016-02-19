# tn

This is a command-line program for keeping your own journals. It's
licensed under the [GPL version 3][gpl-gnu], which you can find in the
[LICENSE file][license-file] Tn is written in Haskell. It's not yet
usable, so don't try to use it.

## History

I've been trying on and off to write this program since 2014, but have
never gotten to the point where I've actually had a usable
program. Hence, why we're on version 4.0, and I still don't have a
usable version yet. Each new major version has been a complete rewrite
because the previous version was a disaster.

tn is licensed as such because I originally wanted to use it as a tool
to record the food I was eating. The natural name would be `fj`, for
"food journal". However, I use the [Colemak keyboard layout][2], and
typing `fj` is somewhat awkward. Moreover, in 2014, I had only begun
using the Colemak layout. Hitting the key immediately under left index
finger, followed by the key under the right index finder yielded the
sequence `tn` in Colemak.

## Plans for version 4

So, here are my plans for version 4, which I may or may not follow

* The program, at its core, allows you to keep journal entries, written
  in [Markdown][mkdn], and optionally encrypted.
* Tn will, via [Pandoc][pandoc], allow the user a variety of export
  formats. So, if you want to print your journal, you could output it as
  an HTML file or a word processor document. If you wanted a more
  external-program-friendly format, tn will simply allow you to export
  your journal as a tarball.
* Tn should, by default, store the data in a XDG-correct location, such
  as `~/.local/share/tn` on UNIX-like systems, or whatever the
  equivalent is on Windows.
* Tn will eventually allow multiple journals, as well as a hierarchy of
  journals.
* Tn will allow that practice of replacing `tn some-long-command` with
  `tn slc`.
* Tn should at some point allow a graphical interface, so that people
  besides UNIX nerds can use it.
  
## Installing

The version on Hackage is out of date, and also really terrible, so
don't try to `cabal install tn` just yet. You'll need [git][git-install]
and [The Haskell Stack][hs-stack].

    git clone https://github.com/pharpend/tn.git
    cd tn
    stack setup
    stack build

I welcome any contributions anyone wants to make. If you find a bug or a
feature request, use the [GitHub bug tracker][issues]. If you want to
contribute, use the normal method of contributing to projects on GitHub.

## Usage

There is no usage yet! Check back later!

## Contacting me

You can contact me at `peter@harpending.org`, or `pharpend` on FreeNode.

## Copyright

Copyright (C) 2014-2016 Peter Harpending

This program is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
Public License for more details.

You should have received a copy of the GNU General Public License along
with this program. If not, see <http://www.gnu.org/licenses/>.

[colemak]: http://colemak.com/
[git]: https://git-scm.com/book/en/v2/Getting-Started-Installing-Git
[github-bug-tracker]: https://github.com/pharpend/tn/issues
[gpl-gnu]: https://gnu.org/licenses/
[hs-stack]: http://docs.haskellstack.org/en/stable/README.html
[mkdn]: https://en.wikipedia.org/wiki/Markdown
[license-file]: LICENSE
[pandoc]: http://pandoc.org/
