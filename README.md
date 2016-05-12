# tn

This is a command-line program for keeping your own journals, written in
[Haskell][hs] and licensed under the [ISC license][iscl]. You can find a
copy of the ISC licensein the [LICENSE file][license-file]

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

## Specification

*   `tn "some string"` records the journal entry to
    `~/.local/share/tn/journal.yml` with the time.

    * adding `--stdout` prints the output to stdout
    * adding `-` takes the input from stdin
    * adding `--stdin` takes the input from stdin
  
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

[colemak]: http://colemak.com/
[git]: https://git-scm.com/book/en/v2/Getting-Started-Installing-Git
[github-bug-tracker]: https://github.com/pharpend/tn/issues
[gpl-gnu]: https://gnu.org/licenses/
[iscl]: https://www.haskell.org/
[hs]: https://www.haskell.org/
[hs-stack]: http://docs.haskellstack.org/en/stable/README.html
[mkdn]: https://en.wikipedia.org/wiki/Markdown
[license-file]: LICENSE
[pandoc]: http://pandoc.org/
