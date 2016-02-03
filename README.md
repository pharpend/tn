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

[colemak]: http://colemak.com/
[gpl-gnu]: https://gnu.org/licenses/
[mkdn]: https://en.wikipedia.org/wiki/Markdown
[license-file]: LICENSE
[pandoc]: http://pandoc.org/
