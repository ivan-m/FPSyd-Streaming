---
title: I Streamed a Stream
author: Ivan Lazar Miljenovic
date: 25 October, 2017
...

`Streaming`
===========

About the library
-----------------

Notes
:   * It's a streaming library called streaming

> * Unfortunately named
> * Performs well compared to the competition
> * Whilst being a lot simpler!

How I discovered `streaming`
----------------------------

Notes
:   * Never used any stream processing library before; lazy I/O
      sufficed!
    * Had used postgresql-simple with quiver, and could have adapted
      the code to any stream processing library, but thought to use a
      standard one.

> * Introduced to quiver by Patryk Zadarnowski
> * Adopted a package pipes
>     - Converted to quiver as I needed return values
> * New project: let's pick a library that's actually used by others.
>     - Needed PostgreSQL support
> * Conduit: all solutions used `persistent`
> * `pipes-postgresql-simple` by Oliver Charles
>     - Wait, it's deprecated?
>     - What's this "streaming" library?

Stream Processing
-----------------

Notes
:   * Avoiding calling it "streaming"
    * Nowadays, "stream processing" generally refers to parallel
      programming.
    * Usage predates that.
    * Everyone knows what it is in Haskell, but no real definition.
    * Not just Haskell: Scala, F#

. . .

> [...] P J Landin's original use for streams was to model the
> histories of loop variables, but he also observed that streams could
> have been used as a model for I/O in ALGOL 60.
>
> -- _A Survey Of Stream Processing, R. Stephens, 1995_

* * *

> _Stream processing_ defines a pipeline of operators that transform,
> combine, or reduce (even to a single scalar) large amounts of
> data. Characteristically, data is accessed strictly linearly rather
> than randomly and repeatedly -- and processed uniformly. The upside
> of the limited expressiveness is the opportunity to process large
> amount of data efficiently, in constant and small space.
>
> -- _Oleg Kiselyov_, <http://okmij.org/ftp/Streams.html>

A Brief, Incomplete, and Mostly Wrong History of Stream Processing {style="font-size:75%;width:100%"}
==================================================================

* * * *

Notes
:   * This is for Haskell
    * No claims made as to accuracy or completeness

... with apologies to James Iry

Pre-History (aka pre-2000)
--------------------------

Notes
:   * Apparently quote was said at Haskell Symposium in 2008, so
      people took that motto from the future to heart.

> * Lazy I/O
> * People grumbled, but accepted it.
> * After all: "avoid success at all costs".
> * `english -XAllowAmbiguousGrammar`{.bash}

Iteratees
---------

> * By Oleg Kiselyov
> * Dates back to ~2008
> * Everyone praised it for its amazing promise...
> * ... but being by Oleg, hardly anyone understood how it worked

`iteratee`
----------

> * Implementation of Iteratees by John Lato
> * 2009 -- 2014 (with Hackage update in 2016)

`enumerator`
------------

Notes
:   * Not sure when/why people started adopting this instead of
      `iteratee`
    * e.g. `snap` quickly switched to this

> * Alternative implementation of Iteratees by John Millikin
> * 2010 -- 2011
> * Often referred to as an "iteratee" still

`conduit`
---------

Notes
:   * Absorbed ideas from each other
    * Conduit now has an implementation of pipes under the hood
    * Conduit still claims to be more "production grade"

> * By Michael Snowman
> * First released 2011, still active
> * Declared as a more industrial/production-grade solution than
>   pipes
>     - Without defining either term
>     - Even though pipes didn't yet exist

`pipes`
-------

> * By Gabriel Gonzalez
> * First released in 2012, still active
> * Tried to validate anti-Haskeller's opinions of "too much maths" by
>   basing it on Category Theory.

The great conduit-pipes war
---------------------------

> * Fought primarily on the battlefields of Twitter and Reddit
> * Devolved into "best frenemies" situation
> * > Enemies, as well as lovers, come to resemble each other over a
>   > period of time.
>   >
>   > -- _Sydney J. Harris_

`machines`
----------

Notes
:   * Translates to "this is not a pipe"

> * By Edward Kmett
>     - As such, lots of great ideas with minimal documentation
> * First released in 2012, still active
> * "Networked stream transducers"
> * Allows for multiple inputs
> * _Ceci n'est pas une pipe_

`io-streams`
------------

Notes
:   * Less used than conduits and pipes

> * By Gregory Collins
> * First released in 2013, still active
> * Aims to be simpler (e.g. no monad transformers) than the others
> * Developed for use with Snap Framework

`quiver`
--------

Notes
:   * I probably wrote the most quiver code out of anyone
    * The complexity in question is to allow both pushing and pulling
      of values, but in practice we just pushed.

> * By Patryk Zadarnowski
> * All releases in 2015
> * What you get when you say "You know what pipes needs? More
>   complexity!"
> * Return values actually useful!

`streaming`
-----------

Notes
:   * The subject of this talk
    * See, I got here eventually!

> * By Michael Thompson
>     - Now maintained by Andrew Martin and a GitHub organization
> * First released in 2015
> * "The freely generated stream on a streamable functor"

Streaming Announcement
----------------------

Notes
:   * Wait, that isn't right...

> I'm doing a (free) stream processing library (just a hobby, won’t be
> big and professional like gnu) for Haskell. This has been brewing
> since august, and is starting to get ready. I’d like any feedback on
> things people like/dislike in pipes, as my library  is an attempt to
> implement `FreeT` in the style of `Pipes.Internal`.

Actual Streaming Announcement
-----------------------------

Notes
:   * OK, real announcement on haskell-pipes mailing list
    * Wait, there's a stream in the stream?

> It's probably a terrible idea!
>
> `streaming` is an attempt to implement `FreeT` in the style of
> `Pipes.Internal`, with a zillion more associated functions. There is
> a Prelude especially for the fundamental 'Producer' case - `Stream
> ((,) a) m r` and its iterations, `Stream (Stream ((,)a) m) m r`.

Compare the types
=================

`pipes`
-------

Notes
:   * This first as it makes it easier to compare conduit to pipes
    * Can feed values in both directions

```haskell
data Proxy a' a b' b m r
    = Request a' (a  -> Proxy a' a b' b m r )
    | Respond b  (b' -> Proxy a' a b' b m r )
    | M          (m    (Proxy a' a b' b m r))
    | Pure    r
```

`conduit`
---------

Notes
:   * `Pipe ⩰ Proxy + Leftovers`

```haskell
newtype ConduitM i o m r = ConduitM
  { unConduitM :: forall b.
     (r -> Pipe i i o () m b) -> Pipe i i o () m b }

data Pipe l i o u m r =
    HaveOutput (Pipe l i o u m r) (m ()) o
  | NeedInput (i -> Pipe l i o u m r) (u -> Pipe l i o u m r)
  | Done r
  | PipeM (m (Pipe l i o u m r))
  | Leftover (Pipe l i o u m r) l
```

`streaming`
-----------

Notes
:   * No input values!
    * `Functor f`
    * Where are the actual output values?

```haskell
data Stream f m r = Step !(f (Stream f m r))
                  | Effect (m (Stream f m r))
                  | Return r
```

. . .

Wait, that can't be it, can it?

Outputting values
-----------------

```haskell
-- | A left-strict pair; the base functor
--   for streams of individual elements.
data Of a b = !a :> b
  deriving (Functor)
```

. . .

```haskell
Stream (Of a) m r ≈ m ([a], r)
```

> * Why `f` rather than `Step !a (Stream f m r)`?
>     - Patience!

What this means
---------------

Notes
:   * Conduit is considering ditching foralls

> * `Stream (Of a) m r` is analogous to a `Source` or `Producer` in
>   conduit and pipes
>     - No bidirectional support, but how often is that needed?
> * No need for `forall` vs `()` vs `Void` confusion!
> * All other stream processors represent how to transform input to
>   output.
>     - Streaming instead uses normal functions and function
>       composition!
> * Conduit uses a similar type for its internal Stream Fusion
>   framework!

Using `streaming`
=================

Modules
-------

> * `Streaming` module operates on generic `(Functor f)`; functions
>   have unique names.
> * `Streaming.Prelude` typically uses `Of` and should be imported
>   qualified.
>     - Re-exports `Streaming`.

Example
-------

Notes
:   * Function composition!
    * If you take away the `S.`, it looks like normal Haskell if we
      didn't restrict IO
    * Though `print` would need to be mapped.
    * Note the simple `takeWhile` to stop trying to get more input.
    * Demo

```haskell
import qualified Streaming.Prelude as S
import           Text.Read            (readMaybe)

double :: Int -> Int
double = (*2)

doubleLines :: IO ()
doubleLines = S.print
              . S.map double
              . S.mapMaybe readMaybe
              . S.takeWhile (not . null)
              $ S.repeatM getLine
```

Minimal tutorials required!
---------------------------

Notes
:   * No fancy operators, types tend to plug and play
    * Knowing MFunctor and hoist can be helpful
    * Lots of standard instances (exceptions, MonadBase, etc.)
    * Could not think of actual tutorial-worthy material

> * Notice the lack of streaming-specific operators!
> * If you're used to Haskell and using lists, then using `streaming`
>   requires minimal mental switching.
> * Though there are a few "gotchas"...
> * ... that actually reflect the power of what it provides.

`f` vs `Of`
-----------

Notes
:   * The `f` vs `Of` disconnect can get a bit confusing
    * e.g. choosing which mapping function that you want
    * Latter two are from `Streaming` (don't need to be qualified)
    * Why deal with the extra effort?

```haskell
S.map :: (Monad m) => (a -> b)
         -> Stream (Of a) m r -> Stream (Of b) m r

S.mapM :: (Monad m) => (a -> m b)
          -> Stream (Of a) m r -> Stream (Of b) m r

S.maps :: (Functor f, Monad m) => (forall x. f x -> g x)
          -> Stream f m r -> Stream g m r

S.mapped :: (Functor f, Monad m) => (forall x. f x -> m (g x))
            -> Stream f m r -> Stream g m r
```

Example redux
-------------

What if you want to handle parsing errors?

. . .

```haskell
import qualified Streaming.Prelude as S
import           Text.Read            (readEither)

doubleLinesError :: IO ()
doubleLinesError = S.print
                   . S.map double
                   . S.stdoutLn
                   . S.map ("Not an Int: " ++)
                   . S.partitionEithers
                   . S.map readEither
                   . S.takeWhile (not . null)
                   $ S.repeatM getLine
```

Pop Quiz
--------

Notes
:   * Answer: b
    * Demo

### _Will this:_

> a) Print all error cases first?
> b) Print error cases as they occur?

What black magic is this?
-------------------------

Notes
:   * The ability to have Streams containing other Streams is very useful!

. . .

```haskell
S.partitionEithers :: (Monad m)
                      => Stream (Of (Either a b)) m r
                      -> Stream (Of a) (Stream (Of b) m) r
```

Streams of Streams
------------------

Notes
:   * Leftover support isn't as powerful as in Conduit; typically used
      with functions like `splitAt`.
    * Streams as Monadic effects typically handled seperately; using
      `hoist` here doesn't work well.
    * Found stream of stream to work nicer than the pipes-group
      `FreeT` solution.

> * `Stream f m (Stream f m r)`{.haskell}
>     - Leftovers (ala Conduit)
> * `Stream f (Stream g m) r`{.haskell}
>     - A stream created as a Monadic effect
> * `Stream (Stream f m) m r`{.haskell}
>     - An actual stream of streams (e.g. grouping).
> * `Stream (Of (Stream f m v)) m r`{.haskell}
>     - Don't think this is useful
> * `Stream (ByteString m) m r`{.haskell}
>     - Using `streaming-bytestring`.

Production Example
------------------

TODO

How does it compare?
====================

## {id="benchmarks" data-background="images/benchmarks.png" data-background-size="auto 100%"}

Notes
:   * Based off of benchmarks from machines library
    * Old results, as I had trouble running the benchmark
    * With exception first one, streaming is first result (in first
      benchmark it's the second one)

Resource Handling
-----------------

> * Currently has `ResourceT` support
> * Found not to work very well in practice
> * Consensus on using `bracket`/`withXXX`/continuation idiom
> * My solution: `streaming-with`
>     - Helper class to make it easier to write and use brackets

Available Packages
------------------

Notes
:   * `streaming-commons` is an unrelated package, but used by
    `streaming-utils`.
    * utils has pipes support, networking, compression, etc.
    * eversion is for converting to the `foldl` library
    * osm == Open Street Map
    * process not yet released

+--------------------+-------------------------------+
| `streaming`        | `streaming-bytestring`        |
| `streaming-utils`  | `streaming-postgresql-simple` |
| `streaming-wai`    | `streaming-conduit`           |
| `streaming-png`    | `streaming-eversion`          |
| `streaming-osm`    | `streaming-cassava`           |
| `streaming-with`   | `streaming-concurrency`       |
| `streaming-binary` | `streaming-process` †         |
+--------------------+-------------------------------+

Usage
-----

> * Not used in as many packages as conduit or pipes.
> * It is used in Sparkle by Tweag though.

Stream on! {data-background="images/stream.jpg" data-background-color="white"}
==========

---
# reveal.js settings
theme: night
transition: concave
backgroundTransition: zoom
center: true
history: true
css: custom.css
...
