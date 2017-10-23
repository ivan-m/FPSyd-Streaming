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
> * Currently maintained by Andrew Martin
> * Performs well compared to the competition

Stream Processing
-----------------

Notes
:   * Avoiding calling it "streaming"
    * Nowadays, "stream processing" generally refers to parallel
      programming.
    * Usage predates that.
    * Everyone knows what it is in Haskell, but no real definition.

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



About this configuration
========================

## Headings

* Pandoc puts top-level headings on their own page

    - I use setext style headers for them to help make them stand out
      in the markdown

* Level-2 headings create new slides

    - I prefer atx style headers for those to help add in metadata
      (e.g. images)

* You can use other headings as well

    - Level-6 headings have been configured in the CSS to help show up
      as image attribution.

## Image-based slide {data-background="images/haskell.png" data-background-color="white"}

* This slide has a background image

* You may need to put attribution last (Pandoc gets confused sometimes).

###### Attribution for image

## Speaker notes

* I use a filter to turn a definition list labeled `Notes` into
  speaker notes (hit `o` in reveal.js).

*
    ```markdown
    Notes
    :   * Note 1
        * Note 2
    ```

* If you don't want to use this, you can remove this filter from the
  pandoc-mode settings (and thus won't need pandoc-as-a-library
  installed).

Notes
:   * Note 1
    * Note 2

## Other

* reveal.js settings - including specification of the CSS file - are
  set in a YAML block at the bottom of the markdown file.

* You can also abuse YAML blocks to add in comments (e.g. to help
  organise your file, or have a bibliography section with all your
  links).

---
# reveal.js settings
theme: night
transition: concave
backgroundTransition: zoom
center: true
history: true
css: custom.css
...
