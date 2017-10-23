FPSyd-Streaming
================

[![Build Status](https://travis-ci.org/ivan-m/FPSyd-Streaming.svg)](https://travis-ci.org/ivan-m/FPSyd-Streaming)

> Talk presented at FP-Syd on 25 October, 2017.

Generated slides are available [here].

Abstract
--------

Many people are familiar with Haskell stream processing libraries such
as pipes or conduit. However, they can be a bit daunting to newcomers:
they contain both input and output types in the same representation
and they require all these new operators rather than using well-known
function composition.

The [streaming] library by Michael Thompson takes a simpler approach
to represent how to stream data through your code. It offers a more
familiar API to anyone used to using lists, whilst still offering a
lot of power and flexibility. It also compares very well in terms of
performance: conduit's fusion framework tends to use a similar
structure!

In this talk we will have an overview of the streaming ecosystem and
how it compares to pipes and conduit, and discuss how and why you
might (and might not) want to use it.

[streaming]: https://hackage.haskell.org/package/streaming
[here]: http://ivan-m.github.io/FPSyd-Streaming
