<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. haskell-opengl</a>
<ul>
<li><a href="#sec-1-1">1.1. About</a></li>
<li><a href="#sec-1-2">1.2. Current progress</a></li>
<li><a href="#sec-1-3">1.3. Build</a></li>
</ul>
</li>
</ul>
</div>
</div>

# haskell-opengl<a id="sec-1" name="sec-1"></a>

## About<a id="sec-1-1" name="sec-1-1"></a>

A haskell port of examples from .

SDL is used instead of GLFW because of personal preference.

FRP techniques might be applied for managing time-varying values but a
non-FRP version written completely in plain old monad will also be
provided in the future.

## Current progress<a id="sec-1-2" name="sec-1-2"></a>

-   Hello, Triangle

## Build<a id="sec-1-3" name="sec-1-3"></a>

    $ stack build