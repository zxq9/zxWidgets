# zxWidgets
Library of metawidget wrappers for wxErlang.

## What problem does this solve?
Erlang has fairly simple-to-use GUI bindings to the wxWidgets library. The bindings
themselves are fine, but the underlying library, being a strong example of OOP GUI
design, is not a very natural fit for an Erlanger's mindset. zxWidgets is an attempt
to wrap some of the most annoyingly verbose cases where Erlang's functional style
and wx's OOP style collide and provide a functional interface that operates (as
often as possible) over declarative descriptions, lists, and other functional
constructs, as well as offloading as many of the boilerplate tasks as possible.

## Links

### Documentation
http://zxq9.com/projects/zx_widgets/docs/

### Source
https://github.com/zxq9/zxWidgets

## Approach
zxWidgets provides a set of helper functions and "meta-widgets" that construct complex
GUI pieces from simpler ones in ways that programmers often find themselves wanting to
do repeatedly but not wanting to write the code for over and over. In fact, this
library is slowly being assembled from one-off GUI helper modules I've written to take
care of [AnnoyingTask] in the course of working on other projects.

In the case that a metawidget doesn't quite do what you want, sometimes it can be
very helpful to read the zx source to figure out what part you want to adjust, and then
write your own wx code based on the basic zx metawidget that was similar to what you
wanted. Because the zx source code can occasionally be more useful than the actual
metawidgets themselves a goal of this project is to produce well-documented, readable
code.

### Application or library?
This is a library application -- meaning it does not require calling zx_widgets:start()
and there are no processes or supervision trees involved here. Most functions either
return useful values that can be used in wxErlang calls or zx metawidgets.

## A note on versions
This library will be forever incomplete, partly because the things people want to do
with GUIs will never stop changing, and partly because GUI coders will never stop
identifying common patterns underlying the way interfaces are constructed. The current
release is extremely sparse in functionality but is already being used in external
projects, so to prevent breaking other code I am releasing this as a *very* early
v1.0 now. That means the existing API will *not* change util another major version
increase, though it is very likely that there will be a very large number of subversion
increases as new metawidgets are added over time.

## Contributing
Anything missing? Silly? Stupid? Buggy? Want to actually use it in a real project (and
so really want some erlang.mk or rebar3 files added)? Send a pull request, file a bug,
or just send me an email.
