Introduction
============

Happstack is a family of libraries for creating fast, modern, and
scalable web applications. A majority of the libraries are loosely
coupled so that you can choose the technologies that are best for your
particular application. You are not required to use any particular
library for your database, templating, routing, etc.

However, if you are new to Haskell web development, too many choices
can be overwhelming. So, there are three places you might consider
starting.

[`happstack-lite`](http://happstack.com/page/view-page-slug/9/happstack-lite-tutorial)
is the quickest and easiest way to get started. It uses `blaze-html`
for HTML generation and simple dynamic routing combinators. It has a
very small, but sufficient API, for writing many web applications.

[`happstack-foundation`](http://www.youtube.com/watch?v=7Wmszk4wZxQ)
provides a more powerful, type-safe, and feature rich starting
point. It has type-safe URL routing, a template system that allows you
to embed literal XML syntax in your Haskell code, integrated support
for `acid-state` databases, type-safe form processing, and more.

[`clckwrks`](http://www.clckwrks.com/) is a higher-level web framework
built on top of the same technology that `happstack-foundation`
uses. `clckwrks` makes it easy to develop web applications by
providing off-the-shelf plugins and themes. It even has (very)
experimental support for installing new themes and plugins with out
restarting the server. clckwrks plugins can do just about
anything. Current plugins include a CMS/blogging system, a media
gallery, an ircbot, and more.

This book covers the many libraries commonly used by Happstack
developers. You can feel free to skip around and read just the
sections you are interested in. Each section comes with a small,
self-contained example that you can download and run locally.

This book was recently converted to a new build system. If you find
any mistakes, formatting errors, bad links, etc, please report them to
jeremy@n-heptane.com.

In addition to the HTML version of this book you can also read it in
[PDF](happstack-book.pdf), [EPUB/NOOK](happstack-book.epub), and
[Mobi/Kindle](happstack-book.mobi).



