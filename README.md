# protopost

_**a composition environment**_

---

## Introduction

_protopost_ is an online text composition environment, supporting WYSIWYG-style HTML composition and text/preview-style composition of markdown, plaintext, or HTML. It's the publishing front-end of a content management system like WordPress, divorced from any back-end that actually might publishes the pieces.

The intent is that any such back-end should be a separate service to which the composition environment publishes. In theory, that service could publish the compositions in any way it sees fix. In practice, the intention is for the back-end to be a "static-site manager", a service that converts the composed posts into the source file of a text-based static-side generator, then runs the generator to recreate the static-site.

The goal is to create an environment that from authors' perspective is very much like WordPress or Substack &dash; that doesn't require writers to fire up a text editor and compose markdown with YAML front-matter and such &mdash; but by which what's generated is a stand-alone, resilient, relocatable static site, which can be hosted or rehosted anywhere, maintained in its full history via git, even read offline. (The back-end static site manager should handle managing the sites it maintains under version control.)

The hope is to bring the technical and ethical benefits of regenerable static-sites to less technical writers not inclined to manage directories of plaintext under git with command-line scripts.

## Features

Protopost tries to be very neurotic about not losing work. It autosaves revisions frequently, and permits users to browse and restore revisions at will. Protopost allows users to subscribe to specific RSS feeds in order to scan them for comments, which are recognized as posts with [`iffy:in-reply-to`](https://tech.interfluidity.com/xml/iffy/index.html#iffy-in-reply-to) elements in their RSS. (For now you can subscribe, but the scanning for and retrieving comments is not yet implemented!)

## Status

The status of this project is still very early. It's a piece of "project restack", an attempt to reproduce the tools associated with newsletter platforms in an open, community hostable, ethical, and technically resilient manner. A related, much more mature tool is [_feedletter_](https://github.com/swaldman/feedletter), which will convert any site emitting full-content RSS [into a beautifully formatted newsletter](https://tech.interfluidity.com/2024/01/29/feedletter-tutorial/index.html), as well as [syndicate post notifications](https://tech.interfluidity.com/2025/01/14/syndicating-rss-to-mastodon-and-bluesky-with-feedletter/) to Mastodon and Bluesky. That one is ready to go! Please use it!

A test instance is of _protopost_ available at [https://protopost.test.mchange.com/](https://protopost.test.mchange.com/). It's gated — if you'd like to give it a try please contact [`swaldman@mchange.com`](mailto:swaldman@mchange.com).

Information on running your own instance will hopefully come soon.

## Acknowledgements

This project has been supported in part by external financial sponsorship.
Many thanks to Chris Peel for his support!

