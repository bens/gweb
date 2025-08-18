---
title: gweb
generate-toc: true
tangles:
    - name: main
      path: main.hs
      language: hs
    - name: main-append
      path: append.hs
      language: hs
    - name: main-link
      path: linked.hs
      language: hs
---

`gweb` is a [`pandoc`](https://pandoc.org)-based tool for keeping source code and documentation in sync. You write in Markdown, interleave and link code blocks, and `gweb` tangles them into compilable source files. This prevents documentation drifting away from the code it describes.

Key features:

  - [`noweb`](https://www.cs.tufts.edu/~nr/noweb/)-style links -- compose code in chunks and reference them across your document.

  - Multiple outputs -- generate both user-friendly docs (HTML) and source code files.

  - Semantic HTML -- clean structure with IDs and classes, leaving styling up to you.

  - Team-oriented markup -- developer-only comments and reviewer remarks for collaboration.

  - Inline diagramming -- embed visual explanations directly with [GraphViz](https://graphviz.org/) and [Pikchr](https://pikchr.org/), while keeping everything in plain text.

  - Inline plotting -- create data plots with [Gnuplot](http://gnuplot.info/), sourcing data inline or from external files.

See the example in `./example`, and run `make example` to try it out on this very `README.md` file.

# Why I Built This

This tool originated at Geora, where we had a rapidly evolving GraphQL schema and a strong need for documentation that stayed in sync with the schema. We also wanted the flexibility to include developer-only remarks to ourselves without exposing those to end users.

Initially we tried the [Pollen](https://docs.racket-lang.org/pollen/) system in Racket, but as the project grew, performance became a bottleneck and rebuilds became painfully slow. I was confident I could build something faster while retaining the flexibility we needed. We ended up with `gweb`, a custom documentation processor that ran quickly, integrated smoothly, and gave us complete control.

In practice, we used it successfully for our GraphQL schema documentation, where it worked well to keep user-facing docs up to date with the evolving schema. While we didn't adopt it more widely across the system, the approach proved itself in that domain.

# YAML Header

The header of a document must exist and provide the title for the document.

```yaml
---
title: My Example
---
```

These other options are recognised as well:

  - `generate-toc` whether to generate a table of contents, a boolean.
  - `tangles` described below.

# Code block

The core feature of a literate programming tool is support for source code blocks. These are normal Markdown code blocks with an ID.

``` {#main}
module Main (main) where
```

To have this be tangled into a source file, it needs to be specified in the YAML document header:

```yaml
tangles:
    - name: main
      path: main.hs
      language: hs
```

- `name` is the source block ID to use as the tangle root block.
- `path` is the file path where the tangled source code should be written.
- `language` is a string describing which programming or markup language is to be tangled.

`language` is defined but not currently used, the intent is to use it to support syntax highlighting eventually.

## Appending code to blocks

You can provide more than one block with the same block ID, the first such block is the "canonical" one, and others are treated as additions to it. So if there is a `main-append` block like so:

``` {#main-append}
module Main (main) where
```

you can have another block with the same ID

``` {#main-append}
main :: IO
main = putStrLn "Hello, world!"
```

and the two blocks will be tangled as one following the other.

## Linking

Starting with the same code example again, rather than having a linear chain of blocks using the same block ID, we can link to another block with a different ID for a smaller section of code.

``` {#main-link}
module Main (main) where

main :: IO ()
main = do
    <<hello>>
    pure ()
```

and now we can fill in the space with another block with the proper ID.

``` {#hello}
putStrLn "Hello, World!"
```

Links are best placed on their own line, indentation is handled sensibly but having non-indent text on the same line as the angle-bracket link might give unwanted results.

# Collaboration support
## Developer-only Comments

`gweb` lets you separate user-facing docs from developer notes. Extra discussion meant only for the dev team can be wrapped in a block like this:

```markdown
::::: {.dev-only}
Never speak of what you witness here.
:::::
```

This won't just be hidden in user documentation, it won't be present at all.

## Reviewer Remarks

A variant on developer-only blocks is reviewer remarks, where a reviewer wants to leave a comment on the state of the documentation or code and be able to sign their name directly. This is another type of block.

```markdown
::::: {.review-remark reviewer=joe}
LGTM.
:::::
```

The distinction from developer-only blocks lies in the intended voice. Developer-only comments speak as the collective narrator conveying something to the development team at a future time, while a reviewer remark is a specific individual adding their particular point of view.

# Visual documentation

`gweb` supports embedding diagrams and plots directly into your documentation, so readers can see visual representations while still having a fully textual source document.

## GraphViz

You can embed diagrams directly into your documentation. For example, a GraphViz graph can be defined inline:

``` {.dot style="width: 20em"}
digraph {
  graph [rankdir=BT, margin=0];
  {B; C}    -> A;
  {D; E; F} -> B
  F         -> C
}
```

The output is rendered to SVG and embedded into the HTML output, so there are no extra files to manage.

## Pikchr

Diagrams can also be created and rendered with `pikchr`.

``` {.pikchr style="width: 20em"}
boxwid = 0.35; boxht = 0.25;

A: box "A"

B_1: box "B" with n at 0.25 below A.s
B_2: box "C"

C_1: box "D" with n at 0.25 below B_1.s
C_2: box "E"
C_3: box "F"
C_4: box "G"
```

## Plotting Data

`gweb` also supports inline data visualization with Gnuplot. Data can be provided as a table inside the document, or sourced from an external file.

Tables defined inline are referenced by name (with a $ sigil when used inside a plot script):

``` {.gnuplot table="foo" style="width: 20em"}
set xtic 1
plot $foo using 1:2 with lines title "foo"
```

::::: {data-table="foo" .hidden}
   X    Y
---- ----
   1   10
   2   12
   3    9
   4    3
   5   10
:::::

For larger or generated datasets, you can bind a file instead. In this case, the bound name is used directly (without a `$` sigil):

``` {.gnuplot file-foo="gnuplot.dat" style="width: 20em"}
set xtic 1
plot foo using 1:2 with lines title "foo"
```

# Project Status

The tool is stable: it was used internally for several years with issues fixed as they arose. Future improvements are mostly extensions or changes in direction, for example, exposing it as a library to make it easier to build custom literate programming tools, similar to [XMonad](http://xmonad.org).

The code is well tested with golden tests, with property tests for one finicky feature too. Diagram integrations can cause test failures when tools (like GraphViz) include version comments in their output; stripping those comments would make the tests more reliable.

# Roadmap
## Multi-document projects

Currently, `gweb` only considers a single file in a run. Supporting projects split across multiple documents while still linking blocks between them is the next logical step to improve scalability for larger code bases.

## Per-language line & file markings

CPP allows you record what file and line generated some source code. `gweb` should support this, but also allow for other languages that support a similar mechanism.

## Parameterised blocks

Support for parameterised blocks is planned, enabling more flexible reuse of code fragments.

## Syntax highlighting

Syntax highlighting is also on the roadmap. The challenge is ensuring accuracy for languages with context-dependent syntax. Ideally, highlighting would be applied after tangling, then mapped back to the originating code blocks in the documentation.
