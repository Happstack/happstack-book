Templating for HTML and Javascript
==================================

Happstack supports a number of third party templating and HTML
libraries. It is easy to add support for additional libraries, if your
favorite does not already have support.

The three most popular choices are `HSP`, `blaze-html`, and `heist`.

`blaze-html` is a fast, combinator based HTML creation library.

pros:

  * Claims to be fast (some benchmarks to back this up)
  * Use of combinators ensures output is always well-formed and free of typos in the names of elements and attributes
  * Automatic escaping of String values
  * Able to use the power of Haskell in your templates
  * Type-checked at compile time to ensure no template values are missing
  * Nice syntax (compared to the old html and xhtml libraries.)

cons:

  * Requires you to recompile in order to update the template
  * Makes it easy to mix the application logic and view code together, making it hard to update later (therefore you must have self control)
  * Only suitable for generating HTML documents
  * Not ideal for having templates written by a web designer who does not know Haskell
  * No compile-time assurances that generated html/xml is valid (though it will be well-formed).
  * The `Html` monad is not a real monad, nor is it a monad transformer. This eliminates some advantage usage possibilities.

`HSP` allows you to embed literal XML syntax inside your Haskell code. A pre-processor or `QuasiQuoter` rewrites the literal XML into normal haskell function calls, and then the code is compiled.

pros:

  * Templates are compiled, so they are pretty fast (needs more benchmarks to support this statement however)
  * You have the full power of Haskell in your templates, because it is Haskell (with a purely syntactic extension)
  * Type-checked at compile time to ensure types are correct and no template values are missing
  * Automatic escaping of String values
  * Syntax is very similar to XML/HTML, so it is easy to learn
  * Can be easier to work with when trying to populate a template from a complex Haskell type
  * Can be used to generate HTML or XML

cons:

  * Requires you to recompile in order to update the template
  * Error messages are sometimes misleading or hard to understand
  * Makes it easy to mix the application logic and view code together, making it hard to update later (therefore you must have self control)
  * Only suitable for generating XML and HTML documents
  * Not ideal for having templates written by a web designer who does not know Haskell (although the xml syntax helps)
  * No compile-time assurances that generated html/xml is valid (though it will be well-formed).


`Heist` uses a combination of external XML files and Haskell code to perform templating.

 pros:

  * changes to the external XML files can be reloaded with out having to recompile and restart the server
  * a large portion of the template system is standard XHTML in external templates, making it easier for web designers to use

cons:

 * prone to silent runtime errors

