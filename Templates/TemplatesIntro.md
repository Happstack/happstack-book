Templating for HTML and Javascript
==================================

Happstack supports a number of third party templating and HTML
libraries. It is easy to add support for additional libraries, if your
favorite does not already have support.

Each templating system has it's own set of advantages and drawbacks.

<dl>
 <dt>BlazeHtml</dt>
   <dd>The BlazeHtml library provides combinators for generating HTML 4 and HTML 5 in Haskell.
    pros:
     <ul>
      <li>Claims to be fast (some benchmarks to back this up)</li>
      <li>Use of combinators ensures output is always well-formed and free of typos in the names of elements and attributes</li>
      <li>Automatic escaping of String values</li>
      <li>Able to use the power of Haskell in your templates</li>
      <li>Type-checked at compile time to ensure no template values are missing</li>
      <li>Nice syntax (compared to the old html and xhtml libraries.)</li>
     </ul>
    cons:
     <ul>
      <li>Requires you to recompile in order to update the template</li>
      <li>Makes it easy to mix the application logic and view code together, making it hard to update later (therefore you must have self control)</li>
      <li>Only suitable for generating HTML documents</li>
      <li>Not ideal for having templates written by a web designer who does not know Haskell</li>
      <li>No compile-time assurances that generated html/xml is valid (though it will be well-formed).</li>
      <li>The `Html` monad is not a real monad, nor is it a monad transformer. This eliminates some advantage usage possibilities.</li>
     </ul>
   </dd>

 <dt>HSP</dt>
   <dd>HSP allows you to embed literal XML syntax inside your Haskell code. A pre-processor rewrites the literal XML into normal haskell function calls, and then the code is compiled.
    pros:
     <ul>
      <li>Templates are compiled, so they are pretty fast (needs more benchmarks to support this statement however)</li>
      <li>You have the full power of Haskell in your templates, because it is Haskell (with a purely syntactic extension)</li>
      <li>Type-checked at compile time to ensure types are correct and no template values are missing</li>
      <li>Automatic escaping of String values</li>
      <li>Syntax is very similar to XML/HTML, so it is easy to learn</li>
      <li>Can be easier to work with when trying to populate a template from a complex Haskell type</li>
      <li>Can be used to generate HTML or XML</li>
     </ul>
    cons:
     <ul>
      <li>Requires you to recompile in order to update the template</li>
      <li>Error messages are sometimes misleading or hard to understand</li>
      <li>Makes it easy to mix the application logic and view code together, making it hard to update later (therefore you must have self control)</li>
      <li>Only suitable for generating XML and HTML documents</li>
      <li>Not ideal for having templates written by a web designer who does not know Haskell (although the xml syntax helps)</li>
      <li>No compile-time assurances that generated html/xml is valid (though it will be well-formed).</li>
     </ul>
   </dd>
  <dt>Hamlet</dt><dd></dd>
  <dt>HStringTemplate</dt><dd></dd>
  <dt>Heist</dt><dd></dd>
  <dt>XSLT</dt><dd></dd>
  <dt>more to come..</dt><dd></dd>
</dl>

