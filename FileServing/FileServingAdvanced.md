
Advanced File Serving
---------------------

`serveDirectory` and `serveFile` should cover a majority of your file serving needs. But if you want something a little different, it is also possible to roll-your-own solution. The `Happstack.Server.FileServe.BuildingBlocks` module contains all the pieces used to assemble the high-level `serveDirectory` and `serveFile` functions. You can reuse those pieces to build your own custom serving functions. For example, you might want to use a different method for calculating the mime-types, or perhaps you want to create a different look-and-feel for directory browsing, or maybe you want to use something other than `sendFile` for sending the files. I recommend starting by copying the source for `serveDirectory` or `serveFile` and then modifying it to suit your needs.
