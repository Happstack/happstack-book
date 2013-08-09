
Cookie Issues
-------------

Despite their apparently simplicity, `Cookies` are the source of many bugs and security issues in web applications. Here are just a few of the things you need to keep in mind.

**Security issues**

To get an understanding of cookie security issues you should search for:

 * [http://www.google.com/search?q=cookie+security+issues](cookie security issues)
 * [http://www.google.com/search?q=cookie+XSS](cookie XSS).

One important thing to remember is that the user can modify the
cookie. So it would be a bad idea to do, `addCookie Session (mkCookie
"userId" "1234")` because the user could modify the cookie and change
the userId at will to access other people's accounts.

Also, if you are not using `https` the cookie will be sent unencrypted.

**Delayed Effect**

When you call `addCookie` the `Cookie` will not be available until
after that `Response` has been sent and a new `Request` has been
received. So the following code will not work:


~~~~ {.haskell}
do addCookie Session (mkCookie "newCookie" "newCookieValue")
   v <- look "newCookie"
   ...
~~~~

The first time it runs, `look` will fail because the cookie was not
set in the current `Request`. Subsequent times `look` will return the
old cookie value, not the new value.

**Cookie Size**

Browsers impose limits on how many cookies each site can issue, and how big those cookies can be. The RFC recommends browsers accept a minimum of 20 cookies per site, and that cookies can be at least 4096 bytes in size. But, implementations may vary. Additionally, the cookies will be sent with every request to the domain. If your page has dozens of images, the cookies will be sent with every request. That can add a lot of overhead and slow down site loading times.

A common alternative is to store a small session id in the cookie, and store the remaining information on the server, indexed by the session id. Though that brings about its own set of issues.

One way to avoid having cookies sent with every image request is to host the images on a different sub-domain. You might issues the cookies to www.example.org, but host images from images.example.org. Note that you do not actually have to run two servers in order to do that. Both domains can point to the same IP address and be handled by the same application. The app itself may not even distinguish if the requests were sent to `images` or `www`.

**Server Clock Time**

In order to calculate the `expires` date from the `max-age` or the `max-age` from the `expires` date, the server uses `getCurrentTime`. This means your system clock should be reasonably accurate. If your server is not synchronized using `NTP` or something similar it should be.

**Cookie Updates are Not Atomic**

Cookie updates are not performed in any sort of atomic manner. As a
result, the simple cookie demo contains a race condition. We get the
`Cookie` value that was included in the `Request` and use it to create
an updated `Cookie` value in the `Response`. But remember that the
server can be processing many requests in parallel and the browser can
make multiple requests in parallel. If the browser, for example,
requested 10 images at once, they would all have the same initial
cookie value. So, even though they all updated the counter by 1, they
all started from the same value and ended with the same value. The
count could even go backwards depending on the order `Requests` are
received and `Responses` are processed.

