fch
===

fch is a program for taking files and generating a means of compiling them into
your project as a string litteral. fch attempts to provide you with a clean
means of using the file by giving you the string and the length. It also
optionally lets the user decide on a 'module name' which will setup a module for
the file to make. in C, this means using define guards, in Haskell or Scheme,
this is an actual module.


Why?
----

When working with libguile, I wanted a way to compile store a Scheme program
into my C project while still being able to work on the Scheme file by itself.
I was copy and pasting this into a string litteral in C, and wished I could add
this to the build process, so I decided to generalize it to work with other
languages.


Contributing
------------

If you would like to file a bug or add a language, please issue a pull request
at [my github](https://github.com/llllllllll/fch) or send me an email at
joejev@gmail.com.
