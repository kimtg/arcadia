# Arcadia: An implementation of the Arc programming language #

(C) 2014 Kim, Taegyoon

Arc is a dialect of Lisp.

## Special form
quote set fn if mac while

## Built-in
no pair? is apply < t / * - + cons cdr car scar scdr mod

## Library
rreduce list def abs reduce reverse unary-map map append caar cadr and or quasiquote let len do ++ -- =

## Features
* Easy-to-understand garbage collection
* Implicit indexing

## See also
[Arc Tutorial](http://old.ycombinator.com/arc/tut.txt)

## Building

Using [qo](http://github.com/andlabs/qo), run this command:

```console
$ qo
```

The resulting binary (`Arcadia`) will be put in the current working directory. 
Move it around as you see fit.

## License ##

   Copyright 2014 Kim, Taegyoon

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

   [http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
