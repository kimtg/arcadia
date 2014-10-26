# Arcadia: An implementation of the Arc programming language #

(C) 2014 Kim, Taegyoon

Arc is a dialect of Lisp.

## Run
```
Usage: arcadia [OPTIONS...] [FILES...]

OPTIONS:
    -h    print this screen.
    -v    print version.
```

## Special form
`assign fn if mac quote while`

## Built-in
`* + - / < apply car cdr cons expt eval is log macex mod pr readline scar scdr sqrt t type writeb quit rand read string sym system`

## Library
`++ -- <= = > >= abs and append caar cadr cddr def do do1 each for idfn isa isnt join len let list map map1 no nthcdr or pair quasiquote reduce reverse rreduce sref uniq unless w/uniq when with`

## Features
* Easy-to-understand garbage collection
* Implicit indexing

## See also
* [Arc Tutorial](http://old.ycombinator.com/arc/tut.txt)
* [Try Arc: Arc REPL In Your Web Browser](http://tryarc.org/)

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
