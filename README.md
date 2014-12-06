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
`* + - / < > apply bound car cdr close coerce cons cos disp expt eval infile int is log macex maptable mod newstring outfile quit rand read readline scar scdr sin sqrt sread stderr stdin stdout string sym system t table tan trunc type write writeb`

## Library
`++ -- <= = >= abs acons adjoin afn aif alist all alref and append assoc atom avg best caar cadr carif case caselet cddr check complement compose copy def do do1 each even fill-table find firstn for get idfn iflet in isa isnt iso join keep keys len let list listtab map map1 max med median memtable merge mergesort min multiple nearest no nthcdr number obj odd or pair pop pos positive pr prn pull push pushnew quasiquote reclist reduce rem rev rfn rotate round roundup rreduce set some sort sref swap tablist testify trues uniq unless vals w/table w/uniq when whenlet wipe with withs zap`

## Features
* Easy-to-understand garbage collection
* Implicit indexing
* [Syntax sugar](http://arclanguage.github.io/ref/evaluation.html) (`[]`, `~`, `.`, `!`, `:`)

## See also
* [Arc Tutorial](http://old.ycombinator.com/arc/tut.txt)
* [Arc Documentation](http://arclanguage.github.io/ref/index.html)
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
