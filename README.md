# Arcadia: An implementation of the Arc programming language #

(C) 2014 Kim, Taegyoon

Arc is a dialect of Lisp.

## Build
```
make
```

With [readline](http://cnswww.cns.cwru.edu/php/chet/readline/rltop.html) support,
```
make readline
```

For Visual C++, use .sln file.

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
`* + - / < > apply bound car cdr close coerce cons cos disp err expt eval flushout infile int is log macex maptable mod newstring outfile quit rand read readline scar scdr sin sqrt sread stderr stdin stdout string sym system t table tan trunc type write writeb`

## Library
`++ -- <= = >= abs accum acons adjoin afn aif alist all alref and append assoc atom avg best caar cadr carif case caselet cddr check complement compose copy def do do1 drain each empty even fill-table find firstn for forlen get idfn iflet in isa isnt iso join keep keys len let list listtab loop map map1 max med median memtable merge mergesort min multiple nearest no noisy-each nor nthcdr number obj odd on ontable or pair pop pos positive pr prn pull push pushnew quasiquote reclist reduce rem repeat rev rfn rotate round roundup rreduce set some sort sref swap tablist testify trues uniq unless until vals w/table w/uniq when whenlet whiler whilet wipe with withs zap`

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
