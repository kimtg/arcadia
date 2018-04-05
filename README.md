# Arcadia: An implementation of the Arc programming language #

Arc is a dialect of Lisp.

## Build
```
make
```

With [readline](http://cnswww.cns.cwru.edu/php/chet/readline/rltop.html) support,
```
make readline
```

With [MinGW](http://www.mingw.org/),
```
mingw32-make mingw
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
`* + - / < > apply bound car ccc cdr close coerce cons cos disp err expt eval flushout infile int is len log macex maptable mod newstring outfile quit rand read readline scar scdr sin sqrt sread stderr stdin stdout string sym system t table tan trunc type write writeb`

## Library
`++ -- <= = >= aand abs accum acons adjoin afn aif alist all alref and andf assoc atend atom avg before best bestn caar cadr carif caris case caselet catch cddr check commonest compare complement compose consif conswhen copy copylist count counts cut dedup def do do1 dotted drain each empty even fill-table find firstn flat for forlen get idfn iflet in insert-sorted insort insortnew intersperse isa isnt iso join keep keys last len< len> let list listtab loop map map1 mappend max med median mem memtable merge mergesort min mismatch most multiple n-of nearest no noisy-each nor nthcdr number obj odd on only ontable or orf pair point pop pos positive pr prn pull push pushnew quasiquote rand-choice rand-elt range reclist recstring reduce reinsert-sorted rem repeat retrieve rev rfn rotate round roundup rreduce set single some sort split sref sum summing swap tablist testify tuples trues union uniq unless until vals w/table w/uniq when whenlet whiler whilet wipe with withs zap`

## Features
* Easy-to-understand mark-and-sweep garbage collection
* Tail call optimization
* Implicit indexing
* [Syntax sugar](http://arclanguage.github.io/ref/evaluation.html) (`[]`, `~`, `.`, `!`, `:`)

## See also
* [Arc Tutorial](http://old.ycombinator.com/arc/tut.txt)
* [Arc Documentation](http://arclanguage.github.io/ref/index.html)
* [Try Arc: Arc REPL In Your Web Browser](http://tryarc.org/)

## License ##

   Copyright 2014-2018 Kim, Taegyoon

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

   [http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
