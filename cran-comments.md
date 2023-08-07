## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)


## R CMD check results

```
0 errors ✔ | 0 warnings ✔ | 5 notes ✖
```

There were no ERRORS or WARNINGS. There were 5 NOTES, 2 of which are "New submission". 

```
❯ On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Lukas Isermann <lukas.isermann@uni-mannheim.de>'
  
  New submission
  
❯ On fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... [5s/11s] NOTE
  Maintainer: ‘Lukas Isermann <lukas.isermann@uni-mannheim.de>’
  
  New submission
```

The other three notes relate to a likely bug in miktex (see https://github.com/r-hub/rhub/issues/503) that can be ignored,

```
❯ On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'
```

a likely issue on the ubuntu and fedora testing machine (see https://github.com/r-hub/rhub/issues/548) that I have no power to fix,

```
❯ On ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found
```

and a bug in rhub that can be ignored (may relate to this issue here: https://github.com/r-hub/rhub/issues/560). 


```
❯ On windows-x86_64-devel (r-devel)
  checking for non-standard things in the check directory ... NOTE
```
