## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)


## R CMD check results

```
0 errors ✔ | 0 warnings ✔ | 3 notes ✖
```

There were no ERRORS or WARNINGS. There were 3 NOTES. One of them relates to a likely bug in miktex (see https://github.com/r-hub/rhub/issues/503) that can be ignored.

```
❯ On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'
```    

One of them is a likely issue on the ubuntu and fedora testing machine (see https://github.com/r-hub/rhub/issues/548) that I have no power to fix.

```
❯ On ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found
```


The last Note likely refers to a bug in rhub that can be ignored (may relate to this issue here: https://github.com/r-hub/rhub/issues/560).

```
❯ On windows-x86_64-devel (r-devel)
  checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''
```
