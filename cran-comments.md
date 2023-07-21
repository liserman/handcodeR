## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)


## R CMD check results

There were no ERRORS or WARNINGS. 

There were 6 NOTES, 3 of which are "New submission". The other three notes relate to the README.rmd file being written in Rmarkdown and therefore resulting in a non-standard file at the top level, a likely bug in miktex (see https://github.com/r-hub/rhub/issues/503) that can be ignored, and a likely issue on the ubuntu testing machine (see https://github.com/r-hub/rhub/issues/548) that I have no power to fix. 

```
❯ On windows-x86_64-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Lukas Isermann <lukas.isermann@uni-mannheim.de>'
  
  New submission

❯ On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking top-level files ... NOTE
  Non-standard file/directory found at top level:
    'README.Rmd'

❯ On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

❯ On ubuntu-gcc-release (r-release)
  checking CRAN incoming feasibility ... [6s/21s] NOTE
  Maintainer: ‘Lukas Isermann <lukas.isermann@uni-mannheim.de>’
  
  New submission

❯ On ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

❯ On fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... [6s/23s] NOTE
  Maintainer: ‘Lukas Isermann <lukas.isermann@uni-mannheim.de>’
  
  New submission

0 errors ✔ | 0 warnings ✔ | 6 notes ✖
```
