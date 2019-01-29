# aire.zmvm

Version: 0.8.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘aire.zmvm-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: get_latest_imeca
    > ### Title: Get the latest pollution values for each station
    > ### Aliases: get_latest_imeca
    > 
    > ### ** Examples
    > 
    > df <- get_latest_imeca()
    Error in curl::curl_fetch_memory(url, handle = handle) : 
      Failed to connect to www.aire.cdmx.gob.mx port 80: Network is unreachable
    Calls: get_latest_imeca ... request_fetch -> request_fetch.write_memory -> <Anonymous>
    Execution halted
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 52 marked UTF-8 strings
    ```

# anipaths

Version: 0.9.6

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘knitr’ ‘rgdal’
      All declared Imports should be used.
    ```

# bioRad

Version: 0.4.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.2Mb
      sub-directories of 1Mb or more:
        data      1.6Mb
        extdata   2.6Mb
    ```

# BPEC

Version: 1.3.0

## In both

*   checking whether package ‘BPEC’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/checks.noindex/BPEC/new/BPEC.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘BPEC’ ...
** package ‘BPEC’ successfully unpacked and MD5 sums checked
** libs
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC  -Wall -g -O2  -c bpecfunction.c -o bpecfunction.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC  -Wall -g -O2  -c hashingfunctions.c -o hashingfunctions.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC  -Wall -g -O2  -c loopfunctions.c -o loopfunctions.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC  -Wall -g -O2  -c matrices.c -o matrices.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC  -Wall -g -O2  -c randomnumbers.c -o randomnumbers.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC  -Wall -g -O2  -c registerDynamicSymbol.c -o registerDynamicSymbol.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC  -Wall -g -O2  -c treelikelifunctions.c -o treelikelifunctions.o
clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o BPEC.so bpecfunction.o hashingfunctions.o loopfunctions.o matrices.o randomnumbers.o registerDynamicSymbol.o treelikelifunctions.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/checks.noindex/BPEC/new/BPEC.Rcheck/BPEC/libs
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/BPEC/rJava/libs/rJava.so':
  dlopen(/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/BPEC/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/BPEC/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘BPEC’
* removing ‘/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/checks.noindex/BPEC/new/BPEC.Rcheck/BPEC’

```
### CRAN

```
* installing *source* package ‘BPEC’ ...
** package ‘BPEC’ successfully unpacked and MD5 sums checked
** libs
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC  -Wall -g -O2  -c bpecfunction.c -o bpecfunction.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC  -Wall -g -O2  -c hashingfunctions.c -o hashingfunctions.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC  -Wall -g -O2  -c loopfunctions.c -o loopfunctions.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC  -Wall -g -O2  -c matrices.c -o matrices.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC  -Wall -g -O2  -c randomnumbers.c -o randomnumbers.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC  -Wall -g -O2  -c registerDynamicSymbol.c -o registerDynamicSymbol.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC  -Wall -g -O2  -c treelikelifunctions.c -o treelikelifunctions.o
clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o BPEC.so bpecfunction.o hashingfunctions.o loopfunctions.o matrices.o randomnumbers.o registerDynamicSymbol.o treelikelifunctions.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/checks.noindex/BPEC/old/BPEC.Rcheck/BPEC/libs
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/BPEC/rJava/libs/rJava.so':
  dlopen(/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/BPEC/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/BPEC/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘BPEC’
* removing ‘/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/checks.noindex/BPEC/old/BPEC.Rcheck/BPEC’

```
# countyweather

Version: 0.1.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# DataVisualizations

Version: 1.1.4

## In both

*   checking whether package ‘DataVisualizations’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/checks.noindex/DataVisualizations/new/DataVisualizations.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘DataVisualizations’ ...
** package ‘DataVisualizations’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/DataVisualizations/Rcpp/include" -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/DataVisualizations/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/DataVisualizations/Rcpp/include" -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/DataVisualizations/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c c_inPSphere2D.cpp -o c_inPSphere2D.o
clang++ -std=gnu++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o DataVisualizations.so RcppExports.o c_inPSphere2D.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: directory not found for option '-L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0'
ld: warning: directory not found for option '-L/usr/local/gfortran/lib'
ld: library not found for -lgfortran
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [DataVisualizations.so] Error 1
ERROR: compilation failed for package ‘DataVisualizations’
* removing ‘/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/checks.noindex/DataVisualizations/new/DataVisualizations.Rcheck/DataVisualizations’

```
### CRAN

```
* installing *source* package ‘DataVisualizations’ ...
** package ‘DataVisualizations’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/DataVisualizations/Rcpp/include" -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/DataVisualizations/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/DataVisualizations/Rcpp/include" -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/DataVisualizations/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c c_inPSphere2D.cpp -o c_inPSphere2D.o
clang++ -std=gnu++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o DataVisualizations.so RcppExports.o c_inPSphere2D.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: directory not found for option '-L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0'
ld: warning: directory not found for option '-L/usr/local/gfortran/lib'
ld: library not found for -lgfortran
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [DataVisualizations.so] Error 1
ERROR: compilation failed for package ‘DataVisualizations’
* removing ‘/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/checks.noindex/DataVisualizations/old/DataVisualizations.Rcheck/DataVisualizations’

```
# epiflows

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘htmlwidgets’
      All declared Imports should be used.
    ```

# FLightR

Version: 0.4.7

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rgdal’
      All declared Imports should be used.
    Missing or unexported object: ‘ggmap::has_goog_key’
    ```

# ggvoronoi

Version: 0.8.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      5: tryCatch(loadNamespace(name), error = function(e) stop(e))
      6: tryCatchList(expr, classes, parentenv, handlers)
      7: tryCatchOne(expr, names, parentenv, handlers[[1L]])
      8: value[[3L]](cond)
      
      ══ testthat results  ════════════════════════════════════════════════════════════
      OK: 12 SKIPPED: 0 FAILED: 5
      1. Error: Voronoi diagram heatmaps work correctly with continuous fill (@test_aes_continuous.R#36) 
      2. Error: Voronoi diagram heatmaps work correctly with discrete fill (@test_aes_discrete.R#36) 
      3. Error: Voronoi diagram outlines work correctly with blank diagram (@test_outline.R#33) 
      4. Error: Voronoi diagram outlines work correctly with continuous fill (@test_outline.R#37) 
      5. Error: Voronoi diagram outlines work correctly with discrete fill (@test_outline.R#41) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# helminthR

Version: 1.0.6

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘knitr’ ‘rmarkdown’
      All declared Imports should be used.
    ```

# hurricaneexposure

Version: 0.0.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘hurricaneexposuredata’
    ```

# inlabru

Version: 2.1.9

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘INLA’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘INLA’
    ```

# mapr

Version: 0.4.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 164 marked UTF-8 strings
    ```

# momentuHMM

Version: 1.4.3

## In both

*   checking whether package ‘momentuHMM’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/checks.noindex/momentuHMM/new/momentuHMM.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘momentuHMM’ ...
** package ‘momentuHMM’ successfully unpacked and MD5 sums checked
** libs
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/Rcpp/include" -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/Rcpp/include" -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c XBloop.cpp -o XBloop.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/Rcpp/include" -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c getDM.cpp -o getDM.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/Rcpp/include" -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c momentuHMM_init.c -o momentuHMM_init.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/Rcpp/include" -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c nLogLike.cpp -o nLogLike.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/Rcpp/include" -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c trMatrix.cpp -o trMatrix.o
clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o momentuHMM.so RcppExports.o XBloop.o getDM.o momentuHMM_init.o nLogLike.o trMatrix.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: directory not found for option '-L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0'
ld: warning: directory not found for option '-L/usr/local/gfortran/lib'
ld: library not found for -lgfortran
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [momentuHMM.so] Error 1
ERROR: compilation failed for package ‘momentuHMM’
* removing ‘/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/checks.noindex/momentuHMM/new/momentuHMM.Rcheck/momentuHMM’

```
### CRAN

```
* installing *source* package ‘momentuHMM’ ...
** package ‘momentuHMM’ successfully unpacked and MD5 sums checked
** libs
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/Rcpp/include" -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/Rcpp/include" -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c XBloop.cpp -o XBloop.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/Rcpp/include" -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c getDM.cpp -o getDM.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/Rcpp/include" -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c momentuHMM_init.c -o momentuHMM_init.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/Rcpp/include" -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c nLogLike.cpp -o nLogLike.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/Rcpp/include" -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c trMatrix.cpp -o trMatrix.o
clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o momentuHMM.so RcppExports.o XBloop.o getDM.o momentuHMM_init.o nLogLike.o trMatrix.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: directory not found for option '-L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0'
ld: warning: directory not found for option '-L/usr/local/gfortran/lib'
ld: library not found for -lgfortran
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [momentuHMM.so] Error 1
ERROR: compilation failed for package ‘momentuHMM’
* removing ‘/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/checks.noindex/momentuHMM/old/momentuHMM.Rcheck/momentuHMM’

```
# moveHMM

Version: 1.6

## In both

*   checking whether package ‘moveHMM’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/checks.noindex/moveHMM/new/moveHMM.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘moveHMM’ ...
** package ‘moveHMM’ successfully unpacked and MD5 sums checked
** libs
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/Rcpp/include" -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/Rcpp/include" -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c moveHMM_init.c -o moveHMM_init.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/Rcpp/include" -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c nLogLike.cpp -o nLogLike.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/Rcpp/include" -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c trMatrix.cpp -o trMatrix.o
clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o moveHMM.so RcppExports.o moveHMM_init.o nLogLike.o trMatrix.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: directory not found for option '-L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0'
ld: warning: directory not found for option '-L/usr/local/gfortran/lib'
ld: library not found for -lgfortran
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [moveHMM.so] Error 1
ERROR: compilation failed for package ‘moveHMM’
* removing ‘/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/checks.noindex/moveHMM/new/moveHMM.Rcheck/moveHMM’

```
### CRAN

```
* installing *source* package ‘moveHMM’ ...
** package ‘moveHMM’ successfully unpacked and MD5 sums checked
** libs
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/Rcpp/include" -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/Rcpp/include" -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c moveHMM_init.c -o moveHMM_init.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/Rcpp/include" -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c nLogLike.cpp -o nLogLike.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/Rcpp/include" -I"/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c trMatrix.cpp -o trMatrix.o
clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o moveHMM.so RcppExports.o moveHMM_init.o nLogLike.o trMatrix.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: directory not found for option '-L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0'
ld: warning: directory not found for option '-L/usr/local/gfortran/lib'
ld: library not found for -lgfortran
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [moveHMM.so] Error 1
ERROR: compilation failed for package ‘moveHMM’
* removing ‘/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/checks.noindex/moveHMM/old/moveHMM.Rcheck/moveHMM’

```
# rsinaica

Version: 0.6.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 467 marked UTF-8 strings
    ```

# spew

Version: 1.3.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Loading required package: sp
    Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "10"
    Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "10"
    Quitting from lines 45-64 (spew-quickstart.Rmd) 
    Error: processing vignette 'spew-quickstart.Rmd' failed with diagnostics:
    polygon edge not found
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘Rmpi’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘maptools’
      All declared Imports should be used.
    ```

# totalcensus

Version: 0.5.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        data   5.3Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 64 marked Latin-1 strings
      Note: found 548 marked UTF-8 strings
    ```

# trackeR

Version: 1.2.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 13.7Mb
      sub-directories of 1Mb or more:
        data      2.1Mb
        doc       2.5Mb
        extdata   8.4Mb
    ```

# ubeR

Version: 0.1.4

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(ubeR)
      > 
      > test_check("ubeR")
      [31m──[39m [31m1. Failure: raise error on fail to geocode (@test_utils.R#4) [39m [31m────────────────[39m
      `geocode("Kryptonopolis")` threw an error with unexpected message.
      Expected match: "Unable to find location 'Kryptonopolis'."
      Actual message: "Google now requires an API key.\n       See ?register_google for details."
      
      ══ testthat results  ════════════════════════════════════════════════════════════
      OK: 8 SKIPPED: 0 FAILED: 1
      1. Failure: raise error on fail to geocode (@test_utils.R#4) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# vmsbase

Version: 2.2.0

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available: ‘cairoDevice’ ‘gWidgetsRGtk2’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# wdpar

Version: 0.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘methods’ ‘rappdirs’ ‘sp’ ‘stringi’
      All declared Imports should be used.
    ```

# weathercan

Version: 0.2.8

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘xml2’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 25 marked UTF-8 strings
    ```

