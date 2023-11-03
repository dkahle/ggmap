# DataVisualizations

<details>

* Version: 1.3.2
* GitHub: https://github.com/Mthrun/DataVisualizations
* Source code: https://github.com/cran/DataVisualizations
* Date/Publication: 2023-10-10 08:10:02 UTC
* Number of recursive dependencies: 155

Run `revdepcheck::revdep_details(, "DataVisualizations")` for more info

</details>

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
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 15.0.0 (clang-1500.0.40.1)’
using SDK: ‘’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/DataVisualizations/Rcpp/include' -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/DataVisualizations/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -I/opt/homebrew/Cellar/gsl/2.7.1/include -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/DataVisualizations/Rcpp/include' -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/DataVisualizations/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -I/opt/homebrew/Cellar/gsl/2.7.1/include -c c_pde.cpp -o c_pde.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/DataVisualizations/Rcpp/include' -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/DataVisualizations/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -I/opt/homebrew/Cellar/gsl/2.7.1/include -c c_quantile.cpp -o c_quantile.o
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -L/opt/homebrew/Cellar/gsl/2.7.1/lib -L/opt/homebrew/lib/gcc/current -o DataVisualizations.so RcppExports.o c_pde.o c_quantile.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0 -L/opt/gfortran/lib -lgfortran -lemutls_w -lquadmath -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0' not found
ld: warning: search path '/opt/gfortran/lib' not found
ld: library 'emutls_w' not found
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [DataVisualizations.so] Error 1
ERROR: compilation failed for package ‘DataVisualizations’
* removing ‘/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/checks.noindex/DataVisualizations/new/DataVisualizations.Rcheck/DataVisualizations’


```
### CRAN

```
* installing *source* package ‘DataVisualizations’ ...
** package ‘DataVisualizations’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 15.0.0 (clang-1500.0.40.1)’
using SDK: ‘’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/DataVisualizations/Rcpp/include' -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/DataVisualizations/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -I/opt/homebrew/Cellar/gsl/2.7.1/include -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/DataVisualizations/Rcpp/include' -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/DataVisualizations/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -I/opt/homebrew/Cellar/gsl/2.7.1/include -c c_pde.cpp -o c_pde.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/DataVisualizations/Rcpp/include' -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/DataVisualizations/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -I/opt/homebrew/Cellar/gsl/2.7.1/include -c c_quantile.cpp -o c_quantile.o
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -L/opt/homebrew/Cellar/gsl/2.7.1/lib -L/opt/homebrew/lib/gcc/current -o DataVisualizations.so RcppExports.o c_pde.o c_quantile.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0 -L/opt/gfortran/lib -lgfortran -lemutls_w -lquadmath -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0' not found
ld: warning: search path '/opt/gfortran/lib' not found
ld: library 'emutls_w' not found
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [DataVisualizations.so] Error 1
ERROR: compilation failed for package ‘DataVisualizations’
* removing ‘/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/checks.noindex/DataVisualizations/old/DataVisualizations.Rcheck/DataVisualizations’


```
# momentuHMM

<details>

* Version: 1.5.5
* GitHub: https://github.com/bmcclintock/momentuHMM
* Source code: https://github.com/cran/momentuHMM
* Date/Publication: 2022-10-18 20:52:35 UTC
* Number of recursive dependencies: 147

Run `revdepcheck::revdep_details(, "momentuHMM")` for more info

</details>

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
** using staged installation
** libs
using C compiler: ‘Apple clang version 15.0.0 (clang-1500.0.40.1)’
using C++ compiler: ‘Apple clang version 15.0.0 (clang-1500.0.40.1)’
using SDK: ‘’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/Rcpp/include' -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -I/opt/homebrew/Cellar/gsl/2.7.1/include -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/Rcpp/include' -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -I/opt/homebrew/Cellar/gsl/2.7.1/include -c XBloop.cpp -o XBloop.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/Rcpp/include' -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -I/opt/homebrew/Cellar/gsl/2.7.1/include -c getDM.cpp -o getDM.o
...
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/Rcpp/include' -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -I/opt/homebrew/Cellar/gsl/2.7.1/include -c nLogLike.cpp -o nLogLike.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/Rcpp/include' -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -I/opt/homebrew/Cellar/gsl/2.7.1/include -c trMatrix.cpp -o trMatrix.o
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -L/opt/homebrew/Cellar/gsl/2.7.1/lib -L/opt/homebrew/lib/gcc/current -o momentuHMM.so RcppExports.o XBloop.o getDM.o momentuHMM_init.o nLogLike.o trMatrix.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0 -L/opt/gfortran/lib -lgfortran -lemutls_w -lquadmath -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0' not found
ld: warning: search path '/opt/gfortran/lib' not found
ld: library 'emutls_w' not found
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [momentuHMM.so] Error 1
ERROR: compilation failed for package ‘momentuHMM’
* removing ‘/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/checks.noindex/momentuHMM/new/momentuHMM.Rcheck/momentuHMM’


```
### CRAN

```
* installing *source* package ‘momentuHMM’ ...
** package ‘momentuHMM’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘Apple clang version 15.0.0 (clang-1500.0.40.1)’
using C++ compiler: ‘Apple clang version 15.0.0 (clang-1500.0.40.1)’
using SDK: ‘’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/Rcpp/include' -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -I/opt/homebrew/Cellar/gsl/2.7.1/include -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/Rcpp/include' -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -I/opt/homebrew/Cellar/gsl/2.7.1/include -c XBloop.cpp -o XBloop.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/Rcpp/include' -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -I/opt/homebrew/Cellar/gsl/2.7.1/include -c getDM.cpp -o getDM.o
...
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/Rcpp/include' -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -I/opt/homebrew/Cellar/gsl/2.7.1/include -c nLogLike.cpp -o nLogLike.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/Rcpp/include' -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/momentuHMM/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -I/opt/homebrew/Cellar/gsl/2.7.1/include -c trMatrix.cpp -o trMatrix.o
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -L/opt/homebrew/Cellar/gsl/2.7.1/lib -L/opt/homebrew/lib/gcc/current -o momentuHMM.so RcppExports.o XBloop.o getDM.o momentuHMM_init.o nLogLike.o trMatrix.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0 -L/opt/gfortran/lib -lgfortran -lemutls_w -lquadmath -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0' not found
ld: warning: search path '/opt/gfortran/lib' not found
ld: library 'emutls_w' not found
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [momentuHMM.so] Error 1
ERROR: compilation failed for package ‘momentuHMM’
* removing ‘/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/checks.noindex/momentuHMM/old/momentuHMM.Rcheck/momentuHMM’


```
# moveHMM

<details>

* Version: 1.9
* GitHub: https://github.com/TheoMichelot/moveHMM
* Source code: https://github.com/cran/moveHMM
* Date/Publication: 2023-05-08 18:20:05 UTC
* Number of recursive dependencies: 79

Run `revdepcheck::revdep_details(, "moveHMM")` for more info

</details>

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
** using staged installation
** libs
using C compiler: ‘Apple clang version 15.0.0 (clang-1500.0.40.1)’
using C++ compiler: ‘Apple clang version 15.0.0 (clang-1500.0.40.1)’
using SDK: ‘’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/Rcpp/include' -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -I/opt/homebrew/Cellar/gsl/2.7.1/include -c RcppExports.cpp -o RcppExports.o
clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/Rcpp/include' -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c moveHMM_init.c -o moveHMM_init.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/Rcpp/include' -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -I/opt/homebrew/Cellar/gsl/2.7.1/include -c nLogLike.cpp -o nLogLike.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/Rcpp/include' -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -I/opt/homebrew/Cellar/gsl/2.7.1/include -c trMatrix.cpp -o trMatrix.o
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -L/opt/homebrew/Cellar/gsl/2.7.1/lib -L/opt/homebrew/lib/gcc/current -o moveHMM.so RcppExports.o moveHMM_init.o nLogLike.o trMatrix.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0 -L/opt/gfortran/lib -lgfortran -lemutls_w -lquadmath -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0' not found
ld: warning: search path '/opt/gfortran/lib' not found
ld: library 'emutls_w' not found
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [moveHMM.so] Error 1
ERROR: compilation failed for package ‘moveHMM’
* removing ‘/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/checks.noindex/moveHMM/new/moveHMM.Rcheck/moveHMM’


```
### CRAN

```
* installing *source* package ‘moveHMM’ ...
** package ‘moveHMM’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘Apple clang version 15.0.0 (clang-1500.0.40.1)’
using C++ compiler: ‘Apple clang version 15.0.0 (clang-1500.0.40.1)’
using SDK: ‘’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/Rcpp/include' -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -I/opt/homebrew/Cellar/gsl/2.7.1/include -c RcppExports.cpp -o RcppExports.o
clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/Rcpp/include' -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c moveHMM_init.c -o moveHMM_init.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/Rcpp/include' -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -I/opt/homebrew/Cellar/gsl/2.7.1/include -c nLogLike.cpp -o nLogLike.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/Rcpp/include' -I'/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/library.noindex/moveHMM/RcppArmadillo/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -I/opt/homebrew/Cellar/gsl/2.7.1/include -c trMatrix.cpp -o trMatrix.o
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -L/opt/homebrew/Cellar/gsl/2.7.1/lib -L/opt/homebrew/lib/gcc/current -o moveHMM.so RcppExports.o moveHMM_init.o nLogLike.o trMatrix.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0 -L/opt/gfortran/lib -lgfortran -lemutls_w -lquadmath -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0' not found
ld: warning: search path '/opt/gfortran/lib' not found
ld: library 'emutls_w' not found
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [moveHMM.so] Error 1
ERROR: compilation failed for package ‘moveHMM’
* removing ‘/Users/david_kahle/Dropbox/dev/ggmap/ggmap/revdep/checks.noindex/moveHMM/old/moveHMM.Rcheck/moveHMM’


```
