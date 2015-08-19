# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.2.2 (2015-08-14) |
|system   |x86_64, darwin13.4.0         |
|ui       |RStudio (0.99.441)           |
|language |(EN)                         |
|collate  |en_US.UTF-8                  |
|tz       |America/Chicago              |

## Packages

|package     |*  |version |date       |source         |
|:-----------|:--|:-------|:----------|:--------------|
|geosphere   |   |1.4-3   |2015-07-02 |CRAN (R 3.2.0) |
|jpeg        |   |0.1-8   |2014-01-23 |CRAN (R 3.2.0) |
|mapproj     |   |1.2-4   |2015-08-03 |CRAN (R 3.2.0) |
|png         |   |0.1-7   |2013-12-03 |CRAN (R 3.2.0) |
|RgoogleMaps |   |1.2.0.7 |2015-01-21 |CRAN (R 3.2.0) |

# Check results
9 checked out of 9 dependencies 

## clifro (2.4-0)
Maintainer: Blake Seers <blake.seers@gmail.com>  
Bug reports: https://github.com/ropensci/clifro/issues

```
checking R code for possible problems ... NOTE
speed_plot,cfWind-missing : .local: no visible binding for global
  variable ‘speed’
speed_plot,cfWind-missing : .local: no visible binding for global
  variable ‘spd.sd’
```
```
DONE
Status: 1 NOTE
```

## move (1.5.514)
Maintainer: Bart Kranstauber <bart.kranstauber@uni-konstanz.de>

__OK__

## OutbreakTools (0.1-13)
Maintainer: Thibaut Jombart <t.jombart@imperial.ac.uk>

__OK__

## RAM (1.2.1)
Maintainer: Wen Chen <Wen.Chen@agr.gc.ca>

__OK__

## sidier (3.0.1)
Maintainer: A.J. Muñoz-Pajares <ajesusmp@ugr.es>

__OK__

## spoccutils (0.1.0)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropensci/spoccutils/issues

```
checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  8: gist_POST(paste0(ghbase(), "/gists"), gist_auth(), ghead(), body, ...)
  9: POST(url, auth, headers, body = body, encode = "json", ...)
  10: request_build("POST", hu$url, body_config(body, encode), config, ...)
  11: gist_auth()
  12: stop("In non-interactive environments, please set GITHUB_PAT env to a GitHub", " access token (https://help.github.com/articles/creating-an-access-token-for-command-line-use)", 
         call. = FALSE)
  
  testthat results ================================================================
  OK: 6 SKIPPED: 0 FAILED: 1
  1. Error: map_gist works as expected 
  
  Error: testthat unit tests failed
  Execution halted
```
```
DONE
Status: 1 ERROR
```

## SWMPr (2.1.0)
Maintainer: Marcus W. Beck <mbafs2012@gmail.com>  
Bug reports: https://github.com/fawda123/SWMPr/issues

__OK__

## vmsbase (2.0)
Maintainer: Lorenzo D'Andrea <support@vmsbase.org>

__OK__

## weatherr (0.1)
Maintainer: Stan Yip <stanyip101@gmail.com>

__OK__

