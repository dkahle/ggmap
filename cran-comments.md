## Submission summary

This is a resubmission.

----

This is a small update that fixes some minor bugs in the check and updates some geocoding features. This should mainly only affect the internal operation of the package.  I notified all reverse dependencies authors on July 31, giving them two weeks to fix any problems; it has now been nearly three weeks with no responses.

## Test environments
* local OS X install, R 3.2.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs. 

## Downstream dependencies
I ran `R CMD check` on all 9 reverse dependencies (https://github.com/dkahle/ggmap/blob/master/revdep/summary.md). 

None appeared to fail due to changes in ggmap.
