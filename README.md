# Allocation scoring rules for forecast evaluation

A place to collect work on forecast evaluation via allocation scoring rules and social utility in general.

## Analysis set-up

The analysis here uses a pipeline-based workflow implemented with [the `targets` package for R](https://books.ropensci.org/targets/).

1. The analysis has been confirmed to work with R v4.2.3.
1. The analysis requires the following installations (specific version requirements not enforced at the moment, but perhaps at a future time will be saved using `renv`):
  - from CRAN: `targets`, `tarchetypes`, `tidyverse`, `gh`, `crew`
  - from GitHub: `alloscore`, `distfromq`, `covidHubUtils` (along with several dependencies; see [https://github.com/reichlab/covidHubUtils](https://github.com/reichlab/covidHubUtils?tab=readme-ov-file#installation)).
  
## Running Analysis
From an R console at the project root, 
1. `source(_targets.R)`
1. `tar_make()` (this could take a while, e.g., ~20 minutes on an M2 Mac)

The objects required to knit `./alloscore_manuscript/manuscript.Rnw` should now reside in
`_targets/objects` (which is gitignored).