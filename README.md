# marinegeo.utils

R tools to support MarineGEO data management.

The package accesses the MarineGEO metadata registry (partner and site codes,
table definitions, controlled vocabularies, numeric ranges, and taxonomic
lookups) and builds a small set of utilities on top of it:

- **QA/QC** — validate a data table against the MarineGEO standard with a
  single call to `qc_run()`, or run individual checks (missing values, data
  types, controlled vocabularies, numeric ranges, row uniqueness).
- **Registry access** — look up reference tables with `utl_mg_get_registry()`
  and list available tables with `utl_mg_list_tables()`.
- **Taxonomy and functional groups** — attach taxonomic classifications and
  functional group labels with `utl_mg_get_taxonomic_classifications()` and
  `utl_mg_assign_functional_groups()`.
- **Data wrangling helpers** — backfill derived cover and density fields,
  generate stable row UUIDs, and order columns to the standard.
- **Visualization** — quick annual time series plots via
  `viz_mg_timeseries_annual()`.
- **Table Wrangler** — a bundled Shiny app for inspecting and reshaping
  tabular data without writing code: `shiny_launch_table_wrangler()`.

## Installation

The package is not on CRAN; install it from GitHub. Use `pak`:

```r
# install.packages("pak")
pak::pak("MarineGEO/marinegeo-R-utilities")
```

Note that the repository is named `marinegeo-R-utilities`, but the installed
package is loaded as `marinegeo.utils`:

```r
library(marinegeo.utils)
```

To install the in-development version instead, point at the `develop` branch:

```r
pak::pak("MarineGEO/marinegeo-R-utilities@develop")
```
