# Maintainer data workflow

Run the full data build from the package root:

```sh
Rscript -e 'renv::restore()'
Rscript data-raw/build.R
```

The build requires the official CLEA lower-chamber source file at:

```text
data-raw/raw/clea/clea_lc_20251015.RData
```

Download the source manually from the
[Constituency-Level Elections Archive](https://electiondataarchive.org/data-and-documentation/clea-lower-chamber-elections-archive/).
The source bundle is intentionally excluded from Git and from the built
package.

Expected SHA-256:

```text
a63e075a41379bb8e1cb41664f9260b166d90fb5339c83a11d7f5f4382e96b87
```

The build verifies this checksum before loading the file, applies every script
under `data-raw/corrections/`, validates the resulting datasets, and replaces
the package data files only after all validations pass.

The tracked CSV files under `data-raw/manual/` contain manually assembled
patches and classification inputs. Their provenance is recorded in
`data-raw/manual-sources.csv`.
