# Data provenance and terms

The package datasets are derived from the
[Constituency-Level Elections Archive (CLEA)](https://electiondataarchive.org/data-and-documentation/clea-lower-chamber-elections-archive/),
with documented corrections and additions from the sources cited in
`data-raw/corrections/` and `data-raw/manual-sources.csv`.

The raw CLEA distribution is not included in this repository, its Git history,
or the built package. Maintainers must obtain it directly from the official
archive.

## Cleaning and exclusions

The pinned CLEA Release 18 source is checked against the SHA-256 checksum in
`data-raw/build.R` before it is loaded. The maintainer pipeline then applies
610 election-specific scripts in controlled environments, incorporates the
tracked manual inputs, builds the simple-electoral-systems classifier, and
validates both outputs before replacing the package datasets.

The `simple_systems` dataset includes first-past-the-post, list PR,
single-non-transferable-vote, and other elections explicitly classified as
simple by the documented classifier. It excludes elections before 1900,
systems with upper tiers or other complex allocation rules, the United States
and Panama, and elections with unresolved missing votes, missing seats,
aggregate-party codes, or vote/seat rank inconsistencies.

## Known limitations

The correction scripts reflect the best sources identified by the maintainers
but do not guarantee that every source error has been identified. `clean_clea`
retains CLEA sentinel values and 888 duplicate rows where no audited correction
has been made. Classifications and derived statistics inherit limitations in
CLEA, V-Dem, and the manually assembled supporting sources.

The installed package is approximately 31 MB because both public datasets are
included deliberately. This size is expected for a data package and allows
users to load the documented datasets without downloading the undistributable
raw CLEA bundle.

The MIT licence applies to the package code. It does not independently
relicense the source data or supersede any terms attached to the underlying
sources. Users should cite CLEA and the relevant original sources when using
the derived datasets.
