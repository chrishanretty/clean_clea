#' Cleaned Constituency-Level Elections Archive
#'
#' A cleaned version of Release 18 of the Constituency-Level Elections Archive
#' lower-chamber dataset. Election-specific corrections are documented in the
#' package repository under `data-raw/corrections/`.
#'
#' @details
#' The maintainer pipeline verifies the pinned source checksum before applying
#' 610 election-specific correction scripts and tracked manual patches. These
#' corrections can replace values, restructure results, add documented missing
#' rows, or remove elections and rows whose source data cannot be reconciled.
#'
#' @section Known limitations:
#' This dataset preserves CLEA sentinel values and 888 duplicate rows where no
#' audited correction has been made. It inherits the coverage and coding
#' limitations of CLEA and the cited correction sources.
#'
#' @format A tibble with 1,296,813 rows and 33 variables:
#' \describe{
#'   \item{release}{CLEA release identifier.}
#'   \item{id}{CLEA election identifier.}
#'   \item{rg}{CLEA region code.}
#'   \item{ctr_n}{Country or territory name.}
#'   \item{ctr}{Country or territory numeric code.}
#'   \item{yr}{Election year.}
#'   \item{mn}{Election month.}
#'   \item{sub}{Subnational election identifier, where applicable.}
#'   \item{cst_n}{Constituency name.}
#'   \item{cst}{Constituency identifier.}
#'   \item{mag}{Constituency district magnitude.}
#'   \item{pty_n}{Party name.}
#'   \item{pty}{Party identifier.}
#'   \item{can}{Candidate name.}
#'   \item{pev1}{First-election-period eligible voters.}
#'   \item{vot1}{First-election-period voters.}
#'   \item{vv1}{First-election-period valid votes.}
#'   \item{ivv1}{First-election-period invalid votes.}
#'   \item{to1}{First-election-period turnout.}
#'   \item{cv1}{First-election-period candidate votes.}
#'   \item{cvs1}{First-election-period candidate vote share.}
#'   \item{pv1}{First-election-period party votes.}
#'   \item{pvs1}{First-election-period party vote share.}
#'   \item{pev2}{Second-election-period eligible voters.}
#'   \item{vot2}{Second-election-period voters.}
#'   \item{vv2}{Second-election-period valid votes.}
#'   \item{ivv2}{Second-election-period invalid votes.}
#'   \item{to2}{Second-election-period turnout.}
#'   \item{cv2}{Second-election-period candidate votes.}
#'   \item{cvs2}{Second-election-period candidate vote share.}
#'   \item{pv2}{Second-election-period party votes.}
#'   \item{pvs2}{Second-election-period party vote share.}
#'   \item{seat}{Seats won.}
#' }
#' @source
#' [CLEA Lower Chamber Elections Archive, Release 18 (October 15,
#' 2025)](https://electiondataarchive.org/)
#' @references
#' Kollman, K., Hicken, A., Caramani, D., Backer, D. A., and Lublin, D.
#' Constituency-Level Elections Archive.
"clean_clea"

#' Elections Conducted Under Simple Electoral Systems
#'
#' An analysis-ready subset of [clean_clea] containing elections classified as
#' simple electoral systems using V-Dem classifications and documented manual
#' corrections. Vote and seat shares and district-level summary statistics are
#' included.
#'
#' @details
#' The classifier combines V-Dem with tracked manual classifications.
#' `simple_systems` excludes elections before 1900, systems not classified as
#' simple, the United States and Panama, and elections that retain missing
#' votes, missing seats, aggregate-party codes, or inconsistent vote and seat
#' rankings. Uncontested results are assigned one analytical vote before shares
#' and summary statistics are calculated.
#'
#' @section Known limitations:
#' The data inherit the coverage and coding limitations of CLEA, V-Dem, and the
#' tracked manual sources. Legal thresholds and complex allocation rules may
#' not be fully represented by district-level summary statistics.
#'
#' @format A tibble with 280,569 rows and 31 variables:
#' \describe{
#'   \item{id}{CLEA election identifier.}
#'   \item{iso3}{Three-letter country or territory code.}
#'   \item{country}{Country or territory name.}
#'   \item{subregion}{United Nations geographic subregion.}
#'   \item{region}{United Nations geographic region.}
#'   \item{yr}{Election year.}
#'   \item{mn}{Election month.}
#'   \item{cst_n}{Constituency name.}
#'   \item{cst}{Constituency identifier.}
#'   \item{m}{District magnitude.}
#'   \item{pty_n}{Party name.}
#'   \item{pty}{Party identifier.}
#'   \item{can}{Candidate name.}
#'   \item{c}{Candidate votes in the source data.}
#'   \item{p}{Party votes in the source data.}
#'   \item{v}{Vote count used for analysis.}
#'   \item{s}{Seats won.}
#'   \item{pv}{District vote share.}
#'   \item{ps}{District seat share.}
#'   \item{uncontested}{Whether the source result was uncontested.}
#'   \item{electoral_system}{V-Dem electoral-system classification.}
#'   \item{threshold}{Whether a legal electoral threshold is recorded.}
#'   \item{nv0}{Actual number of vote-winning parties or candidates.}
#'   \item{ns0}{Actual number of seat-winning parties or candidates.}
#'   \item{nv2}{Effective number of vote-winning parties or candidates.}
#'   \item{ns2}{Effective number of seat-winning parties or candidates.}
#'   \item{d}{Gallagher disproportionality index.}
#'   \item{w}{Vote share cast for parties or candidates winning no seats.}
#'   \item{tx}{Threshold of exclusion, calculated as `1 / (m + 1)`.}
#'   \item{tr}{Threshold of representation, calculated as `1 / (m * nv2)`.}
#'   \item{tmin}{Minimum of `tx` and `tr`.}
#' }
#' @source
#' [CLEA Lower Chamber Elections Archive](https://electiondataarchive.org/)
#' and [V-Dem](https://www.v-dem.net/).
"simple_systems"
