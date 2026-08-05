# Baseline-scenario LAG accounting (methodology Section 8.1).
#
# NOTE: equation numbers reference the FAF v2.0 working draft dated
# 2026-07-31 ("Improved Forest Management for Fire-Adapted Forests",
# M0159). The methodology is still being finalized; @details tags cite
# provisional equation numbers and MUST be reconciled against the frozen
# document before release. See context/DECISIONS.md.

#' Raw annual change in baseline live aboveground carbon
#'
#' Computes the provisional (raw) annual change in baseline live aboveground
#' carbon density (LAG) for one pixel as the first difference of consecutive
#' wall-to-wall remote-sensing observations. This is the entry point to the
#' baseline accounting: the raw change is later tested and, where warranted,
#' corrected to an *accepted* change (`ΔLAG*`) by the Appendix II procedures
#' before any carbon statement is closed.
#'
#' @details
#' Implements **Eq (1)**: the raw annual baseline LAG change,
#' \deqn{\Delta LAG^{RS}_{bsl,i,t} = LAG^{RS}_{bsl,i,t} - LAG^{RS}_{bsl,i,t-1}.}
#' Sign convention: negative values are losses, positive values are gains.
#' The value returned here is provisional (superscript RS); it carries no
#' pathway attribution and has not passed the accepted-value interface.
#'
#' Equation number is provisional (FAF v2.0 working draft, 2026-07-31) and
#' must be re-verified against the frozen methodology.
#'
#' @param lag_rs Numeric vector of raw remote-sensing baseline LAG
#'   observations for a single pixel, ordered by annual step from the
#'   validation anchor (t = 0) forward; units Mg C ha^-1.
#'
#' @return Numeric vector of raw annual changes, length
#'   `length(lag_rs) - 1`, aligned to steps `t = 1 .. T`; units Mg C ha^-1.
#'
#' @examples
#' # Eleven annual observations (t = 0..10) -> ten annual changes
#' lag <- c(120, 118, 121, 123, 122, 125, 127, 108, 110, 112, 114)
#' delta_lag_baseline(lag)
#'
#' @family baseline accounting
#' @export
delta_lag_baseline <- function(lag_rs) {
  if (!is.numeric(lag_rs)) {
    stop("`lag_rs` must be a numeric vector.", call. = FALSE)
  }
  if (length(lag_rs) < 2L) {
    stop("`lag_rs` must have at least two observations to difference.",
         call. = FALSE)
  }
  diff(lag_rs)
}
