# R checks the version floor of an Imports entry at load time only for a
# package that NAMESPACE imports from. The imports of arraydesign and
# detfield make the floors in DESCRIPTION stop an install or a load on an
# older arraydesign or detfield.
#' @keywords internal
#' @importFrom rlang %||% .data
#' @importFrom arraydesign place_receivers
#' @importFrom detfield positionr_ring_colours
"_PACKAGE"

# promises::future_promise() needs 'future', and shiny::includeMarkdown()
# needs 'markdown', but the package calls neither by name. Naming them here
# keeps R CMD check from reporting two Imports as unused.
needed_at_run_time <- function() list(future::plan, markdown::mark_html)
