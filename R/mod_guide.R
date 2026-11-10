# ---- Module: guide ----
# The guide is a markdown file in inst/guide, so it can be read and checked
# without running the app. Every number in it carries its source.

mod_guide_ui <- function(id) {
  bslib::card(
    id = shiny::NS(id, "guide"),
    bslib::card_body(guide_html())
  )
}

# Links in the guide open in a new tab, so a click never leaves the app.
guide_html <- function() {
  html <- as.character(shiny::includeMarkdown(guide_path()))
  shiny::HTML(gsub("<a href=", "<a target=\"_blank\" rel=\"noopener\" href=", html,
                   fixed = TRUE))
}

guide_path <- function() system.file("guide", "guide.md", package = "arraydesignR", mustWork = TRUE)
