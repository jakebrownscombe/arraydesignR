# The README is public text. The tests that read it take it from the source
# tree, so they skip in R CMD check, where the README is not beside the tests.

# A README link that 404s on GitHub or on the site. GitHub reads the README
# at the repository root and pkgdown at the site root. articles/ and
# reference/ exist only on the site; R/, DESCRIPTION and the other source
# files only in the repository. pkgdown turns a link to NEWS.md into
# NEWS.html, which it does not build. It rewrites a man/ path in a src or an
# image only, so a figure from man/ resolves in both places. The rule: a
# link, a reference definition or an href is a full URL or an anchor (#),
# and an image or a src is a full URL or a path under man/. That is stricter
# than pkgdown for a root .md file it builds as a page, such as LICENSE.md;
# a full URL reaches that page too. The check reads a Markdown link, a
# reference definition, an href, a Markdown image and a src. Spaces or an
# angle bracket before the target, spaces round =, quotes or none, and upper
# case change none of this. The lines are read as one text, so link text
# across two lines or with brackets inside it is read as one link.
lead <- "\\s*<?\\s*"
url <- "[a-z][a-z0-9+.-]*:|//"
# The first character of a target; a space, a bracket or a quote is markup.
target <- "[^\\s)<>\"']"
image_ok <- paste0("(?!", url, "|(\\./)?man/)")
link_ok <- paste0("(?!", url, "|#)")
md_image <- "!\\[[^\\]]*\\]\\([^)]*\\)"
broken_image <- paste0(
  "(?i)!\\[[^\\]]*\\]\\(", lead, image_ok, target, "|",
  "src\\s*=\\s*[\"']?", lead, image_ok, target
)
broken_link <- paste0(
  "(?im)\\]\\(", lead, link_ok, target, "|",
  "^\\s*\\[[^\\]]+\\]:", lead, link_ok, target, "|",
  "href\\s*=\\s*[\"']?", lead, link_ok, target
)
# TRUE when the text, given as lines, holds a link or a figure that 404s.
# Images are checked, then cut, so the link check never reads an image path.
is_broken_link <- function(x) {
  text <- paste(x, collapse = "\n")
  grepl(broken_image, text, perl = TRUE) ||
    grepl(broken_link, gsub(md_image, "", text, perl = TRUE), perl = TRUE)
}

readme_section <- function(path, heading) {
  lines <- readLines(path, warn = FALSE)
  start <- grep(paste0("^## ", heading, "$"), lines)
  if (length(start) != 1) return(NA_character_)
  rest <- lines[-seq_len(start)]
  end <- grep("^## ", rest)
  body <- if (length(end)) rest[seq_len(end[1] - 1)] else rest
  out <- paste(trimws(body[nzchar(trimws(body))]), collapse = " ")
  # A knitted README carries pandoc's curly apostrophe; a hand-written one
  # the straight one. The words are what is pinned.
  out <- gsub("\xe2\x80\x99", "'", out, useBytes = TRUE)
  gsub("\xe2\x80\x98", "'", out, useBytes = TRUE)
}

test_that("the README's 'How the code was written' section is the same in every package", {
  own <- testthat::test_path("..", "..", "README.md")
  skip_if(!file.exists(own), "README.md of the source tree is not beside the tests")
  text <- readme_section(own, "How the code was written")
  expect_identical(text, paste(
    "The code and this documentation were written with an AI coding assistant,",
    "Claude Code v2.1.145."
  ))
  siblings <- file.path(dirname(normalizePath(testthat::test_path("..", ".."))),
                        c("detfield", "detqc", "arraydesign"), "README.md")
  siblings <- siblings[file.exists(siblings)]
  skip_if(!length(siblings), "no sibling package beside this one")
  for (f in siblings) expect_identical(readme_section(f, "How the code was written"), text)
})

test_that("the README's Cite section gives the entries of inst/CITATION, in order", {
  # citation() gives the positionR entry first, then the package's own, and
  # the README shows both in that order. The section is written by hand, so
  # it is compared here with the citation file read against this DESCRIPTION.
  # Markdown marks are dropped from both sides.
  own <- testthat::test_path("..", "..", "README.md")
  skip_if(!file.exists(own), "README.md of the source tree is not beside the tests")
  root <- testthat::test_path("..", "..")
  meta <- as.list(read.dcf(file.path(root, "DESCRIPTION"))[1, ])
  cites <- utils::readCitationFile(file.path(root, "inst", "CITATION"), meta = meta)
  expect_length(cites, 2)
  expect_match(cites[[1]]$title, "^positionR: ")
  expect_match(cites[[2]]$title, "^arraydesignR: ")
  flat <- function(x) gsub("\\s+", " ", trimws(gsub("[*_<>]", "", x)))
  expect_identical(flat(readme_section(own, "Cite")),
                   flat(paste(format(cites, style = "text"), collapse = " ")))
})

test_that("the README installs the suite from GitHub in dependency order", {
  # The README installs from GitHub the suite packages the app needs, each
  # after the suite packages it imports. Remotes names exactly the suite
  # packages that DESCRIPTION declares, as jakebrownscombe/<package>, so a
  # GitHub install resolves them. detqc is not installed: the app does not use it.
  own <- testthat::test_path("..", "..", "README.md")
  skip_if(!file.exists(own), "README.md of the source tree is not beside the tests")
  text <- readme_section(own, "Install")
  expect_match(text, "remotes::install_github(", fixed = TRUE)
  expect_match(text, "arraydesignR::run_app()", fixed = TRUE)

  desc <- read.dcf(testthat::test_path("..", "..", "DESCRIPTION"))
  field <- function(name) {
    if (!name %in% colnames(desc)) return(character())
    v <- trimws(strsplit(desc[1, name], ",")[[1]])
    v[nzchar(v)]
  }
  pkgs <- function(name) trimws(sub("\\(.*$", "", field(name)))
  # The suite is what the README installs one by one, read from its own text.
  listed <- regmatches(text, gregexpr("\"jakebrownscombe/[A-Za-z0-9.]+\"", text))[[1]]
  listed <- setdiff(gsub("\"|jakebrownscombe/", "", listed), "positionR")
  expect_identical(tail(listed, 1), "arraydesignR")
  expect_false("detqc" %in% listed)
  declared <- intersect(c(pkgs("Imports"), pkgs("Suggests")), listed)
  expect_true(length(declared) > 0)
  # Each package the app imports is installed before the app.
  expect_true(all(match(declared, listed) < match("arraydesignR", listed)))
  # Remotes names exactly the declared suite packages, under jakebrownscombe.
  remotes <- field("Remotes")
  expect_true(all(startsWith(remotes, "jakebrownscombe/")))
  expect_setequal(sub("^jakebrownscombe/", "", remotes), declared)
  # Where the siblings sit beside this package, each one in the README comes
  # after the suite packages it imports.
  root <- dirname(normalizePath(testthat::test_path("..", "..")))
  for (p in listed) {
    f <- file.path(root, p, "DESCRIPTION")
    if (!file.exists(f)) next
    imp <- read.dcf(f, fields = "Imports")[1, 1]
    imp <- if (is.na(imp)) character() else trimws(sub("\\(.*$", "", strsplit(imp, ",")[[1]]))
    before <- intersect(imp, listed)
    expect_true(all(match(before, listed) < match(p, listed)), info = p)
  }
})

test_that("the README's links and figures resolve on GitHub as well as on the site", {
  # GitHub reads the README at the repository root, where articles/ and
  # reference/ do not exist. Articles are linked by their site URL, and
  # figures from man/figures, which pkgdown rewrites for the site.
  own <- testthat::test_path("..", "..", "README.md")
  skip_if(!file.exists(own), "README.md of the source tree is not beside the tests")
  text <- readLines(own, warn = FALSE)
  expect_false(is_broken_link(text))
  figs <- regmatches(text, regexpr("man/figures/[A-Za-z0-9_.-]+", text))
  expect_gt(length(figs), 0)
  root <- testthat::test_path("..", "..")
  for (f in figs) expect_true(file.exists(file.path(root, f)), info = f)
})

test_that("the README link check catches each form that 404s and passes the ones that resolve", {
  broken <- c("[Get started](articles/arraydesignR.html)", "[run_app](reference/run_app.html)",
              "<a href=\"reference/run_app.html\">run_app</a>",
              "<img src=\"reference/figures/app_map.png\" />",
              "<a href=\"man/figures/app_map.png\"><img src=\"man/figures/app_map.png\" /></a>",
              "<a href=\"articles/arraydesignR.html\">Get started</a>",
              "<a href='articles/arraydesignR.html'>Get started</a>",
              "[Get started](./articles/arraydesignR.html)",
              "<img src='./reference/figures/app_map.png' />",
              "[gs]: articles/arraydesignR.html",
              "<a href = \"articles/arraydesignR.html\">Get started</a>",
              "<A HREF=\"articles/arraydesignR.html\">Get started</A>",
              "[Get started](<articles/arraydesignR.html>)",
              "<a href=articles/arraydesignR.html>Get started</a>",
              "[the map](man/figures/app_map.png)",
              "[Get started](/articles/arraydesignR.html)",
              "<a href=\"/reference/run_app.html\">run_app</a>",
              "[Get started]( articles/arraydesignR.html)",
              "[Get started](../articles/arraydesignR.html)",
              "[run_app](R/run_app.R)",
              "[ref]: ../reference/run_app.html",
              "<img src=\"/reference/figures/app_map.png\" />",
              "[run the\napp](R/run_app.R)", "[see [run_app]](R/run_app.R)",
              "![the map](vignettes/app_map.png)",
              "<img src=\"articles/arraydesignR_files/map.png\" />",
              "[x](DESCRIPTION)", "[x](NEWS.md)", "[n]: NEWS.md",
              "<a href=\"DESCRIPTION\">x</a>", "![logo](logo.png)", "<img src=\"/man/a.png\" />")
  for (b in broken) expect_true(is_broken_link(b), info = b)
  # Two lines read as one text, as the README is read.
  expect_true(is_broken_link(c("See [the", "app](R/run_app.R).")))
  fine <- c("<img src=\"man/figures/logo.png\" align=\"right\" height=\"138\" alt=\"\" />",
            "![Map tab](man/figures/app_map.png)", "<img src='man/figures/app_map.png' />",
            paste0("[Get started](https://jakebrownscombe.github.io/arraydesignR/articles/",
                   "arraydesignR.html)"),
            "[Shiny](https://shiny.posit.co/r/reference/shiny/)",
            "[source](https://github.com/jakebrownscombe/arraydesignR/blob/main/R/run_app.R)",
            "[![Map](man/figures/app_map.png)](https://jakebrownscombe.github.io/arraydesignR/)",
            "[Cite](#cite)", "[Shiny](<https://shiny.posit.co/>)",
            "<a href=\"https://jakebrownscombe.github.io/arraydesignR/\">site</a>",
            "<img src='./man/figures/app_map.png' />")
  for (f in fine) expect_false(is_broken_link(f), info = f)
  expect_false(is_broken_link(c("![Map", "tab](man/figures/app_map.png)")))
})
