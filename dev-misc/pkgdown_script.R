# Customize pkgdown by tweaking
# Make site
pkgdown::build_site()

# Add logo and tools folder (to match github readme)
if (!dir.exists("docs/tools")) {
  dir.create("docs/tools")
}
file.copy("tools/logo.png", "docs/tools/logo.png", overwrite = TRUE)
file.copy("tools/pkgdownshield.svg", "docs/tools/pkgdownshield.svg", overwrite = TRUE)

# Add logo to header
all_html_files <- list.files("docs/", pattern = ".html$", recursive = TRUE)
purrr::walk(all_html_files, function(x) {
  text <- readr::read_lines(file.path("docs", x))
  depth <- stringr::str_count(x, "/")
  img_loc <- paste0(paste(rep("../", depth), collapse = ""), "tools/logo.png")
  navbar_brand_line <- which(stringr::str_detect(text, stringr::coll("<a class=\"navbar-brand\"")))
  if (length(navbar_brand_line) == 0) return(TRUE)
  text[navbar_brand_line] <- paste(
    text[navbar_brand_line],
    paste0("<img src=\"", img_loc, "\" height=\"50\" width=\"50\" align=\"left\" display=\"block\" style=\"margin: 5px 20px 5px 0px\">"),
    sep = "\n"
  )
  readr::write_lines(text, file.path("docs", x))
})
