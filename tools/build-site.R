#!/usr/bin/env Rscript
# tools/build-site.R
#
# Renders `packages/pyport/vignettes/pyport.Rmd` and
# `packages/pyrt/vignettes/pyrt.Rmd` into `docs/site/vignette-pyport.html` and
# `docs/site/vignette-pyrt.html` with the project navigation header and hex
# sticker logos, and copies SVG logos into `docs/site/assets/`.

suppressPackageStartupMessages({
  pkgload::load_all("packages/pyrt", quiet = TRUE)
  pkgload::load_all("packages/pyport", quiet = TRUE)
})

dir.create("docs/site/assets", recursive = TRUE, showWarnings = FALSE)
file.copy("packages/pyport/man/figures/logo.svg", "docs/site/assets/logo-pyport.svg", overwrite = TRUE)
file.copy("packages/pyrt/man/figures/logo.svg", "docs/site/assets/logo-pyrt.svg", overwrite = TRUE)

md_to_html_body <- function(md_lines) {
  # Strip YAML frontmatter
  if (length(md_lines) >= 1L && trimws(md_lines[[1L]]) == "---") {
    end_idx <- which(trimws(md_lines[-1L]) == "---")
    if (length(end_idx) >= 1L) {
      md_lines <- md_lines[(end_idx[[1L]] + 2L):length(md_lines)]
    }
  }

  escape_html <- function(s) {
    s <- gsub("&", "&amp;", s, fixed = TRUE)
    s <- gsub("<", "&lt;", s, fixed = TRUE)
    s <- gsub(">", "&gt;", s, fixed = TRUE)
    s
  }

  format_inline <- function(s) {
    s <- escape_html(s)
    s <- gsub("`([^`]+)`", "<code>\\1</code>", s)
    s <- gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", s)
    s
  }

  out <- character()
  in_code <- FALSE
  in_list <- FALSE
  code_buf <- character()

  for (ln in md_lines) {
    if (grepl("^```", ln)) {
      if (in_list) {
        out <- c(out, "</ul>")
        in_list <- FALSE
      }
      if (!in_code) {
        in_code <- TRUE
        code_buf <- character()
      } else {
        in_code <- FALSE
        out <- c(out, paste0("<pre><code>", escape_html(paste(code_buf, collapse = "\n")), "</code></pre>"))
      }
      next
    }
    if (in_code) {
      code_buf <- c(code_buf, ln)
      next
    }
    if (grepl("^###\\s+", ln)) {
      if (in_list) { out <- c(out, "</ul>"); in_list <- FALSE }
      out <- c(out, sprintf("<h3>%s</h3>", format_inline(sub("^###\\s+", "", ln))))
    } else if (grepl("^##\\s+", ln)) {
      if (in_list) { out <- c(out, "</ul>"); in_list <- FALSE }
      out <- c(out, sprintf("<h2>%s</h2>", format_inline(sub("^##\\s+", "", ln))))
    } else if (grepl("^#\\s+", ln)) {
      if (in_list) { out <- c(out, "</ul>"); in_list <- FALSE }
      out <- c(out, sprintf("<h1>%s</h1>", format_inline(sub("^#\\s+", "", ln))))
    } else if (grepl("^[-*]\\s+", ln)) {
      if (!in_list) { out <- c(out, "<ul>"); in_list <- TRUE }
      out <- c(out, sprintf("<li>%s</li>", format_inline(sub("^[-*]\\s+", "", ln))))
    } else if (nzchar(trimws(ln))) {
      if (in_list) { out <- c(out, "</ul>"); in_list <- FALSE }
      out <- c(out, sprintf("<p>%s</p>", format_inline(ln)))
    }
  }
  if (in_list) out <- c(out, "</ul>")
  paste(out, collapse = "\n")
}

render_vignette_page <- function(rmd_path, out_html, title, logo_rel, active_tab) {
  tmp_md <- tempfile(fileext = ".md")
  on.exit(unlink(tmp_md), add = TRUE)
  knitr::knit(rmd_path, output = tmp_md, quiet = TRUE)
  md_lines <- readLines(tmp_md, warn = FALSE, encoding = "UTF-8")
  body_html <- md_to_html_body(md_lines)

  page <- sprintf(
'<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8"/>
  <meta name="viewport" content="width=device-width, initial-scale=1"/>
  <title>%s</title>
  <link rel="stylesheet" href="style.css"/>
</head>
<body>
  <nav class="navbar">
    <div class="nav-inner">
      <a class="brand" href="index.html">
        <img src="%s" alt="logo"/>
        <span>pyport &amp; pyrt</span>
      </a>
      <div class="nav-links">
        <a href="index.html">Home</a>
        <a class="%s" href="vignette-pyport.html">pyport Vignette</a>
        <a class="%s" href="vignette-pyrt.html">pyrt Vignette</a>
        <a href="reference.html">API Reference</a>
        <a href="https://github.com/lauyeehow1986-hub/r_python_converter">GitHub</a>
      </div>
    </div>
  </nav>
  <main class="container">
    <div style="display:flex;align-items:center;gap:1.5rem;margin-bottom:2rem;">
      <img src="%s" alt="hex logo" style="width:96px;height:auto;"/>
      <h1 style="margin:0;">%s</h1>
    </div>
%s
  </main>
  <footer class="footer">
    Built with <strong>pyport</strong> 0.1.0 &amp; <strong>pyrt</strong> 0.1.0 &middot; MIT License &middot; <a href="https://github.com/lauyeehow1986-hub/r_python_converter">GitHub Repository</a>
  </footer>
</body>
</html>',
    title,
    logo_rel,
    if (active_tab == "pyport") "active" else "",
    if (active_tab == "pyrt") "active" else "",
    logo_rel,
    title,
    body_html
  )

  con <- file(out_html, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(page, con = con, sep = "\n", useBytes = TRUE)
  cat("Rendered vignette:", out_html, "\n")
}

render_vignette_page(
  "packages/pyport/vignettes/pyport.Rmd",
  "docs/site/vignette-pyport.html",
  "Converting Python Packages into Standalone Native R Packages (pyport)",
  "assets/logo-pyport.svg",
  "pyport"
)

render_vignette_page(
  "packages/pyrt/vignettes/pyrt.Rmd",
  "docs/site/vignette-pyrt.html",
  "Python Runtime Semantics in Native R (pyrt)",
  "assets/logo-pyrt.svg",
  "pyrt"
)
