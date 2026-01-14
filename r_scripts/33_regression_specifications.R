# Exact-size econ-style spec (PDF + PNG), matching QMD body font

if (!requireNamespace("pdftools", quietly = TRUE)) install.packages("pdftools")
if (!requireNamespace("tinytex",  quietly = TRUE)) install.packages("tinytex")
library(pdftools)

out_dir <- "/Users/cycoldiron/Desktop/congress-fiscal-tweets/results/good_regression_tables"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Match your QMD body font size (keep as-is to preserve font size)
base_pt <- 11

# --- same setup as before (pdftools/tinytex/out_dir/base_pt = 11) ---

make_spec_png <- function(tag_num, eq_lines, notes_items, outfile_stub, dpi = 350) {
  tex_path <- file.path(out_dir, paste0(outfile_stub, ".tex"))
  pdf_path <- file.path(out_dir, paste0(outfile_stub, ".pdf"))
  png_path <- file.path(out_dir, paste0(outfile_stub, ".png"))
  
  latex <- c(
    sprintf("\\documentclass[preview,border=14pt,%spt,fleqn,leqno]{standalone}", base_pt),
    "\\usepackage{amsmath,amssymb}",
    "\\usepackage{newtxtext,newtxmath}",
    "\\usepackage{enumitem}",
    "\\setlength{\\mathindent}{0pt}",
    "\\setlength{\\abovedisplayskip}{4pt}",
    "\\setlength{\\belowdisplayskip}{4pt}",
    "\\allowdisplaybreaks",
    "\\begin{document}",
    "\\begin{minipage}{0.92\\linewidth}",
    "\\begin{equation}",
    paste0("\\tag{", tag_num, "}"),
    "\\begin{aligned}",
    paste(eq_lines, collapse = " \\\\ "),
    "\\end{aligned}",
    "\\end{equation}",
    "\\vspace{0.25em}",
    "{\\itshape",
    "\\begin{itemize}[leftmargin=*,nosep]",
    paste0("\\item ", notes_items, collapse = "\n"),
    "\\end{itemize}",
    "}",
    "\\end{minipage}",
    "\\end{document}"
  )
  
  writeLines(latex, tex_path)
  if (!tinytex::is_tinytex()) stop("LaTeX (tinytex) not found. Run tinytex::install_tinytex() once.")
  ok <- TRUE
  tryCatch({ tinytex::latexmk(tex_path, engine = "xelatex") }, error = function(e) ok <<- FALSE)
  if (!ok || !file.exists(pdf_path)) { tinytex::pdflatex(tex_path); tinytex::pdflatex(tex_path) }
  if (!file.exists(pdf_path)) stop("PDF not created: ", pdf_path)
  pdftools::pdf_convert(pdf_path, filenames = png_path, dpi = dpi)
  c(pdf = pdf_path, png = png_path)
}

# (1) Legislative windows
eq1 <- c(
  "\\operatorname{logit}(Y_{it}) = \\alpha_i + \\gamma_t",
  "+\\; \\beta_{1}(\\text{GOP}_i\\!\\times\\!\\text{LegWin}_t)",
  "+\\; \\beta_{2}(\\text{GOP}_i\\!\\times\\!\\text{COVID}_t)",
  "+\\; \\beta_{3}(\\text{GOP}_i\\!\\times\\!\\text{IRA/CHIPS}_t)",
  "+\\; \\beta_{4}(\\text{GOP}_i\\!\\times\\!\\text{Bill partisanship}_t)",
  "+\\; \\beta_{5}(\\text{GOP}_i\\!\\times\\!\\text{Fiscal magnitude}_t)",
  "+\\; \\varepsilon_{it}"
)
notes1 <- c(
  "Baseline: Democrats; outside legislative windows.",
  "Fixed effects: member $(\\alpha_i)$ and calendar month $(\\gamma_t)$; SEs two-way clustered by member and month.",
  "Outcome: member–month logit of deficit-tweet share.",
  "Bill partisanship (z) and fiscal magnitude (z) are standardized month-level measures."
)

# (2) Presidency × legislative windows
eq2 <- c(
  "\\operatorname{logit}(Y_{it}) = \\alpha_i + \\gamma_t",
  "+\\; \\beta_{1}(\\text{GOP}_i\\!\\times\\!\\text{LegWin}_t)",
  "+\\; \\beta_{2}(\\text{GOP}_i\\!\\times\\!\\text{Minority presidency}_t)",
  "+\\; \\beta_{3}(\\text{GOP}_i\\!\\times\\!\\text{LegWin}_t\\!\\times\\!\\text{Minority presidency}_t)",
  "+\\; \\beta_{4}(\\text{GOP}_i\\!\\times\\!\\text{COVID}_t)",
  "+\\; \\beta_{5}(\\text{GOP}_i\\!\\times\\!\\text{IRA/CHIPS}_t)",
  "+\\; \\beta_{6}(\\text{GOP}_i\\!\\times\\!\\text{Bill partisanship}_t)",
  "+\\; \\beta_{7}(\\text{GOP}_i\\!\\times\\!\\text{Fiscal magnitude}_t)",
  "+\\; \\varepsilon_{it}"
)
notes2 <- c(
  "Coefficients are GOP–Dem gaps; the three-way term is the shift in the legislative-window gap under minority presidency.",
  "Fixed effects: member and month; SEs two-way clustered by member and month.",
  "Standardized covariates: Bill partisanship (z), Fiscal magnitude (z)."
)

# (3) Government control combos
eq3 <- c(
  "\\operatorname{logit}(Y_{it}) = \\alpha_i + \\gamma_t",
  "+\\; \\beta_{1}(\\text{GOP}_i\\!\\times\\!\\text{LegWin}_t\\,\\text{(GOP trifecta)})",
  "+\\; \\beta_{2}(\\text{GOP}_i\\!\\times\\!\\text{LegWin}_t\\!\\times\\!\\text{Dem trifecta})",
  "+\\; \\beta_{3}(\\text{GOP}_i\\!\\times\\!\\text{LegWin}_t\\!\\times\\!\\text{GOP Senate+Pres})",
  "+\\; \\beta_{4}(\\text{GOP}_i\\!\\times\\!\\text{COVID}_t)",
  "+\\; \\beta_{5}(\\text{GOP}_i\\!\\times\\!\\text{IRA/CHIPS}_t)",
  "+\\; \\beta_{6}(\\text{GOP}_i\\!\\times\\!\\text{Bill partisanship}_t)",
  "+\\; \\beta_{7}(\\text{GOP}_i\\!\\times\\!\\text{Fiscal magnitude}_t)",
  "+\\; \\varepsilon_{it}"
)
notes3 <- c(
  "Baseline: GOP–Dem gap during legislative windows under a GOP trifecta; rows show changes under Dem trifecta or GOP Senate+Presidency.",
  "Fixed effects: member and month; SEs two-way clustered by member and month.",
  "Standardized covariates: Bill partisanship (z), Fiscal magnitude (z)."
)

p1 <- make_spec_png("1", eq1, notes1, "legislative-windows_spec")
p2 <- make_spec_png("2", eq2, notes2, "majority-context-presidency_spec")
p3 <- make_spec_png("3", eq3, notes3, "majority-context-chamber-presidency_spec")

