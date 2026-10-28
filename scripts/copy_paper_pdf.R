# Quarto post-render step: keep a copy of the rendered paper PDF in paper/ so
# it can be committed and linked from the README. Skipped for HTML-only renders.
pdf <- file.path("_site", "paper", "paper.pdf")
if (file.exists(pdf)) invisible(file.copy(pdf, file.path("paper", "paper.pdf"), overwrite = TRUE))
