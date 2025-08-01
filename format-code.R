#!/usr/bin/env Rscript

# Format all R code using styler
if (!requireNamespace("styler", quietly = TRUE)) {
  install.packages("styler")
}

cat("Formatting R files...\n")
styler::style_pkg(scope = "tokens")

# Format C++ files using clang-format
if (Sys.which("clang-format") != "") {
  cat("Formatting C++ files...\n")
  system2("clang-format", c("-i", "src/*.cpp"))
} else {
  cat("clang-format not found - skipping C++ formatting\n")
}

cat("Code formatting complete!\n")