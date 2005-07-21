filename <- system.file("data-ex/exampleVComments.R", package="R.utils")
lines <- readLines(filename)

cat("Code before preprocessing:\n")
displayCode(lines)

lines <- VComments$compile(lines)

cat("Code after preprocessing:\n")
displayCode(lines)
