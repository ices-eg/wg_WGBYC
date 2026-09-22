#function to print directory contains into the readme
printdir<-function(dir_path,file0="README.md"){
# short script to print data directory structure in the readme.md
print_dir_tree <- function(path, prefix = "") {
  files <- list.files(path, include.dirs = TRUE, full.names = FALSE)
  files <- files[order(files)] # Sort for consistent output
#
  for (i in seq_along(files)) {
    full_path <- file.path(path, files[i])
    is_last <- i == length(files)
#
    # Print the current item
    if (file.info(full_path)$isdir) {
      if (is_last) {
        cat(prefix, "└── 📁 ", files[i], "\n", sep = "")
        new_prefix <- paste0(prefix, "    ")
      } else {
        cat(prefix, "├── 📁 ", files[i], "\n", sep = "")
        new_prefix <- paste0(prefix, "│   ")
      }
      # Recursively print subdirectory
      print_dir_tree(full_path, new_prefix)
    } else {
      if (is_last) {
        cat(prefix, "└── 📄 ", files[i], "\n", sep = "")
      } else {
        cat(prefix, "├── 📄 ", files[i], "\n", sep = "")
      }
    }
  }
}

# Capture the output
output <- capture.output(print_dir_tree(dir_path))
#remove script
output<-output[!grepl("printdir",output)]

# Add a header
header <- c(
  sprintf("Directory tree for %s", dir_path),
  sprintf("Generated on: %s", Sys.Date()),
  "==============================",
  ""
)

# Combine header and output
full_output <- c(header, "```bash", output,"```")

# Write to a text file
writeLines(full_output, file0,sep="\n")
}

