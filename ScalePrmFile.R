# script for scaling up adam simulations (or scaling down)
# the function takes two things as arguments, the factor by which to scale up the prm file
# and the folder in which to operate, note that the function searches in all folders "downstream"
# of the folder which is chosen

# install.packages("stringr")
# install.packages("stringi")
# library(stringr) # < ------ NEEDED 
# library(stringi)   # < ------ NEEDED
ScaleThatThing <- function (root, scaling.factor) {
  prm.files <-
    list.files(path = root,
               pattern = "*.prm$",
               recursive = T)
  for (i in 1:length(prm.files)) {
    path <- paste(root, prm.files[i], sep = "")
    prm.file <- readLines(path)
    pos <-
      grep (pattern = "selection_scheme=", x = prm.file) # finds the line for selection schemes
    n.sel.lines <-
      as.numeric(unlist(str_split(
        string = prm.file[grep(pattern = "selection_groups", x = prm.file)], pattern = "="
      ))[2]) # finds the number of selection lines
    selection.lines <-
      as.matrix(prm.file[(pos + 1):(pos + n.sel.lines)]) # extracts the lines into a seperate matrix
    selection.lines <-
      apply(t(
        apply(
          selection.lines,
          1,
          FUN = ReplaceNumbers,
          scaling.factor = 10
        )
      ), 1, FUN = stri_flatten, collapse = " ") # replaces the numbers
    prm.file <-
      prm.file[-((pos + 1):(pos + n.sel.lines))]  # deletes the old lines
    prm.file <-
      append(prm.file, selection.lines, after = pos) # puts in the nice new lines
    writeLines(prm.file, path) # writes the edited file
  }
}


ReplaceNumbers <- function (selection.lines , scaling.factor) {
  x <-
    unlist(str_split(string = str_trim(selection.lines[1]), pattern = "(\\s+)"))
  x[15] <-
    as.numeric(unlist(str_split(
      string = str_trim(selection.lines[1]), pattern = "(\\s+)"
    ))[15]) * scaling.factor
  
  return(x)
}