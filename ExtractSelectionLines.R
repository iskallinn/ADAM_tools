# function to extract selection lines into a vector in R environment
# easier than pasting the fuckers into R when replacing them with replace_prm_files function

ExtractSelectionLines <-function ( pathtofile ) { 
  prm_file <- readLines(pathtofile)

  pos  <-
    grep(prm_file, pattern = "selection_scheme=") # find where the selection lines begin
  pos1  <-pos+
    grep(prm_file[pos:length(prm_file)], pattern = "/")[1]-1 # find where the selection lines begin
  selection.lines <- prm_file[pos:pos1]
  # method for detecting if the "/" operator is in the same line as the last selection line
  if (TRUE %in% ( str_detect(selection.lines, pattern = "(.*)(\\/)")) == TRUE) { 
    selection.lines[str_detect(selection.lines, pattern = "(.*)(\\/)")] <- 
      gsub(selection.lines[str_detect(selection.lines, pattern = "(.*)(\\/)")] ,pattern = "/", replacement = "")
  }
  #remove the header
    if (TRUE %in%( str_detect(selection.lines, pattern = "selection_scheme")) == TRUE)  {
  selection.lines <-   selection.lines[!str_detect(selection.lines, pattern = "selection_scheme")]
    }
  # remove the "/"
  if (TRUE %in%( str_detect(selection.lines, pattern = "/")) == TRUE)  {
    selection.lines <-   selection.lines[!str_detect(selection.lines, pattern = "/")]
  }
  selection.lines <-   selection.lines[str_detect(selection.lines, pattern = "(\\d)+")]
  return(selection.lines)
  }