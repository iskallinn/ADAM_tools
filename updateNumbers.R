# auxilliary function that updates selection_stage in observation namelist
# and in economic values namelist
# NOTE: this is not a general algorithm, it is specifically built for a case
# where selection lines where added and the selection stage numbers got out of sync. 
# this code will NOT work where there are fewer selection steps in the new selection lines 

UpdateNumbers <- function (root) { 
  prmfiles <-
    list.files(path = root,
               pattern = "*.prm$",
               recursive = T)
  
for (n in 1:length(prmfiles))  {
  path <- paste(root, prmfiles[n], sep = "/")
  prm_file <- readLines(path)
  pos  <-
    grep(prm_file, pattern = "selection_scheme=") # find where the selection lines begin
  pos1  <- pos +
    grep(prm_file[pos:length(prm_file)], pattern = "/")[1] - 1 # find where the selection lines begin
  
  # find the numbers of stages with observation as destiny
  realize.obs.sel.steps <-
    str_detect(string = prm_file[(pos + 1):pos1], pattern = "(')(\\s)+(0.(\\d))(\\s+)((\\d+)(\\s+)){5}(8)")
  realize.obs.sel.steps <-
    rbind(c(1:length(realize.obs.sel.steps)), realize.obs.sel.steps)
  
  realize.obs.sel.steps <- as.data.frame(t(realize.obs.sel.steps))
  realize.obs.sel.steps <-
    realize.obs.sel.steps[realize.obs.sel.steps$realize.obs.sel.steps != F , ]
  colnames(realize.obs.sel.steps) = c("selection.line", "realize.obs")
  
  pos  <-
    grep(prm_file, pattern = "MaleObservations=") + 1 # find where the selection lines begin
  
  pos1  <-
    grep(prm_file, pattern = "FemaleObservations=") # find where the selection lines begin
  obs <-  prm_file[(pos):pos1]
  obs <-
    matrix(
      data = as.numeric(unlist(str_split(
        str_trim(obs[str_detect(string = prm_file[(pos):pos1], pattern = "\\d")]), pattern = "\\s+"
      ))),
      nrow =
        as.numeric(unlist(str_split(prm_file[grep(prm_file, pattern = "nMaleObs")], pattern = "="))[2])
      ,
      ncol = 10,
      byrow = T
    )
  # obs <- obs[obs[, 2] != -9, ]
  if (FALSE %in% (is.element(obs[, 2], realize.obs.sel.steps$selection.line) == T)) {
    for (i in 1:nrow(obs)) {
      for (k in 1:nrow(realize.obs.sel.steps)) {
        if (obs[i, 2] == realize.obs.sel.steps[k, 1]|obs[i, 2] == -9) {
          break()
        } else if (obs[i, 2] != realize.obs.sel.steps[k, 1])
          obs[i, 2] <- realize.obs.sel.steps[k, 1]
      }
    }
  }
  pos  <-
    grep(prm_file, pattern = "MaleObservations=")+1 # find where the selection lines begin
  
  pos1  <-
    grep(prm_file, pattern = "FemaleObservations=")-1 # find where the selection lines begin
 prm_file <-  prm_file[-(pos:pos1)]
  prm_file <-
    append(
      x = prm_file,
      values = c(
        apply(obs, 1, paste, collapse = " ")
      ),
      after = (pos-1)
    )
  # browser()
  # female observations
  pos  <-
    grep(prm_file, pattern = "FemaleObservations=") + 1 # find where the selection lines begin
  
  pos1  <-
    grep(prm_file[pos:length(prm_file)], pattern = "/")[1]+pos # find where the selection lines begin
  obs <-  prm_file[(pos):pos1]
  obs <-
    matrix(
      data = as.numeric(unlist(str_split(
        str_trim(obs[str_detect(string = obs, pattern = "\\d")]), pattern = "\\s+"
      ))),
      nrow =
        as.numeric(unlist(str_split(prm_file[grep(prm_file, pattern = "nFemaleObs")], pattern = "="))[2])
      ,
      ncol = 12,
      byrow = T
    )
  # obs <- obs[obs[, 2] != -9, ]
  if (FALSE %in% (is.element(obs[, 2], realize.obs.sel.steps$selection.line) == T)) {
    for (i in 1:nrow(obs)) {
      for (k in 1:nrow(realize.obs.sel.steps)) {
        if (obs[i, 2] == realize.obs.sel.steps[k, 1]|obs[i, 2] == -9) {
          break()
        } else if (obs[i, 2] != realize.obs.sel.steps[k, 1])
          obs[i, 2] <- realize.obs.sel.steps[k, 1]
      }
    }
  }
  pos  <-
    grep(prm_file, pattern = "FemaleObservations=")+1 # find where the selection lines begin
  
    pos1  <-
    grep(prm_file[pos:length(prm_file)], pattern = "/")[1]+pos  
    
  prm_file <-  prm_file[-(pos:pos1)]
  prm_file <-
    append(
      x = prm_file,
      values = c(
        apply(obs, 1, paste, collapse = " "),"/"
      ),
      after = (pos-1)
    )
  
  # module for updating economic values
  
  # read in the matrix with the selection scheme, make numbers for the lines
  pos  <-
    grep(prm_file, pattern = "selection_scheme=") # find where the selection lines begin
  pos1  <- pos +
    grep(prm_file[pos:length(prm_file)], pattern = "/")[1] - 1 # find where the selection lines begin
  selection.lines <- prm_file[pos:pos1]
  
  selection.array <-
    matrix(unlist(str_split(str_trim(selection.lines[str_detect(string = selection.lines, pattern = "(\\d)")]), pattern = "\\s+")),
           nrow = (as.numeric(unlist(str_split(prm_file[grep(prm_file, pattern = "selection_groups")],pattern="="))[2]) ),
           ncol = 26, # this is fixed, the number of elements in each line in the selection scheme
           byrow = T)
  selection.array <- cbind(seq(1,nrow(selection.array)),selection.array)
  
 browser()
  # then remove everything except polyblup
  selection.array <-  selection.array[selection.array[,13] == '\'polyblup\'',]
  # read in the economic values
  pos  <-
    grep(prm_file, pattern = "economicValueEbv=") 
  pos1  <- pos +
    grep(prm_file[pos:length(prm_file)], pattern = "/")[1] 
  economicValueEbv <- prm_file[pos:pos1]
  # make them into a matrix
  economicValueEbv <- 
    matrix(unlist(str_split(str_trim(economicValueEbv[str_detect(string = economicValueEbv, pattern = "(\\d)")]), pattern = "\\s+")),
           nrow = (as.numeric(unlist(str_split(prm_file[grep(prm_file, pattern = "nEconomicValueEbv")],pattern="="))[2]) ),
         ncol = (2+as.numeric(unlist(str_split(prm_file[grep(prm_file, pattern = "nebv")],pattern="="))[2]) ),
         byrow = T)
  # remove the old, cbind the first column from the selection.array
  
  economicValueEbv[,1] <- selection.array[,1]
  
  # remove the old lines, in with the new
  prm_file <-  prm_file[-(pos:pos1)]
  prm_file <-
    append(
      x = prm_file,
      values = c(
        paste("economicValueEbv="),
        apply(economicValueEbv, 1, paste, collapse = " "),"/"
      ),
      after = (pos-1)
    )
  
  writeLines(prm_file, path)
} # end for loop
  } # end function