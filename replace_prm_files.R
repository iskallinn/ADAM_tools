# Function for manipulating prm files over whole directory structures, subdirectories, searching for prm files
# install.packages("stringr") # <--- Need to install this
library(stringr)
#
# root <-
#   "C:/Users/au384062/Dropbox/Projects/ADAM/RegEx_practice/MBLUP/"

# function is supposed to be dynamic so it is not necessary to utilize all the arguments in the function call, only those
# the user wants to change out.
# NOTE this function ONLY WORKS IF THE MATRICES BEGIN IN THE LINE AFTER THE SOME.ADAM.MATRIX= LINE!!!
# it is possible to check for this and make this function change the prm file to be like that :)
# NOTE: if the TEV is in a seperate line from economicValueTbv= , then this function will fail miserably, since mine are all like that
# i can't be bothered to make a check for that and a different method if it is in another line, it should be relatively simple

# TODO 
# currently it is not possible to switch out an dmu observation matrix if there is no design matrix changes

# NOTE 
# replacement of selection schemes is currently not working as intended. Does work but only for cases
# where the scheme to be replaced exactly fits the one which is supposed to be changed out. 
LarissaFixMyPrmFiles <-
  function (root,
            # the highest directory in which the functions works, searches in all lower directories for prm file
            new_gmat = 0,            # gmatrix to replace
            new_designmat = 0,      # new designmatric
            new_tev = 0 ,           # new vector for true economic values
            new_ev = 0,             # new vector for economic values
            new_ebvobs = 0,         # for namelist DMU
            new_resmat = 0,         # new residual matrix (WDirectError)
            org_gmat = 0,           # gmatrix to be replaced
            org_designmat = 0,      # orginal designmatrix
            org_ebvobs = 0,         # for namelist DMU
            org_ev = 0,             # orginal economic values
            org_resmat = 0,         # orginal residuals (WDirectError)
            org_tev = 0,
            org_selLines = 0,
            new_selLines = 0)           # orginal tev
  {
    # browser()
# for some dire reason this check doesnt work anymore, something to do with working directory i think
     # if (file.exists(root) == FALSE) {
     #   stop("folder root does not exit")
     # }
    
    if ((is.matrix(new_gmat) == TRUE &
         is.matrix(new_designmat) == FALSE) == FALSE)
      
      
      if (is.matrix(new_gmat) == TRUE &
          is.matrix(org_gmat) == TRUE) {
      }
    prmfiles <-
      list.files(path = root,
                 pattern = "*.prm$",
                 recursive = T)
    log <- file(description = paste(root,"log", sep = "/"), open = "w") # open log connection
    for (i in 1:length(prmfiles)) {
      path <- paste(root, prmfiles[i], sep = "/")
      prm_file <-  readLines(path)
      writeLines(prm_file, paste(path, "bak", sep = "_")) # write backup of each file
      prm_file <- gsub(pattern="\t", replacement = " ", x=prm_file) # remove all tabs from the input file, makes life easier
    # I begin by checking if the org_gmatrix argument fits to the matrix in the i'th prm file
      # browser()
if (is.matrix(org_gmat) == TRUE) { 
  pos  <-
        grep(prm_file, pattern = "polygenicMatrix") # find where the gmatrix begins
    ############# Checks ################
      gmatrix.check <- (FALSE %in% (matrix(
        c((as.numeric(
          unlist(str_split(str_trim(prm_file[(pos + 1):(pos + nrow(org_gmat + 1))]), pattern = " "))
        ))),
        nrow = nrow(org_gmat),
        ncol = ncol(org_gmat)
      ) == org_gmat) == FALSE)
} else if (is.matrix(org_gmat) == FALSE ) { 
  gmatrix.check <- FALSE
  }  
      pos  <-
          grep(prm_file, pattern = "residualMatrix") # find where the residual matrix begins
        # tests that the resmatrix from input.prm is the same as the org_gmat from input
        # it only passes the test if all values of gmatrix are the same as from the input
if ( is.matrix(org_resmat) == TRUE ) 
  {      
  resmatrix.check <-  (FALSE %in% (matrix(
          c((as.numeric(
            unlist(str_split(str_trim(prm_file[(pos + 1):(pos + nrow(org_gmat + 1))]), pattern = " "))
          ))),
          nrow = nrow(org_resmat),
          ncol = ncol(org_resmat)
        ) == org_resmat) == FALSE)
} else if (is.matrix(org_resmat) == FALSE ) { 
  resmatrix.check <- FALSE 
  }     

  
design.check <- ( is.matrix(new_designmat) == TRUE & is.matrix(org_designmat == TRUE)) # check if the program should bother with designmat
ev.check <- (  FALSE %in% (length(new_ev) > 1  & length(org_ev) > 1) == FALSE)             
ebvobs.check <-  ( is.matrix(new_ebvobs) == TRUE & is.matrix(org_ebvobs == TRUE))
tev.check <- (  FALSE %in% (length(new_tev) > 1  & length(org_tev) > 1) == FALSE) # checks if the vectors are longer than 1
sel.lines.check <- ( is.character(new_selLines) == TRUE & is.character(org_selLines) == TRUE)
      ########### Design matrices and number of traits ##############
      if (design.check == TRUE) {
        pos  <-
          grep(prm_file, pattern = "nobs") # find where the line with the TEV is located
        nobs_org <-
          as.numeric(unlist(str_split((
            unlist(str_split(prm_file[(pos)], pattern = "="))[2]
          ), pattern = " ")))
        pos  <-
          grep(prm_file, pattern = "ntbv") # find where the line with the TEV is located
        ntbv_org <-
          as.numeric(unlist(str_split((
            unlist(str_split(prm_file[(pos)], pattern = "="))[2]
          ), pattern = " ")))
        pos  <-
          grep(prm_file, pattern = "nebv") # find where the line with the TEV is located
        
        nebv_org <-
          as.numeric(unlist(str_split((
            unlist(str_split(str_trim(prm_file[(pos)]), pattern = "="))[2]
          ), pattern = " ")))
        
        pos  <-
          grep(prm_file, pattern = "ZDirectGenetic") # find where the line with the TEV is located
        prm_dm <-
          str_trim(prm_file[pos:(pos + ntbv_org)]) # remove whitespace from beginning
        prm_dm <-
          gsub (pattern = "ZDirectGenetic=|/", replacement = NA, prm_dm)
        prm_dm <-
          matrix(c(as.numeric(unlist(
            str_split(unlist(prm_dm)[-is.na(unlist(prm_dm))], pattern = " ")
          ))),
          nrow = ntbv_org,
          ncol = nobs_org,
          byrow = T) # extract the matrix from the prm file
        
if (FALSE %in% (dim(prm_dm) == dim(org_designmat))== FALSE) { # first check if design matrix in prm file is comformable to the 
  # design mat one wants to switch out
  if (FALSE %in% (prm_dm == org_designmat) == FALSE) {
          # check if the design mat in the prm fits with the org argument
          
          if (nrow(org_designmat) == nrow(new_designmat)) {
            for (i in 1:nrow(new_designmat)) {
              # replace the values
              prm_file[pos + i] <-
                paste0(new_designmat[i, ], collapse = " ")
            }
            # WDirectError
            pos  <-
              grep(prm_file, pattern = "WDirectError=") # find where the line with the design mat starts
            for (i in 1:nrow(new_designmat)) {
              # replace the values
              prm_file[pos + i] <-
                paste0(new_designmat[i, ], collapse = " ")
            }
          } else if (nrow(org_designmat) < nrow(new_designmat)) {
            # loop for switching them out when orginal is smaller than replacement
            # need to remove line that are superfluous
            #Zdirect &
            pos  <-
              grep(prm_file, pattern = "ZDirectGenetic") # find where the line with the TEV is located
            for (i in 1:(nrow(new_designmat) - nrow(org_designmat))) {
              prm_file <-
                append(prm_file, "", pos + i) # first add an empty value at the end of the current matrix
            }
            for (i in 1:nrow(new_designmat)) {
              # replace the values
              prm_file[pos + i] <-
                paste0(new_designmat[i, ], collapse = " ")
            }
            #WDirectError
            pos  <-
              grep(prm_file, pattern = "WDirectError=") # find where the line with the design mat starts
            
            for (i in 1:(nrow(new_designmat) - nrow(org_designmat))) {
              prm_file <-
                append(prm_file, "", pos + i) # first add an empty value at the end of the current matrix
            }
            for (i in 1:nrow(new_designmat)) {
              # replace the values
              prm_file[pos + i] <-
                paste0(new_designmat[i, ], collapse = " ")
            }
            
          } else if (nrow(org_designmat) > nrow(new_designmat)) {
            # loop for switching them when replacement is smaller than orginal
            # add lines
            #Zdirect
            # browser()
            pos  <-
              grep(prm_file, pattern = "ZDirectGenetic") # find where the line with the TEV is located
            
            for (i in 1:nrow(new_designmat)) {
              prm_file[pos + i] <- paste0(new_designmat[i, ], collapse = " ")
              if ( i == nrow(new_designmat)) { 
                # browser()
                prm_file <- prm_file[-((pos+i+((nrow(org_designmat) - nrow(new_designmat)))):(pos+i+(nrow(org_designmat) - nrow(new_designmat))))]
              }
              
            }
            # for (i in 1:(nrow(org_designmat) - nrow(new_designmat))) {
            #   prm_file <-
            #     prm_file[-(pos  + nrow(new_designmat) + i)] # remove extra lines
            # }
            
            #WDirectError
            # browser()
            pos  <-
              grep(prm_file, pattern = "WDirectError") # find where the line with the design mat starts
            for (i in 1:nrow(new_designmat)) {

              prm_file[pos + i] <- paste0(new_designmat[i, ], collapse = " ")
              if ( i == nrow(new_designmat)) { 
                # browser()
                prm_file <- prm_file[-((pos+i+((nrow(org_designmat) - nrow(new_designmat)))):(pos+i+(nrow(org_designmat) - nrow(new_designmat))))]
                }
            }
            # for (i in 1:(nrow(org_designmat) - nrow(new_designmat))) {
            #   prm_file <-
            #     prm_file[-(pos + nrow(new_designmat) + i)] # remove extra lines
            # }
          }
          pos <- pos + nrow(new_designmat)
          pos1 <-
            grep(prm_file, pattern = "&ECONOMICVALUES|&OBSERVATIONCONSTRAINTS|&COMBINEDOBSERVATIONS")
          # find next namelist
          if ((TRUE %in% str_detect(prm_file[pos:pos1], pattern = "/")) == FALSE) {
            prm_file <-
              append(prm_file, "/", pos1 - 1) # add a close to the previous namelist if there isnt one
            
          }
          
          # change out popparameters
          number_of_traits_new <- nrow(new_gmat)
          number_of_traits_old <- nrow(org_gmat)
          nobs_old  <-
            nrow(org_designmat) # number of observation, from orginal design matrix
          nobs_new <-
            nrow(new_designmat) # number of observations, from design matrix
          org_traits <- nrow(org_gmat)
          
          prm_file <-
            str_replace(
              prm_file,
              paste("ntbv=", number_of_traits_old, sep = ""),
              paste("ntbv=", number_of_traits_new, sep = "")
            )
          prm_file <-
            str_replace(
              prm_file,
              paste("nres=", number_of_traits_old, sep = ""),
              paste("nres=", number_of_traits_new, sep = "")
            )
          prm_file <-
            str_replace(
              prm_file,
              paste("nobs=", number_of_traits_old, sep = ""),
              paste("nobs=", number_of_traits_new, sep = "")
            ) # number of obs in
          
          #pop parameters is most of the time equal to number of traits, unless there are maternal traits
          
          if ('TRUE' %in% str_detect(prm_file, paste("nebv=", number_of_traits_old, sep = '')) == TRUE) {
            prm_file <-
              str_replace(
                prm_file,
                paste("nebv=", number_of_traits_old, sep = ""),
                paste("nebv=", number_of_traits_new, sep = "")
              )
          } # function to check if the number of ebv is the same as the number of traits, if it is, it replaces the value
          
        }
}        # check if there is a dmu obs
        if (TRUE %in% (grep(prm_file, pattern = "ebv_observation")) == FALSE) {
          # figure out if there is ebv_obs
          if (is.matrix(new_ebvobs) == TRUE &
              is.matrix(org_ebvobs) == TRUE ) { 
            # browser()
            
            # figure out if there is a ebv matrix to replace
            pos  <-
              grep(prm_file, pattern = "ebv_observation") # find where the ebv_obs begins
            pos1 <-
              +pos -1 +grep(prm_file[pos:length(prm_file)], pattern = "/")[1] # find where the ebv_obs begins
            # extract ebv matrix from prm file
            prm_ebvmat <-  matrix(
              c((as.numeric(
                unlist(str_split(str_trim(prm_file[(pos + 1):(pos + nobs_org)]), pattern = " "))
              ))),
              nrow = nobs_org,
              ncol =  nebv_org, byrow = T) 
            if (dim(prm_ebvmat) == dim(org_ebvobs)) {
              if (FALSE %in% (prm_ebvmat == org_ebvobs) == FALSE) {
                # only change the matrix if the one from the prm file is equal to the one
                prm_file <- prm_file[-(pos:pos1)]
                prm_file <-
                  append(
                    x = prm_file,
                    values = c(
                      paste("ebv_observation="),
                      apply(new_ebvobs, 1, paste, collapse = " "),
                      "/"
                    ),
                    after = pos
                  )
              }
           }          
          } # close is there new_ebvobs & org_ebvobs
        } # close is there ebv_obs
        
        
      }
      ########## Exchange Gmatrices ############
      # browser()
      if (is.matrix(new_gmat) == TRUE) {
        if (nrow(org_gmat) > nrow(new_gmat)) {
          pos  <-
            grep(prm_file, pattern = "polygenicMatrix") # find where the gmatrix begins
          
          # tests that the gmatrix from input.prm is the same as the org_gmat from input
          # it only passes the test if all values of gmatrix are the same as from the input
          if ( gmatrix.check == TRUE ) {
            for (i in 1:nrow(new_gmat)) {
              pos  <-
                grep(prm_file, pattern = "polygenicMatrix")
              prm_file[pos + i] <- paste0(new_gmat[i,], collapse = " ")
              if (i > (nrow(new_gmat) - (nrow(org_gmat) - nrow(new_gmat) )) ) {
                prm_file <- prm_file[ -( pos + i+ 1 ) ]
              }
            }
           }
          
            # for cases where the replacement matrix is larger than the original
          } else if (nrow(org_gmat) < nrow(new_gmat)) {
            pos  <-
              grep(prm_file, pattern = "polygenicMatrix")
              prm_file <-
                append(prm_file, "", pos + nrow(new_gmat)) # first add an empty value at the end of the current matrix
              for (i in 1:nrow(new_gmat)) {
                # replace the values
                prm_file[pos + i] <- paste0(new_gmat[i,], collapse = " ")
              }
            } else if (nrow(org_gmat) == nrow(new_gmat)) {
              pos  <-
                grep(prm_file, pattern = "polygenicMatrix")
              for (i in 1:nrow(new_gmat)) {
                # replace the values
                prm_file[pos + i] <- paste0(new_gmat[i,], collapse = " ")
              }
              
            } 
          } # close for gmatrix replacement
         
      
      ########## Exchange Residual matrix ############
      # browser()
      if (is.matrix(new_resmat) == TRUE) {
        pos  <-
          grep(prm_file, pattern = "residualMatrix") # find where the residual matrix begins
        if (resmatrix.check == TRUE ) {
          # browser()
          if (nrow(org_resmat) > nrow(new_resmat)) {
            # in case the orginal is larger than the replacement
            for (i in 1:nrow(new_resmat)) {
              pos  <-
                grep(prm_file, pattern = "residualMatrix")
              prm_file[pos + i] <- paste0(new_resmat[i,], collapse = " ")
              if (i > (nrow(new_resmat) - (nrow(org_resmat) - nrow(new_resmat) )) ) {
                prm_file <- prm_file[ -( pos + i+ 1 ) ]
                }
              }
            
            
            
          } else if (nrow(org_resmat) < nrow(new_resmat)) {
            # for cases where the replacement matrix is larger than the original
            pos  <-
              grep(prm_file, pattern = "residualMatrix")
            prm_file <-
              append(prm_file, "", pos + nrow(new_resmat)) # first add an empty value at the end of the current matrix
            for (i in 1:nrow(new_resmat)) {
              # replace the values
              prm_file[pos + i] <- paste0(new_resmat[i,], collapse = " ")
            }
          } else if (nrow(org_resmat) == nrow(new_resmat)) {
            pos  <-
              grep(prm_file, pattern = "residualMatrix")
            for (i in 1:nrow(new_resmat)) {
              # replace the values
              prm_file[pos + i] <- paste0(new_resmat[i,], collapse = " ")
            }
          }
        }
      }
      # browser()
      ########## Replace True economic value ############
      if (tev.check == TRUE) {
        # first check if the input is there
        pos  <-
          grep(prm_file, pattern = "economicValueTbv") # find where the line with the TEV is located
        
        t <-
          as.numeric(unlist(str_split((
            unlist(str_split(prm_file[(pos)], pattern = "="))[2]
          ), pattern = " ")))
        
        if (FALSE %in% is.na(t) ==  TRUE) { # check if there is a newline
        # this rather convoluted thing is to extract the TEV from the prm file, change it into numeric
        t <- t[-is.na(t)]
        if (TRUE %in% (org_tev == t) == TRUE) {
          # first check if the economic value in the prm file match the one we are switching out
          prm_file[pos] <-
            paste0(c("economicValueTbv=", (new_tev)), collapse = " ") # replace TEV
        }
        } else if (FALSE %in% is.na(t) ==  FALSE) {
          t <-
            as.numeric(unlist(str_split((
              unlist(str_split(str_trim(prm_file[(pos+1)]), pattern = " "))
            ), pattern = " ")))
          if (FALSE %in% (org_tev == t) == FALSE) {
            # first check if the economic value in the prm file match the one we are switching out
            prm_file[pos+1] <- paste(new_tev, collapse =" ") # replace TEV
              
        }
        
        } 
        }
 ########## Replace economic values ############
    # browser()
      if (is.vector(new_ev) == TRUE &
          ev.check == T) {
        # check for input
        pos  <-
          grep(prm_file, pattern = "nEconomicValueEbv") # find where the line with the EV is located
        prm_file[pos]
        number_of_ev <-
          as.numeric(unlist(str_split(prm_file[(pos)], pattern = "="))[2])
        pos  <-
          grep(prm_file, pattern = "economicValueEbv") # find where the line with the EV is located
        
        for (i in 1:number_of_ev) {
          prm_file[pos + i] <-
            sub(
              pattern = "(\\d)(\\s)+(\\d)(\\s)+(\\d.*)",
              replacement = paste("\\1\\2\\3\\4", paste0(new_ev, collapse = " "), sep =
                                    ""),
              prm_file[pos + i]
            )
        }
      }
############ Replace selection lines ##################
if (sel.lines.check == TRUE) { 
  browser()
  pos  <-
    grep(prm_file, pattern = "selection_scheme=") # find where the selection lines begin
  pos1  <-pos+
    grep(prm_file[pos:length(prm_file)], pattern = "/")[1]-1 # find where the selection lines begin
  selection.lines <- prm_file[pos:pos1]
  
  selection.array <-
    matrix(unlist(str_split(str_trim(selection.lines[str_detect(string = selection.lines, pattern = "(\\d)")]), pattern = "\\s+")),
           nrow = (as.numeric(unlist(str_split(prm_file[grep(prm_file, pattern = "selection_groups")],pattern="="))[2]) ),
           ncol = 26, # this is fixed, the number of elements in each line in the selection scheme
           byrow = T)
org_sel_array <-  matrix(unlist(str_split(str_trim(org_selLines[str_detect(string = org_selLines, pattern = "(\\d)")]), pattern = "\\s+")),
                         nrow = length(org_selLines),
                         ncol = 26, # this is fixed, the number of elements in each line in the selection scheme
                         byrow = T)
if (nrow(selection.array) == nrow(org_sel_array)){
  if (FALSE %in% (selection.array == org_sel_array) == FALSE) { # make sure that it doesnt switch out matrices that are different
  prm_file <- prm_file[-(pos:pos1)]
  prm_file <-
    append(
      x = prm_file,
      values = c(
        paste(
          "selection_scheme="),
        new_selLines,
        "/"
      ),
      after = pos
    )
  pos  <-
    grep(prm_file, pattern = "selection_groups=") # find where the selection lines begin
  prm_file[pos] <- paste("selection_groups=", length(new_selLines), sep="")
  }}
  }
############ check if there are illegal obs numbers #############
      pos  <-
        grep(prm_file, pattern = "nobs") # find where the line with the TEV is located
      
      max.obs.numb <- as.numeric(unlist(str_split((
        unlist(str_split(prm_file[(pos)], pattern = "="))[2]
      ), pattern = " ")))
      pos  <-
        grep(prm_file, pattern = "nMaleObs") # 
      obs <- as.numeric(unlist(str_split((
        unlist(str_split(prm_file[(pos)], pattern = "="))[2]
      ), pattern = " ")))
      pos  <-
        grep(prm_file, pattern = "MaleObservations") # 
      pos1  <-
        grep(prm_file, pattern = "FemaleObservations") # 
      if (TRUE %in% (str_detect(prm_file[pos:pos1], pattern = paste("(\\s)(",max.obs.numb+1,")","(.*)", sep="")))== TRUE) {
        cat(paste("illegal obs numb in MaleObservations", "path=", path, sep= " "), append = T, file = log)
        cat("\n", append = T, file = log)
      }
      
      # female obs check
      pos  <-
        grep(prm_file, pattern = "nFemaleObs") # 
      obs <- as.numeric(unlist(str_split((
        unlist(str_split(prm_file[(pos)], pattern = "="))[2]
      ), pattern = " ")))
      pos  <-
        grep(prm_file, pattern = "FemaleObservations") # 
      pos1  <-
        grep(prm_file[pos:length(prm_file)], pattern = "/") # 
      pos1 <- pos+pos1[1]
      if (TRUE %in% (str_detect(prm_file[pos:pos1], pattern = paste("(\\s)(",max.obs.numb+1,")","(.*)", sep="")))== TRUE) {
        cat(paste("illegal obs numb in FemaleObservations", "path=", path, sep= " "), append = T, file = log)
        cat("\n", append = T, file = log)
      }
########## Write lines at end of loop ###########      
      writeLines(prm_file, path)
      cat(paste("input.prm file changed, path=", path, sep= " "), append = T, file = log)
      cat("\n", append = T, file = log)
      
    } # close for loop of prm files
    closeAllConnections()
  } # close of function

# this function is for quickly replacing the prm files with their backup, in case of some fuck up
IScrewedUpPrm <- function( root ) {
  prmfiles <-  list.files(path = root, pattern = "*.prm_bak$", recursive = T)
  orgs <- list.files(path = root,pattern = "*.prm$", recursive = T)
  for ( i in 1:length(prmfiles)) {
    path <- paste(root, prmfiles[i], sep= "/")
    org_path <- paste(root, orgs[i], sep= "/")
    prm_file <-  readLines(path)
    writeLines(prm_file,org_path) # write backups as prm
  }
}
#
# FixMyPrm(
# root, # the highest directory in which the functions works, searches in all lower directories for prm files
# new_gmat, # gmatrix to replace
# new_designmat,
# new_tev,  # new vector for true economic values
# new_ev,
# new_ebvobs ,
# new_resmat,
# org_gmat,
# org_designmat,
# org_ebvobs ,
# org_ev,
# org_resmat )
# LarissaFixMyPrmFiles(root=root, new_gmat = gmatrix_edit, org_gmat = org_gmat)
