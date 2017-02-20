# Function for making prior files for DMU based on input.prm files
# note that this only checks that the prior file matches or does not match the input.prm
library(stringr)


root <- "C:/Users/au384062/Dropbox/Projects/ADAM/RegEx_practice/MBLUP/"

MakePriorFiles <- function ( root) {  
  # browser()
  prmfiles <-
    list.files(path = root,
               pattern = "*.prm$",
               recursive = T)
  priorfiles <- list.files( path = root, pattern = "dmu.polyblup.parm$", recursive = T)
  log <- file(description = paste(root,"prior.fix.log", sep = "/"), open = "w") # open log connection
  
  # check if there are folders containing input.prm, but no parm
  raw.prior.files <- paste(root,gsub ( pattern = 'dmu.polyblup.parm$', replacement = "", x = priorfiles),sep = "")
  raw.prm.files <- paste(root,gsub ( pattern = 'input.prm', replacement = "", x = prmfiles),sep = "")
  missing.prior <- raw.prm.files[-match(raw.prior.files , raw.prm.files)] # folders lacking prm files
  
  for (i in 1:length(prmfiles[match(raw.prior.files , raw.prm.files)]) ) {   
    path <- paste(root, priorfiles[i], sep = "")
     
      prior.file <-  readLines(path)
writeLines(prior.file, paste(path, "bak", sep = "_")) # write backup of each file
    prm_file <- readLines(paste(root, prmfiles[i], sep=""))
    prm_file <- gsub(pattern = "\t", replacement = " ", x =prm_file)
 
    ######### Check if Priors match input.prm ########
    pos <- grep(x = prior.file, pattern = "(^2)(.*)") # find the res mat rows
    gm_prm <-
      matrix(c(as.numeric(unlist(
        str_split(
          string = str_trim(gsub(
            x = prior.file[-pos],
            pattern = "(^1)",
            replacement = ""
          )),
          pattern = " "
        )
      ))), nrow = length(pos), ncol = 3, byrow=T)
    # make matrix out of info from prior file
      upper_tri <- matrix( data = 0, nrow=max(gm_prm[,1]), ncol=max(gm_prm[,2])) # makes upper triangular matrix
      upper_tri[cbind(gm_prm[,1], gm_prm[,2])] <- gm_prm[,3]
      low <- t(upper_tri) # make the lower by transposing the upper
      diag(low) <- 0 # remove the diagonal
      gmatrix.from.prior <- low + upper_tri # full matrix
    # check if matrices match, first gmatrix, TODO currently the function replaces both parts
      # since I havent bothered to make a check if both match, for large directories probably one should
      # make such a check, for speed
      # TODO 2: vectorize this, for loops are slow!
    ######### Make new prior file if they do not match #######
      pos <- grep(x = prm.file, pattern = "ntbv") # find the number of ebv
      n.tbv <- as.numeric(unlist(str_split( pattern = "=", string = prm.file[pos]))[2])
      
      pos  <-
        grep(prm_file, pattern = "polygenicMatrix") # find where the gmatrix begins
      prm_gm <-  matrix(
        c((as.numeric(
          unlist(str_split(prm_file[(pos + 1):(pos + n.tbv)], pattern = " "))
        ))),
        nrow = n.tbv,
        ncol = n.tbv ) 
      
      if ( (F %in% (dim(gmatrix.from.prior)==dim(prm_gm)) == T)) {
        logcon <-  paste(root,"prior.fix.log", sep = "/")
        output <- file(description = path, open = "w")
        cat("Priors written to dmu.polyblup.prm in file", path,"\n", file = logcon,append = T)
        # cat("\n", file = logcon)
        close(output)
        outputfile <- path
        for ( i in 1:nrow( prm_gm ) ) {
          
          for ( l in 1:ncol( prm_gm ) ) {
            con <- file(description = outputfile, open = "a") 
            if ( i <= l ) {  
              cat( 1, i, l, prm_gm[ i,l ], file = con, sep = " " )
              
              cat("\n", file = con )
              closeAllConnections()
            }
          }
        }
        pos  <-
          grep(prm_file, pattern = "residualMatrix") # find where the gmatrix begins
        prm_rm <-  matrix(
          c((as.numeric(
            unlist(str_split(prm_file[(pos + 1):(pos + n.tbv)], pattern = " "))
          ))),
          nrow = n.tbv,
          ncol = n.tbv ) 
        for ( i in 1:nrow( prm_rm ) ) {
          
          for ( l in 1:ncol( prm_rm ) ) {
            con <- file(description = outputfile, open = "a") 
            if ( i <= l ) {  
              cat( 2, i, l, prm_rm[ i,l ], file = con, sep = " " )
              
              cat("\n", file = con )
              closeAllConnections()
            }
          }
        }
        
        
      } else if ( (F %in% (dim(gmatrix.from.prior)==dim(prm_gm)) == F)) {
        if (  (F %in% (gmatrix.from.prior == prm_gm) == T)) {
        logcon <-  paste(root,"prior.fix.log", sep = "/")
        output <- file(description = path, open = "w")
        cat("Priors written to dmu.polyblup.prm in file", path,"\n", file = logcon,append = T)
        # cat("\n", file = logcon)
        close(output)
        outputfile <- path
        for ( i in 1:nrow( prm_gm ) ) {
          
          for ( l in 1:ncol( prm_gm ) ) {
            con <- file(description = outputfile, open = "a") 
            if ( i <= l ) {  
              cat( 1, i, l, prm_gm[ i,l ], file = con, sep = " " )
              
              cat("\n", file = con )
              closeAllConnections()
            }
          }
        }
        pos  <-
          grep(prm_file, pattern = "residualMatrix") # find where the gmatrix begins
        prm_rm <-  matrix(
          c((as.numeric(
            unlist(str_split(prm_file[(pos + 1):(pos + nrow(gmatrix.from.prior + 1))], pattern = " "))
          ))),
          nrow = nrow(gmatrix.from.prior),
          ncol = ncol(gmatrix.from.prior))
        for ( i in 1:nrow( prm_rm ) ) {
          
          for ( l in 1:ncol( prm_rm ) ) {
            con <- file(description = outputfile, open = "a") 
            if ( i <= l ) {  
              cat( 2, i, l, prm_rm[ i,l ], file = con, sep = " " )
              
              cat("\n", file = con )
              closeAllConnections()
            }
          }
        }
        
        }
      }
    closeAllConnections()
      } 
  
  # next portion is to make parm files where there are none
  
if (length(missing.prior) != 0)  {  
  for ( i in length (missing.prior) ) { 
    prior.file <- file(description =paste( missing.prior [i], "dmu.polyblup.parm", sep ='') , open = 'w')
    path <- paste(missing.prior, "dmu.polyblup.parm", sep="")
    prm_file <- readLines(paste(missing.prior, "input.prm", sep="")) 
    prm_file <- gsub(pattern = "\t", replacement = " ", x =prm_file) 
    nrow_prm <-
      as.numeric(unlist(str_split((
        unlist(str_split(prm_file[(grep(prm_file, pattern = "ntbv"))], pattern = "="))[2]
      ), pattern = " "))) 
    pos  <-
      grep(prm_file, pattern = "polygenicMatrix") # find where the gmatrix begins
suppressWarnings(    prm_gm <-   matrix(
      c(as.numeric(
        unlist(str_split(prm_file[(pos + 1):(pos + (nrow_prm + 1))], pattern = " "))
      ))[is.na((as.numeric(
        unlist(str_split(prm_file[(pos + 1):(pos + (nrow_prm + 1))], pattern = " "))
      )))!=T],
      nrow = nrow_prm,
      ncol = nrow_prm))
    outputfile <- path
      for ( i in 1:nrow( prm_gm ) ) {
        
        for ( l in 1:ncol( prm_gm ) ) {
          con <- file(description = outputfile, open = "a") 
          if ( i <= l ) {  
            cat( 1, i, l, prm_gm[ i,l ], file = con, sep = " " )
            
            cat("\n", file = con )
            closeAllConnections()
          }
        }
      }
      pos  <-
        grep(prm_file, pattern = "residualMatrix") # find where the gmatrix begins
suppressWarnings(            prm_rm <-  matrix(
  c((as.numeric(
    unlist(str_split(prm_file[(pos + 1):(pos + n.tbv)], pattern = " "))
  ))),
  nrow = n.tbv,
  ncol = n.tbv ) 
)
# note: the suppress warnings is just because the program looks for numerics to handle cases where the prm file has a "/" 
# in the same line as the last line of the residual matrix
for ( i in 1:nrow( prm_rm ) ) {
        
        for ( l in 1:ncol( prm_rm ) ) {
          con <- file(description = outputfile, open = "a") 
          if ( i <= l ) {  
            cat( 2, i, l, prm_rm[ i,l ], file = con, sep = " " )
            
            cat("\n", file = con )
            closeAllConnections()
          }
        }
      }

      
    
    
    
  }
}
  closeAllConnections()
  
} 

# MakePriorFiles(root)
