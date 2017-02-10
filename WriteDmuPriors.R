library(Matrix)
# Function that makes a dmu priors file from a given set of gmatrix and resmatrices


MakeVarParm <- function (gmat, resmat) {
  closeAllConnections()
  output <- file(description = "dmu.polyblup.parm", open = "w")
  close(output)
  outputfile <- paste( getwd(), 'dmu.polyblup.parm', sep = '/') 
  for ( i in 1:nrow( gmat ) ) {
    
    for ( l in 1:ncol( gmat ) ) {
      con <- file(description = outputfile, open = "a") 
      if ( i <= l ) {  
        cat( 1, i, l, gmat[ i,l ], file = con, sep = " " )
        
        cat("\n", file = con )
        closeAllConnections()
      }
    }
  }
  for ( i in 1:nrow( resmat ) ) {
    
    for ( l in 1:ncol( resmat ) ) {
      con <- file(description = outputfile, open = "a") 
      if ( i <= l ) {  
        cat( 2, i, l, resmat[ i,l ], file = con, sep = " " )
        
        cat("\n", file = con )
        closeAllConnections()
      }
    }
  }
}
