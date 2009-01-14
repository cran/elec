`print.elec.data` <-
function( x, n=4, ... ) {
                                        # "N"           "C"           "f"           "V"           "C.names"     "total.votes" "margin"      "margin.per"  "totals"      "winners"     "losers"      "audit"       "Ms"         
  Z = x
  cat( "Z frame:  N = ", Z$N, "\tC/f = ", Z$C, "/", Z$f, "\ttotal votes = ", 
      Z$total.votes, "    M = ", Z$margin,
      " (", round(100*Z$margin.per), "%)\n",
      "\t\tNames = ", paste(Z$C.names, " (", round(100*Z$totals/Z$total.votes), "%)", sep="", collapse=", "), "\t  Winners = ", 
      paste( Z$winners, collapse=", " ), "    Losers = ", 
      paste( Z$losers, collapse=", "), "\n", sep="")
  if ( Z$C > 2 ) { 
    cat( "Pairwise margins:\n" )
    print( Z$Ms )
  }
  
  cat( "Sample votes (", length( rownames(Z$V) ), " records)\n", sep="" )
  print( head( Z$V, n ) , fill=TRUE, labels=c("\t"))
  if ( !is.null(Z$audit ) ) {
    cat( "Sample Audits (", length( rownames(Z$audit) ), " records)\n", sep="" )
    print(	head( Z$audit, n ) )
    
                                        #fill=TRUE, labels=c("\t")
  } else {
    cat( "(No audit information)\n" )
  }
  
  invisible( Z )

}

