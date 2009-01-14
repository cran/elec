`compute.audit.errors` <-
function( Z, audit=NULL,
  calc.e_p=calc.pairwise.e_p,
  w_p = weight.function("no.weight"),
  bound.col="tot.votes",
  err.override = NULL ) {
  
  if ( !is.null(audit) ) {
  	Z$audit = audit
  }
                                        # calculate the errors
  Z$audit$err = calc.e_p( Z, err.override=err.override )
  
                                        # weight the errors
  Z$audit$err.weighted = w_p[[1]]( Z$audit$err, Z$audit[[bound.col]], Z$margin )

  Z$audit
}

