`calc.overstatement.e_p` <-
function( Z ) {
  ## One way of calculating the errors for a collection of audited precints.
  ## This one is the sum of all winner overcounts plus the sum of all 
  ## loser undercounts (for each precinct)
  ## Return: Vector (of length of audited precincts) of found errors by precinct. 
  
  apply( Z$audit, 1, 
        function(p) {sum( pmax(p[Z$winners],0) ) + sum( pmax(-1*p[Z$losers],0) )}  )
}

