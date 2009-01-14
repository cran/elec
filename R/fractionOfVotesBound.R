`fractionOfVotesBound` <-
function( Z, frac=0.4 ) {
  ## This is the 0.4b bound function.  It returns frac * total votes.
  ## Return:   Vector (of length of precincts) of maximum error for 
  ##           each precinct.
  
  Z$V[[Z$tot.votes.col]] * frac / Z$margin
}

