`countVotes` <-
function( Z ) {
  ## Count the total votes for various candidates.
  ## Return:  Updated 'Z' matrix with the total votes as components
  ##          inside it.
  
                                        # total votes in given precinct
  if ( is.null( Z$V[[Z$tot.votes.col]] ) ) {
    warning("Totalling votes from candidates in countVotes()\n" )
    Z$V[[Z$tot.votes.col]] = apply( Z$V[Z$C.names], 1, sum )
  }
  grand.tot = sum(Z$V[[Z$tot.votes.col]])
  
  tots = sapply(Z$V[Z$C.names],sum)
  nt = sort(tots, decreasing=TRUE)
  M = nt[Z$f] - nt[Z$f+1]   # smallest margin (last winner - best loser)
  

  winners = names(nt[1:Z$f])
  losers = setdiff( names(nt), winners )

  Z[c("total.votes", "margin", "margin.per", "totals","winners","losers")] = list(grand.tot, M, M/grand.tot, tots, winners, losers )
  
  Z
                                        #c( Z, list( margin=M, totals=tots, winners=winners, losers=losers ) )
}

