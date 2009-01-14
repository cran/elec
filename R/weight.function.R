`weight.function` <-
function( name=c("no.weight","weight","weight.and.slop", "margin.weight","taint") ) {
  name = match.arg( name )
  switch( name,
     
         ## no weighting
         no.weight= c( function( x, b_p, M ) { x + 0 * b_p }, 
           function( x, b_p, M ) { x + 0 * b_p } ),
         
         ## weighted by size of precint
         weight = c( function( x, b_p, M ) { x / b_p }, 
           function( x, b_p, M ) { x * b_p } ),
         ## weight by size, after a slop of 2 votes has been taken off
         weight.and.slop = c( function( x, b_p, M ) { pmax( x - 2, 0 ) / b_p },
           function( t, b_p, M ) { t * b_p + 2 } ),
         ## for pairwise margin tests
         margin.weight = c( function( x, b_p, M ) { pmax(x - 2/M, 0) / b_p },
           function( x, b_p, M ) { x * b_p + 2/M } ),
         ## taint -- divide by maximum error in the precinct
         taint = c( function( x, b_p, M ) { x / b_p },
            function( x, b_p, M ) { x * b_p } )
           )
}

