#############
# Functions #
#############

# Need functions for up to 4 levels of cateogry variables

# Do we need facility for manual overrides for certain criteria?

# Add in a default value for level 2+

# calcCumulative

    # INPUTS
    #   df    - the odds table to calculate the cumulative values for

    # OUTPUT
    #   output - the transformed table

calcCumulative <- function( df ) {
    
    level <- df %>%
        select( starts_with( 'filter' ) ) %>% 
        dim %>%
        .[2]
    
    if ( level == 0 ) {
        
        output <- df %>% 
            mutate( cumulative_odds = cumsum( odds ) ) %>% 
            filter( odds != 0 )
        
    } else {
        
        filter_names <- paste0( 'filter_', seq( 1, level ) )
        
        output <- df %>% 
            group_by_( .dots = filter_names ) %>% 
            mutate( cumulative_odds = cumsum( odds ) ) %>% 
            ungroup %>% 
            filter( odds != 0 )
        
    } 
    
    return( output )
    
}

# filterTbl

    # INPUTS
    #   tbl           - table to filter
    #   filter_1_val  - the value to filter the table on
    #   select_col    - column to get the value from as a string

    # OUTPUT
    #   output - the filtered table

filterTbl <- function( 
    tbl, ..., select_col = "out_value", default = "0", convert = "none"
) {
    
    filter_values <- list( ... )
    
    level <- tbl %>%
        select( starts_with( 'filter' ) ) %>% 
        ncol
    
    level_check <- filter_values %>% 
        length
    
    if ( level != level_check ) {
        val <- paste(
            "There is a different number of filter columns and",
            "filter values provided."
        )
        stop( val )
    }
    
    tmp <- tbl
    
    for ( i in 1:level ) {
        filter_string <- paste0(
            "filter_", i, " == "
        )
        if ( class( filter_values[[i]][[1]] ) == "character" ) {
            filter_string <- paste0(
                filter_string, '"', filter_values[[i]], '"'
            )
        } else {
            filter_string <- paste0(
                filter_string, filter_values[[i]]
            )
        }
        tmp <- tmp %>% 
            filter_( filter_string )
    }
    
    output <- tmp %>% 
        select_( .dots = select_col )
    
    if ( dim( output )[1] == 0 ) {
        output <- default
    }
    
    if ( convert == "num" ) {
        output <- output %>% 
            as.numeric
    }
    
    return( output )
    
}

# levelValues

    # INPUTS
    #   tbl       - table of odds
    #   n         - numer of values to be outputed
    #   ...       - the filter values for the odds table if applicable
    #   default   - a default value for if no values are found in the table
    #   modification_tbl - if there is a table of modification values to 
    #       'overrule' the main table
    #       NEEDS TESTING
    #   convert   - if the output is required to be numeric this argument
    #       takes the value 'num'

    # OUTPUT
    #   output - a vector of values generated from the tbl and 
    #       modification_tbl inputs

levelValues <- function ( 
    tbl, n, ...,
    default = "default", modification_tbl = data.frame(), convert = "none"
) {
    
    level <- tbl %>% 
        select( starts_with( "filter" ) ) %>% 
        ncol
    
    if ( level > 0 ) {
        tbl <- filterTbl(
            tbl, ..., select_col = c( "out_value", "cumulative_odds" )
        )
    }
    
    level <- modification_tbl %>% 
        select( starts_with( "filter" ) ) %>% 
        ncol
    
    if ( level > 0 ) {
        modification_tbl <- filterTbl(
            modification_tbl, ..., 
            select_col = c( "out_value", "cumulative_odds" )
        )
    }
    
    random_values <- runif( n )
    
    if ( nrow( modification_tbl ) > 0 ) {
        
        tbl <- modification_tbl
        
    }
    
    if ( nrow( tbl ) == 0 ) {
        
        output <- replicate( n, default )
        
        return( output )
        
    }
    
    output <- cut(
        x      = random_values,
        breaks = c( 0, tbl %>% select( cumulative_odds ) %>% .[[1]] ),
        labels = tbl %>% select( out_value ) %>% .[[1]]
    ) %>%
        as.character
    
    if ( convert == "num" ) {
        output <- output %>% 
            as.numeric
    }
    
    return( output )
    
}

# level1AdjValues

    # INPUTS

    # OUTPUT

level1AdjValues <- function ( 
    org_values, filter_value, modification_tbl, n
) {
    
    tbl <- modification_tbl %>% 
        filter( filter_1 == filter_value[1] )
    
    if ( nrow( tbl ) == 0 ) {
        
        output <- org_values
        
        return( output )
        
    }
    
    random_values <- runif( n )
    
    output <- cut(
        x      = random_values,
        breaks = c( 0, tbl %>% select( cumulative_odds ) %>% .[[1]] ),
        labels = tbl %>% select( out_value ) %>% .[[1]]
    ) %>% 
        as.character
    
    return( output )
    
}


# numericValues

    # INPUTS
    #   n           - the number of values to be generated
    #   type        - the type of generating distribution, takes values:
    #       "chisq", "unif", "gamma", "binom", "norm"
    #   df          - the degrees of freedom for the chisq distribution
    #   min, max    - the lower and upper limits of the distribution
    #   shape, rate - the parameters of the gamma distribution
    #   size, prob  - the number of trials, and the probability of success
    #   mean, sd    - the mean and standard deviation for the normal 
    #       distribution
    #   multiplier  - a numeric value to multiply the generated numbers by
    #   round       - the number of decimal places to round to
    #   minimum     - a minimum value to cap the numbers at
    #   maximum     - a maximum value to cap the numbers at
    
    # OUTPUT
    #   . - outputs a vector of values based on the 'type' input

numericValues <- function ( 
    n, type, 
    df = 4, 
    min = 0, max = 1,
    shape = 2, rate = 4,
    size = 1, prob = 0.5,
    mean = 0, sd = 1,
    multiplier = 1, 
    minimum = -Inf, maximum = Inf,
    addition = 0, round = 0
) {
    
    temp_func <- function ( x ) {
        
        val <- x %>% 
            lapply( function( x ) prod( x, multiplier ) ) %>% 
            lapply( function( x ) max( x, minimum ) ) %>%
            lapply( function( x ) min( x, maximum ) ) %>% 
            lapply( function( x ) return( x + addition ) ) %>% 
            unlist %>% 
            round( round )
        
        return( val )
        
    }
    
    if ( type == "chisq" ) {
        return( temp_func( rchisq( n, df ) ) )
    }
    
    if ( type == "unif" ) {
        return( temp_func( runif( n, min, max ) ) )
    }
    
    if ( type == "gamma" ) {
        return( temp_func( rgamma( n, shape, rate ) ) )
    }
    
    if ( type == "binom" ) {
        return( temp_func( rbinom( n, size, prob ) ) )
    }
    
    if ( type == "norm" ) {
        return( temp_func( rnorm( n, mean, sd ) ) )
    }
    
}
