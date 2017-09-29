#############
# Functions #
#############

# Need functions for up to 4 levels of cateogry variables

# Do we need facility for manual overrides for certain criteria?

# Add in a default value for level 2+

# calc_cumulative

    # INPUTS
    #   df    - the odds table to calculate the cumulative values for

    # OUTPUT
    #   output - the transformed table

calc_cumulative <- function( df ) {
    
    level <- df %>%
        select( starts_with( 'filter' ) ) %>% 
        dim %>%
        .[2]
    
    print( level ) 
    
    if ( level == 0 ) {
        
        output <- df %>% 
            mutate( cumulative_odds = cumsum( odds ) )
        
    } else {
        
        filter_names <- paste0( 'filter_', seq( 1, level ) )
        
        output <- df %>% 
            group_by_( .dots = filter_names ) %>% 
            mutate( cumulative_odds = cumsum( odds ) ) %>% 
            ungroup
        
    } 
    
    return( output )
    
}

# level_1_values_function

    # INPUTS
    #   tbl - this is a table containing the desired output values as well
    #         as the probabilies
    #   n   - this is the size of output deisred
    
    # OUTPUT
    #   output - this will be a vector

level_1_values_function <- function ( tbl, n ) {
  
    random_values <- runif( n )

    output <- cut(
        x      = random_values,
        breaks = c( 0, tbl %>% select( cumulative_odds ) %>% .[[1]] ),
        labels = tbl %>% select( out_value ) %>% .[[1]]
    ) %>% 
        as.character

    return( output )
  
}

level_1_adj_values_function <- function ( 
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

# level_2_values_function

    # INPUTS
    #   tbl - this is a table containing the desired output values as well
    #         as the probabilities
    #   filter_value - this is the value that the table will be filtered 
    #         by, the column that will be filtered is filter_1
    #   n - the number of values to create
    #   default - is an argument with a default value
    
    # OUTPUT
    #   output - outputs a vector of values

level_2_values_function <- function ( 
    odds_tbl, filter_value, n, 
    default = "default", modification_tbl = data.frame() 
) {
  
    random_values <- runif( n )
    
    tbl <- modification_tbl %>%
        filter( filter_1 == filter_value[1] )

    if ( nrow( tbl ) == 0 ) {
        
        print( "Here filtering" )
        
        tbl <- odds_tbl %>% 
            filter( filter_1 == filter_value[1] )
        
    }
    
    print( tbl )
  
    if ( nrow( tbl ) == 0 ) {
        
        print( "Here default" )
        
        output <- replicate( n, default )
        
        return( output )
        
    }
  
    output <- cut(
        x      = random_values,
        breaks = c( 0, tbl %>% select( cumulative_odds ) %>% .[[1]] ),
        labels = tbl %>% select( out_value ) %>% .[[1]]
    ) %>% 
        as.character
  
    return( output )
  
}

# level_3_values_function

    # INPUTS
    #   tbl - this is a table containing the desired output values as well
    #         as the probabilities
    #   filter_value_1 - this is the value that the table will be filtered 
    #         by, the column that will be filtered is filter_1
    #   filter_value_2 - this is the value that the table will be filtered
    #         by, the column that will be filtered is filter_2
    #   n - the number of values to create
    
    # OUTPUT
    #   output - outputs a vector of values

level_3_values_function <- function ( 
        tbl, filter_value_1, filter_value_2, n, 
        default = "default", modification_tbl = data.frame()
    ) {
  
    random_values <- runif( n )
  
    tbl <- tbl %>% 
        filter( 
          filter_1 == filter_value_1[1],
          filter_2 == filter_value_2[1]
        )
  
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
    
    return( output )
  
}

# numeric_values

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
    #   . - outputs a vector of values

numeric_values <- function ( 
    n, type, 
    df = 4, 
    min = 0, max = 1,
    shape = 2, rate = 4,
    size = 1, prob = 0.5,
    mean = 0, sd = 1,
    multiplier = 1, round = 0,
    minimum = -Inf, maximum = Inf
) {
    
    temp_func <- function ( x ) {
        
        val <- x %>% 
            lapply( function( x ) prod( x, multiplier ) ) %>% 
            lapply( function( x ) max( x, minimum ) ) %>%
            lapply( function( x ) min( x, maximum ) ) %>% 
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
