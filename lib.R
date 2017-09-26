#############
# Functions #
#############

# Need a function work out the cumulative odds bit of the table
# Need functions for up to 4 levels of cateogry variables
# Need functions for assigning numeric values over a distribution

# Do we need facility for manual overrides for certain criteria?

# Add in a default value for level 2+

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

# numeric_values_function 

numeric_values_function <- function ( 
    n, type, 
    df = 4, 
    min = 0, max = 1,
    shape = 2, rate = 4,
    multiplier = 1, round = 0
) {
    
    temp_func <- function ( x ) {
        round( x * multiplier, round )
    }
    
    if ( type == "chi2" ) {
        return( temp_func( rchisq( n, df ) ) )
    }
    
    if ( type == "unif" ) {
        return( temp_func( runif( n, min, max ) ) )
    }
    
    if ( type == "gamma" ) {
        return( temp_func( rgamma( n, shape, rate ) ) )
    }
    
}
