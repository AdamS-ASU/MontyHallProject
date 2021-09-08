#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#' New game select door
#' @description
#' New monty hall game selecting random door 1-3
#' @details
#' Assign doors 1-3, add sample function to randomize selection
#' @param 
#' Doors assigned 1-3, sample random selection, a.pick assigned as action
#' @return 
#' a.pick returns one randomized door 1-3
#' @examples
#' door 1,2, or 3 selected
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Select Goat Door
#' @description
#' Goat door is assigned here as a continuation of previous actions
#' @details
#' Assign goat door is not a car and not my selection
#' @param 
#' Goat door is 1 door selected not a.pick and not a car
#' @return 
#' return 1 door from limited selections
#' @examples
#' goat door opened as door 1/2/ or 3 depending on initial pick
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Select Stay or Change
#' @description
#' Continue previous logic and make a selection fo stay or change
#' @details
#' Choose either stay or change based on previous doat door opened
#' @param 
#' doors not equal to goat and original/ or switched
#' @return 
#' a door not specified
#' @examples
#' return a.pick door (1) if stay or return door (not opened) as 3
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine winner
#' @description
#' determine if the door you selected final is goat or car
#' @details
#' winner checks if car = T, loser if car = F
#' @param 
#' @return 
#' @examples
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' play full game
#' @description
#' Play the full game with a new random selection function
#' @details
#' Run complete game code and choose stay or switch
#' @param 
#' return results based on selection of stay or switch
#' @return 
#' return 'WIN' or 'LOSE'
#' @examples
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Play x number of games
#' @description
#' Play the full function of the game assigned unmber of times
#' @details
#' assign the number of times to play the game, 100, set the collector and iterator
#' @param 
#' n = 100 game splayed
#' @return 
#' return win percentages based on N played
#' @examples
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
