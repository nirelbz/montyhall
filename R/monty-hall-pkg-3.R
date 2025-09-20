#' @title
#'   The Monty Hall Problem

#' @description
#'  There are three doors, behind one there is a car. Behind the other two there are goats.
#' Can you find the car and win the game? 
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. 
#'   Once the contestant has selected a door, the host
#'   opens one of the remaining two doors to reveal a goat. and then the contestant is
#'   The contestant at this point must now make another choice: 
#'   Should they keep their selection the same, or choose the remaining door? 
#'   Fortunately, with statistics and an R simulation we can figure out which option
#'   gives them the highest chance of finding the car!  
#'
#' @param ... no arguments are used by the function create_game().
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
#'		Have the game "player" select the door
#' @description
#'		select_door() represents the player selecting a door numbered from 1 through 3 inclusive. 
#' @details
#'		The function generates a random pick between 1 and 3. This represents the contestant's choice in initially selecting their door. 
#' @param 
#'		No arguments are used by the function. 
#' @return 
#'		The function returns a pick variable which contains either the integer 1, 2, or 3
#' @examples
#' 		select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' 		Have the host open a door not picked by the player. 
#' @description
#'		open_goat_door() represents the host opening a door behind which sits a goat. This door must not be the same door that the player picked. 
#' @details
#'		The function generates another number between 1 and 3 inclusive, but this time, it must not be the number the player selected AND it must be the number that represents a goat. 
#' @param 
#'		This function takes the game and the a.pick variables as arguments to ensure that the result is not the player selected door. 
#' @return 
#'		This function returns an opened.door variable which contains an integer between 1 and 3 inclusive. 
#' @examples
#'		open_goat_door(this.game, this.pick)
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
#'		The host offers the player the option to swap their door 
#' @description
#'		change_door() represents the host offering the contestant the option to swap their door selection to the only unopened remaining door. 
#' @details
#'		This function takes in a value for a 'stay' variable which represents the player sticking to their initial choice or swapping doors. It outputs the final door selection. 
#' @param 
#' 		This function takes in a 'stay' variable, the opened.door variable, and the first a.pick variable.
#' @return 
#'		This function returns a final.pick variable that is an integer between 1 and 3
#' @examples
#'		change_door(stay=F, opened.door, this.pick)
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
#'		This function determines whether or not the player won a car
#' @description
#'		This function checks the final selection that the player makes and identifies if it matches the integer that represented the door behind which sits a car.
#' @details
#'		This function takes in the final selection variable and the game variable and compares the selection to the variable representing that integer. If and only if it's a car, the player wins. 
#' @param 
#'		This function takes in the arguments for final.pick, returned in the previous function, and our game variable
#' @return 
#'		This function returns results with the string WIN or LOSE
#' @examples
#'		determine_winner(final.pick, this.game)
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
#'	 	The full play_game function runs through each previous function to simulate an entire Monty Hall game scenario.
#' @description
#'	 	This function serves as a way for us to simulate a game to check results where the player both stays and swaps. 
#' @details
#' 		This function creates our game variable with the prizes and doors, simulates the player selections, runs the scenario where the player swaps and stays, then determines whether they won or lost their game, storing that result in a results variable. 
#' @param 
#'		This function takes no arguments 
#' @return 
#' 		This function returns a game.results variable which shows either the player swapped or stayed and whether they won or lost that game. 
#' @examples
#' 		play_game()
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
#'		Plays n games of Monty Hall 
#' @description
#'		Plays n games of Monty Hall with both strategies (stay or swap doors) and records the results. 
#' @details
#'		This function lets us run the previous function n times and records all the results by trying both strategies. This allows us to check across many runs which strategy is the best. 
#' @param 
#'		This function takes in an integer n, representing how many times we want to play the game 
#' @return 
#'		 This function returns results.df, a list of results for each game simulated. 
#' @examples
#'		play_n_games(n=100)
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


#' @title
#'		Unit Test: Beginning the game
#' @description
#'		Simulates beginning the came
#' @details
#'		Once function is implemented, goat, goat and car will be assigned to a "door" at random
#' @param 
#'		This function outputs a string representing the two goats and car in their selected order
#' @return 
#'		Results will  return some variation of goat, goat, car
#' @examples
#'		"car" "goat" "goat"
#' @export


#' this.game <- create_game()


#' @title
#'		Unit Test: Select door
#' @description
#'		Simulates a player selecting their first door 
#' @details
#'		This function will select a door between 1-3 at random
#' @param 
#'		This function outputs an integer between 1-3 representing the player's first selection
#' @return 
#'		Results will  return an integer betwen 1-3
#' @examples
#'		"My initial selection: 3"
#' @export


#' my.initial.pick <- select_door() 



#' @title
#'		Unit Test: Host opens a door
#' @description
#'		Simulates the game host opening a door other than the player's selection that contains a goat
#' @details
#'		The function will select a door that has a goat behind it and is not the player's selected door
#' @param 
#'		Returns an integer between 1-3 that is not selected value
#' @return 
#'		Results will  return an integer betwen 1-3
#' @examples
#'		"The opened goat door: 1"
#' @export


#' opened.goat.door <- open_goat_door( this.game, my.initial.pick )



#' @title
#'		Unit Test: Game Outcome
#' @description
#'		Simulates what would happen in both scenarios where the player either stays or changes door
#' @details
#'		Simulation will provide two outputs representing outcome if player stayed and outcome if player changed 
#' @param 
#'		This function outputs two integer between 1-3 representing final pick and string representing outcome "WIN" or "LOSE"
#' @return 
#'		Final selection and game outcome
#' @examples
#'		> paste0( "My final selection: ", my.final.pick.stay )
#' "My final selection: 3"
#' "WIN"
#' "My final selection: 2"
#' "LOSE"
#' @export



#' my.final.pick.stay <- change_door( stay=T, opened.door=opened.goat.door, a.pick=my.initial.pick )
#' my.final.pick.switch <- change_door( stay=F, opened.door=opened.goat.door, a.pick=my.initial.pick )

#' game.outcome.stay <- determine_winner( final.pick=my.final.pick.stay, game=this.game )
#' game.outcome.switch <- determine_winner( final.pick=my.final.pick.switch, game=this.game )




