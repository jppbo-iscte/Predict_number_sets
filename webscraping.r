library(rvest)
library(stringr) #Replace strings
library(dplyr)
library(xml2)

#################################################
############  Data Understanding ################
#################################################

#Sets the script's folder as the reference folder
#This allows the usage of relative paths for local files that will work on any machine
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Sets the Main URL. To be re-used in the script
main_url <- "https://www.atptour.com"

#Sets the number of seconds that the script will sleep before
#executing a new HTTP request
sleep_seconds <- 0.5

#Gets an external list of Spanish cities.
#Some years being scraped only lists the city where the match was played (Example 1999)
#Source: https://simplemaps.com/data/es-cities
spain_cities <- read.csv("../Data/spanish_cities.csv")

#Gets an external list of Countries
#On the player data, the country can be scraped on two locations
#Via the Ranking IOC 3 Letter Country Code or via the birthplace.
#This File will translate the IOC 3 Letter Code into the country name
#Source: https://github.com/johnashu/datacamp/blob/master/medals/Summer%20Olympic%20medalists%201896%20to%202008%20-%20IOC%20COUNTRY%20CODES.csv
countries <- read.csv("../Data/countries.csv")

#Years that the script will scrape
# years <- list(2017, 2018, 2019, 2021, 2022)
years <- as.list(c(2022:2022))
# years <- list(2007)

#Create the Players Data Frame
#This Data Frame stores the players unchanging data.
#
#This makes possible to collect the info only once, instead of opening the players'
#details every time he plays.
players <- data.frame(
                 name=character(), 
                 full_url=character(), 
                 height_in_cm=numeric(), 
                 country_of_origin=character(),
                 hand=character(),
                 birthday=as.Date(character()),
                 stringsAsFactors=FALSE)

#---------!!!!!!!!!---------
#Uncomment the line below if a Players dataset was already recorded
#This will avoid making unnecessary HTTP requests
#
#players <- read.csv("../Data/players.csv")
#---------!!!!!!!!!---------

#Create the Players Data Frame
#This Data Frame stores all the players' rankings through time
#
#This makes possible to collect the info only once, instead of opening the players'
#rankings every time he plays.
players_rankings <- data.frame(
  full_url=character(), 
  date=as.Date(character()),
  rankings=numeric() 
)

#---------!!!!!!!!!---------
#Uncomment the next two lines if a Player Rankings dataset was already recorded
#This will avoid making unnecessary HTTP requests
#
# players_rankings <- read.csv("../Data/players_rankings.csv")
# players_rankings$date <- as.Date(players_rankings$date)
#---------!!!!!!!!!---------

#Supporting Functions
#This function is used to open the individual URL Links
#It's useful to avoid stop the script when a page fails to open (When it doesn't exists, for example)
readUrl <- function(url) {
  
  #Since a HTTP Request will be performed, first sleep for a pre-determined number of seconds
  Sys.sleep(sleep_seconds)
  print(paste("Opening URL", url))
  #To avoid stopping the script in case of errors
  #we use a Try Catch block
  out <- tryCatch(
    {
      #If the URL is parsed successfully,
      #returns its contents
      return(url %>% as.character() %>% read_html())
    },
    
    #if the URL fails for any reason
    error=function(cond) {
      #returns NA so the script can keep running as usual
      return(NA)
    }
  )
  return(out)
}

#This function opens the Players' Details and return them processed in a consistent format
#to be used by the script
#
#This code was turned in to a Function to reduce the main loop's size.
scrape_players_details <- function(player_winner_full_url) {
  
  #Some players have no info on the Overview page, but the Bio page has the same info and is always consistent.
  #It's a bug on the website that is easily circumvented using a replace
  #Replaces the Overview to Bio and opens the URL
  player_html <- readUrl(str_replace(player_winner_full_url, "overview", "bio"))
  
  #If the Players Page opens successfully
  if (!is.na(player_html)) {
    
    #Parse the Player's name and clean Whitespaces (Inside and around) 
    name <- player_html %>% html_nodes(".player-profile-hero-name") %>% html_text()
    name <- clear_all_whitespaces(name)
    
    #Get the Player's Height HTML Node from the page
    height_in_cm_node <- player_html %>% html_nodes(".table-height-cm-wrapper")

    #If a Height Node is present on the page
    if (length(height_in_cm_node) > 0) {
      #Extract the Text
      height_in_cm <- height_in_cm_node %>% html_text()
      #Remove the "cm"
      height_in_cm <- str_replace_all(height_in_cm, "[(cm)]", "")  
      
    #If the page has no Height Node
    } else {
      #The Height is set to NA  
      height_in_cm <- NA
    }
    
    #Scrape the Player's Origin Via the Ranking Section.
    #The Birthplace is not always available. But the Ranking Country is  
    
    #Scrape the Country Code that the player represents
    country_of_origin_via_code <- player_html %>% html_nodes(".player-flag-code") %>% html_text()
    #Cross-check the Country Code with the IOC Countries Dataframe
    country_of_origin_via_code <- countries[countries$NOC == country_of_origin_via_code,]$Country
  
    #Scrape the Player's Origin Via his birthplace
    country_of_origin_via_birthplace <- player_html %>%
      html_nodes(".player-profile-hero-table tr") %>% .[2] %>%
      html_nodes(".wrap") %>% .[1] %>% html_nodes(".table-value") %>% html_text()
    
    #Cleans the Whitespace from the Country (Inside and Around) 
    #Splits the birthplace using the pattern ", ".
    #The last group from the split usually is the country
    country_of_origin_via_birthplace <- clear_all_whitespaces(country_of_origin_via_birthplace)
    country_of_origin_via_birthplace <- str_split(country_of_origin_via_birthplace, ", ", simplify = FALSE)[[1]][2]
    
    #If the Country via the Country Code is available, use it
    if (length(country_of_origin_via_code) > 0) {
      country_of_origin <- country_of_origin_via_code
    
    #If the Country via the Country Code is NOT available
    #but the Countri via Birthplace is, use the Country via Birthplace
    } else if (length(country_of_origin_via_birthplace) > 0) {
      country_of_origin <- country_of_origin_via_birthplace

    #If none are available, sets the Country as NA
    } else {
      country_of_origin <- NA
    }
    
    #Scrape the HTML node that usually contains the Hand Information
    hand_node <- player_html %>% 
      html_nodes(".player-profile-hero-table tr") %>% .[2] %>%
      html_nodes(".wrap") %>% .[2] %>% html_nodes(".table-value") %>% html_text()
    
    #Clears the Whitespace from the Hand Node
    hand <- clear_outer_whitespaces(hand_node)
    #Splits the hand information using the pattern ", "
    #and record the first group from the split
    hand <- str_split(hand, ", ", simplify = FALSE)[[1]][1]

    #Scrapes the HTML node that usually holds the player's birthday
    #and gets the text contents
    birthday <- player_html %>% html_nodes(".table-birthday") %>% html_text()
    
    #If the Birthday node contains anything
    if (length(birthday) > 0 ) {
      
      #Removes all Whitespaces
      birthday <- clear_all_whitespaces(birthday)
      #Replaces any parenthesis with nothing
      birthday <- str_replace_all(birthday, "[()]", "")
      #Replaces dots as separators to dashes to keep consistency
      birthday <- str_replace_all(birthday, "[.]", "-")      
    
    #If the Birthay Node is empty, sets the birthday as NA
    } else {
      birthday <- NA
    }
  
  #If the Player's page doesn't exists or returns an error
  #Sets all player data to NA
  } else {
    name <- height_in_cm <- country_of_origin <- hand <- birthday <- NA
  }
  
  #Create a list with a consistent structure of processed values for the target player
  #to be returned to the main loop
  new_player_list = list("name"=name, 
                         "full_url"=player_winner_full_url,
                         "height_in_cm"=height_in_cm,
                         "country_of_origin" =country_of_origin,
                         "hand"=hand,
                         "birthday"=birthday)
  
  
  #Return to the list to the main loop
  return(new_player_list)
}


#This function opens the Players' Ranking Details and return them processed in a consistent format
#to be used by the script
#
#This code was turned in to a Function to reduce the main loop's size.
scrape_players_ranking_details <- function(player_winner_full_url) {

  #The Player Rankings URL follows the same structure as the 
  #Player's overview Page. Just changing the trailing information
  player_winner_rankings_url <- str_replace(player_winner_full_url, "overview", "rankings-history")
  
  #Opens the URL
  player_rankings_html <- readUrl(player_winner_rankings_url)
  
  #If the Rankings Page returns any content
  if (!is.na(player_rankings_html)) {  
  
    #Scrapes the HTML node that contains the table with the complete rankings
    player_rankings_nodes <- player_rankings_html %>% html_nodes("#playerRankHistoryContainer table.mega-table")
    
    #If the Table Node contains anything
    if (length(player_rankings_nodes) > 0) {
      
      #Gets the Node and transform it in a table structure that 
      #can be transformed by the script
      player_rankings <- player_rankings_nodes %>% html_table()
      
      #Transforms the table in a R Standard Data.Frame
      player_rankings <- as.data.frame(player_rankings)
  
      #Transform the Date that comes as String to a standard R Date Format
      player_rankings$Date <- as.Date(player_rankings$Date, format="%Y.%m.%d")
      
      #Renames the Columns to keep the information consistent and clear
      player_rankings <- player_rankings %>% 
        dplyr::rename(date = Date, rankings = Singles)
      
      #Adds a column with the player's URL
      #To make it searchable and unique
      player_rankings$full_url <- player_winner_full_url
      
      #Removes the "Doubles" Rankings. It's not relevant for the script
      player_rankings <-subset(player_rankings, select = -c(Doubles) )
    
    #If the page is empty or malformed, creates the Data Row with empty information
    } else {
      
      #Creates an empty Player Rankings Data Frame
      player_rankings <- data.frame(
        full_url=character(), 
        date=character(),
        rankings=numeric() 
      )
  
      #Create an empty row so the script knows that this player was already parsed
      #but has no information
      player_rankings[1,] <- c("full_url"=player_winner_full_url, "date"=NA, "rankings"=NA)
      
    }
  
  #If the page returns an error, creates the Data Row with empty information    
  } else {
    
    #Creates an empty Player Rankings Data Frame
    player_rankings <- data.frame(
      full_url=character(), 
      date=character(),
      rankings=numeric() 
    )
    #Create an empty row so the script knows that this player was already parsed
    #but has no information    
    player_rankings[1,] <- c("full_url"=player_winner_full_url, "date"=NA, "rankings"=NA)
  }    
  
  #Returns the Standard Player Rankings DataFrame
  #to the main loop
  return(player_rankings)    

}

#This function clears the Whitespace from around the strings
#Additionally checks for Tabs and Newlines
#It was transformed in a function since it's used many times
clear_outer_whitespaces <- function(str) {
  
  #Clear all white spaces from the argument's start and end
  str <- trimws(str, which = c("both"), whitespace = "[ \t\r\n]")  
  
}

#This function gets the multiple Whitespace from between the strings
#and transforms them in a single space. Additionally checks for Tabs and Newlines.
#It was transformed in a function since it's used many times
clear_inner_whitespaces <- function(str) {
  
  #Replaces all spaces, tabs, and new lines by a specified character.
  #In our case: #
  str <- str_replace_all(str, pattern="[ \t\r\n]", repl="#")
  
  #The pattern (?:) creates a capture group that matches any amount of the character
  #defined earlier and replaces it with a single space
  str <- str_replace_all(str, pattern="((?:#)+)", repl=" ")
}

#This functions joins the two types of Whitespace Clearing Functions above
clear_all_whitespaces <- function(str) {
  str <- clear_outer_whitespaces(str)
  str <- clear_inner_whitespaces(str)
}


#This is the script's main function.
#It loops through the years interval list set in the beginning
for (year in years) {
  
  print(paste("Starting", year))
  
  #Scrape the HTML code with all ATP and Challenger Matches for the current loop's year.
  #If Empty (""), scrapes the ATP, otherwise scrapes the Challenger 
  for (tourney_type in list("", "&tournamentType=ch")) {
  
    #Create the Matches Data Frame
    #this Data Frame is reset every year loop (After being saved to CSV).
    #
    #This increases the script reliability since, if it fails, the last complete year
    #have already been saved.
    matches <- data.frame(
      tourney_badge=character(),
      tourney_name=character(),
      date=as.Date(character()),
      pavement=character(),
      prize=character(),
      round=character(),
      player=character(),
      player_ranking_last_month=numeric(),
      opponent=character(),
      opponent_ranking_last_month=numeric(),
      sets_raw=character()
    )      
    
    #Opens the HTML page with all tourneys for the Year being looped
    main_html <- readUrl(paste0(main_url ,"/en/scores/results-archive?year=", year, tourney_type))
    
    #Create a list with all Table Rows with individual tournaments
    tournaments <- main_html %>% html_nodes(".results-archive-table tbody tr")
    
    #Loops through the individual table rows (Tourneys)
    for (tourney in tournaments) {
      
      #Get the table column that contains the tourney details page link
      #It's the fifth item with the class "tourney-details" (index: 5)
      result_cell <- tourney %>% html_nodes(".tourney-details") %>% .[5] %>% html_node("a")

      #Get the link for the tourney details page
      tourney_partial_url <- ifelse(length(result_cell) > 0, html_attr(result_cell, name="href"), NA)
      
      #This conditional filters out tournaments that were cancelled or didn't happen yet
      #(Result button don't exist) href=NA)
      if (!is.na(tourney_partial_url)) {
        
        #Concatenate the main URL with the extracted one
        #adding a filter to get only the Singles Matches
        full_tourney_url <- paste0(main_url, tourney_partial_url, "?matchType=singles")
        
        #Scrape the Tourney's Name from the table
        tourney_name <- clear_all_whitespaces(tourney %>% html_nodes(".tourney-title") %>% html_text())
        
        #Gets the Tourney Location Text and clears its Whitespace
        tourney_location <- clear_all_whitespaces(tourney %>% html_nodes(".tourney-location") %>% html_text())
        #Splits the Tourney Location using the pattern ", "
        #Usually the Country is the last group on the resulting list
        tourney_location_split <- str_split(tourney_location, ", ", simplify = FALSE)[[1]]
        
        #If the location has more than one value split by a comma
        #usually means that it follows the structure City, Country
        if (length(tourney_location_split) > 1) {
          
          #Set the Tourney Country to the last value of the split function
          tourney_country <- tourney_location_split[length(tourney_location_split)]
  
        #If the location has only one value split by a comma (Usually the City)
        } else {
          
          #Checks on the Spain Cities Data Frame if the city name exists as a city in Spain
          tourney_country <- spain_cities[spain_cities$city == tourney_location_split,]$country
          
          #If the City name is found on the Data Frame, we'll double check it
          #since Spain, Mexico, and Argentina shares cities with the same name
          if (length(tourney_country) > 0 ) {
          
            print(paste("Country for Tourney '", tourney_name,"' couldn't be determined. Opening Tourney details."))
            
            #Gets the Tournament Details URL node
            tourney_overview_partial_url <- tourney %>% html_nodes(".tourney-title") %>% html_attr(name="href")

            #Check if the Tournament URL exists
            if (length(tourney_overview_partial_url) > 0) {
              #Opens the Tournament URL to check if there's any additional information on the country
              tourney_overview_page <- readUrl(paste0(main_url, tourney_overview_partial_url))
              
              #If the Tourney Details Page returns an HTML page
              if (!is.na(tourney_overview_page)) {
                
                #Gets the Tourney Location Details and clean the Whitespaces
                tourney_location <- clear_all_whitespaces(tourney_overview_page %>% html_nodes(".player-profile-hero-dash .hero-date-range") %>% .[1] %>% html_text())
                
                #Splits the Tourney Location using the comma as separator
                tourney_location_split <- str_split(tourney_location, ", ", simplify = FALSE)[[1]]
                
                #If the location has more than one value split by a comma
                #usually means that it follows the structure City, Country                
                if (length(tourney_location_split) > 1) {
                  
                  #Set the Tourney Country to the last value of the split function
                  tourney_country <- tourney_location_split[length(tourney_location_split)]            
                
                }
              
              #If the Tourney Details Page is empty
              } else {
                
                #Sets the Tourney Country as NA
                tourney_country <- "NA"
                
              }              
              
            #If the Tourney Details Page doesn't exist
            } else {
              #Sets the Tourney Country as NA
              tourney_country <- "NA"
            }
          
          #If the City is not a recognizable Spanish City name
          } else {
            #Sets the Tourney Country as NA
            tourney_country <- "NA"
          }
            
        }
        
        #If the Tourney Country is Spain and the Tourney was not suspended
        if (tourney_country == "Spain" && !grepl("(Suspended)", tourney_name, fixed=TRUE)) {
        
          print(full_tourney_url)
          
          #Opens the HTML page with all matches for the Tourney being looped
          tourney_html <- readUrl(full_tourney_url)
          
          #Gets the Tourney has a Header with basic information
          tourney_header_node <- tourney_html %>% html_nodes(".tourney-results-wrapper")
          
          #If the Tourney has the basic information (Dates, Prize, Ground, etc.)
          if (length(tourney_header_node %>% html_nodes("tr")) > 0) {
            
            #Scrapes the Tourney Badge. It's the Tourney Category/Type
            tourney_badge_node <- tourney_html %>% html_nodes(".tourney-badge-wrapper img")
            
            #If the Tourney Badge exists
            if (length(tourney_badge_node) > 0) {
              
              #Sets the Tourney Badge Column as the image name
              tourney_badge <- tourney_badge_node %>% html_attr(name="src")

            #If the Tourney Badge doesn't exist
            } else {
              #Sets the Tourney Badge as NA
              tourney_badge <- NA
            }
              
            #Scrapes the text from the Tourney Dates
            tourney_dates <- clear_all_whitespaces(tourney_header_node %>% html_nodes(".tourney-dates") %>% html_text())
            
            #Separates the Dates in Start and End Dates
            tourney_date_from <- as.Date(str_split(tourney_dates, " - ", simplify = FALSE)[[1]][1], format="%Y.%m.%d")
            tourney_date_to <- str_split(tourney_dates, " - ")[[1]][2] 
            
            #Scrapes text from the Pavement Node
            pavement <- clear_all_whitespaces(tourney_html %>% html_nodes(".tourney-details-table-wrapper tr .tourney-details") %>% .[2] %>% html_node(".info-area") %>% html_text())
            #Scrapes text from the Prize Node
            monetary_prize <- clear_all_whitespaces(tourney_html %>% html_nodes(".tourney-details-table-wrapper .tourney-details.prize-money .item-value") %>% html_text())
            
            #If the Tourney Matches Page has a table with matches
            if (tourney_html %>% html_nodes("table.day-table") %>% length() > 0 ) {
              
              #Scrapes the Tourney Match Rounds in a list format
              tourney_matches_rounds <- tourney_html %>% html_nodes("table.day-table thead")
              #Sets an index to detect which tourney round is currently being looped
              tourney_matches_rounds_loop_index <- 1 
              
              #Loops through each Match Round in order to get the matches inside
              for (match_round in tourney_matches_rounds) {
                
                #Scrapes the Match Round name (Final, SemiFinals, 1st Round Qualifying, etc.)
                match_round <- clear_all_whitespaces(match_round %>% html_text())
                print(paste0("   ",match_round))
                
                #Gets all the matches from the current round being looped (Index)
                tourney_matches <- tourney_html %>% html_nodes("table.day-table tbody") %>% .[tourney_matches_rounds_loop_index] %>% html_nodes("tr")
                
                #Loops through all matches inside the current Round
                for (match in tourney_matches) {
                
                  #Gets the HTML node that contains the sets results
                  number_of_sets_node <- match %>% html_nodes(".day-table-score a")
                  
                  #Checks if there a node was found
                  if (length(number_of_sets_node) > 0) {
                    
                    #Selects the nodes that records the tie brakes
                    #And remove them
                    tie_break_nodes <- number_of_sets_node %>% html_nodes("sup")
                    xml_remove(tie_break_nodes)
                    
                    #Records the Sets content with the whitespaces cleared
                    sets_raw <- clear_all_whitespaces(number_of_sets_node %>% html_text())
                    
                  #If no nodes are found
                  } else {
                    #Sets the Sets variable to NA
                    sets_raw <- NA
                  }
                  
                  #Scrapes both the Player and Opponent HTML nodes
                  player_winner_node <- match %>% html_nodes(".day-table-name") %>% .[1]
                  player_defeated_node <- match %>% html_nodes(".day-table-name") %>% .[2]
                  
                  #Loop to gather the Player and Opponent Information
                  #Since the same data has to be gathered from both of them,
                  #this loop avoids codes duplication
                  for (player in list("player_winner", "player_defeated")) {
                    
                    #Gets the Node of the Player being looped
                    player_reference <- get((paste0(player, "_node")))
                    
                    #Assigns the Player Name Variable
                    assign(
                      (paste0(player, "_name")),
                      clear_all_whitespaces(player_reference %>% html_text())
                    )
                    
                    #Assigns by default NA to the Player's Last Month Rankings
                    #The script will try to change this variable on the next block of code if the player has rankings
                    #This is to avoid code failures when, for example: Matches where the player is BYE or has no URL.
                    assign(
                      paste0(player, "_last_month_ranking"), NA
                    )
                    
                    #Gets the link where the player details are stored
                    player_link <- player_reference %>% html_nodes("a")
                    
                    #If the links exists and has information
                    if (length(player_link) > 0) {
                      
                      #If the link has the "overview" information, means it's well formed
                      #The link is not valid otherwise
                      if (grepl("/overview", player_link)) {
                        
                        #Gets the url from the Player Link Node.
                        #This URL is just a partial one, and has to be joined
                        #with the Main URL before being called
                        assign(
                          paste0(player, "_partial_url"),
                          player_link %>% html_attr(name="href")
                        )                      
                        
                        # When the player's name has a . (dot), the url is malformed on the website
                        # This replaces the dot on the player_partial_url
                        # Example: https://www.atptour.com/en/players/alex-bogomolov-jr./b842/overview
                        assign(
                          paste0(player, "_partial_url"),
                          str_replace(get(paste0(player, "_partial_url")), "[.]", "")
                        )
  
                        #Sets the player's full url by joining the partial and the main URLs
                        assign(
                          paste0(player, "_full_url"),
                          paste(main_url, get(paste0(player, "_partial_url")), sep="") 
                        )
                        
                        #Checks, via the URL, if the player already exists in the Players Dataset
                        if (!any(players$full_url == get(paste0(player, "_full_url")))) {
                          
                          # If it doesn't exists, call the scrape_players_details() Function
                          #
                          # Return Format:
                          # "name", "full-url", "height_in_cm", "country_of_origin", "hand", "birthday"
                          players[nrow(players) + 1,] <- scrape_players_details(get(paste0(player, "_full_url")))
                          
                        }
                        
                        #Checks, via the URL, if the player already has the rankings recorded on
                        #the Rankings Dataset
                        if (!any(players_rankings$full_url == get(paste0(player, "_full_url")))) {
                          
                          # If it doesn't exists, call the scrape_players_ranking_details() Function
                          #
                          # Return Format:
                          # "full_url", "date", "rankings"                          
                          players_rankings <- rbind(players_rankings, scrape_players_ranking_details(get(paste0(player, "_full_url"))))
                          
                        }
                        
                        #Selects all rankings that are from the player
                        #And are recorded between 30 and 60 days before the tournament
                        assign(
                          paste0(player, "_rankings"),
                          players_rankings[
                            (
                              (players_rankings$date <= (tourney_date_from - 30) & players_rankings$date >= (tourney_date_from - 60)) &
                                players_rankings$full_url == get(paste0(player, "_full_url"))),]                        
                        )
  
                        #Removes the Rankings that are NA
                        assign(
                          paste0(player, "_rankings"),
                          get(paste0(player, "_rankings"))[!(is.na(get(paste0(player, "_rankings"))$rankings)),]
                        )
                        
                        #If there are rankings left 
                        if (nrow(get(paste0(player, "_rankings"))) > 0) {
                          #Selects the newer ranking on the list
                          assign(  
                            paste0(player, "_last_month_ranking"),
                            get(paste0(player, "_rankings"))[which.max(get(paste0(player, "_rankings"))$date),]$rankings                  
                          )
                        #If no rankings match the criteria above
                        } else {
                          #Assigns NA to the player's ranking
                          assign(  
                            paste0(player, "_last_month_ranking"),
                            NA  
                          )
                        }                      
                      
                      }                    
                    
                    }
                    
                  } 
    
                  print(paste("    ", player_winner_name,"x",player_defeated_name))
                  
                  #Sets up a match Data Frame with all the information gathered earlier
                  new_match_list = list("tourney_badge" = tourney_badge,
                                        "tourney_name"= tourney_name,
                                        "date"= tourney_date_from,
                                        "pavement"= pavement,
                                        "prize"= monetary_prize,
                                        "round"= match_round,
                                        "player"= player_winner_name,
                                        "player_ranking_last_month"= player_winner_last_month_ranking,
                                        "opponent"= player_defeated_name,
                                        "opponent_ranking_last_month"= player_defeated_last_month_ranking,
                                        "sets_raw" = sets_raw
                                        #,"number_of_sets"= number_of_sets
                  )

                  #Inserts the new created match in the matches Data Frame
                  matches[nrow(matches) + 1,] <- new_match_list
                  
                }
                
                #When the loop reach this points it means that all games in a round were parsed
                #Adds an index so the loop can start the new round
                tourney_matches_rounds_loop_index <- tourney_matches_rounds_loop_index + 1
              }
              
            }
          
          }
          
        }
        
      }
        
    }
    
    #Gets the names of columns of the matches Data Frame
    #This variable is to store columns that won't change name
    matches_columns <- colnames(matches)  
    
    #Joins the players information for the winner player
    matches <- left_join(matches, players, by=c("player"="name"))
    
    #Renames all new joined columns with the prefix player_
    #Except the column names of the original Data Frame
    matches <- matches %>%
      rename_with(~paste0("player_", .), -!!matches_columns)
    
    #Joins the players information for the defeated player
    matches_columns <- colnames(matches)  
    
    #Renames all new joined columns with the prefix oponnent_
    #Except the column names of the original Data Frame    
    matches <- left_join(matches, players, by=c("opponent"="name"))
    matches <- matches %>%
      rename_with(~paste0("opponent_", .), -!!matches_columns)
    
    #Records a suffix to differentiate between ATP and Challenger files
    year_csv_suffix <- ifelse(tourney_type == "", "", "_ch")
    
    #Writes the CSV on disk
    write.csv(matches, paste0("../Data/Years/", year, year_csv_suffix, ".csv"), row.names = FALSE)
    print(paste0("Finished Year ", year," for ", ifelse(tourney_type == "", "ATP", "Challenger")," Successfully"))
    # stop("The script was programmed to end here")
  }
  
}

#Writes to CSV all the stored Player data.
#This can be used later if the script needs running again
write.csv(players, paste0("../Data/players.csv"), row.names = FALSE)

#Writes to CSV all the stored Player Rankings data.
#This can be used later if the script needs running again
write.csv(players_rankings, "../Data/players_rankings.csv", row.names = FALSE)
