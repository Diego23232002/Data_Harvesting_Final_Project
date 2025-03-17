library(scrapex)
library(rvest)
library(httr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(xml2)

atp_link <- "https://www.atptour.com/es/rankings/singles?rankRange=0-100&region=all&dateWeek=Semana%20Actual&SortField=Ranking&SortAscending=True"
browseURL(prep_browser(atp_link))


# the website does not let you scrape it unless an user-agent is set.

html_atp_website <- httr::GET(atp_link, 
                          httr::add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/134.0.0.0 Safari/537.36 Edg/134.0.0.0
; Diego Fernandez / dieguitus23@gmail.com")) |>
  read_html()


# let´s try to retrieve the tables: there are only 2 tables and they seem the same, I´ll extract the 1st one.
# It looks like we can only extract in the tables the players that did not move up or down in the ranking.

all_tables <- html_atp_website |> html_table()

atp_table <- all_tables[[1]]
atp_table


# let´s try another way: Regular expressions: the // looks for every "span" and [@class='lastName'] means: 
# Select elements that have an attribute class with the value "lastName"
# xml_text only extracts the text part, the name of the players.

player_name <- xml_find_all(html_atp_website, "//span[@class='lastName']") |> 
  xml_text()
player_name

# let´s store it in a table:
atp_ranking <- tibble(
  ranking = 1:100,
  name = player_name)
atp_ranking
# Let´s now extract the next 100 players:
atp_link2 <-"https://www.atptour.com/es/rankings/singles?rankRange=101-200&region=all&dateWeek=Semana+Actual&SortField=Ranking&SortAscending=True"

html_atp_website2 <- httr::GET(atp_link2, 
                              httr::add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/134.0.0.0 Safari/537.36 Edg/134.0.0.0
; Diego Fernandez / dieguitus23@gmail.com")) |>
  read_html()

player_name2 <- xml_find_all(html_atp_website2, "//span[@class='lastName']") |> 
  xml_text()
player_name2

# let´s store it in a table:
atp_ranking2 <- tibble(
  ranking = 101:200,
  name = player_name2)
atp_ranking2

# Let´s extract the last 100 players
atp_link3 <-"https://www.atptour.com/es/rankings/singles?rankRange=201-300&region=all&dateWeek=Semana+Actual&SortField=Ranking&SortAscending=True"

html_atp_website3 <- httr::GET(atp_link3, 
                               httr::add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/134.0.0.0 Safari/537.36 Edg/134.0.0.0
; Diego Fernandez / dieguitus23@gmail.com")) |>
  read_html()

player_name3 <- xml_find_all(html_atp_website3, "//span[@class='lastName']") |> 
  xml_text()
player_name3

# let´s store it in a table:
atp_ranking3 <- tibble(
  ranking = 201:300,
  name = player_name3)
atp_ranking3

# Finally, let´s join the 3 tables and keep only the last name (surname)

final_ranking_table <- rbind(atp_ranking, atp_ranking2, atp_ranking3)
final_ranking_table

final_ranking_table <- final_ranking_table |> 
  mutate(name = gsub("^\\s+|\\s+$", "", name)) |>   # clean extra spaces
  mutate(name = sub(".*\\s+", "", name))
final_ranking_table
#-------------------------------------------------------------------------------
# Example with Sinner: extracting age, country, height and weight from his web. We have run into some problems 
# because the variables we want to extract are not visible. We suspect selenium is needed to read javascript code.

require(RSelenium)


# to run Chrome in headless mode
browser_capabilities <- list(
  chromeOptions =
    list(
      args = list("--headless")
    )
)

remDr <- rsDriver(port = 4444L, browser = "chrome")
driver <- remDr$client
driver$navigate("https://www.atptour.com/es/players/jannik-sinner/s0ag/overview")

Sys.sleep(10)


ul_element_left <- driver$findElement(using = "xpath", value = "//ul[contains(@class, 'pd_left')]")

ul_element_right <- driver$findElement(using = "xpath", value = "//ul[contains(@class, 'pd_right')]")



li_elements_left <- ul_element_left$findElements(using = "xpath", value = ".//li")
li_elements_right <- ul_element_right$findElements(using = "xpath", value = ".//li")


# Obtener el texto de todos los elementos <li> LEFT
li_texts <- sapply(li_elements_left, function(x) x$getElementText())
full_text <- paste(li_texts, collapse = " ")
full_text

# Obtener el texto de todos los elementos <li> RIGHT

li_texts_right <- sapply(li_elements_right, function(x) x$getElementText())
full_text_right <- paste(li_texts_right, collapse = " ")

# Extraer los valores específicos con expresiones regulares
country <- str_extract(full_text_right, "\\((.*?)\\)") |>  str_replace_all("[\\(\\)]", "")
age <- as.numeric(gsub(".*Edad\\s*(\\d+).*", "\\1", full_text))  # Extrae el número 23 (Edad)
weight <- as.numeric(gsub(".*Peso\\s*(\\d+).*", "\\1", full_text))  # Extrae el número 77 (Peso)
height <- as.numeric(gsub(".*Estatura\\s*(\\d+).*", "\\1", full_text))  # Extrae el número 191 (Estatura)

# Crear un data.frame con los valores extraídos
df <- data.frame(age = age, weight = weight, height = height, country = country)
df

driver$close()
remDr$server$stop()

# We were able to scrape the variables we need for the Sinner example,
# now we just need to replicate that with every player.
# (it is at the end, doesn´t work)
#------------------------------------------------------------------------------
# Now let's get the wins/looses in 2025
library(RSelenium)
remDr <- rsDriver(port = 4444L, browser = "chrome")
driver <- remDr$client
driver$navigate("https://www.atptour.com/es/stats/win-loss-index?indexCategory=all&timeCategory=ytd&country=all&sortBy=index&sortDirection=desc")

Sys.sleep(5)
page_source <- driver$getPageSource()[[1]]

html_content <- read_html(page_source)

rows <- html_content %>%
  html_nodes("tr")

# Loop through rows and extract each cell’s content
table_data <- lapply(rows, function(row) {
  row %>%
    html_nodes("td") %>%
    html_text(trim = TRUE)
})

table_winloss <- do.call(rbind, lapply(table_data, function(x) {
  if (length(x) == 4) x else c(x, rep("", 4 - length(x)))  
})) %>% as.data.frame()


colnames(table_winloss) <- c("Player", "Index", "Titles", "WinLoss")
table_winloss

# let´s clean the table:
table_winloss <- table_winloss |> 
  # Remove the number prefix from the Player column
  mutate(Player = gsub("^\\d+", "", Player)) |> 
  mutate(Player = trimws(Player)) |>  # Clean any extra spaces
  
  # Split the WinLoss column into Wins and Losses
  separate(WinLoss, into = c("Wins", "Losses"), sep = "-", convert = TRUE) 

table_winloss <- table_winloss|> 
  slice(-1) # remove the first row
  

driver$close()
remDr$server$stop()
#------------------------------------------------------------------------------
# the points won with first serve.
remDr <- rsDriver(port = 4444L, browser = "chrome")
driver <- remDr$client
driver$navigate("https://www.atptour.com/es/stats/individual-game-stats?factType=Aces&year=2024&surface=all&country=all&sortBy=percentage&sortDirection=desc")

Sys.sleep(5)
page_source <- driver$getPageSource()[[1]]

html_content <- read_html(page_source)

rows <- html_content %>%
  html_nodes("tr")

# Loop through rows and extract each cell’s content
table_data <- lapply(rows, function(row) {
  row %>%
    html_nodes("td") %>%
    html_text(trim = TRUE)
})

table_1st_serve <- do.call(rbind, lapply(table_data, function(x) {
  if (length(x) == 5) x else c(x, rep("", 5 - length(x)))  
})) %>% as.data.frame()


colnames(table_1st_serve) <- c("Player", "1st_serve_win_%", "Points_won", "Total_points", "Matches_played")
table_1st_serve


# Now, let´s clean the table:

table_1st_serve <- table_1st_serve |> 
  mutate(Player = gsub("^\\d+", "", Player)) |> 
  slice(-1)
table_1st_serve

driver$close()
remDr$server$stop()
#------------------------------------------------------------------------------
# the aces

remDr <- rsDriver(port = 4444L, browser = "chrome")
driver <- remDr$client
driver$navigate("https://www.atptour.com/es/stats/individual-game-stats?factType=Aces&year=2024&surface=all&country=all&sortBy=percentage&sortDirection=desc")

Sys.sleep(5)

page_source <- driver$getPageSource()[[1]]

html_content <- read_html(page_source)

rows <- html_content %>%
  html_nodes("tr")

# Loop through rows and extract each cell’s content
table_data <- lapply(rows, function(row) {
  row %>%
    html_nodes("td") %>%
    html_text(trim = TRUE)
})

table_aces <- do.call(rbind, lapply(table_data, function(x) {
  if (length(x) == 3) x else c(x, rep("", 3 - length(x)))  
})) %>% as.data.frame()


colnames(table_aces) <- c("Player", "Aces_number", "Matches_played")
table_aces


# Now, let´s clean the table:

table_aces <- table_aces |> 
  mutate(Player = gsub("^\\d+", "", Player)) |> 
  slice(-1)
table_aces

driver$close()
remDr$server$stop()
#------------------------------------------------------------------------------
# games won % (when the player has to serve)

remDr <- rsDriver(port = 4444L, browser = "chrome")
driver <- remDr$client
driver$navigate("https://www.atptour.com/es/stats/individual-game-stats?factType=Service-Games-Won&year=2024&surface=all&country=all&sortBy=percentage&sortDirection=desc")

Sys.sleep(5)

page_source <- driver$getPageSource()[[1]]

html_content <- read_html(page_source)

rows <- html_content %>%
  html_nodes("tr")

# Loop through rows and extract each cell’s content
table_data <- lapply(rows, function(row) {
  row %>%
    html_nodes("td") %>%
    html_text(trim = TRUE)
})

table_games_won_serve <- do.call(rbind, lapply(table_data, function(x) {
  if (length(x) == 5) x else c(x, rep("", 5 - length(x)))  
})) %>% as.data.frame()


colnames(table_games_won_serve) <- c(
  "Player", "%games_won(serve)", "games_won_serving", 
  "total_games_serving", "Matches_played")
table_games_won_serve


# Now, let´s clean the table:

table_games_won_serve <- table_games_won_serve |> 
  mutate(Player = gsub("^\\d+", "", Player)) |> 
  slice(-1)

driver$close()
remDr$server$stop()
#------------------------------------------------------------------------------
# games won % (when the player has to receive the serve)

remDr <- rsDriver(port = 4444L, browser = "chrome")
driver <- remDr$client
driver$navigate("https://www.atptour.com/es/stats/individual-game-stats?factType=Return-Games-Won&year=2024&surface=all&country=all&sortBy=percentage&sortDirection=desc")

Sys.sleep(5)

page_source <- driver$getPageSource()[[1]]

html_content <- read_html(page_source)

rows <- html_content %>%
  html_nodes("tr")

# Loop through rows and extract each cell’s content
table_data <- lapply(rows, function(row) {
  row %>%
    html_nodes("td") %>%
    html_text(trim = TRUE)
})

table_games_won_receiving <- do.call(rbind, lapply(table_data, function(x) {
  if (length(x) == 5) x else c(x, rep("", 5 - length(x)))  
})) %>% as.data.frame()


colnames(table_games_won_receiving) <- c(
  "Player", "%games_won(receive)", 
  "games_won_receiving", "total_games_receiving", "Matches_played")
table_games_won_receiving


# Now, let´s clean the table:

table_games_won_receiving <- table_games_won_receiving |> 
  mutate(Player = gsub("^\\d+", "", Player)) |> 
  slice(-1)

driver$close()
remDr$server$stop()
#------------------------------------------------------------------------------
# break points saved % 

remDr <- rsDriver(port = 4444L, browser = "chrome")
driver <- remDr$client
driver$navigate("https://www.atptour.com/es/stats/individual-game-stats?factType=Break-Points-Saved&year=2024&surface=all&country=all&sortBy=percentage&sortDirection=desc")

Sys.sleep(5)

page_source <- driver$getPageSource()[[1]]

html_content <- read_html(page_source)

rows <- html_content |> 
  html_nodes("tr")

# Loop through rows and extract each cell’s content
table_data <- lapply(rows, function(row) {
  row %>%
    html_nodes("td") %>%
    html_text(trim = TRUE)
})

table_break_points_saved <- do.call(rbind, lapply(table_data, function(x) {
  if (length(x) == 5) x else c(x, rep("", 5 - length(x)))  
})) %>% as.data.frame()


colnames(table_break_points_saved) <- c(
  "Player", "%break_points_saved", 
  "points_won", "total_points", "Matches_played")
table_break_points_saved




# Now, let´s clean the table:

table_break_points_saved <- table_break_points_saved |> 
  mutate(Player = gsub("^\\d+", "", Player)) |> 
  slice(-1)

driver$close()
remDr$server$stop()
#------------------------------------------------------------------------------
# Finally, let´s join all the tables
All_player_variables <-table_break_points_saved |> 
  full_join(table_games_won_receiving, by = "Player") |> 
  full_join(table_games_won_serve, by = "Player") |> 
  full_join(table_aces, by = "Player") |> 
  full_join(table_1st_serve, by = "Player") |> 
  full_join(table_winloss, by = "Player")

All_player_variables

All_player_variables <- All_player_variables |> 
  mutate(Player = gsub("^\\s+|\\s+$", "", Player)) |>   # clean extra spaces
  mutate(Player = sub(".*\\s+", "", Player))

# Let´s join the All_player_variables with the final_ranking_table table.

sss <- final_ranking_table |> left_join(All_player_variables, by = c("name" = "Player")) 

sss |> distinct(name)

# the end (for now)
#-------------------------------------------------------------------------------
require(RSelenium)

# to run Chrome in headless mode
browser_capabilities <- list(
  chromeOptions =
    list(
      args = list("--headless")
    )
)

remDr <- rsDriver(port = 4444L, browser = "chrome", extraCapabilities = browser_capabilities)
driver <- remDr$client
driver$navigate(atp_link)

Sys.sleep(10)


player_link_node <- driver$findElements(using = "xpath", "//li[@class='name']//a")

player_links <- sapply(player_link_node, function(node) {
  node$getElementAttribute("href")[[1]]
})
player_links

# Let´s create an empty list to store each player´s data
player_data <- list()



# Now that we can extract each player´s website, let´s iterate over each 
# player link and extract some variables. (it doesn´t work,)

for (link in player_links) {
  # Navigate to each player's profile page
  driver$navigate(paste0(atp_link, link)) 
  
  # Extract player details from 'pd_left' and 'pd_right' sections
  ul_element_left <- driver$findElement(using = "xpath", value = "//ul[contains(@class, 'pd_left')]")
  ul_element_right <- driver$findElement(using = "xpath", value = "//ul[contains(@class, 'pd_right')]")
  
  li_elements_left <- ul_element_left$findElements(using = "xpath", value = ".//li")
  li_elements_right <- ul_element_right$findElements(using = "xpath", value = ".//li")
  
  # Get text from each <li> in 'pd_left' and 'pd_right'
  li_texts_left <- sapply(li_elements_left, function(x) x$getElementText())
  li_texts_right <- sapply(li_elements_right, function(x) x$getElementText())
  
  # Combine all the texts into a single string
  full_text_left <- paste(li_texts_left, collapse = " ")
  full_text_right <- paste(li_texts_right, collapse = " ")
  
  # Extract specific values using regex from the full text
  country <- str_extract(full_text_right, "\\((.*?)\\)") |> str_replace_all("[\\(\\)]", "") # Extract the country
  age <- as.numeric(gsub(".*Edad\\s*(\\d+).*", "\\1", full_text_left))  # Extract age
  weight <- as.numeric(gsub(".*Peso\\s*(\\d+).*", "\\1", full_text_left))  # Extract weight
  height <- as.numeric(gsub(".*Estatura\\s*(\\d+).*", "\\1", full_text_left))  # Extract height
  
  # Store the extracted data for this player
  player_data[[link]] <- list(
    country = country,
    age = age,
    weight = weight,
    height = height
  )
}

# Let´s close the session


driver$close()
remDr$server$stop()
