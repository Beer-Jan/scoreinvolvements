library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

cat2pos <- function(catPosition) {
  
  dplyr::case_when(
    catPosition == 1 | catPosition == 6 ~ "Midfielder",
    catPosition == 2 | catPosition == 7 ~ "Forward",
    catPosition == 4 | catPosition == 5 ~ "Defender",
    catPosition == 3                    ~ "Ruck",
    .default = "Midfielder"
  )
  
}

# Converts team names from the AFL style to the AFLTables style
convert_team_names <- function(name) {
  name <- str_replace(name, "Adelaide Crows", "Adelaide")
  name <- str_replace(name, "Geelong Cats", "Geelong")
  name <- str_replace(name, "Gold Coast SUNS", "Gold Coast")
  name <- str_replace(name, "GWS GIANTS", "GWS")
  name <- str_replace(name, "Sydney Swans", "Sydney")
  name <- str_replace(name, "West Coast Eagles", "West Coast")
}

# Brisbane, Gold Coast, Western Bulldogs look awful
teamColours <- function(name) {
  
  if (name == "Adelaide") {
    return(c("#002b5c", "#e21937", "#ffd200"))
  } 
  if (name == "Brisbane") {
    return(c("#a30046", "#fdbe57", "#0055a3"))
  }
  if (name == "Carlton") {
    return(c("#0e1e2d", "white", "white"))
  }
  if (name == "Collingwood") {
    return(c("black", "white", "white"))
  }
  if (name == "Essendon") {
    return(c("black", "#cc2031", "#cc2031"))
  }
  if (name == "Fremantle") {
    return(c("#2a0d54", "white", "white"))
  }
  if (name == "Geelong") {
    return(c("white", "#002b5c", "#002b5c"))
  }
  if (name == "Gold Coast") {
    return(c("#e02112", "#fcdb06", "#1379c1"))
  }
  if (name == "GWS") {
    return(c("#ff7900", "#313c42", "#313c42"))
  }
  if (name == "Hawthorn") {
    return(c("#4d2004", "#fbbf15", "#fbbf15"))
  }
  if (name == "Melbourne") {
    return(c("#0f1131", "#cc2031", "#cc2031"))
  }
  if (name == "North Melbourne") {
    return(c("#1a3b8e", "white", "white"))
  }
  if (name == "Port Adelaide") {
    return(c("black", "#00a2a5", "white"))
  }
  if (name == "Richmond") {
    return(c("black", "#ffd200", "#ffd200"))
  }
  if (name == "St Kilda") {
    return(c("black", "#ed1b2f", "white"))
  }
  if (name == "Sydney") {
    return(c("#e1251b", "white", "white"))
  }
  if (name == "West Coast") {
    return(c("#003087", "#f2a900", "#f2a900"))
  }
  if (name =="Western Bulldogs") {
    return(c("#033395", "#bd002b", "white"))
  }
  
}

createSIGraph <- function(year, team, minGames = 9, colours = "default") {

  playerPositions <- read.csv("playerpositions.csv")
  playerStats <- read.csv("allscoreinvolvements.csv")
  
  pos2scoreInvolvements <- playerPositions %>%
    filter(season == year) %>%
    mutate(lastName = str_replace(lastName, "^([A-Z])([A-Z])", "\\1'\\2"))
  
  playerStats <- playerStats %>%
    filter(season == year) %>%
    mutate(teamName = convert_team_names(teamName)) %>%
    inner_join(pos2scoreInvolvements, by=c("firstName", "lastName", "teamName")) %>%
    mutate(name = paste(firstName, lastName),
           teamName = str_replace(teamName, "Brisbane Lions", "Brisbane"),
           position = cat2pos(position)) %>%
    filter(teamName == team) %>%
    select(c(teamName, name, scoreInvolvements, position, gamesPlayed))
  
  customColours <- c("Defender" = "blue", "Forward" = "red", "Midfielder" = "#02e63f", "Ruck" = "yellow")
  
  if (colours == "team") {
    colourSet <- teamColours(team)
  } else {
    colourSet <- c("white", "black", "black")
  }
  
  toPlot <- playerStats %>% 
    filter(gamesPlayed >= minGames)
  
  ggplot(toPlot, 
         aes(x = reorder(name, -scoreInvolvements), y = scoreInvolvements, fill = position)) +
    geom_bar(stat = "identity", colour = "black", linewidth = 0.1, width = 1, position = position_dodge(width = 0.8)) +
    labs(title = paste("Score Involvements for", team, "Players in", year),
         x = "Player",
         y = "Avg % Score Involvements / game",
         fill = "Position") +
    scale_fill_manual(values = customColours) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = colourSet[3]),
          axis.text.y = element_text(colour = colourSet[3]),
          axis.title.x = element_text(colour = colourSet[2]),
          axis.title.y = element_text(colour = colourSet[2]),
          panel.grid.major.y = element_line("black"),
          panel.grid.minor.y = element_line("black"),
          plot.title = element_text(colour = colourSet[2], size = 15),
          panel.grid.major.x = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
          plot.background = element_rect(colour = colourSet[1], fill = colourSet[1]),
          panel.background = element_rect("#e9e9e9"),
          legend.position = "inside",
          legend.position.inside = c(0.95, 0.945),
          legend.justification = c("right", "top"),
          legend.background = element_rect(fill = "white", color = "black"),
          plot.margin = margin(10, 10, 10, 20)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 41))

}
