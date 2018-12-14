

#code below is from Ewen Gallic#
# Author : Ewen Gallic http://editerna.free.fr/wp/
# Date : 2014-08-03

# Any comments are welcome.

library(grid)
library(ggplot2)
library(gridExtra)
library(gtable)


###############
## FUNCTIONS ##
###############

# http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
# Function to create circles
circle_fun <- function(center=c(0,0), diameter=1, npoints=500, start=0, end=2){
  tt <- seq(start*pi, end*pi, length.out=npoints)
  data.frame(
    x = center[1] + diameter / 2 * cos(tt),
    y = center[2] + diameter / 2 * sin(tt)
  )
}

# Gives y coordinates of the opposite side
rev_y <- function(y) 94-y

# Converts inches to feet
inches_to_feet <- function(x) x/12

# Given the angle theta and the court data frame,
# rotates the coordinates of the court by an angle theta
rotate_court <- function(court, theta=pi/2){
  court_r <- court
  court_r$x <- court_r$x / 180 * pi
  court_r$y <- court_r$y / 180 * pi
  matrice_r <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), ncol = 2)
  coords_r <- apply(court_r[,c("x","y")], 1, function(x) x %*% matrice_r)
  court_r$x <- coords_r[1,] ; court_r$y <- coords_r[2,]
  court_r$x <- court_r$x * 180 / pi
  court_r$y <- court_r$y * 180 / pi
  return(court_r)
}

# From x and y coordinates for a line (represented by a polygon here),
# a number of group and a short description
# creates a data.frame for this line
# in order to use it with ggplot2.
new_coords <- function(x, y, group, descri){
  new_coords_df <- data.frame(x = x, y = y)
  new_coords_df$group <- group
  new_coords_df$side <- 1
  group <- group + 1
  
  # The same thing for the opposite side
  new_coords_df2 <- data.frame(x = x, y = rev_y(y))
  new_coords_df2$group <- group
  new_coords_df2$side <- 2
  group <<- group + 1
  
  # On reunit les donnees
  new_coords_df <- rbind(new_coords_df, new_coords_df2)
  new_coords_df$descri <- descri
  
  return(new_coords_df)
}

###############
## THE COURT ##
###############

# 3 pts circle
cercle_3pts.out <- circle_fun(center = c(25,inches_to_feet(63)), diameter = (20+inches_to_feet(9))*2)
cercle_3pts.in <- circle_fun(center = c(25,inches_to_feet(63)), diameter = (20+inches_to_feet(7))*2)
# Basket circle
cercle_ce <- circle_fun(center = c(25,5+3/12), diameter = 1.5)
# Free throw circle
cercle_lf.out <- circle_fun(center = c(25,19), diameter = 6*2)
cercle_lf.in <- circle_fun(center = c(25,19), diameter = (6-1/6)*2)
# Middle circle
cercle_mil.out <- circle_fun(center = c(25,47), diameter = 6*2)
cercle_mil.in <- circle_fun(center = c(25,47), diameter = (6-1/6)*2)


group <- 1 # We assign the first group, and it gets incremented with each use of new_coords()
court <- new_coords(c(0-1/6,0-1/6,53 + 1/6,53 + 1/6), c(0 - 1/6,0,0,0 - 1/6), group = group, descri = "ligne de fond")
court <- rbind(court, new_coords(x = c(0-1/6,0-1/6,0,0), y = c(0,47-1/12,47-1/12,0), group = group, descri = "ligne gauche"))
court <- rbind(court, new_coords(x = c(50,50,50+1/6,50+1/6), y = c(0,47-1/12,47-1/12,0), group = group, descri = "ligne droite"))
court <- rbind(court, new_coords(x = c(47,47,53,53), y = c(28,28+1/6,28+1/6,28), group = group, descri = "marque entraineur droite"))
court <- rbind(court, new_coords(x = c(inches_to_feet(51),inches_to_feet(51),inches_to_feet(51)+1/6,inches_to_feet(51)+1/6), y = c(0,inches_to_feet(63),inches_to_feet(63),0), group = group, descri = "3pts bas gauche"))
court <- rbind(court, new_coords(x = c(50-inches_to_feet(51)-1/6,50-inches_to_feet(51)-1/6,50-inches_to_feet(51),50-inches_to_feet(51)), y = c(0,inches_to_feet(63),inches_to_feet(63),0), group = group, descri = "3pts bas droit"))
court <- rbind(court, new_coords(x = c(19,19,19+1/6,19+1/6), y = c(0,19,19,0), group = group, descri = "LF bas gauche"))
court <- rbind(court, new_coords(x = c(31-1/6,31-1/6,31,31), y = c(0,19,19,0), group = group, descri = "LF bas droit"))
court <- rbind(court, new_coords(x = c(19,19,31,31), y = c(19-1/6,19,19,19-1/6), group = group, descri = "LF tireur"))
court <- rbind(court, new_coords(x = c(22, 22, 28, 28), y = c(4-1/6,4,4,4-1/6), group = group, descri = "planche"))
court <- rbind(court, new_coords(x = c(cercle_3pts.out[1:250,"x"], rev(cercle_3pts.in[1:250,"x"])),
                                 y = c(cercle_3pts.out[1:250,"y"], rev(cercle_3pts.in[1:250,"y"])), group = group, descri = "cercle 3pts"))
court <- rbind(court, new_coords(x = c(cercle_lf.out[1:250,"x"], rev(cercle_lf.in[1:250,"x"])),
                                 y = c(cercle_lf.out[1:250,"y"], rev(cercle_lf.in[1:250,"y"])), group = group, descri = "cercle LF haut"))
court <- rbind(court, new_coords(x = c(19-0.5,19-0.5,19,19), y = c(7,8,8,7), group = group, descri = "marque 1 LF gauche"))
court <- rbind(court, new_coords(x = c(19-0.5,19-0.5,19,19), y = c(11,11+inches_to_feet(2),11+inches_to_feet(2),11), group = group, descri = "marque 2 LF gauche"))
court <- rbind(court, new_coords(x = c(19-0.5,19-0.5,19,19), y = c(14+inches_to_feet(2),14+inches_to_feet(4),14+inches_to_feet(2),14+inches_to_feet(2)), group = group, descri = "marque 3 LF gauche"))
court <- rbind(court, new_coords(x = c(19-0.5,19-0.5,19,19), y = c(17+inches_to_feet(4),17+inches_to_feet(6),17+inches_to_feet(6),17+inches_to_feet(4)), group = group, descri = "marque 4 LF gauche"))
court <- rbind(court, new_coords(x = c(31,31,31+0.5,31+0.5), y = c(7,8,8,7), group = group, descri = "marque 1 LF droite"))
court <- rbind(court, new_coords(x = c(31,31,31+0.5,31+0.5), y = c(11,11+inches_to_feet(2),11+inches_to_feet(2),11), group = group, descri = "marque 2 LF droite"))
court <- rbind(court, new_coords(x = c(31,31,31+0.5,31+0.5), y = c(14+inches_to_feet(2),14+inches_to_feet(4),14+inches_to_feet(4),14+inches_to_feet(2)), group = group, descri = "marque 3 LF droite"))
court <- rbind(court, new_coords(x = c(0-1/6,0-1/6,50+1/6,50+1/6), y = c(94/2-1/12,94/2, 94/2, 94/2-1/12), group = group, descri = "ligne mediane"))
court <- rbind(court, new_coords(x = c(31,31,31+0.5,31+0.5), y = c(17+inches_to_feet(4),17+inches_to_feet(6),17+inches_to_feet(6),17+inches_to_feet(4)), group = group, descri = "marque 4 LF droite"))
court <- rbind(court, new_coords(x = c(cercle_mil.out[250:500,"x"], rev(cercle_mil.in[250:500,"x"])),
                                 y = c(cercle_mil.out[250:500,"y"], rev(cercle_mil.in[250:500,"y"])), group = group, descri = "cercle milieu grand"))
court <- rbind(court, new_coords(x = cercle_ce[,"x"], y = cercle_ce[,"y"], group = group, descri = "anneau"))

# The data.frame containing all coordinates for the court is now complete.
head(court)
tail(court)

###########
## PLOTS ##
###########

# Whole court 
P <- ggplot() + geom_polygon(data = court, aes(x = x, y = y, group = group), col = "gray") +
  coord_equal() +
  ylim(-2,96) +
  xlim(-5,55) +
  scale_x_continuous(breaks = c(0, 12.5, 25, 37.5, 50)) +
  scale_y_continuous(breaks = c(0, 23.5, 47, 70.5, 94)) +
  xlab("") + ylab("") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), axis.title = element_blank()
  )




# Whole court with rotation
P_180 <- ggplot() + geom_polygon(data = rotate_court(court, theta = pi/2), aes(x = x, y = y, group = group), col = "black") +
  coord_equal() +
  xlim(-2,96) +
  ylim(-55,2) +
  scale_x_continuous(breaks = c(0, 23.5, 47, 70.5, 94)) +
  scale_y_continuous(breaks = c(0, -12.5, -25, -37.5, -50))

# Half court
P_half <- ggplot() + geom_polygon(data = court[court$side==1,], aes(x = x, y = y, group = group), col = "black") +
  coord_equal() +
  xlim(-3,54) +
  ylim(-3,50) +
  scale_y_continuous(breaks = c(0, 23.5, 47)) +
  scale_x_continuous(breaks = c(0, 12.5, 25, 37.5, 50)) +
  xlab("") + ylab("") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), axis.title = element_blank()
  )

#hot cold zones
library(shiny)

#setwd("~/analytics/NCAA.Shot.Chart.App/")

# ll <- 20.75
# unc$is.three <- ((shots$shot_y < ll) & (shots$shot_x > 22)) | (shots$r >= 2.75)
# 
# fullcourt = (1128 / 12)
# 
# widthcourt = 600 / 12

library(grid)
library(ggplot2)
library(gridExtra)
library(gtable)
library(readr)
library(tidyverse)
library(devtools)
library(bigrquery)
library(shiny)
library(plotly)


ui <- fluidPage(
  
  # App title ----
  titlePanel("NCAA Shot Chart"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      textInput(inputId = "player_full_name",
                label = "Search for any NCAA player since 2013 season:", 
                value = "Joel Berry II"), 
      selectizeInput(inputId = "year",
                     label = "Select Seasons:",
                     choices = c(2013:2018), multiple = TRUE, selected = 2017)
      
    ),
    

    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput("shotchart"), 
      textOutput("percentage"),
      dataTableOutput("shottable") 
      
      
    )
  )
)


quoter <- function(player){
  
  new <- paste("'", player, "'", sep="")
  return(new)
}
server <- function(input, output) {
  
  
  newroster <- reactive({
    
    
    roster <- sapply(input$player_full_name, FUN = quoter)
    
    roster <- (paste(roster, collapse =  ", "))
    roster <- gsub('"', '', roster)
    roster <- noquote(roster)
    
    roster <- noquote(paste("(", noquote(roster), ")", sep = ""))
    
    return(roster)
  })
  
  newyears <- reactive({
    
    
    year <- input$year
    
    year <- (paste(year, collapse =  ", "))
    year <- gsub('"', '', year)
    year <- noquote(year)
    
    year <- noquote(paste("(", noquote(year), ")", sep = ""))
    return(year)
  })
  
  
  table <- reactive({
    #ncaa <- player()
    
    playerlist <- newroster()
    yearlist <- newyears()
    
    project <- "ncaa-shot-data-222316" # put your project ID here
    sql <- paste("SELECT
                 *
                 FROM
                 [bigquery-public-data.ncaa_basketball.mbb_pbp_sr]
                 
                 WHERE
                 player_full_name in", playerlist, "AND", "season in", yearlist,  sep = " ")
    ncaa = query_exec(sql, project = project)
    
    
    
    threes <- ncaa %>%  filter(event_type == "threepointmade" | event_type == "threepointmiss" 
                               | event_type == "twopointmade" | event_type == "twopointmiss")
   
    
    
    unc <- threes
    
    unc$event_coord_x <- unc$event_coord_x / 12
    unc$event_coord_y <- unc$event_coord_y / 12
    
    
    x <- unc$event_coord_x
    
    otherhalf <- which(x > 47)
    
    flip <- function(vector){
      #y <- data['event_coord_y']
      x <- vector
      
      x <- 94 - x
      
      return(x)
    }
    
    flip2 <- function(vector){
      #y <- data['event_coord_y']
      y <- vector
      
      y <- 50 - y
      
      return(y)
    }
    
    index1 <- which(colnames(unc) == "event_coord_x")
    index2 <- which(colnames(unc) == "event_coord_y")
    indices <- c(index1, index2)
    unc[otherhalf, index1] <- sapply(unc[otherhalf, index1],  FUN = flip)
    unc[otherhalf, index2] <- sapply(unc[otherhalf, index2],  FUN = flip2)
    
    names(unc)[names(unc) == 'event_coord_y'] <- 'coord_x'
    names(unc)[names(unc) == 'event_coord_x'] <- 'coord_y'
    
    #credit to for court code Ewen Gallic http://editerna.free.fr/wp/
    #source("court.R")
    
    flip3 <- function(vector){
      #y <- data['event_coord_y']
      y <- vector
      
      y <- y - 25
      
      return(y)
    }
    unc[, 'scaled_x_coord'] <- sapply(unc[, 'coord_x'],  FUN = flip3)
    
    
    unc$distance <- sqrt(unc$scaled_x_coord^2 + unc$coord_y)
    
    unc$angle <- atan(unc$coord_y/unc$scaled_x_coord)
    
    jumpshots <- unc %>% filter(threes$shot_type == 'jump shot')
    layups <- unc %>% filter(threes$shot_type == 'jump shot')
    

    threes <- unc
    # threes$event_type <- gsub("threepointmade", '3', threes$event_type)
    # threes$event_type <- gsub("threepointmiss", '0', threes$event_type)
    # 
    # threes$event_type <- gsub("twopointmade", '2', threes$event_type)
    # 
    # threes$event_type <- gsub("twopointmiss", '0', threes$event_type)
    # 
    # threes$outcome <- gsub('2', '1', threes$event_type)
    # threes$outcome <- gsub('3', '1', threes$outcome)
    # 
    # 
    # 
    # threes$event_type <- as.numeric(threes$event_type)
    # threes$outcome = 0
    # for(i in 1:nrow(threes)){
    #   
    # if(threes$event_type[i] > 0){
    #   
    #  threes$outcome[i] = 1
    # }
    # }
    # 
    # 
    # threes <- as.data.frame(threes)
    
    


    # threes$outcome <- as.numeric(gsub("miss", 0, threes$outcome))
    # threes$outcome <- as.numeric(threes$outcome)
    # 
    # threes$value <-  gsub("miss", "", threes$event_type)
    # threes$value <-  gsub("made", "", threes$value )
    # 
    # 
    # threes$value <- as.numeric(gsub("twopoint", 2, threes$value))
    # threes$value <- as.numeric(gsub("threepoint", 3, threes$value))
    # threes$value <- as.numeric(threes$value)
    # threes$pts = 0
    # for(i in 1:nrow(threes)){
    #   if(threes$outcome[i] == 1){
    #     threes$pts[i] <-  threes$value[i]
    #   } 
    # }
   
    #pct <- length(which(threes$event_type == "threepointmade")) / (length(which(threes$event_type == "threepointmade")) + length(which(threes$event_type == "threepointmiss")))
    
    
    #three_pps <- threepct * 3
    

  })
  
  output$shotchart <- renderPlot({
   
    
    threes <- table()
 #bin = hex_bin(x = threes$coord_x, y = threes$coord_y, var4=threes$event_type)
    # hexes = hex_coord_df(x=bin$x, y=bin$y, width=attr(bin,"width"), height=attr(bin,"height"), size=bin$size)
    # hexes$points = rep(bin$col, each=6)
    # #ggplot(hexes, aes(x=x, y=y)) + geom_polygon(aes(fill=points, group=id)) +

    
    
    ggplot(threes, aes(x = coord_x, y = coord_y)) + geom_polygon(data = court[court$side==1,], aes(x = x, y = y, group = group), col = "black") +
      coord_equal() + 
      geom_point() + 
      xlim(-3,54) +
      ylim(-3,50) +
      scale_y_continuous(breaks = c(0, 23.5, 47)) +
      scale_x_continuous(breaks = c(0, 12.5, 25, 37.5, 50)) +
      
      xlab("") + ylab("") +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), axis.title = element_blank(), 
            legend.position = "none"
      ) + geom_point(aes(color = shot_made)) + 
      scale_color_manual(
        values = c("red", "green")
      )
    
  })
  
  
  
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

