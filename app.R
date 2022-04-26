#Emma Croxford
#National Park Capstone

library(shiny)
library(shinyWidgets)
library(maps)
library(mapproj)
library(leaflet)
library(dplyr)
library(htmltools)
library(shinythemes)
library(bslib)
library(magrittr)
library(ggmap)
library(tmaptools)
library(shinyalert)

testing <- read.csv("./www/Capstone.Data.testing.csv", header = TRUE)
testing <- testing[1:63,]
testing$Latitude <- as.numeric(testing$Latitude)
testing$Longitude <- as.numeric(testing$Longitude)
testing$Visitors.2020 <- as.numeric(testing$Visitors.2020)
testing$Entry.Fee <- as.numeric(testing$Entry.Fee)
testing$Amphibians <- as.numeric(testing$Amphibians) 
testing$Mammals <- as.numeric(testing$Mammals)
testing$Birds <- as.numeric(testing$Birds) 
testing$Fish <- as.numeric(testing$Fish)
testing$Reptiles <- as.numeric(testing$Reptiles)
testing$Vascular.Plants <- as.numeric(testing$Vascular.Plants)
geocoded <- data.frame(stringsAsFactors = FALSE)
latmax = -99
latmin = 90
lngmax = -180
lngmin = 180
b = 0
m = -99

npsIcon <- makeIcon("./www/npsLogo.png", iconAnchorX = 10, 
                    iconAnchorY = 10)

homeIcon <- makeIcon("./www/house.png", iconAnchorX = 10, 
                     iconAnchorY = 10)

# Define UI ----
ui <- navbarPage( theme = includeCSS("./www/styles.css"), 
                  id = "nav","United States National Parks",
        tabPanel("Interactive Map",
          div(includeCSS("./www/styles.css"),
              fluidPage(
              fluidRow(
                column(8, wellPanel( style = "background-color: #BAE5F9;padding: 0 0 0 0",
                  leafletOutput("mymap", height="99.5%")),
                  fluidRow(
                    column(4, wellPanel( style = "background-color: #529CD4;",
                      selectInput("month", h5("Month of Visit"),
                                  choices = list("January" = 1, "February" = 2, "March" = 3, "April" = 4,
                                                 "May" = 5, "June" = 6, "July" = 7, "August" = 8,
                                                 "September" = 9, "October" = 10, "November" = 11, "December" = 12, "Select" = 13), selected = 13),
                      conditionalPanel("input.month != 13", 
                                       sliderInput("weather", h5("Weather (F)"),
                                        min = -20, max = 110, value = c(-20, 110))),
                      setSliderColor("#ffffff", 1),
                      selectInput("precip", h5("Precipitation"), 
                                  choices = list("High" = 1, "Average" = 2, "Low" = 3, "No Preference" = 4), selected = 4),
                      selectInput("diverse", h5("Biodiversity Rating"), 
                                  choices = list("High" = 1, "Average" = 2, "Low" = 3, "No Preference" = 4), selected = 4))),
                    column(4, wellPanel( style = "background-color: #C56C39;",
                      checkboxGroupInput("transpo", h5("Transportation"),
                                        choices = list("Train" = "Train", "Plane" = "Plane",
                                                       "Vehicle" = "Vehicle", "Boat" = "Boat")),
                      checkboxGroupInput("hike", h5("Hiking"), 
                                         choices = list("Frontcountry" = 1, "Backcountry" = 2, "Off-Trail" = 3, "No Preference" = 4)))),
                    column(4, wellPanel( style = "background-color: #56903A;",
                      selectInput("visitor", h5("Number of Visitors"), 
                                  choices = list("High" = 1, "Average" = 2, "Low" = 3, "No Preference" = 4), selected = 4),
                      selectInput("state", h5("Location of Park"), 
                                  choices = list("Alaska" = "Alaska", "Arizona" = "Arizona",
                                                 "Arkansas" = "Arkansas", "California" = "California", "Colorado" = "Colorado",
                                                 "Florida" = "Florida", "Hawaii" = "Hawaii", "Indiana" = "Indiana",
                                                 "Kentucky" = "Kentucky", "Maine" = "Maine", "Michigan" = "Michigan",
                                                 "Minnesota" = "Minnesota", "Missouri" = "Missouri", "Montana" = "Montana",
                                                 "Ohio" = "Ohio", "Oregon" = "Oregon", "South Carolina" = "South Carolina",
                                                 "South Dakota" = "South Dakota", "Tennessee" = "Tennessee", "Texas" = "Texas",
                                                 "Utah" = "Utah", "Virginia" = "Virginia", "Washington" = "Washington", "West Virgnia" = "West Virginia",
                                                 "Wyoming" = "Wyoming", "Territory" = "Territory", "No Preference" = "No Preference"), selected = "No Preference"),
                      selectInput("size", h5("Size of Park"), 
                                  choices = list("Big" = 1, "Medium" = 2, "Small" = 3, "No Preference" = 4), selected = 4)
                    )))),
                column(4, wellPanel( style = "height:720px;background-color: #213A1B;",
                  h3("Recommendation System"),
                  h1("1. Choose preferences to locate the best park for you"),
                  h1("2. Click on the map marker to learn more about each park"),
                  textInput("add", h5("Your Location"), value = ""),
                  h6("**City, State, or Country**"),
                  checkboxGroupInput("features", h5("Park Features"),
                                             choices = list("Glaciers" = "Glaciers", "Mountains" = "Mountains",
                                                            "Ocean/Coastal" = "Ocean/Coastal", "Sand Dunes" = "Sand Dunes", 
                                                            "Volcanic" = "Volcanic", "Geothermal" = "Geothermal", "Caves/Karsts" = "Caves/Karsts",
                                                            "Arctic" = "Arctic", "Diving" = "Diving", "Lodge" = "Lodge", "Campground" = "Campground", 
                                                            "Wifi" = "Wifi", "Free Entry" = "Free Entry"))
                  )))))),
        tabPanel("The Parks",
                 div(includeCSS("./www/styles.css"),
                     fluidPage(
                       fluidRow(
                         column(3, div(class = "container", 
                                      img(src = "acadia.jpeg", class = "image"),
                                      div(class = "text", style = "color: #ffffff; font-size: 20px", "Acadia"),
                                      div (class="overlay", 
                                          div(class = "text", 
                                              p("Acadia", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1919"),
                                              p("Location: Maine"),
                                              p("Things to do: Bicycling, Climbing, Picnicking, Hiking, Horseback Riding, 
                                                Carriage Tours, Stargazing, Boating, Swimming, Fishing, Tidepooling, Birdwatching")
                                              )))),
                         column(3, div(class = "container", 
                                       img(src = "americansamoa.jpeg", class = "image"),
                                       div(class = "text", style = "color: #ffffff; font-size: 20px", "American Samoa"),
                                       div (class="overlay", 
                                            div(class = "text", 
                                                p("American Samoa", style = "font-weight: bold;font-size:20px"),
                                                p("Established: 1988"),
                                                p("Location: American Samoa"),
                                                p("Things to do: Fishing, Snorkeling, Diving, Hiking, Beachwalking")
                                            )))),
                         column(3, div(class = "container", 
                                           img(src = "arches.jpeg", class = "image"),
                                           div(class = "text", style = "color: #ffffff; font-size: 20px", "Arches"),
                                           div (class="overlay", 
                                                div(class = "text", 
                                                    p("Arches", style = "font-weight: bold;font-size:20px"),
                                                    p("Established: 1971"),
                                                    p("Location: Utah"),
                                                    p("Things to do: Auto Touring, Backpacking, Bicycling, Canyoneering,
                                                      Hiking, Horseback Riding, Photography, Rock Climbing, Stargazing, Commercial Tours")
                                                )))),
                         column(3, div(class = "container", 
                                       img(src = "badlands.jpeg", class = "image"),
                                       div(class = "text", style = "color: #ffffff; font-size: 20px", "Badlands"),
                                       div (class="overlay", 
                                            div(class = "text", 
                                                p("Badlands", style = "font-weight: bold;font-size:20px"),
                                                p("Established: 1978"),
                                                p("Location: South Dakota"),
                                                p("Things to do: Nigh Sky Viewing, Scenic Driving, Wildlife Watching, Hiking, 
                                                  Guided Tours, Bicycling")
                                            ))))
                     ),
                     fluidRow(
                       column(3, div(class = "container", 
                                     img(src = "bigbend.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Big Bend"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Big Bend", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1944"),
                                              p("Location: Texas"),
                                              p("Things to do: Scenic Driving, Hiking, River Trips, Bicycling, 
                                                Bird Watching, Fishing, horseback Riding, Stargazing, Backpacking")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "biscayne.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Biscayne"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Biscayne", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1980"),
                                              p("Location: Florida"),
                                              p("Things to do: Nature Journaling Club, Fishing, Boating, Camping, Canoeing,
                                                Kayaking, Guided Tours")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "blackcanyon.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Black Canyon of the Gunnison"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Black Canyon of the Gunnison", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1999"),
                                              p("Location: Colorado"),
                                              p("Things to do: Hiking, Rock Climbing, Kayaking, Rafting,
                                                Fishing, Scenic Driving, Horseback Riding, Snowshoe Walks")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "bryce.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Bryce Canyon"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Bryce Canyon", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1928"),
                                              p("Location: Utah"),
                                              p("Things to do: Commericial Tours, Hiking, Backcountry Camping, Horseback Riding, 
                                                Snowshoeing, Cross-Country Skiing, Winter Hiking, Stargazing")
                                          ))))),
                     fluidRow(
                       column(3, div(class = "container", 
                                     img(src = "canyonlands.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Canyonlands"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Canyonlands", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1964"),
                                              p("Location: Utah"),
                                              p("Things to do: Auto Touring, Backcountry, Boating, Climbing, Commercial Guides,
                                                Hiking, Horseback Riding, Stargazing")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "capitolreef.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Capitol Reef"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Captiol Reef", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1971"),
                                              p("Location: Utah"),
                                              p("Things to do: Hiking, Road Tours, Backpacking, Canyoneering,
                                                Rock Climbing, Bouldering, Biking, Backcountry Horseback Riding")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "carlsbad.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Carlsbad Caverns"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Carlsbad Caverns", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1930"),
                                              p("Location: New Mexico"),
                                              p("Things to do: Bat Viewing, Stargazing, Hiking")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "channel.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Channel Islands"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Channel Islands", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1980"),
                                              p("Location: California"),
                                              p("Things to do: Hiking, Backcountry Camping, Picknicking, Boating,
                                                Kayaking, Diving, Snorkeling, Fishing, Surfing, Tidepooling, Whale Watching,
                                                Seal and Sea Lion Viewing, Bird Watching, Wildflower Viewing")
                                          ))))),
                     fluidRow(
                       column(3, div(class = "container", 
                                     img(src = "congaree.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Congaree"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Congaree", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 2003"),
                                              p("Location: South Carolina"),
                                              p("Things to do: Hiking, Canoeing, Kayaking, Fishing")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "crater.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Crater Lake"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Crater Lake", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1902"),
                                              p("Location: Oregon"),
                                              p("Things to do: Boat and Trolley Tours, Bicycling, Bord Watching,
                                                Fishing, Backcountry Camping, Winter Backcountry Camping, Cross-Country Skiing, Snowshoeing, 
                                                Snowboarding, Sledding, Downhill Skiing, Snowmobiling")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "cuyahoga.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Cuyahoga Valley"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Cuyahoga Valley", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 2000"),
                                              p("Location: Ohio"),
                                              p("Things to do: Hiking, Biking, Fishing, Bording, Backpacking, 
                                                Paddling the River, Horse Trails, Questing, Picknicking, Golfing, 
                                                Showshoeing, Ice Fishing, Sledding, Cross-Country Skiing, Downhill Skiing, Snow Tubing")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "deathvalley.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Death Valley"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Death Valley", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1994"),
                                              p("Location: California, Nevada"),
                                              p("Things to do: Sightseeing, Night Exploration, 
                                                Hiking, Backpacking, Backcountry Driving, Backcountry Camping,
                                                Biking, Birdwatching, Horseback Riding")
                                          ))))),
                     fluidRow(
                       column(3, div(class = "container", 
                                     img(src = "denali.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Denali"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Denali", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1917"),
                                              p("Location: Alaska"),
                                              p("Things to do: Wildlife Viewing, Hiking, Backpacking, Cycling,
                                                Photography, Fishing, Flight Seeing, Mushing, Snowmobiling, Winter Camping, Aurora Borealis/Stargazing")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "drytortugas.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Dry Tortugas"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Dry Tortugas", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1992"),
                                              p("Location: Florida"),
                                              p("Things to do: Fishing, Boating, Geocaching, Paddlesports,
                                                Snorkeling, Diving, Swimming, Wildlife Viewing")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "everglades.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Everglades"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Everglades", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1934"),
                                              p("Location: Florida"),
                                              p("Things to do: Bird Watching, Biking, Boating, Canoeing, Kayaking,
                                                Fishing, Hiking, ")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "gatesarctic.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Gates of the Arctic"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Gates of the Arctic", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1980"),
                                              p("Location: Alaska"),
                                              p("Things to do: Backpacking, Hiking, Birding, Climbing, Fishing, Hunting")
                                          ))))),
                     fluidRow(
                       column(3, div(class = "container", 
                                     img(src = "gatewayarch.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Gateway Arch"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Gateway Arch", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 2018"),
                                              p("Location: Missouri"),
                                              p("Things to do: Guided Tours")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "glacier.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Glacier"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Glacier", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1910"),
                                              p("Location: Montana"),
                                              p("Things to do: Hiking, Wilderness Camping, Photography,
                                                Bicycling, Fishing, Boating, Cross-country Skiing")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "glacierbay.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Glacier Bay"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Glacier Bay", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1980"),
                                              p("Location: Alaska"),
                                              p("Things to do: Hiking, Kayaking, Bord Watching, Flight Seeing,
                                                Rafting, Fishing, Hunting, Boating")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "grandcanyon.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Grand Canyon"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Grand Canyon", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1919"),
                                              p("Location: Arizona"),
                                              p("Things to do: Backcountry Hiking, River Trips, Bicycling, Photography,
                                                Mule Trips")
                                          ))))),
                     fluidRow(
                       column(3, div(class = "container", 
                                     img(src = "grandteton.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Grand Teton"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Grand Teton", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1929"),
                                              p("Location: Wyoming"),
                                              p("Things to do: Scenic Drives, Hiking, Biking, Fishing, Boating, Climbing, Mountaineering,
                                                Horseback Riding")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "greatbasin.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Great Basin"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Great Basin", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1986"),
                                              p("Location: Nevada"),
                                              p("Things to do: Stargazing, Hiking, Bird Watching, Wilderflower Viewing,
                                                Auto Touring, Bicycling, Horseback Riding, Wild Caving, Fishing, Pine Nut Gathering")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "greatsand.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Great Sand Dunes"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Great Sand Dunes", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 2004"),
                                              p("Location: Colorado"),
                                              p("Things to do: Hiking, Sandboarding, Sand Sledding,
                                                Photography, Fishing, Commericial Tours, Hunting")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "greatsmokey.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Great Smoky Mountains"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Great Smoky Mountains", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1934"),
                                              p("Location: North Carolina, Tennessee"),
                                              p("Things to do: Auto Touring, Bicycling, Fishing, Hiking,
                                                Horse Riding, Picnicking, Wildlife Viewing, Waterfall Viewing")
                                          ))))),
                     fluidRow(
                       column(3, div(class = "container", 
                                     img(src = "guadalupe.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Guadalupe Mountains"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Guadalupe Mountains", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1966"),
                                              p("Location: Texas"),
                                              p("Things to do: Secnic Drives, Hiking, Backpacking, Horseback Riding,
                                                Birding, Stargazing, Wildlife Viewing")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "haleakala.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Haleakala"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Haleakala", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1961"),
                                              p("Location: Hawai'i"),
                                              p("Things to do: Hiking, Stargazing, Wilderness Camping")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "hawaiivolcanoes.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Hawai'i Volcanoes"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Hawai'i Volcanoes", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1916"),
                                              p("Location: Hawai'i"),
                                              p("Things to do: Hiking, Backcountry Hiking, Drive Tours")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "hotsprings.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Hot Springs"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Hot Springs", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1921"),
                                              p("Location: Arkansas"),
                                              p("Things to do: Hiking, Scenic Drives, Biking, Photography, Picnicking,
                                                Fishing, Hot Springs")
                                          ))))),
                     fluidRow(
                       column(3, div(class = "container", 
                                     img(src = "indianadunes.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Indiana Dunes"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Indiana Dunes", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 2019"),
                                              p("Location: Indiana"),
                                              p("Things to do: Beach-Going, Swimming, Biking, Bird Watching,
                                                Camping, Fishing, Paddling, Hiking, Horseback Riding, Picnicking, Geocaching,
                                                Sledding, Cross-Counrty Skiing, Snowshoeing")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "isleroyale.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Isle Royale"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Isle Royale", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1940"),
                                              p("Location: Michigan"),
                                              p("Things to do: Backpacking, Boating, Canoeing, kayaking, 
                                                Hiking, Fishing, Scuba Diving")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "joshuatree.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Joshua Tree"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Joshua Tree", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1994"),
                                              p("Location: California"),
                                              p("Things to do: backpacking, Biking, Birding, Climbing, Highlining, Slacklining,
                                                Hiking, Horseback Riding, Photography, Stargazing, Wildflower Viewing")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "katmai.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Katmai"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Katmai", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1980"),
                                              p("Location: Alaska"),
                                              p("Things to do: Backcountry Hiking and Camping, Bear Watching,
                                                Boating, Fishing, Flightseeing, Hunting, Trapping")
                                          ))))),
                     fluidRow(
                       column(3, div(class = "container", 
                                     img(src = "kenai.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Kenai Fjords"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Kenai Fjords", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1980"),
                                              p("Location: Alaska"),
                                              p("Things to do: Boat Tours, Hiking, Flight Seeing, Kayaking, Fishing, Mountaineering,
                                                Dog Sled Tours, Ice Climbing")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "kingscanyon.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Kings Canyon"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Kings Canyon", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1940"),
                                              p("Location: California"),
                                              p("Things to do: Hiking, Overnight Backpacking, Horseback Riding,
                                                Rock Climbing, Fishing, Crystal Cave, Skiing, Showshoeing, Snowplay")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "kobuk.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Kobuk Valley"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Kobuk Valley", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1980"),
                                              p("Location: Alaska"),
                                              p("Things to do: Backpacking, Boating, Fishing, Flightseeing")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "lakeclark.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Lake Clark"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Lake Clark", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1980"),
                                              p("Location: Alaska"),
                                              p("Things to do: Hiking, Bear Viewing, Backpacking, Hunting, 
                                                Boating, Fishing, Birding")
                                          ))))),
                     fluidRow(
                       column(3, div(class = "container", 
                                     img(src = "lassen.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Lassen Volcanic"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Lassen Volcanic", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1916"),
                                              p("Location: California"),
                                              p("Things to do: Auto Touring, Hiking, Boating, Fishing, Swimming,
                                                Backpacking, Biking, Parkcaching, Stargazing, Showshoeing, Skiing, Snowboarding")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "mammoth.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Mammoth Cave"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Mammoth Cave", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1941"),
                                              p("Location: Kentucky"),
                                              p("Things to do: Canoeing, Kayaking, Boating, Fishing, Bicycling,
                                                Hiking, Horseback Riding, Stargazing")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "mesaverde.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Mesa Verde"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Mesa Verde", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1906"),
                                              p("Location: Colorado"),
                                              p("Things to do: Cliff Dwelling Tours, Hiking, Showshoeing, Cross-country Skiing,
                                                Bird Watching, Photography, Stargazing")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "mountrainier.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Mount Rainier"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Mount Rainier", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1899"),
                                              p("Location: Washington"),
                                              p("Things to do: Bicycling, Climbing, Wildflower Viewing, Fishing,
                                                Boating, Hiking")
                                          ))))),
                     fluidRow(
                       column(3, div(class = "container", 
                                     img(src = "newrivergorge.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "New River Gorge"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("New River Gorge", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 2020"),
                                              p("Location: West Virginia"),
                                              p("Things to do: Hiking, Scenic Drives, Whitewater Boating, Bicycling, Backpacking, Climbing,
                                                Hunting, Fishing")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "northcascades.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "North Cascades"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("North Cascades", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1968"),
                                              p("Location: Washington"),
                                              p("Things to do: Bicycling, Birdwatching, Wildlife Viewing, 
                                                Boating, Climbing, Fishing, Hiking, Horseback Riding")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "olympic.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Olympic"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Olympic", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1938"),
                                              p("Location: Washington"),
                                              p("Things to do: Boating, Climbing, Fishing, Hiking, Tidepooling, Wildlife Viewing, Showshoeing, 
                                                Cross-country Skiing, Downhill Skiing, Snowbaording, Tubing")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "petrifiedforest.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Petrified Forest"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Petrified Forest" , style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1962"),
                                              p("Location: Arizona"),
                                              p("Things to do: Hiking, Backpacking, Horseback Riding, Geocaching")
                                          ))))),
                     fluidRow(
                       column(3, div(class = "container", 
                                     img(src = "pinnacles.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Pinnacles"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Pinnacles" , style = "font-weight: bold;font-size:20px"),
                                              p("Established: 2013"),
                                              p("Location: California"),
                                              p("Things to do: Hiking, Talus Caves, Climbing, Bird Watching")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "redwood.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Redwood"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Redwood", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1968"),
                                              p("Location: California"),
                                              p("Things to do: Hiking, Todepoling, Wildlife Viewing, Kayaking,
                                                Bicycling, Horseback Riding")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "rockymountain.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Rocky Mountain"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Rocky Mountain", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1915"),
                                              p("Location: Colorado"),
                                              p("Things to do: Wilderness Camping, Bicycling, Climbing,
                                                Fishing, Hiking, Horseback Riding, Picnicking, Wildlife Viewing,
                                                Snowshoeing, Cross-country Skiing, Sledding")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "saguaro.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Saguaro"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Saguaro", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1994"),
                                              p("Location: Arizona"),
                                              p("Things to do: Hiking, Backcountry Camping, Petroglyph Viewing")
                                          ))))),
                     fluidRow(
                       column(3, div(class = "container", 
                                     img(src = "sequoia.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Sequoia"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Sequoia",style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1890"),
                                              p("Location: California"),
                                              p("Things to do: Hiking, Overnight Backpacking, Horseback Riding,
                                                Rock Climbing, Fishing, Crystal Cave, Skiing, Showshoeing, Snowplay")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "shenandoah.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Shenandoah"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Shenandoah" , style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1935"),
                                              p("Location: Virginia"),
                                              p("Things to do: Hiking, Backpacking, Biking, Birdwatching, 
                                                Fishing, Horseback Riding, Rock Climbing, Earth Caching,
                                                Wildlife Viewing, Photography, Scenic Drives")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "theodore.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Theodore Roosevelt"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Theodore Roosevelt", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1978"),
                                              p("Location: North Dakota"),
                                              p("Things to do: Backcountry Camping, Bicycling, Canoeing, Kayaking,
                                                Cross-country Skiing, Snowshoeing, Fishing, Hiking, Horseback Riding, Wildlife Viewing")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "virginislands.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Virgin Islands"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Virgin Islands", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1956"),
                                              p("Location: U.S. Virgin Islands"),
                                              p("Things to do: Fishing, Snorkeling, Boating, Hiking")
                                          ))))),
                     fluidRow(
                       column(3, div(class = "container", 
                                     img(src = "voyageurs.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Voyageurs"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Voyageurs", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1975"),
                                              p("Location: Minnesota"),
                                              p("Things to do: Fishing, Bordwatching, Stargazing,
                                                Houseboating, Hiking, Snowshoeing")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "whitesands.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "White Sands"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("White Sands", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 2019"),
                                              p("Location: New Mexico"),
                                              p("Things to do: Backcountry Camping, Bicycling, Hiking, Photography, 
                                                Picnicking, Sledding, Pack Animal Riding")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "windcave.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Wind Cave"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Wind Cave", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1903"),
                                              p("Location: South Dakota"),
                                              p("Things to do: Cave Tours")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "wrangell.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Wrangell St.Elias"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Wrangell St.Elias", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1980"),
                                              p("Location: Alaska"),
                                              p("Things to do: Backpacking, Hiking, Mountaineering, Boating,
                                                Hunting, Fishing, Wildlife Veiwing, ORVing, Snowmobiles, Cross-country Skiing,
                                                Snowshoeing")
                                          ))))),
                     fluidRow(
                       column(3, div(class = "container", 
                                     img(src = "yellowstone.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Yellowstone"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Yellowstone", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1872"),
                                              p("Location: Wyoming, Montana, Idaho"),
                                              p("Things to do: Wildlife Viewing, Hiking, Photography,
                                                Backcountry Camping, Fishing, Boating, Bicycling, 
                                                Horseback Riding, Skiing, Snowshoeing, Snowmobiling")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "yosemite.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Yosemite"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Yosemite", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1890"),
                                              p("Location: California"),
                                              p("Things to do: Photography, Auto Touring, Backpacking,
                                                Biking, Bordwatching, Fishing, Hiking, Horseback Riding,
                                                Picnicking, Rock Climbing, Boating, Cross-country Skiing, 
                                                Snowshoeing, Downhill Skiing, Snowboarding, Ice Skating,
                                                Snow Tubing, Sledding")
                                          )))),
                       column(3, div(class = "container", 
                                     img(src = "zion.jpeg", class = "image"),
                                     div(class = "text", style = "color: #ffffff; font-size: 20px", "Zion"),
                                     div (class="overlay", 
                                          div(class = "text", 
                                              p("Zion", style = "font-weight: bold;font-size:20px"),
                                              p("Established: 1919"),
                                              p("Location: Utah"),
                                              p("Things to do: Backpacking, Bicycling, Canyoneering,
                                                Hiking, River Trips, Rock Climbing, Stargazing, Wildlife Viewing")
                                          )))))
                     
                     ))),
        tabPanel("About this Project")
        )

# Define server logic ----
server <- function(input, output, session) {
  filtered <- reactive({
    #State
    {
    if(input$state == "No Preference"){
      testing <- testing
    }else{
      testing <- testing[testing$State == input$state,]
    }
    }

    #Features
    {
    if("Glaciers" %in% input$features){
      testing <- testing[testing$Glaciers == "Yes",]
    }
    if("Mountains" %in% input$features){
      testing <- testing[testing$Mountains == "Yes",]
    }
    if("Ocean/Coastal" %in% input$features){
      testing <- testing[testing$Ocean.Coastal == "Yes",]
    }
    if("Sand Dunes" %in% input$features){
      testing <- testing[testing$Sand.Dunes == "Yes",]
    }
    if("Volcanic" %in% input$features){
      testing <- testing[testing$Volcanic == "Yes",]
    }
    if("Geothermal" %in% input$features){
      testing <- testing[testing$Geothermal == "Yes",]
    }
    if("Caves/Karsts" %in% input$features){
      testing <- testing[testing$Caves.Karsts == "Yes",]
    }
    if("Rock Formations" %in% input$features){
      testing <- testing[testing$Rock.Formations == "Yes",]
    }
    if("Wetlands" %in% input$features){
      testing <- testing[testing$Wetlands == "Yes",]
    }
    if("Arctic" %in% input$features){
      testing <- testing[testing$Arctic == "Yes",]
    }
    if("Diving" %in% input$features){
      testing <- testing[testing$Dive == "Yes",]
    }
    if("Lodge" %in% input$features){
      testing <- testing[testing$Lodge == "Yes",]
    }
    if("Campground" %in% input$features){
      testing <- testing[testing$Camp.Ground == "Yes",]
    }
    if("Wifi" %in% input$features){
      testing <- testing[testing$Wifi == "Yes",]
    }
    if("Free Entry" %in% input$features){
      testing <- testing[testing$Entry.Fee == 0,]
    }
    }

    #Hiking
    {
    if(4 %in% input$hike){
      testing <- testing
    }else{
      if(1 %in% input$hike){
        testing <- testing[testing$Frontcountry == "Yes",]
      }
      if(2 %in% input$hike){
        testing <- testing[testing$Backcountry == "Yes",]
      }
      if(3 %in% input$hike){
        testing <- testing[testing$Off.Trail.Hiking == "Yes",]
      }
    }
    }

    #Weather Slider
    {
    if(input$month == 13){
        testing <- testing
    }else{
      low <- sprintf("testing$low.%s", input$month)
      high <- sprintf("testing$high.%s", input$month)
      testing <- testing[eval(parse(text = low)) >= input$weather[1] &
                           eval(parse(text = high)) <= input$weather[2],]
    }
    }

    #Precipitation
    {
    if(input$precip == 4){
      testing <- testing
    }else{
      if(input$precip == 3){
        testing <- testing[testing$Precipitation <= 1.1,]
      }
      if(input$precip == 2){
        testing <- testing[testing$Precipitation > 1.1 &
                          testing$Precipitation < 3.35,]
      }
      if(input$precip == 1){
        testing <- testing[testing$Precipitation >= 3.35,]
      }
    }
    }

    #Visitors
    {
    if(input$visitor == 4){
      testing <- testing
    }else{
      if(input$visitor == 3){
        testing <- testing[testing$Visitors.2020 <= 145128,]
      }
      if(input$visitor == 2){
        testing <- testing[testing$Visitors.2020 > 145128 &
                             testing$Visitors.2020 < 1199418,]
      }
      if(input$visitor == 1){
        testing <- testing[testing$Visitors.2020 >= 1199418,]
      }
    }
    }

    #Size of Park
    {
    if(input$size == 4){
      testing <- testing
    }else{
      if(input$size == 3){
        testing <- testing[testing$Acerage <= 53249,]
      }
      if(input$size == 2){
        testing <- testing[testing$Acerage > 53249 &
                             testing$Acerage < 715699,]
      }
      if(input$size == 1){
        testing <- testing[testing$Acerage >= 715699,]
      }
    }
    }

    #Transportation
    {
    if("Vehicle" %in% input$transpo){
      testing <- testing[testing$Vehicle == "Yes",]
    }
    if("Plane" %in% input$transpo){
      testing <- testing[testing$Plane == "Yes",]
    }
    if("Train" %in% input$transpo){
      testing <- testing[testing$Train == "Yes",]
    }
    if("Boat" %in% input$transpo){
      testing <- testing[testing$Boat == "Yes",]
    }
    }

    #Biodiversity
    {
      if(input$diverse == 4){
        testing <- testing
      }else{
        if(input$diverse == 3){
          testing <- testing[(testing$Amphibians + testing$Mammals + testing$Birds + testing$Fish +
                                testing$Reptiles + testing$Vascular.Plants) <= 924,]
        }
        if(input$diverse == 2){
          testing <- testing[(testing$Amphibians + testing$Mammals + testing$Birds + testing$Fish +
                                testing$Reptiles + testing$Vascular.Plants) > 924 &
                               (testing$Amphibians + testing$Mammals + testing$Birds + testing$Fish +
                                  testing$Reptiles + testing$Vascular.Plants) < 1301,]
        }
        if(input$diverse == 1){
          testing <- testing[(testing$Amphibians + testing$Mammals + testing$Birds + testing$Fish +
                                testing$Reptiles + testing$Vascular.Plants) >= 1301,]
        }
      }
    }
    
    testing
  })
  
  #Map Functions
  output$mymap <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 1, maxZoom = 5, zoomControl = FALSE)) %>%
      addTiles() %>%
      addProviderTiles("Esri.NatGeoWorldMap")
  })
  
  observe({
    if(length(filtered()$Longitude) != 0){
      leafletProxy("mymap", data = filtered()) %>%
        clearMarkers() %>%
        addMarkers(lat = filtered()$Latitude, lng = filtered()$Longitude,
                         popup = paste(sep = "", "<div, style = color:#523623>", "<b>", filtered()$Name, "</b>", "</div>",
                                                   "<br/>", "Established in: ", filtered()$Established,
                                                   "<br/>", "For more information click ",
                                                   "<a style = color:#4d942b href='", filtered()$URL, "'>here</a>"
                                                    ), icon = npsIcon) %>%
        fitBounds(max(lngmax, filtered()$Longitude),
                  max(latmax, filtered()$Latitude), 
                  min(lngmin, filtered()$Longitude),
                  min(latmin, filtered()$Latitude))
      if(lngmax != -180)
      {removeMarker(m, "loc")}
      if(input$add != ""){
        result <- geocode_OSM(input$add,
                              details = TRUE, as.data.frame = TRUE)
        if(length(result) == 0 & lngmax != -180){removeMarker(m, "loc")}
        else{
          validate(need(length(result) > 0, "Location not found"))
          lngmax = lngmin = result$lon
          latmax = latmin = result$lat
          m <- leafletProxy("mymap") %>%
            addMarkers(lat = latmax, lng = lngmax, 
                       popup = paste(sep = "", "<div, 
                                       style = color:#56903A>", "You are here", "</div>"), layerId = "loc", icon = homeIcon) %>%
            fitBounds(max(lngmax, filtered()$Longitude),
                      max(latmax, filtered()$Latitude), 
                      min(lngmin, filtered()$Longitude),
                      min(latmin, filtered()$Latitude))
          m
        }
      }
    }else{
      shinyalert(title = "None of the National Parks match your choices. Please Re-evaluate", type = "warning")
    }
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)

#mess with the weather feature
#Finish Parks tab
#Finish about tab

#https://bootswatch.com/default/
#https://github.com/eparker12/nCoV_tracker/blob/master/app.R
#https://shiny.rstudio.com/gallery/covid19-tracker.html