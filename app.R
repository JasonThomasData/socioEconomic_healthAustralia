library(shiny)
library(shinyjs)
library(ggplot2)
library(shadowtext)

#################################################
## FUNCTIONS

chooseState <- function(code) {
  firstChar <- substr(as.character(code), 1, 1)
  characterMap <- list(
    "1" = "NSW",
    "2" = "Victoria",
    "3" = "Queensland",
    "4" = "SA",
    "5" = "WA",
    "6" = "Tasmania",
    "7" = "NT",
    "8" = "ACT"
  )
  (characterMap[firstChar])
}

readableToMachine <- function(population, disease ) {
  populationNames <- list(
    "All ages" = "All.ages",
    "60 years and over" = "60.years.and.over",
    "70 years and over" = "70.years.and.over"
  )
  diseaseNames <- list(
    "Asthma" = "Asthma",
    "Diabetes" = "Diabetes",
    "Heart, stroke and vascular" = "Heart.stroke.vascular",
    "Three or more chronic conditions" = "Three.or.more.chronic"
  )
  (paste(diseaseNames[disease],populationNames[population], sep="....."))
}

socialIndexDescriptionHash <- function(index) {
  description <- list(
    "ISRAD" = "ISRAD [Index of Relative Socio-economic Advantage and Disadvantage]: A low score indicates greater relative social disadvantage and lower relative social advantage, such as low occupation levels and unskilled labour. A high score might indicate high incomes and skilled occupations.",
    "ISRD" = "ISRD [Index of Relative Socio-economic Disadvantage]: A low score indicates greater relative social disadvantage, such as low occupation levels and unskilled labour. This index does not include relative social advantage.",
    "IEO" = "IEO [Index of Education and Occupation]: A low score indicates a lower educational and occupational levels in the area. A high score might indicate relatively high levels of income and education.",
    "IER" = "IER [Index of Economic Resources]: A low score indicates a lack of access to economic resources, such as people renting and households with low income. A higher score indicates home ownership and relatively high household incomes."
  )
  (paste(description[index]))
}

getDataForJurisdiction <- function(mergedData, input) {
  if (input$state.territory == "All states and territories") {
    (mergedData)
  } else {
    (mergedData[mergedData$state.territory == input$state.territory,])
  }
}

getDisplayData <- function(input) {
  populationDiseaseColumnNames <- readableToMachine(input$population, input$disease)
  specificJurisdictionData <- getDataForJurisdiction(mergedData, input)
  data.frame(x=specificJurisdictionData[,c(input$socialIndex)],
             y=specificJurisdictionData[,c(populationDiseaseColumnNames)],
             sa2Label=specificJurisdictionData[,"SA2.Label"])
}

getDisplaySA2Names <- function(input) {
  specificJurisdictionData <- getDataForJurisdiction(mergedData, input)
  (specificJurisdictionData$SA2.Label)
}

getSelectedLocations <- function(i) {
  if (i == 1) {
    preSelected <- c()
  } else if (i == 2) {
    preSelected <- c("Taroona - Bonnet Hill")
  } else if (i == 3) {
    preSelected <- c("Taroona - Bonnet Hill", "Launceston")
  } else if (i == 4) {
    preSelected <- c("Taroona - Bonnet Hill", "Ravenswood", "Launceston")
  } else {
    preSelected <- c("Taroona - Bonnet Hill", "Ravenswood", "Launceston")
  }
  (preSelected)
}

controlButtons <- function(captionIndex) {
  if (captionIndex$data == 1) {
    addClass("backButton", "faded")
  } else if (captionIndex$data == 5) {
    addClass("nextButton", "faded")
    removeClass("optionsPanel", "disabled")
  } else {
    removeClass("backButton", "faded")
    removeClass("nextButton", "faded")
  }
}

#################################################
## DATA PREP

# From the National Health Survey, split into different files, and observations removed for low values and missing data
asthma <- read.csv("asthma.csv")
diabetes <- read.csv("diabetes.csv")
heart_stroke_vascular <- read.csv("heart_stroke_vascular.csv")
three_or_more_chronic <- read.csv("three_or_more_chronic.csv")

# SEIFA data for SA2 only
SA2_SEIFA_2021 <- read.csv("SA2_SEIFA_2021.csv")

#Remove duplicates
diabetes <- diabetes[, -2]
heart_stroke_vascular <- heart_stroke_vascular[, -2]
three_or_more_chronic <- three_or_more_chronic[, -2]
SA2_SEIFA_2021 <- SA2_SEIFA_2021[, -2]

names(SA2_SEIFA_2021)[1] <- "SA2.Code"

mergedData <- merge(asthma, diabetes, "SA2.Code")
mergedData <- merge(mergedData, heart_stroke_vascular, "SA2.Code")
mergedData <- merge(mergedData, three_or_more_chronic, "SA2.Code")
mergedData <- merge(mergedData, SA2_SEIFA_2021, "SA2.Code")

#Label SA2 by state
for (i in 1:nrow(mergedData)) {
  mergedData[i, "state.territory"] <- chooseState(mergedData[i, "SA2.Code"])
}
#head(mergedData)

introCaptions <- c(
  "Tasmania is Australia's smallest state by area and population. However, the people of Tasmania live a world apart in terms of access to opportunities and health outcomes. Click next to learn about the disparities that Tasmania faces.",
  "If you live in Taroona - Bonnet Hill (in Hobart) then you can expect to enjoy relatively high levels of social advantage in your community. Taroona - Bonnet Hill SA2 is highlighted below, in context of the national relationship between ISRAD and chronic disease.",
  "Tasmania's next largest city is Launceston, and the most advantaged community in Launceston is the centre of the city. The people of Launceston's city centre can expect about the same levels of chronic disease, compared to Taroona - Bonnet Hill.",
  "Just 4 kilometres away from Launceston's city centre is Ravenswood. Ranveswood is Tasmania's most disadvantaged community, across a range of socio-economic indicators. People in Ravenswood can expect many more people in their community to suffer with chronic diseases.",
  "Use the options panels below to explore the relationship that social advantage and disadvantage has with health outcomes."
)

#################################################
## UI CODE

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      div.row:nth-child(1) {
        margin-left: 100px;
        margin-bottom: 30px;
        margin-top: 30px;
        padding: 10px;
        width: auto;
        display: inline-flex;
        background: white;
      }
      #backButton {
        display: inline-block;
        margin-right: 5px;
        float: right;
      }
      #nextButton {
        display: inline-block;
        margin-right: 15px;
        float: right;
      }
      .faded {
        opacity: 0.5;
      }
      div.col-sm-3:nth-child(4) {
        padding: 0px;
      }
      .col-sm-2 > form:nth-child(1) {
        margin-left:30px;
      }
      div.col-sm-2 {
        padding-left: 0px;
      }
      body {
        font-size: 12px;
      }
      h1 {
        font-size: 40px;
        margin-left:50px;
      }

      #scatterPlot {
        height: 500px;
        width: 500px;
      }
      .bold-p {
        font-weight:700;
      }
      
     #optionsPanel {
        margin-left:50px;
      }
      
      div.col-sm-2:nth-child(6) > form:nth-child(1) {
        margin-right:50px;
      }
      .container-fluid > div:nth-child(3) {
        margin-left: 50px;
      }
      
      .blink_this {
        animation: blinker 2s linear infinite;
      }
      @keyframes blinker {
        50% {
          background-color: #f4f0f0;
        }
      }
      .disabled {
        pointer-events: none;
        opacity: 0.5;
        background: #CCC;
      }
    "))
  ),
  h1("The relationship between socio-economic indicators and chronic diseases, across Australia"),
  fluidRow(
    fluidRow(
      actionButton("backButton", "Back"),
      actionButton("nextButton", "Next"),
      textOutput("introCaption"),
      id = "storyBox",
      class="blink_this"
    ),
    br(),
    sidebarPanel(
      h4("Variables"),
      selectInput("population", label = "Population age", choices = c("All ages", "60 years and over", "70 years and over")),
      selectInput("disease", label = "Disease", choices = c("Three or more chronic conditions", "Asthma", "Diabetes", "Heart, stroke and vascular")),
      checkboxInput("lobf", "Include line of best fit", value = FALSE, width = NULL),
      checkboxInput("zeroOutliers", "Include zero outliers", value = TRUE, width = NULL),
      selectInput("socialIndex", label = "Social index", choices = c("ISRAD", "ISRD",  "IER", "IEO")),
      textOutput("socialIndexDescription"),
      br(),
      h4("Location"),  
      selectizeInput("focusOnSA2_1", label = "Focus on SA2:", choices = NULL,
                     selected = c("Taroona - Bonnet Hill"), multiple = TRUE, options = NULL),
      selectInput("state.territory", label = "State or territories", choices = c("All states and territories", "NSW", "Victoria", "Queensland", "SA", "WA", "Tasmania", "NT", "ACT")),
      checkboxInput("showSA2Labels", "Show SA2 Labels", value = TRUE, width = NULL),
      width = 4,
      class="disabled",
      id="optionsPanel"
    ),
    mainPanel(
      plotOutput("scatterPlot"),
      width = 5
    ),
    sidebarPanel(
      h4("Notes"),
      p("Some SA2 estimates were removed due to uncertainty relating to low sample sizes."),
      br(),
      p("Each point in the chart represents an SA2 area. The ABS defines SA2 areas to, 'represent a community that interacts together socially and economically', (ABS, 2021)."),
      br(),
      p("Remote territories outside of the mentioned states and territories were not included."),
      br(),
      p("This app is optimised to run on 1080p FHD screens."),
      width=2
    )
  ),
  tags$div(
    h4("References"),
    p("Australian Bureau of Statistics. (2020).", tags$i("National Health Survey: Small Area Estimates, 2017-18 - Australia"), ". ABS.", a("https://www.abs.gov.au/statistics/health/health-conditions-and-risks/national-health-survey/2017-18")),
    p("Australian Bureau of Statistics. (2021).", tags$i("Statistical Area Level 2"), ". ABS.", a("https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026")),
    p("Australian Bureau of Statistics. (2023).", tags$i("Socio-Economic Indexes for Australia (SEIFA), 2021"), ". ABS.", a("https://www.abs.gov.au/statistics/people/people-and-communities/socio-economic-indexes-areas-seifa-australia/latest-release"))
  )
)

#################################################
## SERVER CODE

server <- function(input, output, session) {
  
  observeEvent(input$state.territory, {
    preSelected <- input$focusOnSA2_1
    displaySA2Names <- getDisplaySA2Names(input)
    updateSelectizeInput(session = getDefaultReactiveDomain(),
                         inputId = 'focusOnSA2_1', 
                         choices = c("None", displaySA2Names), 
                         server = TRUE,
                         selected = preSelected)
  })
  
  # SEQUENCE UPON LOAD
  
  captionIndex <- reactiveValues(data = 1)
  addClass("backButton", "faded")
  
  observeEvent(input$backButton, {
    if (captionIndex$data > 1) {
      captionIndex$data <- captionIndex$data - 1
      preSelected <- getSelectedLocations(captionIndex$data)
    }

    controlButtons(captionIndex)
    
    preSelected <- getSelectedLocations(captionIndex$data)
    
    displaySA2Names <- getDisplaySA2Names(input)
    updateSelectizeInput(session = getDefaultReactiveDomain(),
                         inputId = 'focusOnSA2_1', 
                         choices = c("None", displaySA2Names), 
                         server = TRUE,
                         selected = preSelected)
  })
  
  observeEvent(input$nextButton, {
    removeClass("storyBox", "blink_this")
    
    if (captionIndex$data < 5) {
      captionIndex$data <- captionIndex$data + 1
      preSelected <- getSelectedLocations(captionIndex$data)
    }
    
    controlButtons(captionIndex)
    
    preSelected <- getSelectedLocations(captionIndex$data)

    displaySA2Names <- getDisplaySA2Names(input)
    updateSelectizeInput(session = getDefaultReactiveDomain(),
                         inputId = 'focusOnSA2_1', 
                         choices = c("None", displaySA2Names), 
                         server = TRUE,
                         selected = preSelected)
  })
  
  output$introCaption <- renderText({
    introCaptions[captionIndex$data]
  })
  
  # END SEQUENCE UPON LOAD
  
  output$scatterPlot <- renderPlot({
    displayData <- getDisplayData(input)
    
    if(!input$zeroOutliers) {
      displayData[displayData==0] <- NA
    }

    if(input$lobf) {
      modelWidth <- 0.3
    } else {
      modelWidth <- 0
    }

    if(input$showSA2Labels) {
      textSize <- 4
    } else {
      textSize <- 0
    }
    
    scatter <- ggplot(displayData, aes(x, y, label=sa2Label)) + 
      geom_point(alpha = 0.2, size=2 ) +
      geom_smooth(method=lm, se=FALSE, linetype="solid", color="darkred", linewidth = modelWidth) +
      geom_point(data = displayData[displayData$sa2Label %in% input$focusOnSA2_1,], size=3, color="red") +
      geom_shadowtext(aes(label=ifelse(sa2Label %in% input$focusOnSA2_1, as.character(sa2Label), '')),
                color="red",
                fontface = "bold",
                hjust=-0.1,
                vjust=.35,
                bg.color = 'white',
                bg.r = 0.2,
                size = textSize) +
      theme_bw() +
      theme(plot.caption = element_text(hjust = 0)) +
      labs(title=paste("For", tolower(input$population), "in", input$state.territory),
           x=input$socialIndex,
           y=paste(input$disease, "(%)"))
    (scatter)
  }, 
  height = 650)

  output$socialIndexDescription <- renderText({
    socialIndexDescriptionHash(input$socialIndex)
  })
}

shinyApp(ui, server)