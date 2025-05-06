library(rsconnect)
library(shiny)
library(shinydashboard)
library(shinycssloaders)

header <- 
  dashboardHeader(title = "Child Malnutrition")

sidebar <- 
  dashboardSidebar(
    sidebarMenu(
      menuItem("KDE", tabName = "kde", icon = icon("th")),
      menuItem("Overlay map India", icon = icon("th"), tabName = "overlayIndia"),
      # menuItem("Overlay map Karnataka", icon = icon("th"), tabName = "overlayKar"),
      menuItem("LISA India - Univariate", icon = icon("th"), tabName = "indialisauni"),
      menuItem("LISA India - Bivariate", icon = icon("th"), tabName = "indialisabi"),
      menuItem("About", icon = icon("th"), tabName = "about")
    )
  )

body <- 
  dashboardBody(
    tabItems(
        tabItem(tabName = "kde",
              fluidRow(h3("KDE heatmap for Child malnutrition in India.")),
                fluidRow(
                         radioButtons("Indiaradio", h5("Select one of the malnutrition condition:"),
                                      choices = list("Stunting",
                                                     "Underweight",
                                                     "Wasted" ,
                                                     "Overweight",
                                                     "Obese"),
                                        )
                         ),
              fluidRow("Please wait as the map and data gets loaded. This might take few minutes."),
              fluidRow(
                # colum n(width = 6,  plotOutput("KDEIndia"))#,
                tabBox(
                  width = 12,
                  tabPanel(
                    status = "primary",
                    title = "India - KDE",
                    withSpinner(plotOutput("KDEIndia", height = "550px"))
                  ),
                  tabPanel(
                    status = "success",
                    title = "Karnataka - KDE",
                    withSpinner(plotOutput("KDEKarnataka", height = "550px"))
                  )
                )
                
                
              )
      ),
      tabItem(tabName = "overlayIndia",
              # tabBox(
              #   width = 12,
              #   tabPanel(
              #     status = "primary",
              #     title = "India - Overlay",
              #     htmlOutput("QgisOverlayIndia")
              #   ),
              #   tabPanel(
              #     status = "success",
              #     title = "Karnataka - Overlay",
              #     # htmlOutput("QgisOverlayKarnataka")
              #   )
              # )
              fluidPage(
                fluidRow(h3("Overlay using KDE for Child malnutrition in India.")),
                fluidRow("Please wait as the map and data gets loaded. This might take few minutes."),
                fluidRow(
                withSpinner(htmlOutput("QgisOverlayIndia"))
                )
              )
      ),
      # tabItem(tabName = "overlayKar",
      #         # tabBox(
      #         #   width = 12,
      #         #   tabPanel(
      #         #     status = "primary",
      #         #     title = "India - Overlay",
      #         #     htmlOutput("QgisOverlayIndia")
      #         #   )#,
      #           # tabPanel(
      #           #   status = "success",
      #           #   title = "Karnataka - Overlay",
      #           #   # htmlOutput("QgisOverlayKarnataka")
      #           # )
      #         
      #         fluidPage(
      #           # fluidRow(
      #             htmlOutput("QgisOverlayKarnataka")
      #           # )
      #         )
      # ),
      tabItem(tabName= "indialisauni",
              fluidPage(
                fluidRow(h3("LISA - Univariate Local Moran’s I cluster maps for malnutrition condition in India")),
                fluidRow("Please wait as the map and data gets loaded. This might take few minutes."),
                fluidRow( column(width = 6, "Stunting"),column(width = 6, "Underweight")),
                fluidRow(
                  column(width = 6, plotOutput("LISAStunt", height = "250px")),
                  column(width = 6, plotOutput("LISAUnder", height = "250px"))
                  ),
                fluidRow(
                  column(width = 6, "Wasting"),
                  column(width = 6, "Overweight")
                  ),
                fluidRow(
                  column(width = 6, plotOutput("LISAWast", height = "250px")),
                  column(width = 6, plotOutput("LISAOver", height = "250px"))
                ),
                fluidRow(column(width = 6, "Obese")),
                fluidRow(
                  column(width = 12, plotOutput("LISAObese", height = "250px"))
                )
              )
              ),
      tabItem(tabName = "indialisabi",
              fluidPage(
                fluidRow(h3("LISA - Bivariate Local Moran’s I cluster maps for malnutrition condition in India")),
                fluidRow("Please wait as the map and data gets loaded. This might take few minutes."),
                
                fluidRow(
                  column(width = 3, "Stunting - Underweight")
                ),
                fluidRow(
                  column(width = 3, plotOutput("BiLISAStuntUnder", height = "200px"))
                ),
                fluidRow(
                  column(width = 3, "Stunting - Wasting"),
                  column(width = 3, "Underweight - Wasting")
                ),
                fluidRow(
                  column(width = 3, plotOutput("BiLISAStuntWast", height = "200px")),
                  column(width = 3, plotOutput("BiLISAUnderWast", height = "200px"))
                ),
                fluidRow(
                  column(width = 3, "Stunting - Overweight"),
                  column(width = 3, "Underweight - Overweight"),
                  column(width = 3, "Wasting - Overweight"),
                ),
                fluidRow(
                  column(width = 3, plotOutput("BiLISAStuntOver", height = "200px")),
                  column(width = 3, plotOutput("BiLISAUnderOver", height = "200px")),
                  column(width = 3, plotOutput("BiLISAWastOver", height = "200px"))
                ),
                fluidRow(
                  column(width = 3, "Stunting - Obese"),
                  column(width = 3, "Underweight - Obese"),
                  column(width = 3, "Wasting - Obese"),
                  column(width = 3, "Overweight - Obese")
                ),
                fluidRow(
                  column(width = 3, plotOutput("BiLISAStuntObese", height = "200px")),
                  column(width = 3, plotOutput("BiLISAUnderObese", height = "200px")),
                  column(width = 3, plotOutput("BiLISAWastObese", height = "200px")),
                  column(width = 3, plotOutput("BiLISAOverObese", height = "200px"))
                )
              )
              ),
      tabItem( tabName = "about",
               h3("This work has been partially supported by the Mathematical Research Impact Centric Support (MATRICS) grant by the Science and Engineering Board (SERB). This paper has also benefited from the inputs from members of GVCL and EHRC. This study has been possible solely because of the open data available in the public domain, specifically the household- and individual-level raw data and fact sheets for NFHS-4."),
               h6("")
      )
      
    )
  ) 

dashboardPage(header, sidebar, body)