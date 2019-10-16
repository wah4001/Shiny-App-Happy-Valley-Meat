library(shiny)
library(plotly)
library(DT)
library(shinydashboard)
library(readxl)


###############  DATA LOAD IN  ###############

hvm_carcass = read.csv('carcass_calculator_data.csv', header = T)
hvm_protein = read.csv('hvm_protein.csv', header = T)
hvm_calories = read.csv('hvm_calories.csv', header = T)

###############  UI  ###############

shinyUI(
  
  dashboardPage(skin = 'blue',
    dashboardHeader(title = span("Happy Valley Meat", 
                            style = "color: White; font-size: 22px; font-weight: bold; "),
                    tags$li(a(img(src = 'HVMC_logo_green.png', height = "30px"),
                              style = "padding-top:10px; padding-bottom:10px;"),
                            class = "dropdown")),
    
    dashboardSidebar(
      
      sidebarUserPanel("MAIN MENU",
                       subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
                       image = 'HVMC_logo_green.png'),
      
      sidebarMenu(
        menuItem("BEEF CUT", tabName = "beef", icon = icon("list-alt")),
        menuItem("PROTEIN", tabName = "protein", icon = icon("list-alt")),
        menuItem("KILOCALORIES", tabName = "kcal", icon = icon("list-alt")),
        collapsed = F)
      
      ),

    dashboardBody(

      tabItems(

###############  BEEF  ###############

        tabItem(tabName = "beef",
            fluidPage(
              
              titlePanel(h1(id = 'first', "Environmental Impact Report")), 
              tags$style(HTML("#first{color: grey49; font-size: 30px; font-weight: bold;}")),
              
              sidebarLayout(
                
                sidebarPanel(
                  numericInput('weight_beef', 'Enter a value in lbs of selected cut of meat', 
                               1000, min = 1, max = NA, step = 1),
                  radioButtons("cut", label = 'Cut', choices = hvm_carcass$cut)
                  
                ),
                
                mainPanel(
                  tabsetPanel(type = 'tabs',
                              tabPanel('Table', icon = icon("table"), dataTableOutput("tbl_cut", width = "95%", height = "auto")),
                              tabPanel('Plots', icon = icon("list-alt"), 
                                       tabsetPanel(type = 'pills',
                                         tabPanel('Barchart', icon = icon("bar-chart-o"), plotlyOutput("tbl_cut_bar")),
                                         tabPanel('3D', icon = icon("circle"), plotlyOutput("tbl_cut_point"))
                                         )
                                       )
                              )
                          )
                  )
                )
            ),


###############  PROTEIN  ###############

        tabItem(tabName = "protein",
                fluidPage(
                  titlePanel(h1(id = 'first', "Environmental Impact Report")), 
                  tags$style(HTML("#first{color: grey49; font-size: 30px; font-weight: bold;}")),
                  
                  sidebarLayout(
                    
                    sidebarPanel(
                      numericInput('weight_pro', 'Enter a value of protein in lbs', 1000, min = 1, max = NA, step = 1),
                      checkboxGroupInput("food_type_pro", label = "Food Type", unique(hvm_protein$food_type), 
                                         selected = hvm_protein$food_type)
                    ),
                    
                    mainPanel(
                      tabsetPanel(type = 'tabs',
                                  tabPanel('Table', icon = icon("table"), dataTableOutput("tbl_protein", width = "95%", height = "auto")),
                                  tabPanel('Plots', icon = icon("bar-chart-o"), plotlyOutput("pro.all")))
                      )
                    )
                  )
                ),


###############  KCAL  ###############

        tabItem(tabName = "kcal",
                fluidPage(
                  titlePanel(h1(id = 'first', "Environmental Impact Report")), 
                  tags$style(HTML("#first{color: grey49; font-size: 30px; font-weight: bold;}")),
                  
                  sidebarLayout(
                    
                    sidebarPanel(
                      numericInput('weight_cal', 'Enter a value in thousand kcal', 1000, min = 1, max = NA, step = 1),
                      checkboxGroupInput("food_type_kcal", label = "Food Type", unique(hvm_calories$food_type), 
                                         selected = hvm_calories$food_type)
                    ),
                    
                    mainPanel(
                      tabsetPanel(type = 'tabs',
                                  tabPanel('Table', icon = icon("table"), dataTableOutput("tbl_kcal", width = "95%", height = "auto")),
                                  tabPanel('Plots', icon = icon("bar-chart-o"), plotlyOutput("kcal.all")) 
                                  )
                      )
                    )                  
                  )
                )
          ))))
