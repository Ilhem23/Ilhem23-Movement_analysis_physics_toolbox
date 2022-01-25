

library(plyr)  
library(dplyr) 
library(shinydashboard)
library(shiny)
library(tidyr)
library(ggplot2)
library(highcharter)
library(lubridate)
library(stringr)
library(withr)
library(treemap)
library(DT)
library(shinyBS)
library(shinyjs)
library(WDI)
library(geosphere)
library(magrittr)
library(shinycssloaders)
library(timevis)
library(readr)
library(Rcpp)
library(RcppRoll)
library(groupdata2)
library(checkmate)
library(magrittr)

################ header##########
header <- 
    dashboardHeader( title = HTML("Movement Analysis"), 
                     disable = FALSE 
    )

##################### sidebar ##########################
siderbar <- 
    dashboardSidebar( 
        width = 200,
        sidebarMenu(
            id = 'sidebar',
            style = "position: relative; overflow: visible;",
            
            menuItem( "Presentation", tabName = 'presentation', icon = icon('chalkboard-teacher') ),
            
            menuItem( "Project Details", tabName = 'details', icon = icon('info') ),
            
            menuItem( "Upload", tabName = 'upload', icon = icon('upload') ),
            
            menuItem( "DataFrame", tabName = 'dataframe', icon = icon('table') ),
            
            menuItem( "Extract Features", tabName = "features", icon = icon('cogs'), startExpanded = F,
                      menuSubItem('Extract Features Long', tabName = "features_long", icon = icon('cog')),
                      menuSubItem('Extract Features Wide', tabName = "features_wide", icon = icon('cogs'))
                     
            ),
           
            
            menuItem( "Source Code", tabName = 'code', icon = icon('columns')
                      )
        )
    )

#######################body##########################
body <- dashboardBody( 
   
    tags$head(
      
      tags$style(HTML(" body { min-height: 411px !important; } ")),
        ## modify the dashboard's skin color
        tags$style(HTML('
                       /* logo */
                       .skin-blue .main-header .logo {
                       background-color: #006272;
                       }
                       /* logo when hovered */
                       .skin-blue .main-header .logo:hover {
                       background-color: #006272;
                       }
                       /* navbar (rest of the header) */
                       .skin-blue .main-header .navbar {
                       background-color: #006272;
                       }
                       /* active selected tab in the sidebarmenu */
                       .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                       background-color: #006272;
                                 }
                       ')
        ),
        
        ## modify icon size in the sub side bar menu
        tags$style(HTML("
                       
                      .sidebar .sidebar-menu .fa {
                      font-size: 12px;
                      }
                      .sidebar .sidebar-menu .treeview-menu>li>a>.glyphicon {
                      font-size: 13px;
                      }
                      /* Hide icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview>a>.fa-angle-left {
                      display: none;
                      } 
                      "
        )) ,
        
        tags$style( HTML("hr {border-top: 1px solid #000000;}") ),
        
        ## to not show error message in shiny
        tags$style( HTML(".shiny-output-error { visibility: hidden; }") ),
        tags$style( HTML(".shiny-output-error:before { visibility: hidden; }") ),
        
        ## heand dropdown menu size
        #tags$style(HTML('.navbar-custom-menu>.navbar-nav>li>.dropdown-menu { width:100px;}'))
        tags$style(HTML('.navbar-custom-menu>.navbar-nav>li:last-child>.dropdown-menu { width:10px; font-size:10px; padding:1px; margin:1px;}')),
        tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > h4 {width:0px; font-size:0px; padding:0px; margin:0px;}')),
        tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > p {width:0px; font-size:0px; padding:0px; margin:0px;}'))
    ),
    
    ## 3.1 Dashboard body --------------
    tabItems(
      
        tabItem( tabName = 'details',
                 
                    fluidPage(htmlOutput("project"))
                 
        ),
        tabItem( tabName = 'presentation',
                 fluidRow(
                 style = "display: flex !important; justify-content: center !important;",
                 img(src = "gustave.png", height = 80, width = 400),
                 
                 ),
                 fluidRow(
                   style = "display: block !important; justify-content: center !important;",
                   
                   div(
                     
                     h2(" Project Data Analysis"),
                     h3('Movement Data Analysis'),
                     h4('- Physics Toolbox Suite - '),
                     h4('- Realized with R and Shiny - '), 
                     align = 'center'
                   ),
                     div(
                     h3('Supervised by:'),
                     h4('Dr. Etienne Côme'),
                     
                    
                     align = 'center'
                   ),
                   div(
                    
                     h3('Elaborated by:'),
                     h4('AISSAOUI Ilhem'),
                     h4('Diab Bilal'),
                     h4('M2 SIA'),
                     
                     align = 'center'
                   )
                 )
                 
        ),
        
        tabItem( tabName = 'upload',
                
                 div(id = 'ci_howto_ex', fileInput(
                     "file",
                     "Choose CSV File",
                     accept = c(
                         "text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv"
                     )
                 )
                 ),
                 
        ),
        
        
        tabItem( tabName = 'features_long',
                 fluidPage(
                   fluidRow(
                     h3("Extract Features Long", align ='center'),
                     
                     column(12,
                            wellPanel(
                              fluidRow(
                                column(4,sliderInput(
                                  "threshhold_t", label = "Time:",
                                  min = 0, max = 1, value = 0.5
                                )),
                                column(4,sliderInput(
                                  "threshhold_m", label = "Mean:",
                                  min = 0, max = 1, value = 0.45
                                )),
                                column(4,sliderInput(
                                  "threshhold_sd", label = "Standard Deviation:",
                                  min = 0, max = 1, value = 0.2
                                ))
                               
                              )
                  
                            )       
                     ),
                     
                     column(12,
                            plotOutput("plotExtractL", brush = "plot_brush")
                     ),
                   ),
                   fluidRow(
                     h4("DataFrame after extract features long", align ='center'),
                    
                     
                   ),
                   fluidRow(
                     style = "display: flex !important; justify-content: center !important;",
                     radioButtons("radio", "Choose Movement type", c('X','O','N'), selected = 'X', inline = T),
                    
                  
                     ),
                   fluidRow(
                     style = "display: flex !important; justify-content: center !important;",
                    
                     downloadButton("Download", label = "Download csv ", class = "butt2"),
                     tags$head(tags$style(".butt2{color: blue !important;} .fa{font-size: 12px;}")), 
                     
                     
                   ),
                   fluidRow(
                   
                            
                            DT::dataTableOutput("dataFrameTable2")
                     
                   )
                   
                 )
                 
        ),
        
       
        tabItem( tabName = 'features_wide',
                 fluidPage(
                   fluidRow(
                     h3("Extract Features Wide", align ='center'),
                     
                     column(12,
                            wellPanel(
                              fluidRow(
                                column(4,sliderInput(
                                  "threshholdT", label = "Time:",
                                  min = 0, max = 1, value = 0.5
                                )),
                                column(4,sliderInput(
                                  "threshholdM", label = "Mean:",
                                  min = 0, max = 1, value = 0.45
                                )),
                                column(4,sliderInput(
                                  "threshholdSd", label = "Standard Deviation:",
                                  min = 0, max = 1, value = 0.2
                                ))
                                
                              )
                              
                            )       
                     )
                   ),
                   fluidRow(
                     h4("DataFrame after extract features wide", align ='center'),
                     
                     
                   ),
                   fluidRow(
                     style = "display: flex !important; justify-content: center !important;",
                     radioButtons("radio2", "Choose Movement type", c('X','O','N'), selected = 'X', inline = T),
                     
                     
                   ),
                   fluidRow(
                     style = "display: flex !important; justify-content: center !important;",
                     
                     downloadButton("Download2", label = "Download csv ", class = "butt1"),
                     tags$head(tags$style(".butt1{color: blue !important;} .fa{font-size: 12px;}")), 
                     
                     
                   ),
                   fluidRow(
                     
                     
                     DT::dataTableOutput("dataFrameTable3")
                     
                   )
                   
                 )
                
        ),
        tabItem( tabName = 'dataframe',
                 
                 h3("Sensors: Force-g, Linear Accelerometer, Gyroscope", align ='center'),
                 DT::dataTableOutput("dataFrameTable")
        ),
        
        
        tabItem( tabName = 'code',
                 fluidPage(htmlOutput("code"))
                 )
    )
)



## put UI together --------------------
ui <- dashboardPage(title = "M2 SIA", header, siderbar, body)


server <- function(input, output) {
  ### upload file
  file_upload <- reactive({
    inputFile <- input$file
    if (is.null(inputFile))
      return(NULL)
    df <-
      read.csv(
        inputFile$datapath,
        header = TRUE ,
        sep = ","
      )
    return(df)
  })
  ### upload html file of project details
    output$project <- renderUI({
        includeHTML("Project intro to DS.html")
    })
    ### upload html file of notebook
    output$code <- renderUI({
      includeHTML("data_analysis_toolbox.html")
    })
    ### DataFrame of input file
    output$dataFrameTable = renderDataTable(df <- file_upload() ,
                                     options = list(
                                       pageLength = 15,
                                       searching = FALSE ,
                                       scrollX = TRUE,
                                       lengthChange = FALSE
                                     ))
    ### DataFrame of result extract long 
    output$dataFrameTable2 = renderDataTable(df <- extract_feature_long(df <- file_upload(), input$threshhold_m, input$threshhold_sd, input$threshhold_t) ,
                                            options = list(
                                              pageLength = 10,
                                              searching = FALSE ,
                                              scrollX = TRUE,
                                              lengthChange = FALSE
                                            ))
    ### DataFrame of result extract wide 
    output$dataFrameTable3 = renderDataTable(df <- extract_feature_large(df <- file_upload(), input$threshholdM, input$threshholdSd, input$threshholdT) ,
                                             options = list(
                                               pageLength = 10,
                                               searching = FALSE ,
                                               scrollX = TRUE,
                                               lengthChange = FALSE
                                             ))
#############################################################
###################### function definition ##################
    
    extract_feature_long = function(data,threeshold_mean,threeshold_sd,threeshold_t){
      if("...11" %in% colnames(data))
      {
        data = data %>% select(-c(...11))
      }
      if("X" %in% colnames(data))
      {
        data = data %>% select(-c(X))
      }
      
      data= data %>% mutate(gN= abs((gFx * gFx) + (gFy * gFy) + (gFz * gFz) - 0.8 ))
      data= data %>% mutate(gNm = roll_meanl(gN, 10))
      data= data %>% mutate(gNsd = roll_sdl(gN, 10))
      dataCopy= data %>% filter(gNm <= threeshold_mean | gNsd <= threeshold_sd)
      data= data %>% filter(gNm > threeshold_mean & gNsd > threeshold_sd)
      data= data %>% mutate(tm= lag(time)) %>% mutate(dt = time - tm) %>% select(-c(tm))
      data = data %>% mutate(switch= if_else(dt > threeshold_t, 1, 0))
      data= data %>% mutate(switch= if_else(is.na(dt),1,switch))
      vector= rep(1, sum(data$switch))
      data= group(data, n=vector, method='l_starts',col_name = 'pid', starts_col = 'switch')
      data = data %>% mutate(pid= as.numeric(pid), pid= if_else(switch== 1, 0, pid))
      dataCopy= dataCopy %>% mutate(dt= 0)
      dataCopy= dataCopy %>% mutate(pid= 0)
      dataCopy= dataCopy %>% mutate(switch= 0)
      data_res= bind_rows(data, dataCopy)
      return(data_res)
    }
    
    extract_feature_large= function(data,threeshold_mean,threeshold_sd,threeshold_t){
      X_long=extract_feature_long(data,threeshold_mean,threeshold_sd,threeshold_t)
      X_long= X_long %>% drop_na()
      dataCopy= X_long %>% group_by(pid) %>% filter(sum(dt) > 1 & sum(dt) <3 ) %>% 
        mutate(localtime= time - min(time)) %>% mutate(tbin= floor(localtime * 2))
      
      dataCopy = dataCopy %>% group_by(pid, tbin) %>%  summarise(
        gFx_mean= mean(gFx),
        gFy_mean= mean(gFy),
        gFz_mean= mean(gFz),
        ax_mean= mean(ax),
        ay_mean= mean(ay),
        az_mean= mean(az),
        wx_mean= mean(wx),
        wy_mean= mean(wy),
        wz_mean= mean(wz)
      ) %>% pivot_wider( 
        pid,
        tbin,
        values_from = c(
          gFx_mean,gFy_mean,gFz_mean,ax_mean,ay_mean,az_mean,wx_mean,wy_mean,wz_mean),
        values_fill = 0)
      
      return(dataCopy)
    }
    
    
##################################################################
############ plot of extract features long ##################
output$plotExtractL <- renderPlot({
  x_large = extract_feature_long(df <- file_upload(), input$threshhold_m, input$threshhold_sd, input$threshhold_t)
  ggplot(x_large %>% filter(pid > 0)) + geom_line(aes(
    x = time,
    y = gNsd,
    color = as.factor(pid),
    group = pid
  )) + geom_point(data = x_large %>% filter(pid == 00),
                  aes(x = time, y = gNsd),
                  color = "black")
})
########### export the csv file #################"
    output$Download <-
      downloadHandler(
        filename = function() {
          paste('AISSAOUI_DIAB_OUTPUT_LONG_',input$radio, '.csv', sep = '')
        },
        content = function(content) {
          write.csv(extract_feature_long(df <- file_upload(), input$threshhold_m, input$threshhold_sd, input$threshhold_t),
                    content)
        }
      )
    output$Download2 <-
      downloadHandler(
        filename = function() {
          paste('AISSAOUI_DIAB_OUTPUT_LARGE_',input$radio2, '.csv', sep = '')
        },
        content = function(content) {
          write.csv(df <- extract_feature_large(df <- file_upload(), input$threshholdM, input$threshholdSd, input$threshholdT),
                    content)
        }
      )
  
    
    output$result_long <- renderPrint({
      x_large <- extract_feature_long(df <- file_upload(), input$threshhold_m, input$threshhold_sd, input$threshhold_t)
      brushedPoints(x_large, input$plot_brush, allRows = TRUE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
