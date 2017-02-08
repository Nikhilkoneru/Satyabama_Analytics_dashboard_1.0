library(shinydashboard)
library(shiny)
library(plotly)
library(DT)

dashboardPage(
  dashboardHeader(title = 'Reporting Dashboard',
                  tags$li(class = "dropdown",
                          tags$a(href="https://www.linkedin.com/in/konerunikhil", target="_blank", 
                                 tags$img(height = "20px", alt="SNAP Logo", src="https://www.snap.uaf.edu/sites/default/files/pictures/snap_symbol_color.png")
                          )
                  ),
                  dropdownMenuOutput('dropdown')
                  
                  
                  
  ),
  dashboardSidebar(sidebarMenu(
    
    menuItem("INPUT", tabName = "input", icon = icon("dashboard"),
             selectInput("selectexam", label = h3("Select box"), 
                         choices = list("Semester" = 1, "Mid-Semester" = 2),
                         selected = 1),
             fileInput("uploadFile1", "STUDENTS PDF/XLS  FILE",accept = c(".pdf,.xlsx,.xls")),
             fileInput("uploadFile2", "STUDENTS XLS DETAILS FILE",accept = c(".xlsx")),
             fileInput("uploadFile3", "SUBJECT CODE XLS FILE",accept = c(".xlsx")),
             numericInput("sub","ENTER NO OF SUBJECTS",8,min=1,max=20)),
    menuItem("RESULTS", tabName = "student", icon = icon("th")),
    
    menuItem("RESULTS ANALYSIS", tabName = "teacher", icon = icon("th")),
    menuItem("DOWNLOAD SAMPLE DATA", tabName = "teacher", icon = icon("th"),tags$li(class = "dropdown",
                                                                                    tags$a(href="https://drive.google.com/drive/folders/0BzOpodJQqn6cV0VBTVlHUWVUN3c", target="_blank", 
                                                                                           tags$img(height = "40px", alt="SNAP Logo", src="http://www.sathyabamauniversity.ac.in/img/logo-1.png")
                                                                                    )
    ))
    
  )),
  dashboardBody(
    tags$head(tags$style(HTML('
                              .main-header .logo {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 24px;
                              }
                              '))),
    
    tabItems(
      tabItem(tabName = "input"),
      
      tabItem(tabName = "student",
              fluidRow(
                valueBoxOutput("progressBox1"),
                valueBoxOutput("progressBox2"),
                valueBoxOutput("progressBox3")
              ),
              
              # infoBoxes with fill=TRUE
              fluidRow(
                infoBoxOutput("progressBox4"),
                infoBoxOutput("progressBox5"),
                infoBoxOutput("progressBox6")
              ),
              fluidRow(
                box(width = 12,
                    uiOutput("selectme"),
                    box(width= 12,title = "Students Marks List", status = "primary", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                        downloadButton('downloadData', 'Download'),
                        radioButtons("radio", label = "Sort Order",choices = list("PERCENTAGE" = 1, "REGNO" = 2),selected = 1),
                        DT::dataTableOutput('projects')),
                    box(width= 12,title = "Students Range-Marks List", status = "primary", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                        downloadButton('downloadData1', 'Download'),
                        radioButtons("radio2", label = "Sort Order",choices = list("PERCENTAGE" = 1, "REGNO" = 2),selected = 1),
                        sliderInput("slider2", label = "Percentage", min = 0, max = 100, value = c(50, 100)),
                        DT::dataTableOutput('projectsss')),
                    box(width= 12,title = "RESULTS INFO", status = "primary", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                        downloadButton('downloadData2','Download'),
                        DT::dataTableOutput('dt3')),
                    box(width= 12,title = "SUMMARY", status = "primary", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                        downloadButton('downloadData3','Download'),
                        DT::dataTableOutput("summary")),
                    box(width= 12,title = "SUBJECT WISE MARKS", status = "primary", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                        downloadButton('downloadData4', 'Download'),
                        
                        uiOutput("variants"),
                        DT::dataTableOutput('subjectwise')),
                    box(width= 12,title = "SUBJECTS INFO", status = "primary", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                        
                        downloadButton('downloadData5', 'Download'),
                        
                        DT::dataTableOutput('dt2')),
                    box(width= 12,title = "ABSCENT LIST", status = "primary", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                        
                        downloadButton('downloadData6', 'Download'),
                        
                        DT::dataTableOutput('abscentlist'))
                )
              )),
      tabItem(tabName = "teacher",
              fluidRow(
                box(width = 12,
                    uiOutput("variants1"),
                    box(title = "PIE CHAR", background = "blue", solidHeader = TRUE,width = 12,
                    numericInput("regreg","ENTER NO OF SUBJECTS",3411201,min=1,max=20),
                    plotlyOutput("graph6"))
                   
                    
                )))
      
    )))