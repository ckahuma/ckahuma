library(shinydashboard)
library(DT)
#library(shiny)

dashboardPage(
  skin = "purple",
  dashboardHeader(title="MDA-App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home",icon = icon("home"), tabName="Home"),
      menuItem("View Dataset" ,icon = icon("table"), tabName="ViewDataset"),
      menuItem("Summary",icon = icon("list-alt"), tabName="Summary"),
      menuItem("Visualization", icon = icon("bar-chart-o"),tabName="Plot"),
      menuItem("Technical Insights", tabName="technical" ,icon = icon("info-circle")),
      menuItem("Inferences",icon = icon("user-edit"), tabName="Inferences"),
      menuItem("Help", tabName="Help" ,icon = icon("info-circle"))
    )
  ),
  dashboardBody(
   
    #UI Info Box code
    fluidPage({
      fluidRow(
        infoBoxOutput("valueBox1"),
        infoBoxOutput("valueBox2"),
        infoBoxOutput("valueBox3")
        # infoBoxOutput("valueBox4")
        
      )
    }),
   
    tabItems(
      tabItem(tabName = "Home", 
              
              h1("Welcome to MDA- App"),
              
              h3(
                p("MDA-App is an R Application that analyzes app details from Google Play Store. 
                It generates insights which are intended to help developers optimize their applicatios to enhance their downloads.
                This system analyses how different application features affect the download rate of an application on play store.
                The app details can be visualized giving a clear understanding of the data"),
                p("The Internet is a true gold mine of data. E-commerce and review sites are brimming with a lot of untapped data with a prominent potential to convert into meaningful insights that can help with robust decision making.
              Here, we explore using data science and machine learning techniques on data retrieved from one such avenue on the internet, the Google Play Store.
              Details of Dataset:
                
                The Play Store apps data has enormous potential to drive app-making businesses to success. Actionable insights can be drawn for developers to work on and capture the Android market. The dataset is chosen from Kaggle. It is the web scraped data of 10k Play Store apps for analyzing the Android market.
                  It consists of in total of 10841 rows and 13 columns."),
                
                p("The columns of the dataset are as follows:"),
                
                p("1) App (Name)") ,
                
                p("2) Category (App)"),
                
                p("3) Rating (App)") ,
                
                p("4) Reviews (User)"),
                
                p("5) Size (App)"),
                
                p("6) Installs (App)"),
                
                p("7) Type (Free/Paid)"),
                
                p("8) Price (App)"),
                
                p("9) Content Rating (Everyone/Teenager/Adult)"),
                
                p("10) Genres (Detailed Category)"),
                
                p("11) Last Updated (App)"),
                
                p("12) Current Version (App)"),
                
                p("13) Android Version (Support)")
              
              )),
              
      
      tabItem(tabName = "ViewDataset",
              h2("Google Play store dataset"),
              
              fluidPage(
                
                
                # App title ----
                titlePanel("Uploading Files"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    
                    # Input: Select a file ----
                    fileInput("file1", "Choose CSV File",
                              multiple = TRUE,
                              accept = c(
                                ".csv")),
                    
                    uiOutput("selectfile"),
                    
                    # Horizontal line ----
                    tags$hr(),
                    
                    # Input: Checkbox if file has header ----
                    checkboxInput("header", "Header", TRUE),
                    
                    # Input: Select separator ----
                    radioButtons("sep", "Separator",
                                 choices = c(Comma = ",",
                                             Semicolon = ";",
                                             Tab = "\t"),
                                 selected = ","),
                    
                    radioButtons("quote", "Quote",
                                 choices = c(None = "",
                                             "Double Quote" = '"',
                                             "Single Quote" = "'"),
                                 selected = '"'),
                    
                    # Horizontal line ----
                    tags$hr(),
                    
                    # Input: Select number of rows to display ----
                    radioButtons("disp", "Display",
                                 choices = c(Head = "head",
                                             All = "all"),
                                 selected = "head")
                    
                  ),
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    fluidRow(
                      column(12,
                             div(
                               DT::dataTableOutput("mytable")
                             ))
                    )
                    
                  )
                  
                )
                
       
              )
      ),
      
      tabItem(tabName = "Summary",
              h2("Summary Contents"),
              #Code for summary
              
              fluidPage(
                title = "Overall Summary",
                status = "primary",
                solidHeader = T,
                collapsible = TRUE,
                "App data stats",
                verbatimTextOutput("summary")
              ),
              
              #summary by column
              fluidRow(
                box(title = "App Data Column",
                    status = "primary",
                    solidHeader = T,
                    collapsible = TRUE,
                    "To view summary, select a variable below",
                    selectInput("variable","Choose Variable",c("Category","Rating","Reviews","Size",
                                                               "Installs","Type","Content.Rating","Genres",
                                                               "Last.Updated"),selected = "Category")
                    
                ),
                #Box for displaying individual column summary
                box(title = "Individual columns",
                    status = "primary",
                    solidHeader = T,
                    collapsible = TRUE,
                    verbatimTextOutput("summary1")
                )
              )
              ),
      
      
      tabItem(tabName="Plot",
              fluidPage(
                
                fluidRow(
                  
                  tabsetPanel(type="tab",
                              #Dynamic Bar Plots
                              tabPanel("Bar Charts",
                                       selectInput("xs","Choose X-Axis",
                                                   label = "Bar Chart Analysis",
                                                   choices = c("Category",
                                                               "Rating",
                                                               "Type",
                                                               "Content.Rating",
                                                               "Android.Ver"
                                                               )
                                       ),
                                       # box(
                                       #   title = "Bar Chart of Installs",
                                       #   status = "primary",
                                       #   solidHeader = TRUE, 
                                       #   collapsible = TRUE,
                                       #   plotOutput("plot4")
                                       # ),
                                       # box(
                                       fluidRow(
                                         title = "Dynamic Bar Plot",
                                         status = "primary",
                                         solidHeader = TRUE, 
                                         collapsible = TRUE,
                                         
                                         plotOutput("plot5")
                                       )
                                       
                              ),# end of Dynamic Bar Plots
                              tabPanel("Histograms",
                                       h3("Description"),
                                       p("The graphs below show the distribution of different variables (column names) 
                                           with the number of apps in the dataset. The graphs change according to what the user chooses from
                                           the dropdown option."),
                                       fluidRow(
                                         
                                         box(title = "controls for the Graph",status = "primary",solidHeader = T,
                                             sliderInput("bins","Number of Breaks",1,100,50)
                                         ),
                                         box(title = "App Details to show their distributions",status = "primary",solidHeader = T,
                                             "These app deatails were named according to column names in the CSV fie",
                                             selectInput("appDetails","select app property",c("app_rating","price","size"),selected = "app_rating")
                                         )
                                         
                                         
                                         
                                       ),
                                       fluidRow(
                                         
                                         
                                         plotOutput("hist")
                                         
                                         
                                       )
                                       
                                       
                                       
                                       
                              ),
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              # tabPanel("Histograms",
                              #          sliderInput("SlideHist","Histogram Slider",
                              #                      min=1,
                              #                      max=200,
                              #                      value = 10
                              #                      ),
                              #     
                              #         selectInput("histo","Histogram selector",
                              #                      choices = c("Category",
                              #                                  "Installs",
                              #                                  "Last.Updated"
                              #                                  
                              #                      )),
                              #          box(
                              #            title = "Histograms...",
                              #            status = "primary",
                              #            solidHeader = TRUE, 
                              #            collapsible = TRUE,
                              #            plotOutput("hist")
                              #          )
                              # ),
                              
                              tabPanel("Scatter Plots",
                                       selectInput("scat","Scatter Selector",
                                                   label = "Select Scatter Variable",
                                                   choices = c(
                                                               "Size",
                                                               "Category",
                                                               "Type",
                                                               "Rating",
                                                               "Content.Rating",
                                                               "Price",
                                                               "Last.Updated",
                                                               "Current.Ver",
                                                               "Android.Ver"
                                                   )),
                                        
                                       #    box(
                                       #      title = "3D Scatter Plot",
                                       #      status = "primary",
                                       #      solidHeader = TRUE, 
                                       #      collapsible = TRUE,
                                       #      
                                       #      plotOutput("sscatter")
                                       #    ),
                                       # box(
                                       fluidRow(
                                         title = "Dynamic Scatter Plot",
                                         status = "primary",
                                         solidHeader = TRUE, 
                                         collapsible = TRUE,
                                         
                                         plotOutput("scatter1")
                                       )
                                       
                                                  
                                      
                              ),#end of Scatter plot Analysis
                              
                              #Pie Chart Analysis
                              tabPanel("Pie Charts",
                                       selectInput("pie","Pie Chart Selector",
                                                   label = "Select Column to View:",
                                                   choices = c("Content.Rating",
                                                               "Last.Updated","Genres"
                                                   )
                                       ),
                                       box(
                                         title = "Pie Chart for App Category",
                                         status = "primary",
                                         solidHeader = TRUE, 
                                         collapsible = TRUE,
                                         plotOutput("plot2")
                                       ),
                                       box(
                                         title = "Dynamic Pie Chart",
                                         status = "primary",
                                         solidHeader = TRUE, 
                                         collapsible = TRUE,
                                         plotOutput("plot1")
                                       )     
                                       
                              ),#end of Pie Chart Analysis
                              
                              tabPanel("Box Plots",
                                       ui<-fluidPage(
                                         fluidRow(
                                       
                                           plotOutput("bxplot")
                                         ),
                                         fluidRow(
                                           box(
                                             title = "General Box Plot",
                                             status = "primary",
                                             solidHeader = TRUE, 
                                             collapsible = TRUE,
                                             
                                             highchartOutput("hc_plot3")
                                             
                                             
                                             
                                           ),
                                           box(title = "Variation Categories",status = "primary",solidHeader = T,
                                               selectInput("bxplot","select category",c("Size",
                                                                                        "Price",
                                                                                        "Category",
                                                                                        "Type",
                                                                                        "Rating",
                                                                                     
                                                                                        "Reviews",
                                                                                       
                                                                                      
                                                                                        "Android.Ver"),selected = "boxplot_variation")
                                           )
                                           
                                           
                                         )
                                       )
                                       
                                       
                               
                              ),
                              
                              tabPanel("3D-BAR PLOTS",
                                       
                                       selectInput("ab","Density Plots X Axis",
                                                   label = "Analyze Density ",
                                                   choices = c("Rating",
                                                               "Reviews",
                                                               "Size",
                                                               "Price",
                                                               "Current.Ver",
                                                               "Android.Ver"
                                                   )),
                                       # box(
                                       #   title = "Genre/Install Density Distribution",
                                       #   
                                       #   status = "primary",
                                        #   solidHeader = TRUE, 
                                       #   collapsible = TRUE,
                                       #   plotOutput("plot")
                                       # ),
                                       # box(
                                       fluidRow(  
                                       title = "Dynamic Density Distribution",
                                         status = "primary",
                                         solidHeader = TRUE, 
                                         collapsible = TRUE,
                                       highchartOutput("hc11")
                                       )
                                       
                              ),
                              
                              tabPanel("Pyramid Plots",
                                       selectInput("pyramid","Density Plots X Axis",
                                                   label = "Analyse Pyramids",
                                                   choices = c("Content.Rating","Category",
                                                               "Type","Rating",
                                                               "Size",
                                                               "Price",
                                                               "Reviews",
                                                               "Android.Ver"
                                                   )),
                                                   
                                                  fluidRow( 
                                                     title = "Most Popular Categories",
                                                     status = "primary",
                                                     solidHeader = TRUE, 
                                                     collapsible = TRUE,
                                                     highchartOutput("pyrmd1")
                                                   )
                                        
                                       
                                       
                              ),
                              
                              tabPanel("Bubble Plots",
                                       selectInput("bubble","Bubble Plots",
                                                   label = "Analyse using Bubble plots",
                                                   choices = c("Content.Rating","Category",
                                                               "Type","Rating",
                                                               "Size",
                                                               "Price",
                                                             
                                                               "Android.Ver","Genres"
                                                   )),
                                       
                                       fluidRow( 
                                         title = "Most Popular Categories",
                                         status = "primary",
                                         solidHeader = TRUE, 
                                         collapsible = TRUE,
                                         highchartOutput("bubble1")
                                       )                                       
                                       
                              )       
                  ) 
                ))
              
      ),
      #kaca
      
      #Render for user inferences
      tabItem(tabName = "technical",
              p('Genre/Install Density Distribution:'),
              p('From the way the plot is plotted, it is safe to deduce that most Art and Design 
              apps have 10000,100000 and some few with about 50000 installs. Another series of clusters 
              appears in the vehicle category with 1,000,000, 100000 and a few with 50000.
              Those two categories have high density population at 100,000 installs'),
              p('Content Rating Bar Plot:'),
              p('Bar plot represesnts content rating along the y-axis and the counts in that 
              specific content rating category count is plotted on the x-axis.
              It is shown that apps developed with no restriction i.e. Apps developed for everyone 
              have the highest count implying that hubs can emphasize developing apps that
              cut across generations'),
              p('Pie Chart for Installs:'),
              p('Pie chart for installs indicates the significant number of downloads are for 
              apps of 4.0 and above versions. That can act as an indicator of the projected 
              installs over the application versioning.'),
              p('Pie Chart for App Category:'),
              p('Pie chart for Category emphasizes what we shal talk about the bar graph
              for the categories. Family is a category with relatively high installs'),
              p('Scatter Plot:'),
              p('Inference to be made by users.............')
      ),
      
      
      tabItem(tabName = "Inferences",
              h4(
              p("Instructions of Usage: Enter name,select plot to infer and enter inference then click Submit")),
             
              textAreaInput("caption", "Enter Name Here"),
              
              selectInput("infer","Choose Graph to Infer",
                          choices = c("Bar Plot","Pie Chart","Density Plot")),
              
              textAreaInput("alto","Enter inference Here",width = "500px"),
              
              #submitButton("Submit"),
            
              verbatimTextOutput("insights")        
              
      ),
      
   
      tabItem(tabName="Help",
              tabsetPanel( tabPanel("Extras",tableOutput("extras"),
                                    h3("How to view Dataset"),
                                    p("You can view the fields in the dataset used in the MDA-App by clicking on the View Dataset tab."),
                                    p("A page shall open containing the data in table format "),
                                    
                                    h3("How to print Results"),
                                    p(" The MDA-App gives a user capability to print the results of an analysis to a pdf or as an image for storage purposes"),
                                    p("1.	Carry Out an anaylsis "),
                                    p("2.	Go to either Plot or Summary tabs"),
                                    p("3.	Press the print button of the result you want") ,
                                    p("4.	Print window will appear "),
                                    p("5. Select the Name of the document ") ,
                                    p("6. Select the location of the print document. ") ,
                                    p("7.	Press ok to print"),
                                    p("8.	The document will then be downloaded to the choosen location"),
                                    
                                    h3("How to send an E-mail"),
                                    p("The MDA-App gives a user capability to send an E-mail of the results of an analysis straight to their mailbox"),
                                    p("1. Make sure you have a valid E-mail address if not create on first"),
                                    p("2.	Carry out an analysis from Plot or Summary tab"),
                                    p("3. Select the Email Results tab ") ,
                                    p("4. A window shall open "),
                                    p("5. Enter a valid E-mail address"), 
                                    p("6. Press send") ),
                           
                           tabPanel("Visualization",
                                    h3("How to visualize insights from the andriod apps datasets"),
                                    h4("Steps"),
                                    p("1- Click on the Plot tab"),
                                    p("2- Choose among the displayed navigation tabs i.e. Distribution, price, content rating,Comparison in groups, App versions and size"),
                                    p("3- Choose the categories of the distribution graph ou want to be displayed by chooseing from the dropdown list")
                                    
                           ),
                           tabPanel("Summary",
                                    h3("Using the summary tab"),
                                    h4("Steps"),
                                    p("1- Click on the summary tab to display the summary options"),
                                    p("2- Select the dataset column to display its summary such as mean, min and max")
                           ),
                           tabPanel("Inferences",
                                    h3("Using Inferences tab"),
                                    p("The MDA-App gives users ability to add their own and view already made inferences"),
                                    p("1- Click the Inferences tab"),
                                    p("2- View existing inferences based on the visualisations"),
                                    p("3- Click add inference and text field shall be displayed"),
                                    p("4- Submit inference and refresh for it to appear")
                           ),
                           tabPanel("Contact Us",
                                    h3("Get In Touch With the Developers"),
                                    p("The MDA-App has four major developers"),
                                    p("1- Kahuma Clare     - clarekahuma@gmail.com"),
                                    p("2- Bright Benard    - engbenard123@gmail.com "),
                                    p("3- Tusiime Hewwit   - hewitttusiime82@gmail.com "),
                                    p("4- Mutegeki Henry   - henrymutegeki117@gmail.com ")
                           )       
              )    
      )
      #kaca
    )
  )
  
  
)