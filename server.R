library(shiny)
library(DT)
library(tidyverse)
library(highcharter) 
#require("highcharter")
library(lubridate)
library(stringr)
library(xts)
library("magrittr")
library(ggplot2)
library(plotly)
library(plotrix)
library(RColorBrewer)
library(tweenr)
library(tidyverse)
library(gganimate)
library(scatterplot3d)
library(httpuv)
#library(grid)  

#write.csv(data.clean, file="New.csv")

function(input, output){
  
   
  #code to diisplay the select option for the loaded files.
  output$selectfile<-renderUI({
    if(is.null(input$file1)){return()}
    list(
      hr(),
      helpText("select the file you need"),
      selectInput("select","select",choices = input$file1$name)
    )
  })
  
  
  data<-reactive({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath[input$file1$name==input$select],
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    return(df)
  })
  
  
  #code to display  the loaded files
  output$mytable = DT::renderDataTable(
    data(), # data
    class = "display nowrap compact", # style
    filter = "top",# location of column filters
    options = list(  # options
      scrollX = TRUE, # allow user to scroll wide tables horizontally
      pageLength=14,
      lengthMenu=c(1:100)
    )
  )
  
  
  
  #-------------------------------------------------------------------------
  
  #rendering Menu
  
  #-------------------------------------------------------------------------    
  output$menuItem <-renderMenu({
    
    menuItem("Home")
    menuItem("View Dataset")
    menuItem("Summary")
    menuItem("Plot")
    menuItem("Technical Insights")
    menuItem("Inferences")
    menuItem("Help")
    
  })
  
  data_d<-read.csv("googleplaystore.csv")

  
  data.clean <- data_d %>%
    mutate(
      # Eliminate some characters to transform Installs to numeric
      Installs = gsub("\\+", "", as.character(Installs)),
      Installs = as.numeric(gsub(",", "", Installs)),
      
      
      # Eliminate M to transform Size to numeric
      Size = gsub("M", "", Size),
      
      # Replace cells with k to 0 since it is < 1MB
      Size = ifelse(grepl("k", Size), 0, as.numeric(Size)),
      
      # Transform reviews to numeric
      Reviews = as.numeric(Reviews),
      
      # Remove currency symbol from Price, change it to numeric
      Price = as.numeric(gsub("\\$", "", as.character(Price))),
      
      # Remove text in Android version column
      Android.Ver = gsub("and up", "", Android.Ver),
      
      #remove varies with device and make it 0
      Android.Ver = gsub("Varies with device", NA, Android.Ver),
      # Keep only version number to 1 decimal
      Android.Ver = as.numeric(substr(Android.Ver, start = 1, stop = 3)),
      
      #remove varies with device and make it 0
      Current.Ver = gsub("Varies with device", NA, Current.Ver),
    ) %>%
    filter(
      # Two apps had type as 0 or NA, they will be removed 
      Type %in% c("Free", "Paid")
    )
  
  #str(data.clean)
  
  
  #rendering data
  #median info box code
  output$valueBox1 <- renderInfoBox({
    infoBox(title = "Median",
      value = median(c(data.clean$Installs)),"Installs",
      #subtitle = "median value in dataset column Installs",
      fill = FALSE,
      icon("accusoft"),
      color="green"
    )
  })
  
  output$valueBox2 <- renderInfoBox({
    infoBox(title = "max",
            value = max(c(data.clean$Installs)),"Installs",
            #subtitle = "median value in dataset column Installs",
            fill =FALSE,
            icon("refresh"),
            color="yellow"
    )
  })
  
  output$valueBox3 <- renderInfoBox({
    infoBox(title = "mean",
            value = 1971+1144+843+463+460+424+5534,"Apps",
            #subtitle = "median value in dataset column Installs",
            fill = FALSE,
            icon("apple"),
            color="red"
    )
  })
  
  # output$valueBox4 <- renderInfoBox({
  #   infoBox(title = "mean",
  #           value = 499+3,"Apps",
  #           #subtitle = "median value in dataset column Installs",
  #           fill = FALSE,
  #           icon("affiliatetheme"),
  #           color="blue"
  #   )
  # })
  
  #Summary Content Code
  #Prints overall summary
  output$summary <- renderPrint({
    summary(data.clean)
  })
  
  #Print summary by selection of the variable name
  output$summary1<-renderPrint({
   
    if(input$variable=="Category"){
      print(summary(data.clean$Category)
      )
    }
    else if(input$variable=="Rating"){
      print(summary(data.clean$Rating)
      )
    }
    else if(input$variable=="Reviews"){
      print(summary(data.clean$Reviews)
      )
    }
    else if(input$variable=="Size"){
      print(summary(data.clean$Size)
      )
    }
    else if(input$variable=="Installs"){
      print(summary(data.clean$Installs)
      )
    }
    else if(input$variable=="Type"){
      print(summary(data.clean$Type)
      )
    }
    else if(input$variable=="Content.Rating"){
      print(summary(data.clean$Content.Rating)
      )
    }
    else if(input$variable=="Price"){
      print(summary(data.clean$Price)
      )
    }
    else if(input$variable=="Content.Rating"){
      print(summary(data.clean$Content.Rating)
      )
    }
    else if(input$variable=="Genres"){
      print(summary(data.clean$Genres)
      )
    }
    else if(input$variable=="Last.Updated"){
      print(summary(data.clean$Last.Updated)
      )
    }else{                          
      paste("Please check your entry and try again")
    }
  })#end of summary staff
  
  #bar plot for content rating
  output$hc_plot3<-renderHighchart({
    hcboxplot(x = data.clean$Size, var = data.clean$Type, outliers = TRUE, color = "#fb4901", fillColor = "lightblue") %>%
      hc_chart(type = "column") %>%
      hc_add_theme(hc_theme_ffx()) %>%
      hc_title(text = "Application size range (in MB) by Application Type")
  })#end content rating bar
  
  #bar plot for installs
  output$plot4<-renderPlot({
    cyl<-data.clean$Installs
    #lm(formula = Genre~Installs)
    ggplot(data.clean, aes(x=as.factor(cyl), fill=as.factor(cyl) )) +
      geom_bar() #+
      #coord_flip()
  })
  
  #bar plot dynamic render
  output$plot5<-renderPlot({
      # if(input$xs=="Size"){
      #   cyl<-data.clean$Size
      #   #lm(formula = Genre~Installs)
      #   ggplot(data.clean, aes(x=as.factor(cyl), fill=as.factor(cyl) )) +
      #     geom_bar()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
      # }
    if(input$xs=="Rating"){
      cyl<-data.clean$Rating
      #lm(formula = Genre~Installs)
      ggplot(data.clean, aes(x=as.factor(cyl), fill=as.factor(cyl) )) +
        geom_bar()+xlab("App Rating") + ylab("Number of apps")
    }
    else if(input$xs=="Content.Rating"){
      cyl<-data.clean$Content.Rating
      #lm(formula = Genre~Installs)
      ggplot(data.clean, aes(x=as.factor(cyl), fill=as.factor(cyl) )) +
        geom_bar()+xlab("Content Rating") + ylab("Number of apps")
    }
    else if(input$xs=="Category"){
      cyl<-data.clean$Category
      #lm(formula = Genre~Installs)
      ggplot(data.clean, aes(x=as.factor(cyl), fill=as.factor(cyl) )) +
        geom_bar()+theme(axis.text.x = element_text(angle = 45, hjust = 1)) +xlab("App Category") + ylab("Number of apps")
    }
    # else if(input$xs=="Last.Updated"){
    #   cyl<-data.clean$Last.Updated
    #   #lm(formula = Genre~Installs)
    #   ggplot(data.clean, aes(x=as.factor(cyl), fill=as.factor(cyl) )) +
    #     geom_bar()
    # }
    else if(input$xs=="Android.Ver"){
      cyl<-data.clean$Android.Ver
      #lm(formula = Genre~Installs)
      ggplot(data.clean, aes(x=as.factor(cyl), fill=as.factor(cyl) )) +
        geom_bar()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+xlab("Andriod Version") + ylab("Number of apps")
    }
    else if(input$xs=="Type"){
      cyl<-data.clean$Type
      #lm(formula = Genre~Installs)
      ggplot(data.clean, aes(x=as.factor(cyl), fill=as.factor(cyl) )) +
        geom_bar()+xlab("Type of App") + ylab("Number of apps")
    }
    # else if(input$xs=="Price"){
    #   cyl<-data.clean$Price
    #   #lm(formula = Genre~Installs)
    #   ggplot(data.clean, aes(x=as.factor(cyl), fill=as.factor(cyl) )) +
    #     geom_bar()
    # }
  })#end of plot 5
  

  output$plot1<-renderPlot({
    if (input$pie=="Last.Updated"){
      slices <- c(842,623,549,43,460,424,7478)
      lbls <- c("August 3, 2018","August 2, 2018","July 31, 2018","August 1, 2018","July 30, 2018"," July 25, 2018","Other")
      pie3D(slices,labels=lbls,explode=0.1)
      
    }else if (input$pie=="Genres"){
      slices <- c(326,304,294,285,211,164,9255)
      lbls <- c("Tools","Entertainment","Education","Medical","Business","Productivity","Other")
      pie3D(slices,labels=lbls,explode=0.05)
      
    }else if (input$pie=="Content.Rating"){
        slices <- c(3,8714,413,499,1208,2)
        lbls <- c("Adults only 18+","Everyone ","Everyone 10+","Mature 17+","Teen","Unrated")
        pie3D(slices,labels=lbls,explode=0.05)
        
    }
    
  })
  
  #Pie Chart for Installs
  output$plot2<-renderPlot({
    ver_count<-c(1971,1144,843,463,460,424,5534)
    version<-c("Family","Game","Tools","Medical","Business","Productivity","Other")
    pie3D(ver_count,labels=version,explode=0.05,
          main="Pie Chart for App Category")
  })
  
  #Density plot for Genre,Install distribution  
    output$plot<-renderPlot({
      ggplot(data = data.clean[1:200,], aes(x = Genres, y = Installs)) +
        geom_violin(alpha = 0) +
        geom_jitter(alpha = 0.8, color = "tomato")
      #curve(-17.579 + 3.932*x, add=TRUE)
  })
  
  #3D- BAR PLOTS
    output$hc11<-renderHighchart({
      if(input$ab=="Android.Ver"){
   
        data.clean %>%
          filter(Android.Ver > 0, Type %in% c("Free", "Paid")
          ) %>%
          group_by(as.factor(Android.Ver), Type) %>%
          rename(Minimum.Android.Version = "as.factor(Android.Ver)") %>%
          summarize(Total.Installs = sum(Installs)) %>%
          hchart('bar', hcaes(x = 'Minimum.Android.Version', y = 'Total.Installs', group = 'Type'))
          
     
      }
      else if(input$ab=="Rating"){
      #Rating
        
        data.clean %>%
          filter(Rating > 0, Type %in% c("Free", "Paid")
          ) %>%
          group_by(as.factor(Rating), Type) %>%
          rename(Ratings = "as.factor(Rating)") %>%
          summarize(Total.Installs = sum(Installs)) %>%
          hchart('bar', hcaes(x = 'Ratings', y = 'Total.Installs', group = 'Type'))
        
        
      
      }
      else if(input$ab=="Reviews"){
        data.clean %>%
          filter( Reviews> 0, Type %in% c("Free", "Paid")
          ) %>%
          group_by(as.factor(Reviews), Type) %>%
          rename(Review = "as.factor(Reviews)") %>%
          summarize(Total.Installs = sum(Installs)) %>%
          hchart('bar', hcaes(x = 'Review', y = 'Total.Installs', group = 'Type'))
        
      }
     

      else if(input$ab=="Price"){
        data.clean %>%
          filter( Price> 0, Type %in% c("Free", "Paid")
          ) %>%
          group_by(as.factor(Price), Type) %>%
          rename(Prices = "as.factor(Price)") %>%
          summarize(Total.Installs = sum(Installs)) %>%
          hchart('bar', hcaes(x = 'Prices', y = 'Total.Installs', group = 'Type'))
        
        
      }

      else if(input$ab=="Current.Ver"){
        data.clean %>%
          filter( Current.Ver> 0, Type %in% c("Free", "Paid")
          ) %>%
          group_by(as.factor(Current.Ver), Type) %>%
          rename(CurrentVersion = "as.factor(Current.Ver)") %>%
          summarize(Total.Installs = sum(Installs)) %>%
          hchart('bar', hcaes(x = 'CurrentVersion', y = 'Total.Installs', group = 'Type'))
        
      }
 
      else if(input$ab=="Size"){

        data.clean %>%
          filter( Size> 0, Type %in% c("Free", "Paid")
          ) %>%
          group_by(as.factor(Size), Type) %>%
          rename(AppSize = "as.factor(Size)") %>%
          summarize(Total.Installs = sum(Installs)) %>%
          hchart('bar', hcaes(x = 'AppSize', y = 'Total.Installs', group = 'Type'))
  
      }
  })
  
  #box plot for 
  output$plot6 <- renderPlot({
     boxplot(data.clean$App)
  })
  
  #render users name ,plot and inference
  output$insights<-renderText({
    i=0
    if (i<2){
      print("Name:",input$caption,"\n",
            "Plot:",input$alto,"\n",
            "Inference:",input$infer
            )
      i=i+1
    }else{
      print("Hoooo ask admin for help")
    }
    
  })
  
  #choose plot to infer
  output$insights<-renderText({
    if(input$infer=="Bar Plot"){
      input$infer <- "Bar Plot"
    }
    else if(input$infer=="Pie Chart"){
      input$infer <- "Pie Chart"
    }
    else if(input$infer=="Density Plot"){
      input$infer <- "Density Plot"
    }
    
  })
  
  #input text
  output$insights<-renderText({
  
     paste("Name: ",input$caption,sep = '\n',
           
           "Plot Infered: ",input$infer,
           
           "Inference: ",input$alto )
     
   
      
    
  })
  
  #3D Scatter Render
  output$sscatter<-renderPlot({
    if(input$variable=="Category"){
      
    scatterplot3d(data.clean$Category,data.clean$Installs,data.clean$Content.Rating,
                  main="3D Scatter Plot",#title
                  xlab = "Category",
                  ylab="Installs",
                  zlab="Content.Rating",
                  #Patch=data$x4,
                  color = data.clean$Size,
                  angle = 45
    )
    }
  })
  
  #Another scater
  
  #3D Scatter Render
  output$scatter1<-renderPlot({
    
    if(input$scat=="Content.Rating"){
    data<-data.frame(d=data.clean$Content.Rating,#keeping changing this to see differnet aspects
                     a=data.clean$Content.Rating,
                     b=data.clean$Installs)
    data1<-na.omit(data)
    #data
    
    ggplot(data1,aes(a,b,color=format(d))) +
      geom_point(alpha=0.5, size=2.5)#+
    #geom_line(aes(d,a,color="A"))
    }
    else if(input$scat=="Category"){
      data<-data.frame(d=data.clean$Category,#keeping changing this to see differnet aspects
                       a=data.clean$Category,
                       b=data.clean$Installs)
      data1<-na.omit(data)
      #data
      
      ggplot(data1,aes(a,b,color=format(d))) +theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        geom_point(alpha=0.5, size=2.5)#+
      #geom_line(aes(d,a,color="A"))
    }
    else if(input$scat=="Size"){
      data<-data.frame(d=data.clean$Size,#keeping changing this to see differnet aspects
                       a=data.clean$Size,
                       b=data.clean$Installs)
      data1<-na.omit(data)
      #data
      
      ggplot(data1,aes(a,b,color=format(d))) +
        geom_point(alpha=1, size=2.5)#+appDetails
      #geom_line(aes(d,a,color="A"))
    }
    
    else if(input$scat=="Type"){
      data<-data.frame(d=data.clean$Type,#keeping changing this to see differnet aspects
                       a=data.clean$Type,
                       b=data.clean$Installs)
      data1<-na.omit(data)
      #data
      
      ggplot(data1,aes(a,b,color=format(d))) +
        geom_point(alpha=0.5, size=2.5)#+
      #geom_line(aes(d,a,color="A"))
    }
    else if(input$scat=="Rating"){
      data<-data.frame(d=data.clean$Rating,#keeping changing this to see differnet aspects
                       a=data.clean$Rating,
                       b=data.clean$Installs)
      data1<-na.omit(data)
      #data
      
      ggplot(data1,aes(a,b,color=format(d))) +
        geom_point(alpha=0.5, size=2.5)#+
      #geom_line(aes(d,a,color="A"))
    }
    else if(input$scat=="Android.Ver"){
      data<-data.frame(d=data.clean$Android.Ver,#keeping changing this to see differnet aspects
                       a=data.clean$Android.Ver,
                       b=data.clean$Installs)
      data1<-na.omit(data)
      #data
      
      ggplot(data1,aes(a,b,color=format(d))) +
        geom_point(alpha=0.5, size=2.5)#+
      #geom_line(aes(d,a,color="A"))
    }
    else if(input$scat=="Price"){
      data<-data.frame(d=data.clean$Price,#keeping changing this to see differnet aspects
                       a=data.clean$Price,
                       b=data.clean$Installs)
      data1<-na.omit(data)
      #data
      
      ggplot(data1,aes(a,b,color=format(d))) +
        geom_point(alpha=0.5, size=2.5)#+
      #geom_line(aes(d,a,color="A"))
    }
    else if(input$scat=="Last.Updated"){
      data<-data.frame(d=data.clean$Last.Updated,#keeping changing this to see differnet aspects
                       a=data.clean$Last.Updated,
                       b=data.clean$Installs)
      data1<-na.omit(data)
      #data
      
      ggplot(data1,aes(a,b,color=format(d))) +
        geom_point(alpha=0.5, size=2.5)#+
      #geom_line(aes(d,a,color="A"))
    }
    else if(input$scat=="Current.Ver"){
      data<-data.frame(d=data.clean$Current.Ver,#keeping changing this to see differnet aspects
                       a=data.clean$Current.Ver,
                       b=data.clean$Installs)
      data1<-na.omit(data)
      #data
      
      ggplot(data1,aes(a,b,color=format(d))) +
        geom_point(alpha=0.5, size=2.5)#+
      #geom_line(aes(d,a,color="A"))
    }
    
    #Kahuma Allelua Clare
    
  })
  
  #Histogramsss plotedddd
  output$histo<-renderPlot({
    if(input$histo=="Category"){
      
      plot(googleplaystore$Category,n=input$sliderHist, main="Histogram for App Category")
    }else if (input$histo=="Installs"){
      plot(googleplaystore$Installs)
    }else{
      plot(googleplaystore$Last.Updated)
    }
      
  })
  
#NEW HIST

  output$hist<-renderPlot({
    if(input$appDetails== "app_rating"){
      #plot a bar graph showing the distribution of user ratings
      return(ggplot(data = data.clean,aes(x=Rating))+geom_histogram(bins = input$bins)+
               labs(title="A Histogram showing the distribution of user ratings",
                    x="App Ratings",
                    y="Number of apps"))
    }
    if(input$appDetails== "size"){
      #plot a bar graph to show the distribution of app g roups
      return( ggplot(data=data.clean,aes(x=Size))+
                geom_histogram(bins = input$bins)+
                labs(title="A Histogram  showing the distribution of app size",
                     x="Size of App (mbs)",
                     y="Number of apps"
                     
                ))
    }
    if(input$appDetails=="price"){
      return(
        ggplot(data = data.clean,aes(x=Price))+geom_histogram(bins = input$bins)+
          labs(title="Distribution of App prices",
               x="Price",
               y="Number of apps")
      )
    }

    else{
      return(NULL)
    }
    
  })

  ###BUBBLE PLOTS
  
  
  output$bubble1<-renderHighchart({
    if(input$bubble=="Category"){
      data.clean %>%
        count(Category, Installs) %>%
        group_by(Category) %>%
        summarize(
          TotalInstalls = sum(as.numeric(Installs))
        ) %>%
        arrange(-TotalInstalls) %>%
        hchart('scatter', hcaes(x = "Category", y = "TotalInstalls", size = "TotalInstalls", color = "Category")) %>%
        hc_add_theme(hc_theme_538()) %>%
        hc_title(text = "Most popular categories (# of installs)")
  
    }
    
    else if(input$bubble=="Rating"){
      data.clean %>%
        count(Rating,Installs) %>%
        group_by(Rating) %>%
        summarize(
          TotalInstalls = sum(as.numeric(Installs))
        ) %>%
        arrange(-TotalInstalls) %>%
        hchart('scatter', hcaes(x = "Rating", y = "TotalInstalls", size = "TotalInstalls", color = "Rating")) %>%
        hc_add_theme(hc_theme_538()) %>%
        hc_title(text = "Most popular Rating (# of installs)")
      
      
      
    }
    else if(input$bubble=="Size"){
      data.clean %>%
        count(Size,Installs) %>%
        group_by(Size) %>%
        summarize(
          TotalInstalls = sum(as.numeric(Installs))
        ) %>%
        arrange(-TotalInstalls) %>%
        hchart('scatter', hcaes(x = "Size", y = "TotalInstalls", size = "TotalInstalls", color = "Size")) %>%
        hc_add_theme(hc_theme_538()) %>%
        hc_title(text = "Most popular Size (# of installs)")
      
    }
    
    
    else if(input$bubble=="Type"){
      data.clean %>%
        count(Type,Installs) %>%
        group_by(Type) %>%
        summarize(
          TotalInstalls = sum(as.numeric(Installs))
        ) %>%
        arrange(-TotalInstalls) %>%
        hchart('scatter', hcaes(x = "Type", y = "TotalInstalls", size = "TotalInstalls", color = "Type")) %>%
        hc_add_theme(hc_theme_538()) %>%
        hc_title(text = "Most popular Type (# of installs)")
    }
    
    else if(input$bubble=="Price"){
      data.clean %>%
        count(Price,Installs) %>%
        group_by(Price) %>%
        summarize(
          TotalInstalls = sum(as.numeric(Installs))
        ) %>%
        arrange(-TotalInstalls) %>%
        hchart('scatter', hcaes(x = "Price", y = "TotalInstalls", size = "TotalInstalls", color = "Price")) %>%
        hc_add_theme(hc_theme_538()) %>%
        hc_title(text = "Most popular Price (# of installs)")
      
    }
    
    else if(input$bubble=="Content.Rating"){
      data.clean %>%
        count(Content.Rating,Installs) %>%
        group_by(Content.Rating) %>%
        summarize(
          TotalInstalls = sum(as.numeric(Installs))
        ) %>%
        arrange(-TotalInstalls) %>%
        hchart('scatter', hcaes(x = "Content.Rating", y = "TotalInstalls", size = "TotalInstalls", color = "Content.Rating")) %>%
        hc_add_theme(hc_theme_538()) %>%
        hc_title(text = "Most popular Content Rating (# of installs)")
      
    }
    
    
    else if(input$bubble=="Genres"){
      data.clean %>%
        count(Genres,Installs) %>%
        group_by(Genres) %>%
        summarize(
          TotalInstalls = sum(as.numeric(Installs))
        ) %>%
        arrange(-TotalInstalls) %>%
        hchart('scatter', hcaes(x = "Genres", y = "TotalInstalls", size = "TotalInstalls", color = "Genres")) %>%
        hc_add_theme(hc_theme_538()) %>%
        hc_title(text = "Most popular Genres (# of installs)")
      
    }
    
    
    else if(input$bubble=="Android.Ver"){
      data.clean %>%
        count(Android.Ver,Installs) %>%
        group_by(Android.Ver) %>%
        summarize(
          TotalInstalls = sum(as.numeric(Installs))
        ) %>%
        arrange(-TotalInstalls) %>%
        hchart('scatter', hcaes(x = "Android.Ver", y = "TotalInstalls", size = "TotalInstalls", color = "Android.Ver")) %>%
        hc_add_theme(hc_theme_538()) %>%
        hc_title(text = "Most popular Android.Ver (# of installs)")
      
    }
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  #END BUBBLE PLOTS
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##PYRAMIDS
  
  output$pyrmd1<-renderHighchart({
    if(input$pyramid=="Content.Rating"){
      tmp <- data.clean %>%
        group_by(Content.Rating) %>%
        summarize(Total.Installs = sum(Installs)) %>%
        arrange(-Total.Installs)
      
      highchart() %>%
        hc_chart(type = "pyramid") %>%
        hc_add_series_labels_values(
          labels = tmp$Content.Rating, values = tmp$Total.Installs
        ) %>%
        hc_title(
          text="Number of Installs by Content Rating"
        ) %>%
        hc_add_theme(hc_theme_flat())
      
      
    }
    else if(input$pyramid=="Category"){
      tmp <- data.clean %>%
        group_by(Category) %>%
        summarize(Total.Installs = sum(Installs)) %>%
        arrange(-Total.Installs)
      
      highchart() %>%
        hc_chart(type = "pyramid") %>%
        hc_add_series_labels_values(
          labels = tmp$Category, values = tmp$Total.Installs
        ) %>%
        hc_title(
          text="Number of Installs by Category"
        ) %>%
        hc_add_theme(hc_theme_flat())
      
      
      
    }
    else if(input$pyramid=="Type"){
      tmp <- data.clean %>%
        group_by(Type) %>%
        summarize(Total.Installs = sum(Installs)) %>%
        arrange(-Total.Installs)
      
      highchart() %>%
        hc_chart(type = "pyramid") %>%
        hc_add_series_labels_values(
          labels = tmp$Type, values = tmp$Total.Installs
        ) %>%
        hc_title(
          text="Number of Installs by Type"
        ) %>%
        hc_add_theme(hc_theme_flat())
      
    }
    
    
    else if(input$pyramid=="Rating"){
      tmp <- data.clean %>%
        group_by(Rating) %>%
        summarize(Total.Installs = sum(Installs)) %>%
        arrange(-Total.Installs)
      
      highchart() %>%
        hc_chart(type = "pyramid") %>%
        hc_add_series_labels_values(
          labels = tmp$Rating, values = tmp$Total.Installs
        ) %>%
        hc_title(
          text="Number of Installs by Rating"
        ) %>%
        hc_add_theme(hc_theme_flat())
      
    }
    
    else if(input$pyramid=="Android.Ver"){
      tmp <- data.clean %>%
        group_by(Android.Ver) %>%
        summarize(Total.Installs = sum(Installs)) %>%
        arrange(-Total.Installs)
      
      highchart() %>%
        hc_chart(type = "pyramid") %>%
        hc_add_series_labels_values(
          labels = tmp$Android.Ver, values = tmp$Total.Installs
        ) %>%
        hc_title(
          text="Number of Installs by Android Version"
        ) %>%
        hc_add_theme(hc_theme_flat())
      
    }
    
    else if(input$pyramid=="Size"){
      tmp <- data.clean %>%
        group_by(Size) %>%
        summarize(Total.Installs = sum(Installs)) %>%
        arrange(-Total.Installs)
      
      highchart() %>%
        hc_chart(type = "pyramid") %>%
        hc_add_series_labels_values(
          labels = tmp$Size, values = tmp$Total.Installs
        ) %>%
        hc_title(
          text="Number of Installs by Size"
        ) %>%
        hc_add_theme(hc_theme_flat())
      
    }
    
    
    else if(input$pyramid=="Price"){
      tmp <- data.clean %>%
        group_by(Price) %>%
        summarize(Total.Installs = sum(Installs)) %>%
        arrange(-Total.Installs)
      
      highchart() %>%
        hc_chart(type = "pyramid") %>%
        hc_add_series_labels_values(
          labels = tmp$Price, values = tmp$Total.Installs
        ) %>%
        hc_title(
          text="Number of Installs by Price"
        ) %>%
        hc_add_theme(hc_theme_flat())
      
    }
    
    else if(input$pyramid=="Reviews"){
      tmp <- data.clean %>%
        group_by(Reviews) %>%
        summarize(Total.Installs = sum(Installs)) %>%
        arrange(-Total.Installs)
      
      highchart() %>%
        hc_chart(type = "pyramid") %>%
        hc_add_series_labels_values(
          labels = tmp$Reviews, values = tmp$Total.Installs
        ) %>%
        hc_title(
          text="Number of Installs by Reviews"
        ) %>%
        hc_add_theme(hc_theme_flat())
      
    }
    
    
    
    
  })
  
  
  
  
  
  
  
  
 ##END PYRAMIDS
 
  
  
  ##Box plot 
  
  output$bxplot<-renderPlot({
    if(input$bxplot=="Price"){
      return(
        ggplot(data=data.clean,aes(x=as.factor(Installs),y=Price,fill=as.factor(Installs)))+
          theme_bw()+
          geom_boxplot()+
          labs(title="A box plot showing the how price affects user ratings",
               x="Installs",
               y="price")
      )
    }
    if(input$bxplot=="Size"){
      return(
        ggplot(data=data.clean,aes(x=as.factor(Installs),y=Size,fill=as.factor(Installs)))+
          theme_bw()+
          geom_boxplot()+
          labs(title="A box plot showing the how Size of Apps affects App Installs",
               x="Installs",
               y="Size of Apps")       
       
        #Regression model
        # ggplot(data=data.clean,aes(x=Rating,y=Price))+
        #   geom_point()+
        #   geom_smooth(method = lm, se=FALSE)+
        #   labs(title="A scatter plot shhowing the correlation between price and user rating",
        #        x="User rating",
        #        y="Price of Apps"
        #        
        #   )
        
      )
    }
  
    if(input$bxplot=="Reviews"){
      return(
        ggplot(data=data.clean,aes(x=as.factor(Installs),y=Reviews,fill=as.factor(Installs)))+
          theme_bw()+
          geom_boxplot()+
          labs(title="A box plot showing the how Reviews affects App Installs",
               x="Installs",
               y="Reviews")
      )
    }
    
    
    
    if(input$bxplot=="Rating"){
      return(
        ggplot(data=data.clean,aes(x=as.factor(Installs),y=Rating,fill=as.factor(Installs)))+
          theme_bw()+
          geom_boxplot()+
          labs(title="A box plot showing the how Rating affects App Installs",
               x="Installs",
               y="Rating")
      )
    }
    

    
    if(input$bxplot=="Android.Ver"){
      return(
        ggplot(data=data.clean,aes(x=as.factor(Installs),y=Android.Ver,fill=as.factor(Installs)))+
          theme_bw()+
          geom_boxplot()+
          labs(title="A box plot showing the how Android Ver affects Installs",
               x="Installs",
               y="Android.Ver")
      )
    }
    
    
    })
}


