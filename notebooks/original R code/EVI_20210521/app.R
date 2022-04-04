
library(shiny)
library(readxl)
library(ggplot2)
library(cowplot)
library(readr)


filenames <- list.files(path = "forplot_n/", full.names = TRUE)
my.df <- do.call(rbind,
                 lapply(filenames, function(x) 
                   cbind(read.csv(x), 
                         name = tools::file_path_sans_ext(basename(x)))))
my.df$name=gsub("results","",my.df$name)

my.df=as.data.frame(my.df)

forcaption=(paste("Data from", as.Date(min(my.df$dates), origin="1970-01-01"),
                  "until", as.Date(max(my.df$dates), origin="1970-01-01")))



filenames2 <- list.files(path = "forplot_n_US/", full.names = TRUE)
my.df2 <- do.call(rbind,
                  lapply(filenames2, function(x) 
                    cbind(read.csv(x), 
                          name = tools::file_path_sans_ext(basename(x)))))
my.df2$name=gsub("results","",my.df2$name)

my.df2=as.data.frame(my.df2)

forcaption2=(paste("Data from", as.Date(min(my.df2$dates), origin="1970-01-01"),
                   "until", as.Date(max(my.df2$dates), origin="1970-01-01")))


# Use a fluid Bootstrap layout
ui<-fluidPage(
  
  # Give the page a title
  titlePanel("The Epidemic Volatility Index: predictions for COVID-19"),
  
  
  p("Figure 1. Daily confirmed cases of COVID-19, 
  presented on the original scale, with red dots corresponding to dates that, 
  according to EVI, an early warning was issued."),
  
  p("Figure 2. Daily confirmed cases of COVID-19, 
  presented on the logarithmic scale, which facilitates the comparison of 
  the steepness of the epidemic curve between the different waves."),
  
  p("Figure 3. Positive predictive values (PPV ) for the days that an early
  warning was issued. Higher color intensity corresponds to PPV closer to 
  the value of 1."),
  
  p("Figure 4. Negative predictive values (NPV) for the days that an early
  warning was not issued. Higher color intensity corresponds to NPV closer
  to the value of 1."),
  
  p("Confirmed COVID-19 cases are retrieved from the", 
    tags$a(href="https://github.com/CSSEGISandData/COVID-19", 
           "COVID-19 Data Repository.")),
  
  tabsetPanel(
    tabPanel("World", fluid = TRUE,
             sidebarLayout(      
               
               # Define the sidebar with one input
               sidebarPanel(
                 selectInput("region", "Country:", 
                             choices=unique(my.df$name)),
                 hr(),
                 helpText(forcaption),
                 hr(),
                 helpText(tags$b("Cite as"),": Polychronis Kostoulas. The Epidemic Volatility Index: 
      an early warning tool for epidemics.
               Authorea. April 23, 2021.",
                          tags$a(href="https://doi.org/10.22541/au.161918947.77588494/v1", 
                                 "DOI: 10.22541/au.161918947.77588494/v1"))
               ),
               
               # Create a spot for the barplot
               mainPanel(
                 plotOutput("phonePlot1"),
                 plotOutput("phonePlot2"),
                 plotOutput("phonePlot3"),
                 plotOutput("phonePlot4")
                 
               )
               
             )
    ),
    tabPanel("US", fluid = TRUE,
             sidebarLayout(      
               
               # Define the sidebar with one input
               sidebarPanel(
                 selectInput("regionUS", "State:", 
                             choices=unique(my.df2$name)),
                 hr(),
                 helpText(forcaption2),
                 hr(),
                 helpText(tags$b("Cite as"),": Polychronis Kostoulas. The Epidemic Volatility Index: 
      an early warning tool for epidemics.
               Authorea. April 23, 2021.",
                          tags$a(href="https://doi.org/10.22541/au.161918947.77588494/v1", 
                                 "DOI: 10.22541/au.161918947.77588494/v1"))
               ),
               
               # Create a spot for the barplot
               mainPanel(
                 plotOutput("phonePlot1US"),
                 plotOutput("phonePlot2US"),
                 plotOutput("phonePlot3US"),
                 plotOutput("phonePlot4US")
                 
               )
               
             )
    )
  )
  # Generate a row with a sidebar
  
)

# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).

# Define a server for the Shiny app
server<-function(input, output) {
  
  # Fill in the spot we created for a plot
  output$phonePlot1 <- renderPlot({
    
    # Render a barplot
    
    file1=my.df[my.df$name==paste(input$region),]
    
    namm=paste(input$region)
    
    file1=as.data.frame(file1)
    
    
    
    file1$Index_nn=file1$Index
    
    
    for (i in 7:nrow(file1)){
      
      if (file1$Index_nn[i]==1 && file1$Cases[i]>mean(file1$Cases[i:(i-7)]))
      {file1$Index_nn[i]=1} else 
      {file1$Index_nn[i]=0}
    }
    
    
    
    w_s=7
    ratio=1/1.2
    
    true_f_p=rep(0, length(file1$Cases))
    
    for (i in w_s:(length(file1$Cases)-(w_s+1))){
      if (mean(file1$Cases[(i-w_s):i])<=ratio*mean(file1$Cases[(i+w_s):i])) {true_f_p[i]=1} else {true_f_p[i]=0}
    }
    
    final_se=length(which(file1$Index_nn==1 & true_f_p==1))/length(which(true_f_p==1))
    final_spc=length(which(file1$Index_nn==0 & true_f_p==0))/length(which(true_f_p==0))
    
    se_se=sqrt(final_se*(1-final_se)/length(which(true_f_p==1)))
    se_low=max(0, (final_se - 1.96*se_se))
    se_up= min(1, (final_se + 1.96*se_se))
    
    se_sp=sqrt(final_spc*(1-final_spc)/length(which(true_f_p==0)))
    sp_low=max(0, (final_spc - 1.96*se_sp))
    sp_up= min(1, (final_spc + 1.96*se_sp))
    
    subt=paste("Se=",round(final_se,2)," (",round(se_low,2),"; ",round(se_up,2),") & ",
               "Sp=",round(final_spc,2)," (",round(sp_low,2),"; ",round(sp_up,2),")",
               sep="")
    
    
    
    data=file1
    
    data$cases_1=data$Cases*data$Index_n
    data$cases_1[data$cases_1 == 0] <- NA
    data$cases_0=data$Cases*(1-data$Index_n)
    data$cases_0[data$cases_0 == 0] <- NA
    
    data$pvn=data$pvn*(1-data$Index)
    data$pvn[data$pvn == 0] <- NA
    data$pvs=data$pvs*data$Index
    data$pvs[data$pvs == 0] <- NA
    
    
    
    sp3<-ggplot(data, aes(x=Days))+
      geom_point(aes(y=(Cases), color=Index_nn>0), size=0.5)+
      scale_color_manual(values=c("grey69", "red3"))+
      theme(legend.position = "none")+
      labs(y = "Cases", x="")+
      ggtitle(namm)
    #, subtitle = subt)
    sp3
    
    
  })
  
  output$phonePlot2 <- renderPlot({
    
    # Render a barplot
    
    file1=my.df[my.df$name==paste(input$region),]
    
    namm=paste(input$region)
    
    file1=as.data.frame(file1)
    
    
    
    file1$Index_nn=file1$Index
    
    
    for (i in 7:nrow(file1)){
      
      if (file1$Index_nn[i]==1 && file1$Cases[i]>mean(file1$Cases[i:(i-7)]))
      {file1$Index_nn[i]=1} else 
      {file1$Index_nn[i]=0}
    }
    
    
    
    w_s=7
    ratio=1/1.2
    
    true_f_p=rep(0, length(file1$Cases))
    
    for (i in w_s:(length(file1$Cases)-(w_s+1))){
      if (mean(file1$Cases[(i-w_s):i])<=ratio*mean(file1$Cases[(i+w_s):i])) {true_f_p[i]=1} else {true_f_p[i]=0}
    }
    
    final_se=length(which(file1$Index_nn==1 & true_f_p==1))/length(which(true_f_p==1))
    final_spc=length(which(file1$Index_nn==0 & true_f_p==0))/length(which(true_f_p==0))
    
    se_se=sqrt(final_se*(1-final_se)/length(which(true_f_p==1)))
    se_low=max(0, (final_se - 1.96*se_se))
    se_up= min(1, (final_se + 1.96*se_se))
    
    se_sp=sqrt(final_spc*(1-final_spc)/length(which(true_f_p==0)))
    sp_low=max(0, (final_spc - 1.96*se_sp))
    sp_up= min(1, (final_spc + 1.96*se_sp))
    
    subt=paste("Se=",round(final_se,2)," (",round(se_low,2),"; ",round(se_up,2),") & ",
               "Sp=",round(final_spc,2)," (",round(sp_low,2),"; ",round(sp_up,2),")",
               sep="")
    
    
    
    data=file1
    
    data$cases_1=data$Cases*data$Index_n
    data$cases_1[data$cases_1 == 0] <- NA
    data$cases_0=data$Cases*(1-data$Index_n)
    data$cases_0[data$cases_0 == 0] <- NA
    
    data$pvn=data$pvn*(1-data$Index)
    data$pvn[data$pvn == 0] <- NA
    data$pvs=data$pvs*data$Index
    data$pvs[data$pvs == 0] <- NA
    
    
    
    
    
    sp4<-ggplot(data, aes(x=Days))+
      geom_point(aes(y=log(Cases), color=Index_nn>0), size=0.5)+
      scale_color_manual(values=c("grey69", "red3"))+
      labs(y = "log(Cases)", x="")+
      theme(legend.position = "none")
    sp4
  })
  
  output$phonePlot3 <- renderPlot({
    
    # Render a barplot
    
    file1=my.df[my.df$name==paste(input$region),]
    
    namm=paste(input$region)
    
    file1=as.data.frame(file1)
    
    
    
    file1$Index_nn=file1$Index
    
    
    for (i in 7:nrow(file1)){
      
      if (file1$Index_nn[i]==1 && file1$Cases[i]>mean(file1$Cases[i:(i-7)]))
      {file1$Index_nn[i]=1} else 
      {file1$Index_nn[i]=0}
    }
    
    
    
    w_s=7
    ratio=1/1.2
    
    true_f_p=rep(0, length(file1$Cases))
    
    for (i in w_s:(length(file1$Cases)-(w_s+1))){
      if (mean(file1$Cases[(i-w_s):i])<=ratio*mean(file1$Cases[(i+w_s):i])) {true_f_p[i]=1} else {true_f_p[i]=0}
    }
    
    final_se=length(which(file1$Index_nn==1 & true_f_p==1))/length(which(true_f_p==1))
    final_spc=length(which(file1$Index_nn==0 & true_f_p==0))/length(which(true_f_p==0))
    
    se_se=sqrt(final_se*(1-final_se)/length(which(true_f_p==1)))
    se_low=max(0, (final_se - 1.96*se_se))
    se_up= min(1, (final_se + 1.96*se_se))
    
    se_sp=sqrt(final_spc*(1-final_spc)/length(which(true_f_p==0)))
    sp_low=max(0, (final_spc - 1.96*se_sp))
    sp_up= min(1, (final_spc + 1.96*se_sp))
    
    subt=paste("Se=",round(final_se,2)," (",round(se_low,2),"; ",round(se_up,2),") & ",
               "Sp=",round(final_spc,2)," (",round(sp_low,2),"; ",round(sp_up,2),")",
               sep="")
    
    
    
    data=file1
    
    data$cases_1=data$Cases*data$Index_n
    data$cases_1[data$cases_1 == 0] <- NA
    data$cases_0=data$Cases*(1-data$Index_n)
    data$cases_0[data$cases_0 == 0] <- NA
    
    data$pvn=data$pvn*(1-data$Index)
    data$pvn[data$pvn == 0] <- NA
    data$pvs=data$pvs*data$Index
    data$pvs[data$pvs == 0] <- NA
    
    
    
    
    
    sp5<-ggplot(data, aes(x=Days))+
      geom_point(aes(y=log(cases_1), col=pvs), size=0.5)+
      geom_point(aes(y=log(cases_0)), col="grey69", size=0.5)+
      labs(y = "log(Cases)", x="")+
      scale_color_gradient(low = "green", high = "red", limits=c(0, 1))+
      labs(color= "PPV")+
      theme(legend.position = c(0.95, 0.3),
            legend.title = element_text(size=10),
            legend.text = element_text(size=8),
            legend.key.height = unit(0.5, 'cm'))
    sp5
  })
  
  output$phonePlot4 <- renderPlot({
    
    # Render a barplot
    
    file1=my.df[my.df$name==paste(input$region),]
    
    namm=paste(input$region)
    
    file1=as.data.frame(file1)
    
    
    
    file1$Index_nn=file1$Index
    
    
    for (i in 7:nrow(file1)){
      
      if (file1$Index_nn[i]==1 && file1$Cases[i]>mean(file1$Cases[i:(i-7)]))
      {file1$Index_nn[i]=1} else 
      {file1$Index_nn[i]=0}
    }
    
    
    
    w_s=7
    ratio=1/1.2
    
    true_f_p=rep(0, length(file1$Cases))
    
    for (i in w_s:(length(file1$Cases)-(w_s+1))){
      if (mean(file1$Cases[(i-w_s):i])<=ratio*mean(file1$Cases[(i+w_s):i])) {true_f_p[i]=1} else {true_f_p[i]=0}
    }
    
    final_se=length(which(file1$Index_nn==1 & true_f_p==1))/length(which(true_f_p==1))
    final_spc=length(which(file1$Index_nn==0 & true_f_p==0))/length(which(true_f_p==0))
    
    se_se=sqrt(final_se*(1-final_se)/length(which(true_f_p==1)))
    se_low=max(0, (final_se - 1.96*se_se))
    se_up= min(1, (final_se + 1.96*se_se))
    
    se_sp=sqrt(final_spc*(1-final_spc)/length(which(true_f_p==0)))
    sp_low=max(0, (final_spc - 1.96*se_sp))
    sp_up= min(1, (final_spc + 1.96*se_sp))
    
    subt=paste("Se=",round(final_se,2)," (",round(se_low,2),"; ",round(se_up,2),") & ",
               "Sp=",round(final_spc,2)," (",round(sp_low,2),"; ",round(sp_up,2),")",
               sep="")
    
    
    
    data=file1
    
    data$cases_1=data$Cases*data$Index_n
    data$cases_1[data$cases_1 == 0] <- NA
    data$cases_0=data$Cases*(1-data$Index_n)
    data$cases_0[data$cases_0 == 0] <- NA
    
    data$pvn=data$pvn*(1-data$Index)
    data$pvn[data$pvn == 0] <- NA
    data$pvs=data$pvs*data$Index
    data$pvs[data$pvs == 0] <- NA
    
    
    
    
    
    sp6<-ggplot(data, aes(x=Days))+
      geom_point(aes(y=log(cases_0), col=pvn), size=0.5)+
      geom_point(aes(y=log(cases_1)), col="grey69", size=0.5)+
      labs(y = "log(Cases)")+
      scale_color_gradient(low = "green", high = "red", limits=c(0, 1))+
      labs(color= "NPV")+
      theme(legend.position = c(0.95, 0.3),
            legend.title = element_text(size=10),
            legend.text = element_text(size=8),
            legend.key.height = unit(0.5, 'cm'))+
      labs(caption = forcaption)
    sp6
    
    
  })
  
  output$phonePlot1US <- renderPlot({
    
    # Render a barplot
    
    file1=my.df2[my.df2$name==paste(input$regionUS),]
    
    namm=paste(input$regionUS)
    
    file1=as.data.frame(file1)
    
    
    
    file1$Index_nn=file1$Index
    
    
    for (i in 7:nrow(file1)){
      
      if (file1$Index_nn[i]==1 && file1$Cases[i]>mean(file1$Cases[i:(i-7)]))
      {file1$Index_nn[i]=1} else 
      {file1$Index_nn[i]=0}
    }
    
    
    
    w_s=7
    ratio=1/1.2
    
    true_f_p=rep(0, length(file1$Cases))
    
    for (i in w_s:(length(file1$Cases)-(w_s+1))){
      if (mean(file1$Cases[(i-w_s):i])<=ratio*mean(file1$Cases[(i+w_s):i])) {true_f_p[i]=1} else {true_f_p[i]=0}
    }
    
    final_se=length(which(file1$Index_nn==1 & true_f_p==1))/length(which(true_f_p==1))
    final_spc=length(which(file1$Index_nn==0 & true_f_p==0))/length(which(true_f_p==0))
    
    se_se=sqrt(final_se*(1-final_se)/length(which(true_f_p==1)))
    se_low=max(0, (final_se - 1.96*se_se))
    se_up= min(1, (final_se + 1.96*se_se))
    
    se_sp=sqrt(final_spc*(1-final_spc)/length(which(true_f_p==0)))
    sp_low=max(0, (final_spc - 1.96*se_sp))
    sp_up= min(1, (final_spc + 1.96*se_sp))
    
    subt=paste("Se=",round(final_se,2)," (",round(se_low,2),"; ",round(se_up,2),") & ",
               "Sp=",round(final_spc,2)," (",round(sp_low,2),"; ",round(sp_up,2),")",
               sep="")
    
    
    
    data=file1
    
    data$cases_1=data$Cases*data$Index_n
    data$cases_1[data$cases_1 == 0] <- NA
    data$cases_0=data$Cases*(1-data$Index_n)
    data$cases_0[data$cases_0 == 0] <- NA
    
    data$pvn=data$pvn*(1-data$Index)
    data$pvn[data$pvn == 0] <- NA
    data$pvs=data$pvs*data$Index
    data$pvs[data$pvs == 0] <- NA
    
    
    
    sp3us<-ggplot(data, aes(x=Days))+
      geom_point(aes(y=(Cases), color=Index_nn>0), size=0.5)+
      scale_color_manual(values=c("grey69", "red3"))+
      theme(legend.position = "none")+
      labs(y = "Cases", x="")+
      ggtitle(namm)
    #, subtitle = subt)
    sp3us
    
    
  })
  
  output$phonePlot2US <- renderPlot({
    
    # Render a barplot
    
    file1=my.df2[my.df2$name==paste(input$regionUS),]
    
    namm=paste(input$region)
    
    file1=as.data.frame(file1)
    
    
    
    file1$Index_nn=file1$Index
    
    
    for (i in 7:nrow(file1)){
      
      if (file1$Index_nn[i]==1 && file1$Cases[i]>mean(file1$Cases[i:(i-7)]))
      {file1$Index_nn[i]=1} else 
      {file1$Index_nn[i]=0}
    }
    
    
    
    w_s=7
    ratio=1/1.2
    
    true_f_p=rep(0, length(file1$Cases))
    
    for (i in w_s:(length(file1$Cases)-(w_s+1))){
      if (mean(file1$Cases[(i-w_s):i])<=ratio*mean(file1$Cases[(i+w_s):i])) {true_f_p[i]=1} else {true_f_p[i]=0}
    }
    
    final_se=length(which(file1$Index_nn==1 & true_f_p==1))/length(which(true_f_p==1))
    final_spc=length(which(file1$Index_nn==0 & true_f_p==0))/length(which(true_f_p==0))
    
    se_se=sqrt(final_se*(1-final_se)/length(which(true_f_p==1)))
    se_low=max(0, (final_se - 1.96*se_se))
    se_up= min(1, (final_se + 1.96*se_se))
    
    se_sp=sqrt(final_spc*(1-final_spc)/length(which(true_f_p==0)))
    sp_low=max(0, (final_spc - 1.96*se_sp))
    sp_up= min(1, (final_spc + 1.96*se_sp))
    
    subt=paste("Se=",round(final_se,2)," (",round(se_low,2),"; ",round(se_up,2),") & ",
               "Sp=",round(final_spc,2)," (",round(sp_low,2),"; ",round(sp_up,2),")",
               sep="")
    
    
    
    data=file1
    
    data$cases_1=data$Cases*data$Index_n
    data$cases_1[data$cases_1 == 0] <- NA
    data$cases_0=data$Cases*(1-data$Index_n)
    data$cases_0[data$cases_0 == 0] <- NA
    
    data$pvn=data$pvn*(1-data$Index)
    data$pvn[data$pvn == 0] <- NA
    data$pvs=data$pvs*data$Index
    data$pvs[data$pvs == 0] <- NA
    
    
    
    
    
    sp4us<-ggplot(data, aes(x=Days))+
      geom_point(aes(y=log(Cases), color=Index_nn>0), size=0.5)+
      scale_color_manual(values=c("grey69", "red3"))+
      labs(y = "log(Cases)", x="")+
      theme(legend.position = "none")
    sp4us
  })
  
  output$phonePlot3US <- renderPlot({
    
    # Render a barplot
    
    file1=my.df2[my.df2$name==paste(input$regionUS),]
    
    namm=paste(input$region)
    
    file1=as.data.frame(file1)
    
    
    
    file1$Index_nn=file1$Index
    
    
    for (i in 7:nrow(file1)){
      
      if (file1$Index_nn[i]==1 && file1$Cases[i]>mean(file1$Cases[i:(i-7)]))
      {file1$Index_nn[i]=1} else 
      {file1$Index_nn[i]=0}
    }
    
    
    
    w_s=7
    ratio=1/1.2
    
    true_f_p=rep(0, length(file1$Cases))
    
    for (i in w_s:(length(file1$Cases)-(w_s+1))){
      if (mean(file1$Cases[(i-w_s):i])<=ratio*mean(file1$Cases[(i+w_s):i])) {true_f_p[i]=1} else {true_f_p[i]=0}
    }
    
    final_se=length(which(file1$Index_nn==1 & true_f_p==1))/length(which(true_f_p==1))
    final_spc=length(which(file1$Index_nn==0 & true_f_p==0))/length(which(true_f_p==0))
    
    se_se=sqrt(final_se*(1-final_se)/length(which(true_f_p==1)))
    se_low=max(0, (final_se - 1.96*se_se))
    se_up= min(1, (final_se + 1.96*se_se))
    
    se_sp=sqrt(final_spc*(1-final_spc)/length(which(true_f_p==0)))
    sp_low=max(0, (final_spc - 1.96*se_sp))
    sp_up= min(1, (final_spc + 1.96*se_sp))
    
    subt=paste("Se=",round(final_se,2)," (",round(se_low,2),"; ",round(se_up,2),") & ",
               "Sp=",round(final_spc,2)," (",round(sp_low,2),"; ",round(sp_up,2),")",
               sep="")
    
    
    
    data=file1
    
    data$cases_1=data$Cases*data$Index_n
    data$cases_1[data$cases_1 == 0] <- NA
    data$cases_0=data$Cases*(1-data$Index_n)
    data$cases_0[data$cases_0 == 0] <- NA
    
    data$pvn=data$pvn*(1-data$Index)
    data$pvn[data$pvn == 0] <- NA
    data$pvs=data$pvs*data$Index
    data$pvs[data$pvs == 0] <- NA
    
    
    
    
    
    sp5us<-ggplot(data, aes(x=Days))+
      geom_point(aes(y=log(cases_1), col=pvs), size=0.5)+
      geom_point(aes(y=log(cases_0)), col="grey69", size=0.5)+
      labs(y = "log(Cases)", x="")+
      scale_color_gradient(low = "green", high = "red", limits=c(0, 1))+
      labs(color= "PPV")+
      theme(legend.position = c(0.95, 0.3),
            legend.title = element_text(size=10),
            legend.text = element_text(size=8),
            legend.key.height = unit(0.5, 'cm'))
    sp5us
  })
  
  output$phonePlot4US <- renderPlot({
    
    # Render a barplot
    
    file1=my.df2[my.df2$name==paste(input$regionUS),]
    
    namm=paste(input$region)
    
    file1=as.data.frame(file1)
    
    
    
    file1$Index_nn=file1$Index
    
    
    for (i in 7:nrow(file1)){
      
      if (file1$Index_nn[i]==1 && file1$Cases[i]>mean(file1$Cases[i:(i-7)]))
      {file1$Index_nn[i]=1} else 
      {file1$Index_nn[i]=0}
    }
    
    
    
    w_s=7
    ratio=1/1.2
    
    true_f_p=rep(0, length(file1$Cases))
    
    for (i in w_s:(length(file1$Cases)-(w_s+1))){
      if (mean(file1$Cases[(i-w_s):i])<=ratio*mean(file1$Cases[(i+w_s):i])) {true_f_p[i]=1} else {true_f_p[i]=0}
    }
    
    final_se=length(which(file1$Index_nn==1 & true_f_p==1))/length(which(true_f_p==1))
    final_spc=length(which(file1$Index_nn==0 & true_f_p==0))/length(which(true_f_p==0))
    
    se_se=sqrt(final_se*(1-final_se)/length(which(true_f_p==1)))
    se_low=max(0, (final_se - 1.96*se_se))
    se_up= min(1, (final_se + 1.96*se_se))
    
    se_sp=sqrt(final_spc*(1-final_spc)/length(which(true_f_p==0)))
    sp_low=max(0, (final_spc - 1.96*se_sp))
    sp_up= min(1, (final_spc + 1.96*se_sp))
    
    subt=paste("Se=",round(final_se,2)," (",round(se_low,2),"; ",round(se_up,2),") & ",
               "Sp=",round(final_spc,2)," (",round(sp_low,2),"; ",round(sp_up,2),")",
               sep="")
    
    
    
    data=file1
    
    data$cases_1=data$Cases*data$Index_n
    data$cases_1[data$cases_1 == 0] <- NA
    data$cases_0=data$Cases*(1-data$Index_n)
    data$cases_0[data$cases_0 == 0] <- NA
    
    data$pvn=data$pvn*(1-data$Index)
    data$pvn[data$pvn == 0] <- NA
    data$pvs=data$pvs*data$Index
    data$pvs[data$pvs == 0] <- NA
    
    
    
    
    
    sp6us<-ggplot(data, aes(x=Days))+
      geom_point(aes(y=log(cases_0), col=pvn), size=0.5)+
      geom_point(aes(y=log(cases_1)), col="grey69", size=0.5)+
      labs(y = "log(Cases)")+
      scale_color_gradient(low = "green", high = "red", limits=c(0, 1))+
      labs(color= "NPV")+
      theme(legend.position = c(0.95, 0.3),
            legend.title = element_text(size=10),
            legend.text = element_text(size=8),
            legend.key.height = unit(0.5, 'cm'))+
      labs(caption = forcaption2)
    sp6us
    
    
  })
  
}

shinyApp(ui, server, options = list(width = "100%", height = 1600))

