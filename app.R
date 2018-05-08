#please install the following packages using install.packages("") if you don't have them installed
#e.g:
##install.packages(c("ldatuning","tidytext","topicmodels","knitr","dplyr","ggplot2","stringr","tidyverse","shiny","shinydashboard","LDAvis"))
library(tidytext)
library(knitr)
library(dplyr)
library(topicmodels)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(LDAvis)
library(rmarkdown)
library(jsonlite)

ui <- dashboardPage(
  dashboardHeader(title = "EM777 DiscoverText: Bots on Twitter", titleWidth = 350),
  dashboardSidebar(sidebarMenu(
      menuItem("Comparison", tabName = "comparison",icon = icon("table"),
               menuSubItem("URL", tabName = "url"),
               menuSubItem("Retweet", tabName = "retweet"),
               menuSubItem("Expressive Mark", tabName = "expressivemark")),
      menuItem("Topic Modeling", tabName = "topic modeling", icon = icon("language"),
               menuSubItem("Bot", tabName = "bot"),
               menuSubItem("Nonbot", tabName = "nonbot"),
               menuSubItem("Top-topic Comparison", tabName = "toptopic"),
               menuSubItem("Distance between Topics", tabName = "distance"))
    )),
  dashboardBody(tabItems(
    tabItem(tabName = "url",
            fluidRow(
              box(
                width = 10,
                title = "Distribution Comparison of URL",
                status = "primary",
                plotOutput("url_distribution")
              ),
              box(
                width = 10,
                title = "Boxplot Comparison of URL",
                status = "success",
                plotOutput("url_boxplot")
               ),
              box(
                width = 10,
                title = "T-test",
                status = "danger",
                tableOutput("url_ttest")
              )
              )),
    tabItem(tabName = "retweet",
            fluidRow(
              box(
                width = 10,
                title = "Distribution Comparison of Retweet",
                status = "primary",
                plotOutput("retweet_distribution")
              ),
              box(
                width = 10,
                title = "Boxplot comparison of Retweet",
                status = "success",
                plotOutput("retweet_boxplot")
              ),
              box(
                width = 10,
                title = "T-test",
                status = "danger",
                tableOutput("retweet_ttest")
              )
            )),
    tabItem(tabName = "expressivemark",
            fluidRow(
              column(width = 6, box(
                width = "100%",
                title = "Distribution Comparison of Exclamation Mark",
                status = "primary",
                plotOutput("exclamation_distribution")
              ),
              box(
                width = "100%",
                title = "Boxplot Comparison of Exclamation Mark",
                status = "success",
                plotOutput("exclamation_boxplot")
              ),
              box(
                width = "100%",
                title = "T-test",
                status = "danger",
                tableOutput("exclamation_ttest")
              )),
              ##question mark
              column(width = 6, box(
                width = "100%",
                title = "Distribution Comparison of Question Mark",
                status = "primary",
                plotOutput("question_distribution")
              ),
              box(
                width = "100%",
                title = "Boxplot Comparison of Question Mark",
                status = "success",
                plotOutput("question_boxplot")
              ),
              box(
                width = "100%",
                title = "T-test",
                status = "danger",
                tableOutput("question_ttest")
              ))
            )),
    tabItem(tabName = "bot",
            fluidRow(
              box(width = "100%",
                  title = "Bot: Visualize Topic Modeling",sliderInput(inputId="nTerms_bot", "Number of terms to display", min = 20, max = 40, value = 30)
                  ),
              box(width = "100%", visOutput("botChart")),
              conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                               div(
                                 img(src = 'https://icons8.com/preloaders/svg-preview.php?preloader_id=30'),
                                 style = "text-align: center;"
                               )))        
            ),
    tabItem(tabName = "nonbot",
            fluidPage(uiOutput("nonboturl"))        
            ),
    
    tabItem(tabName = "toptopic",
            fluidRow(
              box(
                width = 10,
                title = "Distribution Comparison of Top-topic Proportion",
                status = "primary",
                plotOutput("toptopic_distribution")
              ),
              box(
                width = 10,
                title = "Boxplot Comparison of Top-topic Proportion",
                status = "success",
                plotOutput("toptopic_boxplot")
              ),
              box(
                width = 10,
                title = "T-test on Top-topic Proportion",
                status = "danger",
                tableOutput("toptopic_ttest")
              )
            )),

    tabItem(tabName = "distance",
            fluidRow(
              box(
                width = 10,
                title = "Distribution Comparison of Distance between Topics",
                status = "primary",
                plotOutput("distance_distribution")
              ),
              box(
                width = 10,
                title = "Boxplot Comparison of Distance between Topics",
                status = "success",
                plotOutput("distance_boxplot")
              ),
              box(
              width = 10,
              title = "T-test on Distance between Topics",
              status = "danger",
              tableOutput("distance_ttest"))        
            ))
)))


server <- shinyServer(function(input, output, session) {
  #1. url:
  load("URLfreq.Rdata")
  #url distribution:
  output$url_distribution <- renderPlot({
    ggplot(URLfreq, aes(Number,fill=Type))+geom_density(alpha=0.7)+theme(legend.text = element_text(size=14))
  })
 
  #url boxplot:
  output$url_boxplot <- renderPlot({
    ggplot(URLfreq, aes(y=Number,x=Type, fill=Type))+geom_boxplot()+theme(legend.text = element_text(size=14))+
      stat_summary(fun.y=mean, colour="black", geom="point", shape=18, size=3)
    
  })
  
  #url t-test
  url_ttest <- t.test(URLfreq$Number[URLfreq$Type=="bot"], URLfreq$Number[URLfreq$Type=="nonbot"])
  output$url_ttest <- renderTable({data.frame(Type=c("Bot","Nonbot"), 
                                                  Number=c(length(URLfreq$Type[URLfreq$Type=="bot"]), 
                                                           length(URLfreq$Type[URLfreq$Type=="nonbot"])),
                                                          Mean= c(url_ttest$estimate[1],url_ttest$estimate[2]),
                                                          p_value=c(url_ttest$p.value),
                                                          t=c(url_ttest$statistic))
    
  })
  
  #2. retweet:
  load("RTfreq.Rdata")
  #retweet distribution:
  output$retweet_distribution <- renderPlot({
    ggplot(RTfreq, aes(Number,fill=Type))+geom_density(alpha=0.7)+theme(legend.text = element_text(size=14))
  })
  
  #retweet boxplot:
  output$retweet_boxplot <- renderPlot({
    ggplot(RTfreq, aes(y=Number,x=Type, fill=Type))+geom_boxplot()+theme(legend.text = element_text(size=14))+
      stat_summary(fun.y=mean, colour="black", geom="point", shape=18, size=3)
  })
  
  #retweet t-test:
  rt_ttest <- t.test(RTfreq$Number[RTfreq$Type=="bot"], RTfreq$Number[RTfreq$Type=="nonbot"])
  output$retweet_ttest <- renderTable({data.frame(Type=c("Bot","Nonbot"), 
                                                  Number=c(length(RTfreq$Type[RTfreq$Type=="bot"]), length(RTfreq$Type[RTfreq$Type=="nonbot"])),
                                                  Mean= c(rt_ttest$estimate[1],rt_ttest$estimate[2]),
                                                  p_value=c(rt_ttest$p.value),
                                                  t=c(rt_ttest$statistic))
    
  })
  
  #3. exclamation mark:

  load("EXCLfreq.Rdata")
  #exclamation distribution:
  output$exclamation_distribution <- renderPlot({
    ggplot(EXCLfreq, aes(Number,fill=Type))+geom_density(alpha=0.7)+theme(legend.text = element_text(size=14))
  })
  
  #exclamation boxplot:
  output$exclamation_boxplot <- renderPlot({
    ggplot(EXCLfreq, aes(y=Number,x=Type, fill=Type))+geom_boxplot()+theme(legend.text = element_text(size=14))+
      stat_summary(fun.y=mean, colour="black", geom="point", shape=18, size=3)
  })
  
  #exclamation t-test:
  exclamation_ttest <- t.test(EXCLfreq$Number[EXCLfreq$Type=="bot"], EXCLfreq$Number[EXCLfreq$Type=="nonbot"])
  output$exclamation_ttest <- renderTable({data.frame(Type=c("Bot","Nonbot"), 
                                                  Number=c(length(EXCLfreq$Type[EXCLfreq$Type=="bot"]), length(EXCLfreq$Type[EXCLfreq$Type=="nonbot"])),
                                                  Mean= c(exclamation_ttest$estimate[1],exclamation_ttest$estimate[2]),
                                                  p_value=c(exclamation_ttest$p.value),
                                                  t=c(exclamation_ttest$statistic))
    
  })
  
  #4. question marl:
  load("QUESfreq.Rdata")
  #qustion distribution:
  output$question_distribution <- renderPlot({
    ggplot(QUESfreq, aes(Number,fill=Type))+geom_density(alpha=0.7)+theme(legend.text = element_text(size=14))
  })
  
  #qustion boxplot:
  output$question_boxplot <- renderPlot({
    ggplot(QUESfreq, aes(y=Number,x=Type, fill=Type))+geom_boxplot()+theme(legend.text = element_text(size=14))+
      stat_summary(fun.y=mean, colour="black", geom="point", shape=18, size=3)
  })
  
  #qustion t-test:
  question_ttest <- t.test(QUESfreq$Number[QUESfreq$Type=="bot"], QUESfreq$Number[QUESfreq$Type=="nonbot"])
  output$question_ttest <- renderTable({data.frame(Type=c("Bot","Nonbot"), 
                                                      Number=c(length(QUESfreq$Type[QUESfreq$Type=="bot"]), length(QUESfreq$Type[QUESfreq$Type=="nonbot"])),
                                                      Mean= c(question_ttest$estimate[1],question_ttest$estimate[2]),
                                                      p_value=c(question_ttest$p.value),
                                                      t=c(question_ttest$statistic))
  
  })
  
  #non bot:
  url <- a("nonbot", href="https://docs.google.com/document/d/145YtlddLymoy3oGkQ2fTOUCiJZb5HGhQGi7kHFbiV5M/edit")
  output$nonboturl <- renderUI({
    tagList("URL link:", url)
  })
  
  # top topic:
  load("Top.gamma50.Rdata")
  #top topic distribution:
  output$toptopic_distribution <- renderPlot({
    ggplot(Top.gamma50, aes(Gamma,fill=Type))+geom_density(alpha=0.7)+theme(legend.text = element_text(size=14))
  })
  
  #top topic boxplot:
  output$toptopic_boxplot <- renderPlot({
    ggplot(Top.gamma50, aes(y=Gamma,x=Type, fill=Type))+geom_boxplot()+theme(legend.text = element_text(size=14))+
      stat_summary(fun.y=mean, colour="black", geom="point", shape=18, size=3)
  })
  
  #top topic t-test:
  toptopic_ttest <- t.test(Top.gamma50$Gamma[Top.gamma50$Type=="bot"], Top.gamma50$Gamma[Top.gamma50$Type=="nonbot"])
  output$toptopic_ttest <- renderTable({data.frame(Type=c("Bot","Nonbot"), 
                                                   Gamma=c(length(Top.gamma50$Type[Top.gamma50$Type=="bot"]), length(Top.gamma50$Type[Top.gamma50$Type=="nonbot"])),
                                                   Mean= c(toptopic_ttest$estimate[1],toptopic_ttest$estimate[2]),
                                                   p_value=c(toptopic_ttest$p.value),
                                                   t=c(toptopic_ttest$statistic))
    
  })
  
  
  #2. distance:
  load("distance.Rdata")
  #distance distribution:
  output$distance_distribution <- renderPlot({
    ggplot(distance, aes(Distance,fill=Type))+geom_density(alpha=0.7)+theme(legend.text = element_text(size=14))
  })
  
  #distance boxplot:
  output$distance_boxplot <- renderPlot({
    ggplot(distance, aes(y=Distance,x=Type, fill=Type))+geom_boxplot()+theme(legend.text = element_text(size=14))+
      stat_summary(fun.y=mean, colour="black", geom="point", shape=18, size=3)
  })
  
  #distance t-test:
  distance_ttest <- t.test(distance$Distance[distance$Type=="bot"], distance$Distance[distance$Type=="nonbot"])
  output$distance_ttest <- renderTable({data.frame(Type=c("Bot","Nonbot"), 
                                                  Distance=c(length(distance$Type[distance$Type=="bot"]), length(distance$Type[distance$Type=="nonbot"])),
                                                  Mean= c(distance_ttest$estimate[1],distance_ttest$estimate[2]),
                                                  p_value=c(distance_ttest$p.value),
                                                  t=c(distance_ttest$statistic))
    
  })
  
  #bot lda:
  ldavis50_bot2000 <- load("ldavis50_bot2000.Rdata")
  
  output$botChart <- renderVis({
    if(!is.null(input$nTerms_bot)){
      with(ldavis50_bot2000, 
           createJSON(phi_bot, theta_bot, doc.length_bot, vocab_bot, term.frequency_bot, 
                      R = input$nTerms_bot))} 
  })
  
})

shinyApp(ui,server)