library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)

rm(list = ls()) 	
options("scipen"=100, "digits"=4)


setwd("~/Dropbox/Documents/Projects/Oxford under-representation/")

reg.clean <- read.csv("reg_clean.csv",check.names = F)
reg.clean.long <- read.csv("reg_clean_long.csv",check.names = F)
eth.det.clean <- read.csv("eth_det_clean.csv",check.names = F)
eth.det.clean.long <- read.csv("eth_det_clean_long.csv",check.names = F)
eth.broad.clean <- read.csv("eth_broad_clean.csv",check.names = F)
eth.broad.clean.long <- read.csv("eth_broad_clean_long.csv",check.names = F)

# set textsize

textsize = 25

ui <- navbarPage(theme = shinytheme("yeti"), "Representation at Oxford University",
                 tabPanel("Plots",
                          #titlePanel("Oxford Undergraduate Representation Statistics"),
                          
                          sidebarLayout(      
                            # Define the sidebar with one input
                            sidebarPanel(
                              "These plots allow you to explore the ethnicity and home region of
                              undergraduate applications and admissions to Oxford University. The purpose of the plots
                              is to better communicate the under and over-representation of different groups. Click on the 'about' tab 
                              to learn more.", br(), hr(),
                              selectInput("group_type", "Choose a set of groups to explore:", 
                                          choices= c("Ethnicity (Detailed)","Ethnicity (Broad)","Region")),
                              hr(),
                              "Data types:", br(),
                              helpText("The Index of Representation is the ratio of the proportion of offers/applications to each group and the proportion 
               in the relevant baseline group. Numbers greater than 1 correspond to over-representation,
               whereas numbers less than 1 correspond to under-representation. For example, an index of 2 means that a group is
               twice as common among Oxford applicants/offer holders than they are in the benchmark population."),
                              helpText("Application success is the proportion of applications that resulted in an offer within each group")
                            ),
                            mainPanel(
                              tabsetPanel(tabPanel("Index of Representation",
                              plotOutput("coolplot1", hover = "coolplot_hover1",width = "100%", height = 800),
                              br(),
                              textOutput("selected_var1")),
                              tabPanel("Application success",
                                       plotOutput("coolplot2", hover = "coolplot_hover2",width = "100%", height = 800),
                                       br(),
                                       textOutput("selected_var2"))
                            )))
                 ),
                 tabPanel("About",
                          verbatimTextOutput("summary")
                 )
)

server <- function(input, output) {
  output$coolplot1 <- renderPlot({
    if (input$group_type == "Region"){
      reg.clean.long$Region <- factor(reg.clean.long$Region, 
                                      levels = unique(reg.clean.long$Region[order(reg.clean.long$Score)]))
      ggplot(reg.clean.long ,aes(x = Region, y = Score, fill = Index)) + geom_col(position = "dodge") +
        xlab("Region") + ylab("Index of Representation") +
        coord_flip() +
        theme(text = element_text(size=textsize)) + 
        theme(legend.title=element_blank(),legend.text = element_text(size = textsize)) +
        guides(fill=guide_legend(
          keywidth=0.9,
          keyheight=0.9,
          default.unit="inch")
        )
    }
   
    else if (input$group_type == "Ethnicity (Detailed)") {
      
      eth.det.clean.long$`Ethnic group` <- factor(eth.det.clean.long$`Ethnic group`, 
                                                  levels = unique(eth.det.clean.long$`Ethnic group`[order(eth.det.clean.long$`Representation Index`)]))
      ggplot(eth.det.clean.long ,aes(x = eth.det.clean.long$`Ethnic group`, y = eth.det.clean.long$`Representation Index`,
                                     fill = eth.det.clean.long$type2)) + geom_col(position = "dodge") +
        xlab("Ethnic Group (Detailed)") + ylab("Index of Representation") + 
        coord_flip() +
        theme(text = element_text(size=textsize)) + 
        theme(legend.title=element_blank(),legend.text = element_text(size = textsize)) +
        guides(fill=guide_legend(
          keywidth=0.9,
          keyheight=0.9,
          default.unit="inch")
        )
    }
    else if (input$group_type == "Ethnicity (Broad)") {
      eth.broad.clean.long$`Ethnic group` <- factor(eth.broad.clean.long$`Ethnic group`, 
                                                    levels = unique(eth.broad.clean.long$`Ethnic group`[order(eth.broad.clean.long$`Representation Index`)]))
      ggplot(eth.broad.clean.long ,aes(x = eth.broad.clean.long$`Ethnic group`, y = eth.broad.clean.long$`Representation Index`,
                                       fill = eth.broad.clean.long$type2)) + geom_col(position = "dodge") +
        xlab("Ethnic Group (Broad)") + ylab("Index of Representation") + 
        coord_flip() +
        theme(text = element_text(size=textsize)) + 
        theme(legend.title=element_blank(),legend.text = element_text(size = textsize)) +
        guides(fill=guide_legend(
          keywidth=0.9,
          keyheight=0.9,
          default.unit="inch")
        )
    }
  })
  
  output$coolplot2 <- renderPlot({
    if (input$group_type == "Region") {
      reg.clean$Region <- factor(reg.clean$Region, 
                                 levels = unique(reg.clean$Region[order(reg.clean$success)]))
      ggplot(reg.clean ,aes(x = Region, y = success)) + geom_col(fill='#00BFC4') +
        xlab("Region") + ylab("Success rate (%)") +
        coord_flip() +
        theme(text = element_text(size=textsize)) + 
        theme(legend.title=element_blank(),legend.text = element_text(size = textsize))
    }
 
    else if (input$group_type == "Ethnicity (Detailed)") {
      eth.det.clean$`Ethnic group` <- factor(eth.det.clean$`Ethnic group`, 
                                             levels = unique(eth.det.clean$`Ethnic group`[order(eth.det.clean$success)]))
      ggplot(eth.det.clean ,aes(x = eth.det.clean$`Ethnic group`, y = eth.det.clean$success)) + geom_col() +
        xlab("Ethnic Group (Detailed)") + ylab("Success rate (%)") + geom_col(fill='#00BFC4') + 
        coord_flip() +
        theme(text = element_text(size=textsize)) + 
        theme(legend.title=element_blank(),legend.text = element_text(size = textsize))
    }
    else if (input$group_type == "Ethnicity (Broad)") {
      ggplot(eth.broad.clean ,aes(x = eth.broad.clean$`Ethnic group`, y = eth.broad.clean$success)) + geom_col() +
        xlab("Ethnic Group (Broad)") + ylab("Success rate (%)") + geom_col(fill='#00BFC4') + 
        coord_flip() +
        theme(text = element_text(size=textsize)) + 
        theme(legend.title=element_blank(),legend.text = element_text(size = textsize))
    }
  })

  output$selected_var2 <- renderText({
    if (is.null(input$coolplot_hover2$x)) return("")
    else {
      #lvls <- levels(eth$eth.group)
      #name <- lvls[round(input$coolplot_hover$y)]
      #index <- round(eth$rep.perc[which(eth$eth.group == paste(name))], digits = 2)
      #paste("Individuals of", name, " ethnicity are ",  index, "times as common among Oxford undergraduate entrants 
      #      than they are in the pool of UK nationals of the same age. They make up ", pop.temp, " percent of the (age-adjusted) 
      #      population and", acc.temp, " percent of admissions")
      paste("Hover text here")
    }
  })
  
  output$selected_var1 <- renderText({
    if (is.null(input$coolplot_hover1$x)) return("")
    else {
      #lvls <- levels(eth$eth.group)
      #name <- lvls[round(input$coolplot_hover$y)]
      #index <- round(eth$rep.perc[which(eth$eth.group == paste(name))], digits = 2)
      #paste("Individuals of", name, " ethnicity are ",  index, "times as common among Oxford undergraduate entrants 
      #      than they are in the pool of UK nationals of the same age. They make up ", pop.temp, " percent of the (age-adjusted) 
      #      population and", acc.temp, " percent of admissions")
      paste("Hover text here")
    }
  })
  
  
  }

shinyApp(ui = ui, server = server)