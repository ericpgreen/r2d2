library(shiny)
library(ggplot2)
library(xlsx)
library(tidyverse)
library(reshape2)
library(grid)
library(useful)

dpct <- read.xlsx("data/disclosure_pct.xlsx", sheetName="Sheet1")

datPct <-
  dpct %>%
  mutate(yearLabel=ifelse(is.na(estimate), as.character(year),
                          paste0(year, estimate))) %>%
  mutate(name=paste0(study, ", ", yearLabel, " (", country, ")")) %>%
  arrange(pctDisclosed) %>%
  mutate(id=seq_along(dpct$country)) %>%
  filter(!is.na(minAge)) %>%
  mutate(ours=ifelse(grepl("Finnegan", name), "Our Study", "not")) %>%
  mutate(name=ifelse(grepl("Finnegan", name), "OUR STUDY (Zimbabwe)", name)) %>%
  mutate(psam=ifelse(is.na(psam), 1, 0)) %>%
  filter(!is.na(sampleSize))



ui <- shinyUI(fluidPage(
  
  
  # Application title
  headerPanel("Global Pediatric HIV Disclosure Rates"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    
    h5("Use the widgets below to explore the plot."),
    
    sliderInput("year", 
                "Study year:", 
                min = min(as.numeric(as.character(dpct$year)), na.rm=TRUE),
                max = max(as.numeric(as.character(dpct$year)), na.rm=TRUE),
                value=c(min(as.numeric(as.character(dpct$year)), na.rm=TRUE),
                        max(as.numeric(as.character(dpct$year)), na.rm=TRUE)),
                sep=""),
    
    selectInput("region",
                "Select region(s):",
                choices=levels(dpct$region),
                multiple=TRUE,
                selected=levels(dpct$region)),
    
    sliderInput("ageRange",
                "Select age range:",
                min = min(dpct$minAge, na.rm=TRUE),
                max = max(dpct$maxAge, na.rm=TRUE),
                value=c(min(dpct$minAge, na.rm=TRUE),
                             max(dpct$maxAge, na.rm=TRUE)),
                sep=""),
    
    checkboxInput("psam", 
                  label = span("Highlight studies with probability samples in red.",
                               style="color:red"),
                  value = FALSE)
  ),
  
  mainPanel(

    plotOutput("plots", 
               width="100%",
               height="800px")
  )
  
))

server <- shinyServer(function(input, output) {
  
  datReact <- reactive({

    dat <-
    datPct %>%
      filter(as.numeric(as.character(year)) >= input$year[1] & 
               as.numeric(as.character(year)) <= input$year[2] &
               region %in% input$region &
               minAge >= input$ageRange[1] & maxAge <= input$ageRange[2])
    return(dat)
    
  })

  output$plots <- renderPlot ({
    
    # study x pct disclosed
    studyDisclosed <-
      datReact() %>%
      droplevels(.) %>%
      ggplot(., aes(x=pctDisclosed, y=reorder(as.character(name), pctDisclosed))) +
      xlim(-10,100) +
      labs(x="Percent Disclosed",
           y="") +
      theme_bw() +
      theme(panel.grid=element_blank())

    
      if(input$psam==TRUE & max(datReact()$psam, na.rm=TRUE) == 1) {
        studyDisclosed <-
          studyDisclosed +
          geom_point(aes(x=pctDisclosed, y=reorder(as.character(name), pctDisclosed), 
                         color=as.factor(psam), shape=as.factor(psam))) +
          geom_text(aes(x=pctDisclosed, y=reorder(as.character(name), pctDisclosed), 
                        color=as.factor(psam), label=sampleSize, size=3, hjust=1.5),
                        show.legend=FALSE) +
          scale_color_manual(values=c("0"= "red",
                                      "1" = "gray")) +
          scale_shape_manual(values=c(19,1)) +
          guides(color=FALSE) +
          guides(shape=FALSE)
      }
    
      if(input$psam==FALSE | max(datReact()$psam, na.rm=TRUE)==0) {
        studyDisclosed <-
          studyDisclosed +
          geom_point(aes(x=pctDisclosed, y=reorder(as.character(name), pctDisclosed)), 
                     color="black", shape=19) +
          geom_text(aes(x=pctDisclosed, y=reorder(as.character(name), pctDisclosed), 
                        label=sampleSize, size=3, hjust=1.5),
                    show.legend=FALSE) +
          guides(color=FALSE) +
          guides(shape=FALSE)
      }
              
    # study x age range
    studyAgeRange <-
      datReact() %>%
      select(minAge, maxAge, name, pctDisclosed, ours, psam) %>%
      gather(., key, value, -name, -pctDisclosed, -ours, -psam) %>%
      filter(!is.na(value)) %>%
      droplevels(.) %>%
      ggplot(., aes(x=value, y=reorder(as.character(name), pctDisclosed), 
                    group=name, shape=as.factor(psam), color=as.factor(psam))) +
      labs(x="Study Age Range",
           y="") +
      xlim(0,20) +
      theme_bw() +
      theme(panel.grid=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) 
    
    
      if(input$psam==TRUE & max(datReact()$psam, na.rm=TRUE)==1) {
        studyAgeRange <-
          studyAgeRange +
          geom_point(aes(x=value, y=reorder(as.character(name), pctDisclosed), 
                         group=name, shape=as.factor(psam), color=as.factor(psam))) +
          geom_line(aes(x=value, y=reorder(as.character(name), pctDisclosed), 
                        group=name, shape=as.factor(psam), color=as.factor(psam))) +
          scale_color_manual(values=c("0"="red",
                                      "1"="gray")) +
          scale_shape_manual(values=c(19,1)) +
          guides(color=FALSE) +
          guides(shape=FALSE)
      }
    
    if(input$psam==FALSE | max(datReact()$psam, na.rm=TRUE)==0) {
      studyAgeRange <-
        studyAgeRange +
        geom_point(aes(x=value, y=reorder(as.character(name), pctDisclosed), 
                       group=name),
                       color="black", shape=19) +
        geom_line(aes(x=value, y=reorder(as.character(name), pctDisclosed), 
                      group=name),
                      color="black") +
        scale_color_manual(values=c("black",
                                    gray.colors(7)[4])) +
        scale_shape_manual(values=c(19,1)) +
        guides(color=FALSE) +
        guides(shape=FALSE) 
    }
    
  
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(1,3)))
  print(studyDisclosed, vp=vplayout(1,1:2))
  print(studyAgeRange, vp=vplayout(1,3))

  
  })

})

shinyApp(ui=ui,server=server)