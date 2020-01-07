library(shiny)
library(DT)

library(readr)
library(pdftools)
library(rvest)

library(dplyr)
library(lubridate)
library(janitor)
library(stringr)

source("legislation_mining.R")

Sys.setenv(http_proxy="http://10.85.4.54:8080", https_proxy="http://10.85.4.54:8080")

#leg_raw <- read.csv("Data/EULex-agri.csv",stringsAsFactors = FALSE)
leg_raw <- read_csv("Data/EULex-agri.csv") #,stringsAsFactors = FALSE)

leg_raw$Title <- iconv(leg_raw$Title,"latin1", "UTF-8", sub="") #Deletes unreadable unicode

#https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:22019A0605(01)

leg_data <- leg_raw %>%
  mutate(`Subject matter` = strsplit(as.character(`Subject matter`),", "),
         `EUROVOC descriptor` = strsplit(as.character(`EUROVOC descriptor`),", "),
         leg_year = year(as.Date(`Date of document`,format = "%d/%m/%Y")))

subjects <- unique(Reduce(c,leg_data$`Subject matter`))
descriptors <- unique(Reduce(c,leg_data$`EUROVOC descriptor`))

years <- unique(leg_data$`leg_year`) %>% sort(decreasing = TRUE)


ui <- tagList(
  tags$style("h1 {font-size:18px;
               display:block; }"),
  # Application title
  navbarPage("EU legislation finder",id = "main",
    tabPanel("Legislation search",
      sidebarPanel(
        selectInput("filtertype",
                    "Search by:",
                    choices = c("EUROVOC descriptor" = "descriptor","Subject matter" = "subject"),
                    selected = "EUROVOC descriptor"),
        conditionalPanel("input.filtertype == 'descriptor'",
                         selectizeInput("descriptorselect",
                                        "EUROVOC descriptor:",
                                        choices = descriptors,
                                        multiple = TRUE)
        ),
        conditionalPanel("input.filtertype == 'subject'",
                         selectizeInput("subjectselect",
                                        "Subject matter:",
                                        choices = subjects,
                                        multiple = TRUE)
        ),
        checkboxInput("allanyflag",
                      "Match all criteria?",
                      value = FALSE),
        textInput("titlestring",
                  "Search for specific legislation",
                  placeholder = "You can enter a specific search string here"),
        selectInput("yearselect",
                    "Year of publication:",
                    choices = c("All",years)
        ),
        "Use the tools above to search for active legislation. You can then select the legislation in the table and use the button
        below to search for linked legislation. Alternatively you can navigate to the linked legislation tab above and enter the
        legislation directly",
        actionButton("searchselected",
                     "Find linked legislation")
      ),
      
      mainPanel(
        dataTableOutput("searchtable")
      )
    ),
    tabPanel("Linked legislation",
      sidebarPanel(
        selectInput("pdfselect",
                       "Choose legislation to search",
                       choices = c("",leg_data$Title)),
        textInput("pdftext",
                  "Alternatively, enter the URL here",
                  placeholder = "Paste the PDF URL or use the search above"
                  ),
        checkboxGroupInput("legtype",
                           "Legislation types to show",
                           choices = c("Directive","Decision","Regulation"),
                           selected = c("Directive","Decision","Regulation")
                           ),
        checkboxInput("activeonlyflag",
                      "Show only active legislation?",
                      value = FALSE)
      ),
      mainPanel(
        h1(textOutput("linktitle")),
        hr(),
        h1(uiOutput("weblink")),
        h1(uiOutput("pdflink")),
        hr(),
        dataTableOutput("linktable")
      )
    )
  )
)


server <- function(input, output, session) {
  
  
  leg_output <- reactive({
    if(input$filtertype == "descriptor"){
      if(input$allanyflag == TRUE){
        leg_output <- leg_data %>% filter(sapply(`EUROVOC descriptor`,function(x){length(intersect(input$descriptorselect,unlist(x)))==length(input$descriptorselect)&&length(input$descriptorselect)>0}))
      }
      else{
        leg_output <- leg_data %>% filter(sapply(`EUROVOC descriptor`,function(x){length(intersect(input$descriptorselect,unlist(x)))>0}))
      }
      if(length(input$descriptorselect) == 0 & input$titlestring !=""){leg_output <- leg_data}
    }
    else{
      if(input$allanyflag == TRUE){
        leg_output <- leg_data %>% filter(sapply(`Subject matter`,function(x){length(intersect(input$subjectselect,unlist(x)))==length(input$subjectselect)&&length(input$subjectselect)>0}))
      }
      else{
        leg_output <- leg_data %>% filter(sapply(`Subject matter`,function(x){length(intersect(input$subjectselect,unlist(x)))>0}))
      }
      if(length(input$subjectselect) == 0 & input$titlestring !=""){leg_output <- leg_data}
    }
    
    if(input$titlestring !=""){leg_output <- leg_output %>% filter(grepl(input$titlestring,Title,ignore.case = T))}
    
    if(input$yearselect != "All"){leg_output <- leg_output %>% filter(leg_year == input$yearselect)}
    

    
    return(leg_output)
  })
  
  observeEvent(input$searchselected,{
    req(length(input$searchtable_rows_selected)>0)
    CELEX_selected <- leg_output()$`CELEX number`[input$searchtable_rows_selected]
    pdfURL <- paste0("https://eur-lex.europa.eu/legal-content/EN/TXT/PDF/?uri=CELEX:",CELEX_selected)
    updateTextInput(session,"pdftext",value = pdfURL)
    updateTabsetPanel(session, "main",
                      selected = "Linked legislation")
    
  })
  
  observeEvent(input$pdftext,{
    req(input$pdftext!="")
    updateSelectInput(session,"pdfselect", selected = "")    
  })
  
  observeEvent(input$pdfselect,{
    req(input$pdfselect!="")
    updateTextInput(session,"pdftext", value = "")
    })  
  
  legislation_data <- reactive({
    req(input$pdftext!=""|input$pdfselect!="")
    
    if(input$pdfselect!=""){
      CELEX_selected <- leg_data$`CELEX number`[which(leg_data$Title == input$pdfselect)]
      pdfURL <- paste0("https://eur-lex.europa.eu/legal-content/EN/TXT/PDF/?uri=CELEX:",CELEX_selected)}
    else{pdfURL <- input$pdftext}
    htmlURL <- str_replace(pdfURL,"/PDF","")
    
    validate(need(try(legislation_text <- pdf_text(pdfURL),silent = TRUE),message = "URL not found"),
             need(try(legislation_title <- read_html(htmlURL) %>%
                        html_nodes("#translatedTitle") %>%
                        html_text(),silent = TRUE),message = ""))
    
    weblink <- a("EU Legislation webpage", href = htmlURL, target='_blank')
    pdflink <- a("PDF version", href = pdfURL, target='_blank')
    
    return(list(text = legislation_text,title = legislation_title, weblink = weblink, pdflink = pdflink))
  })
  
    
  references_generate <- reactive({
    req(input$pdftext!=""|input$pdfselect!="")
    req(length(legislation_data()$text)>0)
#    legislation_text <- sapply(legislation_text, function(text) str_replace_all(text," ","")) #to test
    legislation_text <- legislation_data()$text
    references_found <- data.frame(Title = character(),
                                   Category = character(),
                                   Pages = character(),
                                   `CELEX number` = character(),
                                   Link = character(),
                                   PDF = character(),
                                   MatchFlag = numeric())
    return(
      bind_rows(
        references_found,
        find_references(legislation_text,leg_data,"Directive","L","(?<=Directive).*(?=/E)"),
        find_references(legislation_text,leg_data,"Decision","D","(?<=Decision).*(?=/E)"),
        find_references(legislation_text,leg_data,"Regulation","R","(?<=Regulation\\(E.\\)).*(/....)")
      )
    )
  })
  
  references_output <- reactive({
    references_table <- references_generate() %>%
      filter(Category %in% input$legtype,
             str_length(`CELEX number` == 11),
             !grepl("0000", `CELEX number`, fixed = TRUE),
             !grepl("NA", `CELEX number`, fixed = TRUE))

    if(input$activeonlyflag == TRUE){
      references_table <- references_table %>%
        filter(MatchFlag == 1)
    }
    return(references_table %>% select(Title,Category,Pages,`CELEX number`,Link,PDF))
  })
  

  
  output$searchtable <- renderDT({
    datatable({leg_output() %>%
                mutate(Link = paste0("<a href='https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:",`CELEX number`,"' target='_blank'>Link</a>"),
                       PDF = paste0("<a href='https://eur-lex.europa.eu/legal-content/EN/TXT/PDF/?uri=CELEX:",`CELEX number`,"' target='_blank'>PDF</a>")) %>% 
                select(Title,`Date of document`,Link,PDF) %>%
                filter(Title!="")},
              escape = FALSE,
              rownames = FALSE,
              extensions = 'Scroller',
              selection = list(mode = "single", target = 'row'),
              options = list(scrollY = 800,
                             paging = FALSE,
                             scrollCollapse = TRUE))
  })

  output$linktitle <- renderText({
    legislation_data()$title
    })
  
  output$weblink <- renderUI({
    legislation_data()$weblink
  })
  
  output$pdflink <- renderUI({
    legislation_data()$pdflink
  })
    
    
  output$linktable <- renderDT({
    req(input$pdftext!=""|input$pdfselect!="")
#    validate(need(!is.na(leg_output()),"No linked references found"))
    datatable(references_output(),
              escape = FALSE,
              rownames = FALSE,
              extensions = 'Scroller',
              selection = "single",
              options = list(scrollY = 800,
                             paging = FALSE,
                             scrollCollapse = TRUE))
  })
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
