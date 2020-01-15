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

leg_raw <- read_csv("Data/EULex-agri.csv")

leg_raw$Title <- iconv(leg_raw$Title,"latin1", "UTF-8", sub="") #Deletes unreadable unicode

#convert comma separated directory search terms into vectors within the data frame.
leg_data <- leg_raw %>%
  mutate(`Subject matter` = strsplit(as.character(`Subject matter`),", "),
         `EUROVOC descriptor` = strsplit(as.character(`EUROVOC descriptor`),", "),
         leg_year = year(as.Date(`Date of document`,format = "%d/%m/%Y")))

#These are used in the UI options
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
        below to search for linked legislation. Alternatively you can navigate to the linked legislation tab above and select the
        legislation directly",
        actionButton("searchselected_tab1",
                     "Find linked legislation")
      ),
      
      mainPanel(
        dataTableOutput("searchtable")
      )
    ),
    tabPanel("Linked legislation",
      sidebarPanel(
        "This tab searches through the legislation from the EU legislation database and finds all other legislation referenced within
        the document. You can either select active legislation from the dropdown (you can type words to help your search), enter the pdf
        URL directly if you know it, or use the other tab to search the legislation and use the button provided.",
        hr(),
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
                      value = FALSE),
        hr(),
        "Titles do not show automatically for legislation that is no longer in force or has been superseded by a newer
        version, although the links will still work. To identify the title of any inactive legislation, cilck the row
        in the table and click the button below. Note that amending the tick boxes above will reset the titles.",
        actionButton("findtitle",
                     "Identify legislation title"),
        hr(),
        "If you wish to re-run the search on any legislation in the table, click on the row and use this button.",
        actionButton("searchselected_tab2",
                     "Find linked legislation")

      ),
      mainPanel(
        h1(textOutput("linktitle")),
        hr(),
        h1(uiOutput("weblink")),
        h1(uiOutput("pdflink")),
        h1(uiOutput("legstatus")),
        hr(),
        dataTableOutput("linktable")
      )
    )
  )
)


server <- function(input, output, session) {
  
  values <- reactiveValues() #this is used for replacing titles in the table later.

  leg_output <- reactive({
    #This generates the filtered table in the legislation search tab.
    
    #use either descriptors or subjects. Two variants essentially identical method.
    if(input$filtertype == "descriptor"){
      #The filter compares the list of selected descriptors against the lists in the table using an intersect function
      #If the all matches box is ticked, the intersect should be the same length as the number of supplied search terms.
      #Also requires the number of search terms >0, else everything would be returned.
      if(input$allanyflag == TRUE){
        leg_output <- leg_data %>% filter(sapply(`EUROVOC descriptor`,function(x){length(intersect(input$descriptorselect,unlist(x)))==length(input$descriptorselect)&&length(input$descriptorselect)>0}))
      }
      #If not ticked, only require the length to be greater than zero.
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
    
    #Filter by text string if entered (grepl returns true or false if there is a match). Ignore case used.
    if(input$titlestring !=""){leg_output <- leg_output %>% filter(grepl(input$titlestring,Title,ignore.case = T))}
    
    #Filter by years if required.
    if(input$yearselect != "All"){leg_output <- leg_output %>% filter(leg_year == input$yearselect)}
    
    return(leg_output)
  })
  
  
  
  observeEvent(input$searchselected_tab1,{
    #This sends the selected row in the table to the legislation finder
    req(length(input$searchtable_rows_selected)>0)
    
    #extract the selected CELEX number from the table (the input returns an index)
    CELEX_selected <- leg_output()$`CELEX number`[input$searchtable_rows_selected]
    
    #Generate the URL and send it to the relevant input. This will then trigger the relevant reactives
    pdfURL <- paste0("https://eur-lex.europa.eu/legal-content/EN/TXT/PDF/?uri=CELEX:",CELEX_selected)
    updateTextInput(session,"pdftext",value = pdfURL)
    
    #Switch to the other tab
    updateTabsetPanel(session, "main",
                      selected = "Linked legislation")
    
  })
  
  observeEvent(input$searchselected_tab2,{
    #Identical to the above but run on the other tab with altered input variables
    req(length(input$linktable_rows_selected)>0)
    
    CELEX_selected <- references_output()$`CELEX number`[input$linktable_rows_selected]
    pdfURL <- paste0("https://eur-lex.europa.eu/legal-content/EN/TXT/PDF/?uri=CELEX:",CELEX_selected)
    updateTextInput(session,"pdftext",value = pdfURL)
    #No need to switch tab here as we are already on the right one.
  })

  
  
  observeEvent(input$pdftext,{
    #If a pdf URL is entered, clear the selector
    req(input$pdftext!="")
    updateSelectInput(session,"pdfselect", selected = "")    
  })
  
  observeEvent(input$pdfselect,{
    #If a selector is entered, clear the pdf text field
    req(input$pdfselect!="")
    updateTextInput(session,"pdftext", value = "")
    })  
  
  legislation_data <- reactive({
    #This obtains the relevant data from EURLexfor processing
    req(input$pdftext!=""|input$pdfselect!="")
    
    #Generate URL from dropdown lookup or use text as is, depending on user input
    if(input$pdfselect!=""){
      CELEX_selected <- leg_data$`CELEX number`[which(leg_data$Title == input$pdfselect)]
      pdfURL <- paste0("https://eur-lex.europa.eu/legal-content/EN/TXT/PDF/?uri=CELEX:",CELEX_selected)}
    else{pdfURL <- input$pdftext}
    
    #short cut to generating the HTML webpage version
    htmlURL <- str_replace(pdfURL,"/PDF","")
    
    #Obtain legislation text from the pdf and the legislation title from the webpage.
    #The validate wrapper stops the processing if the URL isn't valid, i.e. isn't a pdf.
    validate(need(try(legislation_text <- pdf_text(pdfURL),silent = TRUE),message = "Could not find URL - please try again"),
             need(try(legislation_html <- read_html(htmlURL),silent = TRUE),message = ""))
                      
    legislation_title <- legislation_html %>%
      html_nodes("#translatedTitle") %>%
      html_text()
    legislation_status <- legislation_html %>%
      html_nodes(".forceIndicator") %>%
      html_text()
    
    #Links for use under the title.
    weblink <- a("EU Legislation webpage", href = htmlURL, target='_blank')
    pdflink <- a("PDF version", href = pdfURL, target='_blank')
    
    #Return as a list to be accessed as required.
    return(list(text = legislation_text,title = legislation_title, status = legislation_status, weblink = weblink, pdflink = pdflink))
  })
  
    
  references_generate <- reactive({
    req(input$pdftext!=""|input$pdfselect!="")
    req(length(legislation_data()$text)>0)
    
    #Get the pdf text from the reactive
    legislation_text <- legislation_data()$text
    
    #Start with a blank copy in case nothing is returned.
    references_found <- tibble(Title = NA,
                                   Category = NA,
                                   Pages = NA,
                                   `CELEX number` = NA,
                                   Link = NA,
                                   PDF = NA,
                                   MatchFlag = 0)
    output <- 
      bind_rows(
        references_found,
        #Send the pdf text to the mining functions. Final string of functions are REGEX that find strings with wildcards as follwos
        find_references(legislation_text,leg_data,"Directive","L","(?<=Directive).*(?=/E)"), #Directive ****/****/E
        find_references(legislation_text,leg_data,"Decision","D","(?<=Decision).*(?=/E)"), #Deciion ****/****/E
        find_references(legislation_text,leg_data,"Decision","D","(?<=Decision\\(E.\\)).*(/....)"), #Decision (E*) ****/****
        find_references(legislation_text,leg_data,"Regulation","R","(?<=Regulation\\(E.\\)).*(/....)") #Regulation (E*) ****/****
      )
    return(output)
  })
  
  references_output <- reactive({
    #This takes the main result and applies filters as required to generate the data to be rendered.
    
    references_table <- references_generate() %>%
      filter(Category %in% input$legtype,
             str_length(`CELEX number` == 11),
             !grepl("0000", `CELEX number`, fixed = TRUE),
             !grepl("NA", `CELEX number`, fixed = TRUE))

    if(input$activeonlyflag == TRUE){
      references_table <- references_table %>%
        filter(MatchFlag == 1)
    }
    references_output <- references_table %>% select(Title,Category,Pages,`CELEX number`,Link,PDF)
    
    #Send a copy to be stored as a reactive - this will be used for replacing titles in the output table
    values$references_output <- references_output
    
    return(references_output)
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
              selection = list(mode = "single", target = 'row'), #need this for the button to search selected
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

  output$legstatus <- renderUI({
    legislation_data()$status
  })
    
  output$linktable <- renderDT({
    req(input$pdftext!=""|input$pdfselect!="")
    datatable(references_output(),
              escape = FALSE,
              rownames = FALSE,
              extensions = 'Scroller',
              selection = list(mode = "single", target = 'row'),
              options = list(scrollY = 800,
                             paging = FALSE,
                             scrollCollapse = TRUE))
  })
  
  #Proxy for updating the table
  proxy <- dataTableProxy("linktable")

  observeEvent(input$findtitle,{
    req(length(input$linktable_rows_selected)>0)
    
    #This code gets the URL - identical to the standard process except the row reference is as selected
    CELEX_selected <- values$references_output$`CELEX number`[input$linktable_rows_selected]
    htmlURL <- paste0("https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:",CELEX_selected)
    validate(need(try(legislation_title <- read_html(htmlURL) %>%
                        html_nodes("#translatedTitle") %>%
                        html_text(),silent = TRUE),message = ""))
    
    #Send new title to the proxy version of the data and re-render the table.
    isolate(values$references_output$`Title`[input$linktable_rows_selected] <- legislation_title)
    replaceData(proxy, values$references_output, resetPaging = FALSE, rownames = FALSE) 
    #To note, rownames = FALSE is needed because otherwise everything is filtered out - seems to be simlar to https://github.com/rstudio/DT/issues/403
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
