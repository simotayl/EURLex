library(pdftools)
library(stringr)
library(dplyr)
library(readr)
library(DT)

# EU_lex <- read.csv("Data/EULex-agri.csv",stringsAsFactors = FALSE)
# EU_lex$Title <- iconv(EU_lex$Title,"latin1", "ASCII", sub="") #Deletes unreadable unicode
# 
# CELEX_test <- "31991L0412"
# 
# legislation_text <- pdf_text(paste0("https://eur-lex.europa.eu/legal-content/EN/TXT/PDF/?uri=CELEX:",CELEX_test))
# 
# leg_title <- pdf_info("https://eur-lex.europa.eu/legal-content/EN/TXT/PDF/?uri=CELEX:32019R0006")$keys$Title

#https://eur-lex.europa.eu/legal-content/EN/TXT/PDF/?uri=CELEX:32019R0006
#https://eur-lex.europa.eu/content/tools/TableOfSectors/types_of_documents_in_eurlex.html


# output <- bind_rows(
#   find_references(legislation_text,EU_lex,"Directive","L","(?<=Directive ).*(?=/E)"),
#   find_references(legislation_text,EU_lex,"Decision","D","(?<=Decision ).*(?=/E)"),
#   find_references(legislation_text,EU_lex,"Regulation","R","(?<=Regulation \\(E.\\) ).*(/....)")
# ) %>% filter(Title!="")

find_references <- function(legislation_text,EU_lex,type,index,string_pattern){
  match_base <- sapply(legislation_text,function(text) {
    str_match <- str_extract_all(string = text, pattern = string_pattern)
    str_match <- unlist(sapply(str_match,function(string) str_replace_all(string,"\\.","")))
  }) #finds all instances of the code.
  
  match_simple <- sapply(match_base,function(text){
    str_match <- text[!startsWith(text,"No")]
    str_match <- sapply(str_match, function(string) word(string,1))
  }) #finds all instances not preceeded by 'No'
  
  match_no <- sapply(match_base,function(text){
    str_match <- text[startsWith(text,"No")]
    str_match <- sapply(str_match, function(string) word(unlist(gsub("No ","",string)),1))
  }) #finds all other instances
  
  bind_rows(
    reference_table(EU_lex,match_simple,type,index),
    reference_table(EU_lex,match_no,type,index,reverse = TRUE)
  )
}

reference_table <- function(EU_lex,matched_codes,type,index,reverse = FALSE){
    
  code <- na.omit(unique(unlist(matched_codes))) #list of all the codes found
  pages <- sapply(code,function(x) paste(grep(x,matched_codes),collapse = ", ")) #this creates a comma separated list of pages with matches
  code <- str_replace_all(code," ","")
  if(reverse == TRUE){
    #optional version to account for two different orentations of year and reference in the codes
    CELEX <-sapply(str_split(code,"/"), #function to generate CELEX IDs from the legislation code
                   function(x) {paste0(3, #all legislation has this code at the start
                                       ifelse(str_length(x[2])==2,paste0("19",x[2]),x[2]), #to correct for 1995 appearing as 95 etc
                                       index, #the letter code - see reference file
                                       strrep("0",4-str_length(x[1])), #adds in leading zeros if needed
                                       x[1]) }) 
    
  }
  else {
    #identical to the above, but with the str split references reversed
    CELEX <-sapply(str_split(code,"/"), #function to generate CELEX IDs from the legislation code
                          function(x) {paste0(3, #all legislation has this code at the start
                                             ifelse(str_length(x[1])==2,paste0("19",x[1]),x[1]), #to correct for 1995 appearing as 95 etc
                                             index, #the letter code - see reference file
                                             strrep("0",4-str_length(x[2])), #adds in leading zeros if needed
                                             x[2]) })
  }
  if(length(code)>0){
    directive_table <- tibble(Category = type,Pages = pages,`CELEX number` = CELEX, code = code) %>%
      left_join(EU_lex,by = c("CELEX number" = "CELEX.number")) %>%
      mutate(
        MatchFlag = ifelse(is.na(Publication.Reference),0,1),
        Title = ifelse(is.na(Title),paste("<i>",Category,code,"(inactive)</i>"),Title),
        Link = paste0("<a href='https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:",`CELEX number`,"' target='_blank'>Link</a>"),
        PDF = paste0("<a href='https://eur-lex.europa.eu/legal-content/EN/TXT/PDF/?uri=CELEX:",`CELEX number`,"' target='_blank'>PDF</a>")) %>%
      select(Title,Category,Pages,`CELEX number`,Link,PDF,MatchFlag)
  }
  else{directive_table <- tibble(NA)} #this ensures bind_rows will not have any problems
}





# string_pattern <- "(?<=Directive ).*(?=/E)" #string search pattern. For Directives it searches for "Directive [Code]/E" and returns [Code]
# matched_codes <- sapply(legislation_text,function(text) {
#   str_extract_all(string = text, pattern = string_pattern)}) #finds all instances of the code. TO IMPROVE
# directives <- reference_table(matched_codes,"Directive","L")


#need to split in same manner as regulations
#string_pattern <- "(?<=Decision ).*(?=/E)" #string search pattern. For Directives it searches for "Directive [Code]/E" and returns [Code]

#decisions <- reference_table(matched_codes,"Decision","D")


#string_pattern <- "(?<=Regulation \\(E.\\) ).*(/....)" #string search pattern. For Directives it searches for "Directive [Code]/E" and returns [Code]



#bind_rows(,decisions,regulations_simple,regulations_no)

#datatable(output,escape = F)

