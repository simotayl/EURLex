library(pdftools)
library(stringr)
library(dplyr)
library(readr)
library(DT)

find_references <- function(legislation_text,EU_lex,type,index,string_pattern){
  #This function takes text read from a pdf (a list of strings, one for each page of the pdf) along with a
  #string matching REGEX patter and returns the corresponding table of identified references. The function
  #does the string matching, and then passes the results to a subfunction reference_table to generate the output.
  
  #We first identify all matching strings using str_extract_all, which returns a vector of all the matches.
  #The sapply wrapper iterates through the list, returning a list of vectors. 
  match_base <- sapply(legislation_text,function(text) {
    #first remove all spaces from the code (this is done because old scanned pdfs have unreliable spacing)
    text_condensed <- str_replace_all(text," ","")
    #Then run the match against the specified REGEX.
    str_match <- str_extract_all(text_condensed, pattern = string_pattern)
    #We then eliminate everythig that isn't a digit, a dash or the letter N (latter explained below)
    str_match <- unlist(sapply(str_match,str_replace_all,pattern = "[^0-9N/]",replacement = ""))
  })
  
  #Legislation references are typically either xxxx/year or No. year/xxxx. Because the year is the other way
  #around, we split these cases based on the leading letter N.
  
  #finds all instances not preceeded by 'No' (or just 'N' given the replace above)
  match_simple <- sapply(match_base,function(text){
    #Pick up all matches without an N at the start
    str_match <- text[!startsWith(text,"N")]
    #dump any redundant text left at the end
    str_match <- sapply(str_match, word)
  })
  
  #finds all other instances
  match_no <- sapply(match_base,function(text){
    #Pick up those with the N at the start
    str_match <- text[startsWith(text,"N")]
    #Remove the N and any other redundant text.
    str_match <- sapply(str_match, function(string) word(unlist(str_replace_all(string,"N",""))))
  })
  
  #send both versions of data to the table function below and return combined result
  return(bind_rows(
    #Note the optional reverse variable - this is to account for the year/ref being swapped
      reference_table(EU_lex,match_simple,type,index),
      reference_table(EU_lex,match_no,type,index,reverse = TRUE)
  ))
}

reference_table <- function(EU_lex,matched_codes,type,index,reverse = FALSE){
  #this generates an output table from matched code strings.
  
  #the input is a list of vectors of all the string matches (each list element is a page of the source)
  
  code <- na.omit(unique(unlist(matched_codes))) #generate a single list of all the codes found
  code <- code[str_length(code)<10] #remove erroneous long codes
  
  pages <- sapply(code,function(x) paste(grep(x,matched_codes),collapse = ", ")) #this creates a comma separated list of pages with matches
  
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
  #If codes were found, generate the table and links
  if(length(code)>0){
    #create a tibble
    directive_table <- tibble(Category = type, Pages = pages, `CELEX number` = CELEX, code = code) %>%
      #join the active legislation csv data
      left_join(EU_lex,by = "CELEX number") %>%
      mutate(
        MatchFlag = ifelse(is.na(`Publication Reference`),0,1), #was there a match to active legislation?
        Title = ifelse(is.na(Title),paste("<i>",Category,code,"(amended/not in force)</i>"),Title), #Use the title from the data, or a default if not found
        Link = paste0("<a href='https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:",`CELEX number`,"' target='_blank'>Link</a>"), #hyperlinks
        PDF = paste0("<a href='https://eur-lex.europa.eu/legal-content/EN/TXT/PDF/?uri=CELEX:",`CELEX number`,"' target='_blank'>PDF</a>")) %>%
      select(Title,Category,Pages,`CELEX number`,Link,PDF,MatchFlag) #select what we need
  }
  else{directive_table <- tibble(NA)} #this ensures subsequent bind_rows will not have any problems
}

