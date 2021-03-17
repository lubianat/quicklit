library(stringr)
library(httr)
library(WikidataQueryServiceR)

#' @param query The type of query, one of c("covid", "covid_brazil", "bioinfo_brazil")
prepare_dataset_for_page <- function(query="covid"){
  
  if (query == "covid"){
    articles_df <- get_covid_df() 
  } else if (query == "covid_brazil"){
    articles_df <- get_covid_brazil_df()
  }  else if (query == "bioinfo_brazil"){
    articles_df <- get_brazil_bioinformatics_df()
  }

  
  qids <- c()
  links <- c()
  for (u in articles_df[["item"]])
  {
    qid <- str_replace(u, "http://www.wikidata.org/entity/", "")
    link <- paste0('<a target="_blank" href=', u, ">", qid, "</a>")
    links <- c(links, link)
    qids <- c(qids,qid)
  }
  articles_df[["item"]] <- links
  articles_df[["qid"]] <- qids
  
  links <- c()
  for (qid in articles_df[["qid"]])
  {
    ref = paste0("https://tabernacle.toolforge.org/?#/tab/manual/", qid, "/P921%3BP4510")
    link <- paste0('<a target="_blank" href=', ref, ">", "Tabernacle", "</a>")
    links <- c(links, link)
  }
  articles_df[["Tabernacle"]] <- links
  
  links <- c()
  for (qid in articles_df[["qid"]])
  {
    ref = paste0("https://author-disambiguator.toolforge.org/work_item_oauth.php?id=", qid, "&batch_id=&match=1&author_list_id=&doit=Get+author+links+for+work")
    link <- paste0('<a target="_blank" href=', ref, ">", "Author Disambiguator", "</a>")
    links <- c(links, link)
  }
  articles_df[["Author Disambiguator"]] <- links
  
  links <- c()
  for (qid in articles_df[["qid"]])
  {
    ref = paste0("https://scholia.toolforge.org/work/", qid)
    link <- paste0('<a target="_blank" href=', ref, ">", "Scholia", "</a>")
    links <- c(links, link)
  }
  articles_df[["Scholia"]] <- links
  
  return(articles_df)
}


get_brazil_bioinformatics_df <- function(limit=6){
  magic_number = round(runif(1, min=1, max=3000))
  query = paste0('
SELECT DISTINCT ?item ?label WHERE { 
  ?item wdt:P50 ?author.
  {?author wdt:P108 | wdt:P1416 ?institution.
  ?institution wdt:P17 wd:Q155.
  ?institution wdt:P101 wd:Q128570.}
  UNION 
  {?author wdt:P108 | wdt:P1416 ?institution.
  ?institution wdt:P17 wd:Q155.
  ?author wdt:P101 wd:Q128570.}
  ?item rdfs:label ?label.
  ?item wdt:P2093 ?author_name_string.
  FILTER (lang(?label)="en")
} 
ORDER BY ?item OFFSET ', magic_number,' LIMIT ', as.character(limit), '
')
  articles_df <- query_wikidata(query)
  
  return(articles_df)
  
}



get_covid_brazil_df <-function(limit=6){
  magic_number = round(runif(1, min=1, max=300))
  query = paste0('
SELECT DISTINCT ?item ?label WHERE { 
  ?item wdt:P921 wd:Q84263196.
  ?item wdt:P50 ?author.
  ?author wdt:P108 | wdt:P1416 ?institution.
  ?institution wdt:P17 wd:Q155.
  ?item rdfs:label ?label.
  ?item wdt:P2093 ?author_name_string.
  FILTER (lang(?label)="en")
} 
ORDER BY ?item OFFSET ', magic_number,' LIMIT ', as.character(limit), '
')
  articles_df <- query_wikidata(query)
  
  return(articles_df)
}

get_covid_df <-function(limit=6){
  magic_number = round(runif(1, min=1, max=10000))
  query = paste0('
SELECT DISTINCT ?item ?label WHERE { 
  ?item wdt:P921 wd:Q84263196.
  ?item rdfs:label ?label.
  ?item wdt:P2093 ?author_name_string.
  FILTER (lang(?label)="en")
} 
ORDER BY ?item OFFSET ', magic_number,' LIMIT ', as.character(limit), '
')
  articles_df <- query_wikidata(query)
 
  return(articles_df)
}

