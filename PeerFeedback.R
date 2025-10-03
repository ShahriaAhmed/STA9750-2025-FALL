mp_feedback_locate <- function(N, github_id){
  library(rvest)
  library(glue)
  library(tidyverse)
  library(httr2)
  library(gh)
  
  if(missing(N)){
    N <- menu(title="Which Mini-Project's Peer Feedback cycle is it currently?", 
              choices=c(0, 1, 2, 3, 4))
  }
  
  if(missing(github_id)){
    github_id <- readline("What is your GitHub ID? ")
  }
  
  page <- 1
  issues <- list()
  
  while((length(issues) %% 100) == 0){
    new_issues <- gh("/repos/michaelweylandt/{repo}/issues?state=all&per_page=100&page={page}", 
                     repo=course_repo, 
                     page=page)
    
    issues <- c(issues, new_issues)
    page <- page + 1
  }
  
  pf_urls <- issues |> 
    keep(~ str_detect(.x$title, glue("#0{N}"))) |>
    keep(~ !str_equal(.x$user$login, github_id, ignore_case=TRUE)) |>
    map(~data.frame(html=.x$html_url, comments=.x$comments_url)) |>
    keep(~any(str_detect(map_chr(gh(.x$comments), "body"), github_id))) |>
    map("html") |>
    list_c()
  
  cat(glue("I have found several MP#0{N} issues that may be assigned to you.\n"),
      "Please review the following:\n")
  for(pf in pf_urls){
    cat(" - ", pf, "\n")
  }
  
  to_browser <- menu(title="Would you like me to open these in your web browser?", 
                     choices=c("Yes", "No"))
  
  if(to_browser == 1){
    for(pf in pf_urls){
      browseURL(pf)
    }
  }
  invisible(TRUE)
}




mp_feedback_submit <- function(N, peer_id){
  library(rvest)
  library(glue)
  library(tidyverse)
  library(httr2)
  library(gh)
  library(clipr)
  
  if(missing(N)){
    N <- menu(title="Which Mini-Project's Peer Feedback would you like to check was properly submitted on GitHub?", 
              choices=c(0, 1, 2, 3, 4))
  }
  
  if(missing(peer_id)){
    peer_id <- readline("What is your Peer's GitHub ID? ")
  }
  
  title <- glue("{course_short} {peer_id} MiniProject #0{N}")
  
  page <- 1
  
  issues <- list()
  
  while((length(issues) %% 100) == 0){
    new_issues <- gh("/repos/michaelweylandt/{repo}/issues?state=all&per_page=100&page={page}", 
                     repo=course_repo, 
                     page=page)
    
    issues <- c(issues, new_issues)
    page <- page + 1
  }
  
  issue_names <- vapply(issues, function(x) str_squish(x$title), "")
  
  name_match <- which(issue_names == title)
  
  if(length(name_match) != 1){
    cat("I could not find a unique issue with the title:\n", 
        "    ", sQuote(title),"\n",
        "I'm afraid I can't verify whether the peer feedback was submitted properly\n", 
        "but the issue likely lies with the submittor, not with your feedback.")
    stop("PEER FEEDBACK NOT VERIFIED.")
  } 
  
  issue <- issues[[name_match]]
  
  issue_url <- issue$html_url
  
  template_url <- "https://michael-weylandt.com/STA9750/miniprojects.html"
  
  template_text <- read_html(template_url) |> 
    html_element("#peer-feedback-template") |> 
    html_text() |>
    str_trim() 
  
  template_categories <- c(
    "Written Communication", 
    "Project Skeleton", 
    "Formatting & Display", 
    "Code Quality", 
    "Data Preparation", 
    "Extra Credit"
  )
  
  for(category in template_categories){
    has_score <- FALSE
    
    while(!has_score){
      score <- readline(glue("On a scale of 0 to 10, how many points does {peer_id} deserve for {category}? "))
      
      score <- as.integer(score)
      
      if(!is.na(score) & (score >= 0) & (score <= 10)){
        has_score <- TRUE
      }
    }
    
    has_score <- FALSE
    
    text <- readline(glue("Why did you give {peer_id} {score} points for {category}? "))
    
    template_text <- template_text |>
      str_replace("NN", as.character(score)) |>
      str_replace("TEXT TEXT TEXT", as.character(text))
  }
  
  write_clip(template_text)
  
  readline(paste0(
    "After you hit Enter / Return, you will be taken to a GitHub page.\n",
    "Paste from the clipboard into the comment box at the bottom to\n",
    "pre-populate your peer feedback. Then hit 'Comment' to finish your\n",
    "submission. "))
  
  browseURL(issue_url)
  
  cat(glue("You can now use `mp_feedback_verify({N}, peer_id={peer_id})`\n",
           "to confirm your submission was properly formatted.\n"))
}