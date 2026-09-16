library(tidyverse)
library(httr)
library(rvest)
library(ellmer)
library(openxlsx)

# =========================================================================
# 0. AUTOMATIC ANTHROPIC API KEY CONFIGURATION
# =========================================================================
if (Sys.getenv("ANTHROPIC_API_KEY") == "") {
  # Replace "YOUR_API_KEY_HERE" if running locally, otherwise GitHub handles it
  Sys.setenv(ANTHROPIC_API_KEY = "YOUR_API_KEY_HERE")
  message("🔑 Anthropic API Key configured for this local session.")
}

# =========================================================================
# 1. TARGET PLATFORMS CONFIGURATION
# =========================================================================
target_platforms <- c(
  Databricks   = "https://databricks.com",
  Andrewgelman = "https://statmodeling.stat.columbia.edu/",
  Posit        = "https://posit.co",
  R_Bloggers   = "https://feedburner.com",
  Scott_Mixtape_Substack = "https://causalinf.substack.com/",
  Ahead_of_AI= "https://magazine.sebastianraschka.com/",
  deeplearning_ai= "https://www.deeplearning.ai/the-batch",
  TheSequence= "https://thesequence.substack.com/",
  Import_AI_JackClark= "https://importai.substack.com/", 
  Zeyi_Yang_MIT_Technology_Review= "https://www.technologyreview.com/", 

  # Statistiques et Machine Learning
  Christian_Robert                = "https://xianblog.wordpress.com/category/statistics/",
  Error_Statistics_Philosophy     = "https://errorstatistics.com/",
  Notes_From_a_Data_Witch         = "https://blog.djnavarro.net",
  Observational_Epidemiology      = "https://observationalepidemiology.blogspot.com/",
  R_Bloggers                      = "https://www.r-bloggers.com/",
  Sharon_Lohr                     = "https://www.sharonlohr.com/blog",
  Statistical_Methodology_Meanderings = "https://tpmorris.substack.com",
  Ryan_Giordano                   = "https://rgiordan.github.io/blog.html",
  Statistical_Thinking            = "https://www.fharrell.com/#posts",
  The_Endeavour                   = "https://www.johndcook.com/blog/",
  Thomas_Lumley                   = "https://notstatschat.rbind.io/",
  
  # Visualisation
  Junk_Charts                     = "https://www.junkcharts.com",
  Kieran_Healy                    = "https://kieranhealy.org/",
  Road_to_Larissa                 = "https://roadtolarissa.com",
  
  # Sciences sociales et politiques
  Book_and_Sword                  = "https://www.bookandsword.com",
  Department_of_Data              = "https://www.washingtonpost.com/people/andrew-van-dam/",
  Family_Inequality                = "https://familyinequality.wordpress.com/",
  Gojiberries                     = "https://www.gojiberries.io",
  Imperfect_Information            = "https://rajivsethi.substack.com",
  Inequality_by_Interior_Design    = "https://inequalitybyinteriordesign.wordpress.com",
  Just_the_Social_Facts_Maam      = "https://justthesocialfacts.blogspot.fr/",
  Made_in_America                 = "https://madeinamericathebook.wordpress.com/",
  Marginal_Revolution              = "https://www.marginalrevolution.com/",
  Monthly_Labor_Review             = "https://www.bls.gov/opub/mlr/home.htm",
  Strength_in_Numbers               = "https://gelliottmorris.substack.com",
  Urban_Institute_blog             = "https://www.urban.org/urban-wire",
  
  # Sciences cognitives et comportementales
  Data_Colada                     = "https://datacolada.org/",
  Dorothy_Bishop                  = "https://deevybee.blogspot.com/",
  Inframethodology                 = "https://inframethodology.cbs.dk/?page_id=7555",
  Judgment_Misguided               = "https://judgmentmisguided.blogspot.com",
  Language_Log                    = "https://languagelog.ldc.upenn.edu/nll/",
  
  # Sciences generales et ingenierie
  Media_404                       = "https://www.404media.co",
  Azimuth                          = "https://johncarlosbaez.wordpress.com",
  Dan_Luu                          = "https://danluu.com",
  Idle_Words                       = "https://idlewords.com",
  James_Heathers                  = "https://jamesclaims.substack.com",
  Lucidity                          = "https://ludic.mataroa.blog",
  Marcelo_Rinesi                  = "https://blog.rinesi.com",
  Nick_Brown                      = "https://steamtraen.blogspot.com/",
  Technically_Food                 = "https://technicallyfood.substack.com",
  Retraction_Watch                 = "https://retractionwatch.com/",
  The_Eighteenth_Elephant          = "https://eighteenthelephant.com",
  # Sport
  Defector                         = "https://defector.com/category/chess/",
  Exploring_Baseball_Data_with_R  = "https://baseballwithr.wordpress.com/",
  # Culturel
  Alec_Nevala_Lee                  = "https://nevalalee.wordpress.com",
  Alexandras_Kitchen               = "https://alexandracooks.com",
  Do_You_Write_Under_Your_Own_Name = "https://doyouwriteunderyourownname.blogspot.com",
  Literambivalence                  = "https://jrobertlennon.com/literambivalence",
  MPorcius_Fiction_Log              = "https://mporcius.blogspot.com/",
  Namerology                        = "https://namerology.com/category/articles/",
  News_From_Me                      = "https://www.newsfromme.com/",
  Plagiarism_Today                  = "https://www.plagiarismtoday.com/",
  PostSecret                        = "https://postsecret.com/",
  Psychobabble                      = "https://psychobabble200.blogspot.com",
  Quote_Investigator                = "https://quoteinvestigator.com/",
  Rebecca_Makkai                    = "https://rebeccamakkai.substack.com",
  Stuff_Ive_Been_Reading            = "https://www.thebeliever.net/type/stuff-ive-been-reading/",
  The_Dizzies                        = "https://thedizzies.substack.com/",
  The_Neglected_Books_Page          = "https://neglectedbooks.com",
  The_Amateur                        = "https://cecilycarver.substack.com",
  The_Patron_Saint_of_Superheroes   = "https://thepatronsaintofsuperheroes.wordpress.com/",
  Hugging_Face = "https://huggingface.co"
)

custom_user_agent <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36")

# =========================================================================
# 2. TARGETED WEB SCRAPING ROUTINE (Blogs & Product Spotlights)
# =========================================================================
extract_platform_content <- function(url, name) {
  message("📡 Targeted scraping initiated for: ", name, "...")
  
  tryCatch({
    response <- GET(url, custom_user_agent, config(followlocation = TRUE), timeout(10))
    raw_html <- content(response, as = "text", encoding = "UTF-8")
    page <- read_html(raw_html)
    
    # Selecting the best CSS nodes per platform to filter out navbars/footers
    css_selector <- switch(name,
                           # --- Existantes ---
                           "Databricks"   = ".in-the-spotlight, [class*='featured'], article, h4",
                           "Posit"        = "[class*='customer-story'], [class*='resource'], article, h3",
                           "R_Bloggers"   = ".feed-item, item, entry, [class*='post']",
                           "Hugging_Face" = "[class*='trending'], h4, article, [class*='model']",
                           
                           # --- IA / Actualite ---
                           "Scott_Mixtape_Substack"        = "[class*='post-preview'], article, h3, [class*='post']",
                           "Ahead_of_AI"                   = "[class*='post-preview'], article, h3, [class*='post']",
                           "deeplearning_ai"               = "article, h2, h3, [class*='title'], [class*='card']",
                           "TheSequence"                   = "[class*='post-preview'], article, h3, [class*='post']",
                           "Import_AI_JackClark"           = "[class*='post-preview'], article, h3, [class*='post']",
                           "Zeyi_Yang_MIT_Technology_Review" = "article, h2, h3, [class*='title'], [class*='card']",
                           
                           # --- Statistiques et Machine Learning ---
                           "Christian_Robert"              = "article, h2.entry-title, .entry-title, h2, .post",
                           "Error_Statistics_Philosophy"   = "article, h2.entry-title, .entry-title, h2, .post",
                           "Notes_From_a_Data_Witch"       = "article, h2, h3, li a",
                           "Observational_Epidemiology"    = ".post, h3.post-title, [class*='post-title'], article",
                           "Sharon_Lohr"                   = "article, h2.entry-title, .entry-title, h2, .post",
                           "Statistical_Methodology_Meanderings" = "[class*='post-preview'], article, h3, [class*='post']",
                           "Ryan_Giordano"                 = "article, h2, h3, li a",
                           "Statistical_Thinking"          = "article, h2, h3, li a",
                           "The_Endeavour"                 = "article, h2.entry-title, .entry-title, h2, .post",
                           "Thomas_Lumley"                 = "article, h2, h3, li a",
                           
                           # --- Visualisation ---
                           "Junk_Charts"                   = "article, h2, h3, [class*='title'], [class*='card']",
                           "Kieran_Healy"                  = "article, h2, h3, li a",
                           "Road_to_Larissa"               = "article, h2, h3, li a",
                           
                           # --- Sciences sociales et politiques ---
                           "Book_and_Sword"                = "article, h2.entry-title, .entry-title, h2, .post",
                           "Department_of_Data"            = "article, h2, h3, [class*='title'], [class*='card']",
                           "Family_Inequality"             = "article, h2.entry-title, .entry-title, h2, .post",
                           "Gojiberries"                   = "article, h2.entry-title, .entry-title, h2, .post",
                           "Imperfect_Information"         = "[class*='post-preview'], article, h3, [class*='post']",
                           "Inequality_by_Interior_Design" = "article, h2.entry-title, .entry-title, h2, .post",
                           "Just_the_Social_Facts_Maam"    = ".post, h3.post-title, [class*='post-title'], article",
                           "Made_in_America"               = "article, h2.entry-title, .entry-title, h2, .post",
                           "Marginal_Revolution"           = "article, h2, h3, [class*='title'], [class*='card']",
                           "Monthly_Labor_Review"          = "article, h2, h3, [class*='title'], [class*='card']",
                           "Strength_in_Numbers"           = "[class*='post-preview'], article, h3, [class*='post']",
                           "Urban_Institute_blog"          = "article, h2, h3, [class*='title'], [class*='card']",
                           
                           # --- Sciences cognitives et comportementales ---
                           "Data_Colada"                   = "article, h2.entry-title, .entry-title, h2, .post",
                           "Dorothy_Bishop"                = ".post, h3.post-title, [class*='post-title'], article",
                           "Inframethodology"              = "article, h2.entry-title, .entry-title, h2, .post",
                           "Judgment_Misguided"            = ".post, h3.post-title, [class*='post-title'], article",
                           "Language_Log"                  = "article, h2, h3, li a",
                           
                           # --- Sciences generales et ingenierie ---
                           "Media_404"                     = "article, h2, h3, [class*='title'], [class*='card']",
                           "Azimuth"                       = "article, h2.entry-title, .entry-title, h2, .post",
                           "Dan_Luu"                       = "article, h2, h3, li a",
                           "Idle_Words"                    = "article, h2, h3, li a",
                           "James_Heathers"                = "[class*='post-preview'], article, h3, [class*='post']",
                           "Lucidity"                      = "article, h2, h3, li a",
                           "Marcelo_Rinesi"                = "article, h2.entry-title, .entry-title, h2, .post",
                           "Nick_Brown"                    = ".post, h3.post-title, [class*='post-title'], article",
                           "Technically_Food"              = "[class*='post-preview'], article, h3, [class*='post']",
                           "Retraction_Watch"              = "article, h2.entry-title, .entry-title, h2, .post",
                           "The_Eighteenth_Elephant"       = "article, h2.entry-title, .entry-title, h2, .post",
                           
                           # --- Sport ---
                           "Defector"                      = "article, h2, h3, [class*='title'], [class*='card']",
                           "Exploring_Baseball_Data_with_R" = "article, h2.entry-title, .entry-title, h2, .post",
                           
                           # --- Culturel ---
                           "Alec_Nevala_Lee"               = "article, h2.entry-title, .entry-title, h2, .post",
                           "Alexandras_Kitchen"            = "article, h2.entry-title, .entry-title, h2, .post",
                           "Do_You_Write_Under_Your_Own_Name" = ".post, h3.post-title, [class*='post-title'], article",
                           "Literambivalence"              = "article, h2.entry-title, .entry-title, h2, .post",
                           "MPorcius_Fiction_Log"          = ".post, h3.post-title, [class*='post-title'], article",
                           "Namerology"                    = "article, h2.entry-title, .entry-title, h2, .post",
                           "News_From_Me"                  = "article, h2, h3, li a",
                           "Plagiarism_Today"              = "article, h2.entry-title, .entry-title, h2, .post",
                           "PostSecret"                    = "article, h2, h3, [class*='title'], [class*='card']",
                           "Psychobabble"                  = ".post, h3.post-title, [class*='post-title'], article",
                           "Quote_Investigator"            = "article, h2.entry-title, .entry-title, h2, .post",
                           "Rebecca_Makkai"                = "[class*='post-preview'], article, h3, [class*='post']",
                           "Stuff_Ive_Been_Reading"        = "article, h2, h3, [class*='title'], [class*='card']",
                           "The_Dizzies"                   = "[class*='post-preview'], article, h3, [class*='post']",
                           "The_Neglected_Books_Page"      = "article, h2.entry-title, .entry-title, h2, .post",
                           "The_Amateur"                   = "[class*='post-preview'], article, h3, [class*='post']"
    )
    
    
    filtered_nodes <- page %>% html_nodes(css_selector)
    
    # Fallback to headings if the target CSS structure changes
    if (length(filtered_nodes) == 0) {
      extracted_text <- page %>% html_nodes("h2, h3") %>% html_text(trim = TRUE) %>% paste(collapse = " | ")
    } else {
      extracted_text <- filtered_nodes %>% html_text(trim = TRUE) %>% paste(collapse = " | ")
    }
    return(extracted_text)
    
  }, error = function(e) { 
    return(paste("Platform connection error:", e$message)) 
  })
}

scraped_payloads <- purrr::imap_chr(target_platforms, extract_platform_content)

# =========================================================================
# 3. DATASET PREPARATION & EXCEL ARCHIVING
# =========================================================================
# 1. Format current week's data
current_batch <- tibble(
  Source         = names(target_platforms),
  Collection_Tag = paste("Latest Content -", names(target_platforms)),
  Scrape_Date    = as.character(Sys.Date()),
  Raw_Summary    = scraped_payloads,
  Source_URL     = unname(target_platforms)
) %>%
  mutate(Raw_Summary = map_chr(Raw_Summary, function(x) {
    str_replace_all(x, "<[^>]*>", " ") %>% str_replace_all("\\s+", " ") %>% str_trim() %>% str_trunc(500)
  }))

# 2. Append to historical Excel archive
excel_file <- "tech_pulse_history.xlsx"

if (file.exists(excel_file)) {
  historical_data <- read.xlsx(excel_file)
  updated_history <- bind_rows(historical_data, current_batch)
  message("📈 Existing historical Excel file found. Appending new records.")
} else {
  updated_history <- current_batch
  message("🆕 No historical file found. Initializing a new Excel archive.")
}

# 3. Save Excel spreadsheet with clean formatting
write.xlsx(updated_history, excel_file, overwrite = TRUE, zoom = 100, firstRow = TRUE)

# =========================================================================
# 4. FLAT XML INJECTION STRUCTURE FOR THE LLM
# =========================================================================
xml_articles <- current_batch %>%
  purrr::pmap_chr(function(Source, Collection_Tag, Scrape_Date, Raw_Summary, Source_URL) {
    paste0("<article>\n  <source>", Source, "</source>\n  <title>", Collection_Tag, "</title>\n  <date>", Scrape_Date, "</date>\n  <summary>", Raw_Summary, "</summary>\n  <url>", Source_URL, "</url>\n</article>")
  }) %>% paste(collapse = "\n\n")

xml_reference_links <- "<reference_links>\n  <databricks>https://databricks.com</databricks>\n  <posit>https://posit.co</posit>\n  <r_bloggers>https://feedburner.com</r_bloggers>\n  <hugging_face>https://huggingface.co</hugging_face>\n</reference_links>"

final_xml_payload <- paste(xml_articles, xml_reference_links, sep = "\n\n")

# =========================================================================
# 5. ANTHROPIC CLAUDE API DEPLOYMENT (Engaging Blog Output Style)
# =========================================================================
master_prompt <- "
You are acting as a Lead Research Engineer in Data Science and an elite MLOps Architect. Your objective is to transform raw weekly scraped texts into a highly engaging, professional, and friendly corporate tech newsletter/blog post for the engineering team.

Structure the report using the following layout:
1. **Weekly Editorial Brief**: A macro-level, hype-inducing, yet accurate summary of major industry trends noticed this week.
2. **Deep Dive Technical Focus**: A granular breakdown of the single most disruptive or helpful update extracted from the data.
3. **Platform Roundup**: Write an analytical, punchy, and newsletter-style paragraph for each platform (Databricks, Posit, R-Bloggers, Hugging Face).

*CRITICAL HYPERLINK BOUNDARY RULE*
You must contextually wrap the URLs from the provided <reference_links> block directly inside your analytical sentences as Markdown anchors (e.g., 'Check out the recent open-source model updates over at [Hugging Face](https://huggingface.co)...'). Do not list these links in a detached resource list at the bottom. They must flow naturally within your technical evaluation.
"

message("🧠 Transmitting curated web data to Claude for synthesis...")
chat <- chat_anthropic(model = "claude-3-7-sonnet-20250219", system = master_prompt)
analytical_report <- chat$sendMessage(paste0("<raw_scraped_data>\n", final_xml_payload, "\n</raw_scraped_data>"))

# Save the final analysis locally as a Markdown file
report_filename <- paste0("Tech_Pulse_", Sys.Date(), ".md")
writeLines(analytical_report, con = report_filename)

# =========================================================================
# 6. INSTANT CHANNEL NOTIFICATION (Slack / Teams Webhook)
# =========================================================================
webhook_url <- Sys.getenv("URL_WEBHOOK")
if (webhook_url != "") {
  httr::POST(
    url = webhook_url,
    body = list(text = paste0("📢 *New Weekly Tech Pulse Available!* (", Sys.Date(), ")\n\n", stringr::str_trunc(analytical_report, 800), "\n\n💾 Automated files updated on GitHub: `", report_filename, "` and `", excel_file, "`")),
    encode = "json"
  )
}
