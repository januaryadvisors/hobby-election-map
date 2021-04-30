library(textreadr)
library(here)
library(tidyverse)
library(janitor)

text

"Harris County State Rep District 126.doc"

text <- read_doc(here::here("data", "doc_data", paste0("Harris County State Rep District ", 146, ".doc")))%>% 
  str_replace_all(., "Pct\\.", "Pct") %>% 
  str_replace_all(., "Pct   ", "Pct ") %>%
  str_replace_all(., "Pct  ", "Pct ") %>%
  str_replace_all(., "2020E", "2020")
text

clean_doc <- function(.x) {
  
  text <- read_doc(here::here("data", "doc_data", paste0("Harris County State Rep District ", .x, ".doc"))) %>% 
    str_replace_all(., "Pct\\.", "Pct") %>%
    str_replace_all(., "Pct   ", "Pct ") %>%
    str_replace_all(., "Pct  ", "Pct ") %>%
    str_replace_all(., "2020E", "2020")
  
  no_voters1 <- str_extract_all(text, "\\d+(?= – NO Voters)")[[1]]
  no_voters2 <- str_extract_all(text, "\\d+(?= – No registered)")[[1]]
  no_voters3 <- str_extract_all(text, "\\d+(?= – No reg.)")[[1]]
  no_voters4 <- str_extract_all(text, "\\d+(?=   NO REGISTERED)")[[1]]
  no_voters5 <- str_extract_all(text, "\\d+(?=      NOT IN)")[[1]]
  no_voters6 <- str_extract_all(text, "\\d+(?=     No reg)")[[1]]
  no_voters7 <- str_extract_all(text, "\\d+(?= – No Registered)")[[1]]
  no_voters8 <- str_extract_all(text, "\\d+(?=     \\(1 Registered)")[[1]]
  no_voters9 <- str_extract_all(text, "\\d+(?= – 1 Republican)")[[1]]

  precinct <- str_extract_all(text, "(?<=Pct )\\d+")[[1]] %>% 
    as.data.frame() %>% 
    dplyr::rename("precinct" = ".") %>% 
    filter(!(precinct %in% c(no_voters1, no_voters2, no_voters3,
                             no_voters4, no_voters5, no_voters6, 
                             no_voters7, no_voters8, no_voters9
                             ))) %>% 
    mutate(row_id = row_number())
  
  data <- str_split(text, "\n")[[1]] %>% as.data.frame() %>% 
    clean_names() %>% 
    filter(str_detect(x, "2020")) %>% 
    filter(!str_detect(x, "State Representative")) %>% 
    filter(!str_detect(x, "Summary")) %>% 
    filter(!str_detect(x, ",")) %>% 
    filter(str_detect(x, "^2020")) %>% 
    filter(!str_detect(x, "2020$")) %>%
    filter(!str_detect(x, "2020\\)")) %>%
    filter(!str_detect(x, "Projections")) %>% 
    mutate(
      x = str_remove(x, "2020 "),
      x = str_trim(x),
      row_id = row_number(),
      xx = str_replace_all(x, " ", "_"),
      xx = str_replace_all(xx, "________", "_"),
      xx = str_replace_all(xx, "_______", "_"),
      xx = str_replace_all(xx, "______", "_"),
      xx = str_replace_all(xx, "_____", "_"),
      xx = str_replace_all(xx, "____", "_"),
      xx = str_replace_all(xx, "___", "_"),
      xx = str_replace_all(xx, "__", "_"),
      xx = str_replace_all(xx, "2020_", "")
    ) %>% 
    separate(xx, into = c("x1", "x2", "x3", "x4", "x5", "x6"), sep="_") %>% 
    left_join(., precinct) %>% 
    mutate(District = .x, Year=2020) %>% 
    dplyr::select(Year, District, "Pct"=precinct, "DB_Dem"=x1, "DB_Rep"=x2, 
                  "Sen/Pre_Dem"=x4, "Sen/Pre_Rep"=x5) %>% 
    ungroup() %>% 
    filter(!is.na(Pct))
  
  return(data)
}

test <- clean_doc(133)

docs <- lapply(c(126:135, 137:150), clean_doc)
doc_data <- do.call(rbind, docs) %>% 
  mutate(
    `Sen/Pre_Dem` = ifelse(District==144 & Pct=="346", 0, `Sen/Pre_Dem`),
    `Sen/Pre_Rep` = ifelse(District==132 & Pct=="901", 0, `Sen/Pre_Rep`),
    across(DB_Dem:`Sen/Pre_Rep`, as.numeric)
  ) %>% 
  group_by(Year, Pct) %>% 
  summarise(
    District = first(District),
    across(DB_Dem:`Sen/Pre_Rep`, sum, na.rm=T)
  ) %>% 
  ungroup()
  
saveRDS(doc_data, here::here("data", "data-2020-clean.rds"))

