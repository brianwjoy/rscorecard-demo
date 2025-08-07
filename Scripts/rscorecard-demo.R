
# Load Packages

library(tidyverse)
library(rscorecard)



# Store API Key in R Environment

## use your real key in place of the Xs
sc_key('xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx')

# Explore Variables


sc_dict("MD_EARN_WNE_P10", search_col = "varname")

sc_dict("EARN_MDN_4YR", search_col = "varname")

# Show institution level data

df_inst <- sc_init() %>%
  sc_filter(STABBR == "VT", CONTROL %in% c(1:2)) %>%
  sc_select(unitid,
            instnm,
            stabbr,
            control,
            md_earn_wne_p10,
            count_wne_p10) %>%
  sc_year("latest") %>% 
  sc_get()

# Quick Scatterplot

df_inst %>%
  filter(!is.na(count_wne_p10),
         !is.na(md_earn_wne_p10)) %>% 
  mutate(control = factor(control, labels = c("Public", "Private"))) %>%
  ggplot(aes(x = count_wne_p10, y = md_earn_wne_p10, color = control)) +
  geom_point() +
  geom_text(aes(label = instnm))


# Show program level data

df_program <- sc_init() %>%
  sc_filter(STABBR == "CA", 
            CONTROL %in% c(1:2),
            CIPCODE =="5201", # Restrict to Business/Commerce
            credlev == "3") %>% # Restrict to Bachelors level programs
  sc_select(unitid,
            instnm,
            stabbr,
            cipcode,
            credlev,
            control,
            earn_mdn_4yr,
            earn_count_wne_4yr) %>%
  sc_year("latest") %>% 
  sc_get()

# Quick Scatterplot

df_program %>%
  filter(!is.na(earn_mdn_4yr),
         !is.na(earn_count_wne_4yr)) %>% 
  mutate(control = factor(control, labels = c("Public", "Private"))) %>%
  ggplot(aes(x = earn_count_wne_4yr, y = earn_mdn_4yr, color = control)) +
  geom_point() +
  geom_text(aes(label = instnm))
