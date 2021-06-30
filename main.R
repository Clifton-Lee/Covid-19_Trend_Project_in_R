library(tidyverse)


covid_df <- read_csv("covid19.csv") # import the data 

dim(covid_df)

vector_cols <- colnames(covid_df) #vector variable storing the column names 
print(vector_cols)

head(covid_df) # displays the first 6 rows 

glimpse(covid_df) # summarizes and shows the structure of the dataframe 


covid_df_all_states <- covid_df %>% 
                       filter(Province_State == "All States") %>% 
                       select(-Province_State)

head(covid_df_all_states, 3) # observe the first 3 rows 


covid_df_all_states_daily <- covid_df_all_states %>% 
                             select(Date, Country_Region, 
                                    active, hospitalizedCurr, 
                                    daily_tested, daily_positive)

head(covid_df_all_states_daily, 3) # observe the first 3 rows 



covid_df_all_states_daily_sum <- covid_df_all_states_daily %>% 
                                 group_by(Country_Region) %>% 
                                 summarize(tested = sum(daily_tested),
                                           positive = sum(daily_positive),
                                           active = sum(active),
                                           hospitalized = sum(hospitalizedCurr)) %>% 
                                 arrange(desc(tested))

covid_df_all_states_daily_sum

covid_top_10 <- head(covid_df_all_states_daily_sum, 10) # Top 10 affected nations


countries <- covid_top_10$Country_Region
tested_cases <- covid_top_10$tested
positive_cases <- covid_top_10$positive
active_cases <- covid_top_10$active
hospitalized_cases <- covid_top_10$hospitalized

# Naming the vectors 

names(tested_cases) <- countries
names(positive_cases) <- countries
names(active_cases) <- countries
names(hospitalized_cases) <- countries

# Identifying the top 3 positive against tested cases

result <-  sort(positive_cases/tested_cases, decreasing = TRUE)
positive_tested_top_3 <- result[1:3]
positive_tested_top_3


united_kingdom <- c(positive_tested_top_3["United Kingdom"],
                    tested_cases["United Kingdom"], 
                    positive_cases["United Kingdom"],
                    active_cases["United Kingdom"],
                    hospitalized_cases["United Kingdom"])

united_states <- c(positive_tested_top_3["United States"],
                    tested_cases["United States"], 
                    positive_cases["United States"],
                    active_cases["United States"],
                    hospitalized_cases["United States"])

turkey <- c(positive_tested_top_3["Turkey"],
                    tested_cases["Turkey"], 
                    positive_cases["Turkey"],
                    active_cases["Turkey"],
                    hospitalized_cases["Turkey"])


covid_mat <- rbind(united_kingdom,united_states,turkey)
colnames(covid_mat) <- c("Ratio","tested","positive","active","hospitalized")

covid_mat

question <- "Which countries have had the highest number of positive cases against the number of tests?"
answer <- c("Positive tested cases" = positive_tested_top_3)

data_structure_list <- list("Dataframes" = list(covid_df,covid_df_all_states,
                                                covid_df_all_states_daily,covid_top_10),
                            "Matrices" = list(covid_mat),
                            "Vectors" = list(vector_cols,countries))

covid_analysis_list <- list(question,answer,data_structure_list)
covid_analysis_list[[2]]


## Conclusion 

covid_analysis_list[[2]]


