
# quarterly love report ---------------------------------------------------

url <- "https://www.motesetu.com/matchmaking-guidebook/?rank_all=on&rank_text%5B%5D=S&rank_text%5B%5D=A&rank_text%5B%5D=B&rank_text%5B%5D=C&rank_text%5B%5D=D&sort=1"
page <- readLines(url, warn = F, encoding = "UTF-8")
length(page)


# name --------------------------------------------------------------------

name_exp <- "company-name"
name <- page[grep(name_exp, page)+1]
name <- gsub(" ", "", name)
name <- substr(name, 1, regexpr("<", name)-1)

# score -----------------------------------------------------

score_exp <- "恋愛偏差値"
score <- page[grep(score_exp, page)+1]
matches <- gregexpr(pattern = "[0-9]+",
                    text = score)
score <- as.numeric(unlist(regmatches(score,
                          matches)))
head(score)

# income -----------------------------------------------------------

income_exp <- ">平均年収<"
income <- page[grep(income_exp, page)+1]
matches <- gregexpr(pattern = "[0-9,]+",
                    text = income)
income <- unlist(regmatches(income, matches))
income <- as.numeric(gsub(",", "", income))

# Merge -------------------------------------------------------------------

length(name)
length(score)
length(income)

df <- data.frame(company_name = name,
                 love_standard_score = score,
                 annual_income = income)
head(df)

# Save data ---------------------------------------------------------------

setwd("C:/Users/yukic/Documents/Data")
saveRDS(df, file = "quarterly_love_report.rds")

# EDA ---------------------------------------------------------------------

library(ggplot2)
ggplot(df, aes(x = score)) +
  geom_histogram(bins = 10)
ggplot(df, aes(x = score, y = ..density..)) +
  geom_freqpoly(bins = 10)

mean(df$love_standard_score)
sd(df$love_standard_score)
summary(df$love_standard_score)

ggplot(df, aes(x = annual_income)) +
  geom_histogram(bins = 15)
ggplot(df, aes(x = annual_income, y = ..density..)) +
  geom_freqpoly(bins = 15)

summary(df$annual_income)
