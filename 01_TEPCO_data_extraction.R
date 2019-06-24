
library(tictoc)

tic()

empty_df <- data.frame(date = as.Date(NA, format = "%Y-%m-%d"),
                       opening_price = NA,
                       high_price = NA,
                       low_price = NA,
                       closing_price = NA,
                       volume = NA,
                       adjusted_closing_price = NA,
                       stringsAsFactors = FALSE)

year.list <- seq(1983, 2018)
base_url <- "https://kabuoji3.com/stock/9501/"

for (i in 1:length(year.list)) {

url <- paste(base_url, year.list[i], "/", sep = "")

# Connecting stock price data webpage--------------------------------------
tepco <- readLines(url, warn = FALSE)  
length(tepco)

# Extracting data----------------------------

# Date
date_exp <- paste(year.list[i], "-[0-9]{2}-[0-9]{2}", sep = "")

matches <- gregexpr(pattern = date_exp,
                    text = tepco[grep(date_exp, tepco)])
date <- unlist(regmatches(tepco[grep(date_exp, tepco)],
                          matches))
date <- as.Date(date, format = "%Y-%m-%d")

# Opening price
num_exp <- "[0-9.]+"
matches <- gregexpr(pattern = num_exp,
                    text = tepco[grep(date_exp, tepco)+1])
open <- as.numeric(unlist(regmatches(tepco[grep(date_exp, tepco)+1],
                                     matches)))

# High price
matches <- gregexpr(pattern = num_exp,
                    text = tepco[grep(date_exp, tepco)+2])
high <- as.numeric(unlist(regmatches(tepco[grep(date_exp, tepco)+2],
                                     matches)))

# Low price
matches <- gregexpr(pattern = num_exp,
                    text = tepco[grep(date_exp, tepco)+3])
low <- as.numeric(unlist(regmatches(tepco[grep(date_exp, tepco)+3],
                                    matches)))

# Closing price
matches <- gregexpr(pattern = num_exp,
                    text = tepco[grep(date_exp, tepco)+4])
close <- as.numeric(unlist(regmatches(tepco[grep(date_exp, tepco)+4],
                                      matches)))

# Volume
matches <- gregexpr(pattern = num_exp,
                    text = tepco[grep(date_exp, tepco)+5])
volume <- as.numeric(unlist(regmatches(tepco[grep(date_exp, tepco)+5],
                                       matches)))

# Adjusted closing price
matches <- gregexpr(pattern = num_exp,
                    text = tepco[grep(date_exp, tepco)+6])
adj <- as.numeric(unlist(regmatches(tepco[grep(date_exp, tepco)+6],
                                    matches)))

# Making datafram------------------------------

tepco_df <- data.frame(date = date,
                       opening_price = open,
                       high_price = high,
                       low_price = low,
                       closing_price = close,
                       volume = volume,
                       adjusted_closing_price = adj,
                       stringsAsFactors = FALSE)

empty_df <- rbind(empty_df, tepco_df)
                       
}

tepco <- empty_df[-1, ]

head(tepco)
tail(tepco)

toc()

# EDA
plot(tepco$date, tepco$closing_price, type = "l")
plot(tepco$date, tepco$adj, type = "l")


