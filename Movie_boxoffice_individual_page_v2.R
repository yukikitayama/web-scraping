# Make vector from the previous data--------------------------
box <- read.csv("C:/Users/Ganjouji/Desktop/Data/boxoffice.csv",
                header = TRUE,
                stringsAsFactors = FALSE)
 key_mat <- box[ , c("ID_boxoffice", "keyname_boxoffice")]
# key_mat <- box[181:184 , c("ID_boxoffice", "keyname_boxoffice")]

# Making for loop again
# out_mat <- matrix(NA, nrow = dim(key_mat)[1], ncol = 2)
out_mat <- data.frame(name = key_mat[ , 2],
                      international_gross = NA,
                      opening_weekend = NA,
                      worldwide_gross = NA,
                      rating = NA,
                      length = NA,
                      director = NA,
                      principal_cast = NA,
                      writers = NA,
                      producers = NA,
                      genre = NA)

 n <- dim(key_mat)[1]
# n <- 2

for (i in 1:n) {
  
  # Import individual movie page
  id <- key_mat[i, 1]
  name <- key_mat[i, 2]
  url <- paste("https://pro.boxoffice.com/movie", id, name, sep = "/")
  
  if (isTRUE(class(try(readLines(url, warn = FALSE), TRUE)) == "try-error")) {
    next
  }
  
  else {
  
  source <- readLines(url, warn = FALSE)
    
  # Total International Gross
  # Old movies do not have international gross
  int_exp <- "Total\\sInternational\\sGross:\\s\\$[0-9,]+"
  matches <- gregexpr(pattern = int_exp,
                      text = source[grep(int_exp, source)])
  int <- unlist(regmatches(source[grep(int_exp, source)],
                           matches))
  int <- substr(int, 
                start = regexpr("\\$", int)+1, 
                stop = nchar(int))
  int <- as.numeric(gsub(",", 
                         "", 
                         int))

  # Opening Weekend
  open_exp <- "Opening\\sWeekend:\\s\\$[0-9,]+"
  matches <- gregexpr(pattern = open_exp,
                      text = source[grep(open_exp, source)])    
  open <- unlist(regmatches(source[grep(open_exp, source)],
                            matches))
  open <- substr(open, 
                 start = regexpr("\\$", open)+1,
                 stop = nchar(open))
  open <- as.numeric(gsub(",",
                          "",
                          open))
  
  # Total Worldwide Gross
  world_exp <- "Total\\sWorldwide\\sGross:\\s\\$[0-9,]+"
  matches <- gregexpr(pattern = world_exp,
                      text = source[grep(world_exp, source)])    
  world <- unlist(regmatches(source[grep(world_exp, source)],
                            matches))
  world <- substr(world, 
                 start = regexpr("\\$", world)+1,
                 stop = nchar(world))
  world <- as.numeric(gsub(",",
                          "",
                          world))
  
  # Rating
  rate_exp <- "Rating:\\s.+"
  matches <- gregexpr(pattern = rate_exp,
                      text = source[grep(rate_exp, source)])    
  rate <- unlist(regmatches(source[grep(rate_exp, source)],
                            matches))
  rate <- substr(rate, 
                 start = regexpr(">", rate)+1,
                 stop = nchar(rate))
  rate <- substr(rate, start = 1, stop = regexpr("<", rate)-1)
  

  # Length
  length_exp <- "[0-9]+\\smin"
  matches <- gregexpr(pattern = length_exp,
                      text = source[grep(length_exp, source)])    
  len <- unlist(regmatches(source[grep(length_exp, source)],
                           matches))
  len <- as.numeric(substr(len, start = 1, stop = regexpr("\\s", len)-1))
  
  # Director
  direct_exp <- "Director:"
  direct <- source[grep(direct_exp, source)+2]
  direct <- try(trimws(substr(direct, start = 1, stop = regexpr("<", direct)-1)))
  
  
  # Principal cast
  cast_exp <- "Principal\\sCast:"
  cast <- source[grep(cast_exp, source)+2]
  cast <- try(trimws(substr(cast, start = 1, stop = regexpr("<", cast)-1)))
  
  # Writers
  write_exp <- "Writers:"
  write <- source[grep(write_exp, source)+2]
  write <- try(trimws(substr(write, start = 1, stop = regexpr("<", write)-1)))
  
  # Producers
  prod_exp <- "Producers:"
  prod <- source[grep(prod_exp, source)+2]
  prod <- try(trimws(substr(prod, start = 1, stop = regexpr("<", prod)-1)))
  
  # Genre (need if sentences whether or not there is trailer)
  trail_exp <- "View\\sTrailer"
  if (length(grep(trail_exp, source)) == 0) {
    genre <- source[grep(length_exp, source)+2]
  }
  else {
    genre <- source[grep(trail_exp, source)+2]
  }
  genre <- try(trimws(substr(genre, start = 1, stop = regexpr("<", genre)-1)))
  
  # Saving each data
  if (length(int) == 0) {out_mat[i, "international_gross"] <- NA}
    else {out_mat[i, "international_gross"] <- int}
  if (length(open) == 0) {out_mat[i, "opening_weekend"] <- NA}
    else {out_mat[i, "opening_weekend"] <- open}
  if (length(world) == 0) {out_mat[i, "worldwide_gross"] <- NA}
    else {out_mat[i, "worldwide_gross"] <- world}
  if (length(rate) == 0) {out_mat[i, "rating"] <- NA}
    else {out_mat[i, "rating"] <- rate}
  if (length(len) == 0) {out_mat[i, "length"] <- NA}
    else {out_mat[i, "length"] <- len}
  if (length(direct) == 0) {out_mat[i, "director"] <- NA}
    else {out_mat[i, "director"] <- direct}
  if (length(cast) == 0) {out_mat[i, "principal_cast"] <- NA}
    else {out_mat[i, "principal_cast"] <- cast}
  if (length(write) == 0) {out_mat[i, "writers"] <- NA}
    else {out_mat[i, "writers"] <- write}
  if (length(prod) == 0) {out_mat[i, "producers"] <- NA}
    else {out_mat[i, "producers"] <- prod}
  if (length(genre) == 0) {out_mat[i, "genre"] <- NA}
    else {out_mat[i, "genre"] <- genre}
  
  }
}
head(out_mat)
tail(out_mat)

write.csv(x = out_mat,
          file = "C:/Users/Ganjouji/Desktop/Data/out_mat_v2.csv",
          quote = TRUE,
          row.names = FALSE)

test <- read.csv("C:/Users/Ganjouji/Desktop/Data/out_mat_v2.csv",
                 header = TRUE,
                 stringsAsFactors = FALSE)
head(test)
tail(test)



# Need to modify character variables to be cleaned--------------

sample(out_mat$rating, size = 10, replace = FALSE)
sample(out_mat$length, size = 10, replace = FALSE)
sample(out_mat$director, size = 10, replace = FALSE)
sample(out_mat$principal_cast, size = 10, replace = FALSE)
sample(out_mat$writers, size = 10, replace = FALSE)
sample(out_mat$producers, size = 10, replace = FALSE)
sample(out_mat$genre, size = 10, replace = FALSE)
?trimws
x <- "  Some text. "
x
trimws(x)
trimws(x, "l")
trimws(x, "r")

source <- readLines("https://pro.boxoffice.com/movie/10389/tootsie",
                  warn = FALSE)
source <- readLines("https://pro.boxoffice.com/movie/29227/la-la-land",
                    warn = FALSE)
genre_exp <- "View\\sTrailer"
grep(genre_exp, source)
length(grep(genre_exp, source))
matches <- gregexpr(pattern = genre_exp,
                    text = source[grep(direct_exp, source)+2])    
direct <- source[grep(direct_exp, source)+2]

key_mat <- key_mat[1:2, ]
key_mat[2, 1] <- 999999 

id <- key_mat[2, 1]
name <- key_mat[2, 2]
url <- paste("https://pro.boxoffice.com/movie", id, name, sep = "/")
source <- readLines(url,
                    warn = FALSE)

Sys.setlocale("LC_TIME","C")
Sys.setlocale("LC_ALL", "English")
source <- readLines("https://pro.boxoffice.com/movie/22821/annie-2014",
                    warn = FALSE)
prod_exp <- "Producers:"
prod <- source[grep(prod_exp, source)+2]


