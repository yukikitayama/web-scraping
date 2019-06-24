
# Reference URL -----------------------------------------------------------

# url <- "https://www.rakumachi.jp/syuuekibukken/area/prefecture/dimAll/?page=1&limit=20&pref=15&gross_from=&price_to=&dim[]=2001&sort=property_created_at&sort_type=desc"
# url <- "https://www.rakumachi.jp/syuuekibukken/area/prefecture/dimAll/?page=2&limit=20&pref=15&gross_from=&price_to=&dim[]=2001&sort=property_created_at&sort_type=desc"
# url <- "https://www.rakumachi.jp/syuuekibukken/area/prefecture/dimAll/?page=3&limit=20&pref=15&gross_from=&price_to=&dim[]=2001&sort=property_created_at&sort_type=desc"
# url <- "https://www.rakumachi.jp/syuuekibukken/area/prefecture/dimAll/?page=4&limit=20&pref=15&gross_from=&price_to=&dim[]=2001&sort=property_created_at&sort_type=desc"
# url <- "https://www.rakumachi.jp/syuuekibukken/area/prefecture/dimAll/?page=5&limit=20&pref=15&gross_from=&price_to=&dim[]=2001&sort=property_created_at&sort_type=desc"

# Make list of url of individual page -------------------------------------

front <- "https://www.rakumachi.jp/syuuekibukken/area/prefecture/dimAll/?page="
back <- "&limit=20&pref=15&gross_from=&price_to=&dim[]=2001&sort=property_created_at&sort_type=desc"
front_url <- "https://www.rakumachi.jp"
search_num <- 90
page_num <- 5

for (i in 1:page_num) {
  
  # Loading webpage
  url <- paste(front, i, back, sep = "")
  page <- readLines(url, warn = FALSE, encoding = "UTF-8")

  # Extracting url of individual property page
  ind_key <- "propertyBlock__mainArea"
  key <- page[grep(ind_key, page)]
  key <- substr(key, 
                start = regexpr("window.open", key)+13,
                stop = nchar(key)-4)

  # Creating correct url
  key <- paste(front_url, key, sep = "")

  # Saving url
  if (i == 1) {
    url_vec <- key
  } else {
    url_vec <- c(url_vec, key)
  }

}


# Extracting infomation from individual page ------------------------------

member <- 100
url_vec.2 <- url_vec[nchar(url_vec) < member]
url_num <- length(url_vec.2)
name_vec <- rep(NA, url_num)
price_vec <- rep(NA, url_num)
gross_vec <- rep(NA, url_num)
manage_vec <- rep(NA, url_num)
repair_vec <- rep(NA, url_num)
space_vec <- rep(NA, url_num)
built_vec <- rep(NA, url_num)
year_vec <- rep(NA, url_num)
month_vec <- rep(NA, url_num)
age_vec <- rep(NA, url_num)
traffic_vec <- rep(NA, url_num)
address_vec <- rep(NA, url_num)
const_vec <- rep(NA, url_num)
floor_all_vec <- rep(NA, url_num)
floor_vec <- rep(NA, url_num)
top_vec <- rep(NA, url_num)
direct_vec <- rep(NA, url_num)
land_vec <- rep(NA, url_num)
lending_vec <- rep(NA, url_num)
deal_vec <- rep(NA, url_num)
agency_vec <- rep(NA, url_num)
parent_vec <- rep(NA, url_num)
income_vec <- rep(NA, url_num)

for (i in 1:url_num) {
  
  url <- url_vec.2[i]
  # To avoid character corruption, use UTF-8 encoding in readLines
  page <- readLines(url, warn = FALSE, encoding = "UTF-8")

  # Reducing page to only what I want
  block4 <- grep("ブロック04", page)
  page <- page[block4[1]:block4[2]]
  
# Name (name)  --------------------------------------------------------------------

  name_exp <- ">物件名<"
  name <- page[grep(name_exp, page)+1]
  name <- substr(name, 
                 start = regexpr(">", name)+1, 
                 stop = nchar(name)-5)

# 販売価格 (price) ------------------------------------------------------------
  
  price_exp <- ">販売価格<"
  price <- page[grep(price_exp, page)+2]
  price <- as.numeric(substr(price,
                             start = regexpr(">", price)+1,
                             stop = nchar(price)-9))
  

# 表面利回り (gross) -----------------------------------------------------------

  gross_exp <- "slide2linkovertext[[:punct:]]>表面利回り"
  gross <- page[grep(gross_exp, page)+6]
  gross <- as.numeric(substr(gross,
                      start = regexpr(">", gross)+1,
                      stop = nchar(gross)-8))

# 想定年間収入 (income) ---------------------------------------------------------

  income_exp <- "slide2linkovertext[[:punct:]]>想定年間収入<"
  income <- page[grep(income_exp, page)+5]
  income <- substr(income, regexpr("[0-9]+", income), regexpr("円", income)-1)
  income <- as.numeric(gsub(",", "", income))
  
# 管理費 (manage) ------------------------------------------------------------

  manage_exp <- ">管理費"
  manage <- page[grep(manage_exp, page)+1][1]
  manage <- substr(manage,
                              start = regexpr(">", manage)+1,
                              stop = regexpr("円", manage)-1)
  manage <- as.numeric(gsub(",", "", manage))

# 修繕積立金 (repair) ----------------------------------------------------------

  repair_exp <- ">修繕積立金"
  repair <- page[grep(repair_exp, page)+1][1]
  repair <- substr(repair,
                   start = regexpr(">", repair)+1,
                   stop = regexpr("円", repair)-1)
  repair <- as.numeric(gsub(",", "", repair))
  
# 専有面積 (space) ------------------------------------------------------------

  space_exp <- ">専有面積<"
  space <- page[grep(space_exp, page)+1]
  space <- as.numeric(substr(space,
                             start = regexpr(">", space)+1,
                             stop = regexpr("㎡", space)-1))

# 築年月 (built) -------------------------------------------------------------

  built_exp <- ">築年月<"
  built <- page[grep(built_exp, page)+2]
  built <- gsub(" ", "", built)
  built <- substr(built, 1, regexpr("<", built)-1)

# built year (year) -------------------------------------------------------

  year <- as.numeric(substr(built, 1, 4))

# built month (month) -----------------------------------------------------

  month <- as.numeric(substr(built, regexpr("年", built)+1, regexpr("月", built)-1))  

# 築何年 (total_month) -------------------------------------------------------------

  age <- as.numeric(substr(built, regexpr("築", built)+1, nchar(built)-2))

# 沿線交通 (traffic) ----------------------------------------------------------

  traffic_exp <- ">沿線交通<"
  traffic <- page[grep(traffic_exp, page)+2]
  traffic <- substr(traffic, regexpr(">", traffic)+1, nchar(traffic))
  traffic <- substr(traffic, 1, regexpr("<", traffic)-1)

# 所在地 (address) -----------------------------------------------------------

  address_exp <- ">所在地<"
  # above expression extract both building and agency address
  # so use only the first element
  address <- page[grep(address_exp, page)+1][1]
  address <- substr(address, regexpr(">", address)+1, nchar(address)-12)

# 建物構造 (const) ------------------------------------------------------------

  const_exp <- "slide2linkovertext[[:punct:]]>建物構造<"
  const <- page[grep(const_exp, page)+5]
  const <- substr(const, regexpr(">", const)+1, nchar(const))
  const <- substr(const, 1, regexpr("<", const)-1)
  
# 所在階 (floor) -------------------------------------------------------------

  floor_all <- page[grep(built_exp, page)-1]
  floor_all <- substr(floor_all, regexpr(">", floor_all)+1, nchar(floor_all))
  floor_all <- substr(floor_all, 1, regexpr("<", floor_all)-1)
  floor_all <- gsub(" ", "", floor_all)
  
  # 所在階
  floor <- substr(floor_all, 1, regexpr("階", floor_all)-1)
  floor <- as.numeric(floor)
  
  # 建物最上階
  if (nchar(floor) == 1) {
    top <- substr(floor_all, 4, gregexpr("階", floor_all)[[1]][2]-1)
  } else {
    top <- substr(floor_all, 5, gregexpr("階", floor_all)[[1]][2]-1)
  }
  top <- as.numeric(top)
  # top <- substr(floor_all, regexpr("／", floor_all)+1, nchar(floor_all))
  # top <- substr(top, 1, regexpr("階", top)-1)
  
# 方角 (direct) -------------------------------------------------------------

  direct_exp <- ">方角<"  
  direct <- page[grep(direct_exp, page)+1]
  direct <- substr(direct, regexpr(">", direct)+1, nchar(direct))
  direct <- substr(direct, 1, regexpr("<", direct)-1)

# 総戸数 (total_room) --------------------------------------------------------

  # total_room_exp <- ""  

# 土地権利 (land) -------------------------------------------------------------

  land_exp <- "slide2linkovertext[[:punct:]]>土地権利<"
  land <- page[grep(land_exp, page)+5]
  land <- substr(land, regexpr(">", land)+1, nchar(land))
  land <- substr(land, 1, regexpr("<", land)-1)

# 現況 (lend) ---------------------------------------------------------------

  lend_exp <- "slide2linkovertext[[:punct:]]>現況<"  
  lending <- page[grep(lend_exp, page)+5]
  lending <- substr(lending, regexpr(">", lending)+1, nchar(lending))
  lending <- substr(lending, 1, regexpr("<", lending)-1)

# 取引態様 (deal) -------------------------------------------------------------

  deal_exp <- ">取引態様<"  
  deal <- page[grep(deal_exp, page)+1]
  deal <- substr(deal, regexpr(">", deal)+1, nchar(deal))
  deal <- substr(deal, 1, regexpr("<", deal)-1)

# 不動産屋名 (agency) ----------------------------------------------------------

  agency_exp <- "inquiryArea__realtor"  
  agency <- page[grep(agency_exp, page)+2]
  agency <- substr(agency, regexpr(">", agency)+1, nchar(agency))
  agency <- substr(agency, 1, regexpr("<", agency)-1)
  
  # parent <- substr(agency, regexpr(" ", agency)+1, nchar(agency))
  # parent <- substr(parent, 1, regexpr(" ", parent)-1)
  
# Saving result -----------------------------------------------------------

  name_vec[i] <- name
  price_vec[i] <- price
  gross_vec[i] <- gross
  manage_vec[i] <- manage
  repair_vec[i] <- repair
  space_vec[i] <- space
  built_vec[i] <- built
  year_vec[i] <- year
  month_vec[i] <- month
  age_vec[i] <- age
  traffic_vec[i] <- traffic 
  address_vec[i] <- address
  const_vec[i] <- const
  floor_all_vec[i] <- floor_all
  floor_vec[i] <- floor
  top_vec[i] <- top
  direct_vec[i] <- direct
  land_vec[i] <- land
  lending_vec[i] <- lending
  deal_vec[i] <- deal
  agency_vec[i] <- agency
  # parent_vec[i] <- parent
  income_vec[i] <- income

}


length(name_vec)
length(price_vec)
length(gross_vec)
length(manage_vec)
length(repair_vec)
length(space_vec)
length(built_vec)
length(year_vec)
length(month_vec)
length(age_vec)
length(traffic_vec)
length(address_vec)
length(const_vec)
length(floor_all_vec)
length(floor_vec)
length(top_vec)
length(direct_vec)
length(land_vec)
length(lending_vec)
length(deal_vec)
length(agency_vec)
# length(parent_vec)
length(income_vec)

df <- data.frame(name = name_vec,
                 price = price_vec,
                 gross = gross_vec,
                 income = income_vec,
                 manage = manage_vec,
                 repair = repair_vec,
                 space = space_vec,
                 built = built_vec,
                 year = year_vec,
                 month = month_vec,
                 age = age_vec,
                 traffic = traffic_vec,
                 address = address_vec,
                 const = const_vec,
                 floor_all = floor_all_vec,
                 floor = floor_vec,
                 top = top_vec,
                 direct = direct_vec,
                 land = land_vec,
                 lending = lending_vec,
                 deal = deal_vec,
                 agency = agency_vec,
                 stringsAsFactors = FALSE)

head(df)
str(df)

# general data cleaning ---------------------------------------------------

df$occupancy <- ifelse(df$lending == "賃貸中", 1, 0)
table(df$occupancy)  


# individual data cleaning -----------------------------------------------------------

df$area <- ifelse(regexpr("長岡", df$address) > 0, "Nagaoka", 
                  ifelse(regexpr("三条", df$address) > 0, "Sanjo", 
                         ifelse(regexpr("南魚沼郡", df$address) > 0, "Minamiuonuma",
                                "Niigata")))

df$prefecture <- "Niigata"

# Saving data -------------------------------------------------------------

setwd("C:/Users/yukic/Documents/Data")
# saveRDS(df, "rakumachi_niigata_20181229.rds")
saveRDS(df, "rakumachi_niigata_20181230.rds")

