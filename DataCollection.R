################## Setup
library(rvest)

orgs <- c("Arizona_Cardinals", "Atlanta_Falcons", "Baltimore_Ravens", "Buffalo_Bills", "Carolina_Panthers",
          "Chicago_Bears", "Cincinnati_Bengals", "Cleveland_Browns", "Dallas_Cowboys", "Denver_Broncos",
          "Detroit_Lions", "Green_Bay_Packers", "Houston_Texans", "Indianapolis_Colts", "Jacksonville_Jaguars",
          "Kansas_City_Chiefs", "Miami_Dolphins", "Minnesota_Vikings", "New_England_Patriots", "New_Orleans_Saints",
          "New_York_Giants", "New_York_Jets", "Philadelphia_Eagles", "Pittsburgh_Steelers", "San_Francisco_49ers",
          "Seattle_Seahawks", "Tampa_Bay_Buccaneers", "Tennessee_Titans")

# Defining main function
get_staff <- function(year, org) {
  url <- paste0("https://en.wikipedia.org/wiki/", year, "_", org , "_season")
  
  # Read the HTML content of the page
  page <- read_html(url)
  
  # Find the heading containing "Staff"
  staff_heading <- html_nodes(page, xpath = "//span[contains(text(), 'Staff')]/ancestor::h2")
  
  # Find the immediate following sibling node
  staff_info <- html_nodes(staff_heading, xpath = "following-sibling::*[1]")
  
  # Extract the text content of the immediate following sibling node
  staff_text <- html_text(staff_info)
  
  # Return the staff information, or NA if not found
  if (length(staff_text) > 0) {
    return(staff_text)
  } else {
    return(NA)
  }
}

coaches <- data.frame(year = numeric(), org = character(), staff_text = character(), stringsAsFactors = FALSE)

#### Manual changes based on team names ###########################################
for (year in 2003:2023) {
  for (org in orgs) {
    staff_text <- get_staff(year, org)
    coaches <- rbind(coaches, data.frame(year = year, org = org, staff_text = staff_text, stringsAsFactors = FALSE))
  }
}

for (year in 2003:2019) {
    staff_text <- get_staff(year, "Oakland_Raiders")
    coaches <- rbind(coaches, data.frame(year = year, org = "Oakland_Raiders", staff_text = staff_text, stringsAsFactors = FALSE))
}

for (year in 2020:2023) {
  staff_text <- get_staff(year, "Las_Vegas_Raiders")
  coaches <- rbind(coaches, data.frame(year = year, org = "Las_Vegas_Raiders", staff_text = staff_text, stringsAsFactors = FALSE))
}

for (year in 2003:2016) {
  staff_text <- get_staff(year, "San_Diego_Chargers")
  coaches <- rbind(coaches, data.frame(year = year, org = "San_Diego_Chargers", staff_text = staff_text, stringsAsFactors = FALSE))
}

for (year in 2017:2023) {
  staff_text <- get_staff(year, "Los_Angeles_Chargers")
  coaches <- rbind(coaches, data.frame(year = year, org = "Los_Angeles_Chargers", staff_text = staff_text, stringsAsFactors = FALSE))
}

for (year in 2003:2019) {
  staff_text <- get_staff(year, "Washington_Redskins")
  coaches <- rbind(coaches, data.frame(year = year, org = "Washington_Redskins", staff_text = staff_text, stringsAsFactors = FALSE))
}

for (year in 2020:2021) {
  staff_text <- get_staff(year, "Washington_Football_Team")
  coaches <- rbind(coaches, data.frame(year = year, org = "Washington_Football_Team", staff_text = staff_text, stringsAsFactors = FALSE))
}

for (year in 2022:2023) {
  staff_text <- get_staff(year, "Washington_Commanders")
  coaches <- rbind(coaches, data.frame(year = year, org = "Washington_Commanders", staff_text = staff_text, stringsAsFactors = FALSE))
}

for (year in 2003:2015) {
  staff_text <- get_staff(year, "St._Louis_Rams")
  coaches <- rbind(coaches, data.frame(year = year, org = "St._Louis_Rams", staff_text = staff_text, stringsAsFactors = FALSE))
}

for (year in 2016:2023) {
  staff_text <- get_staff(year, "Los_Angeles_Rams")
  coaches <- rbind(coaches, data.frame(year = year, org = "Los_Angeles_Rams", staff_text = staff_text, stringsAsFactors = FALSE))
}

#########################################################################################################
#########################################################################################################

# Writing coach list to available Excel file
library(openxlsx)

write.xlsx(coaches, "coaches.xlsx")

#########################################################################################################
#########################################################################################################
