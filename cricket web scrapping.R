#loading XML library which is used for reading HTML content
library(XML)

#installing rvest package which is used for web scrapping in R
install.packages("rvest")

#loading rvest library to scrap the data from espncricinfo.com
library(rvest)

#creating function for doing left trim operations
ltrim <- function (x){
  return(sub("^\\s+", "", x))
} 

#creating function for doing right trim operations
rtrim <- function (x){
  return(sub("\\s+$", "", x))
} 

#creating function for doing trim operations from both right and left
bothtrim <- function (x){
  ltrim=sub("^\\s+", "", x)
  result=sub("\\s+$", "", ltrim)
  return(result)
}

#function for getting and mining the data from espncricinfo website
#input to this function is the ID of individual player
getCricketData <- function (inputID){
  #creating an empty data frame
  p_info=data.frame()
  #creating required URL which will be used for getting and mining the data
  urlString=paste("http://www.espncricinfo.com/india/content/player/",inputID,".html")
  urlString=gsub(" ", "", urlString, fixed = TRUE)
  #encoding the URL
  urlHTML=URLencode(urlString, reserved = FALSE, repeated = FALSE)
  #getting individual player information using html()
  player_info <- html(urlHTML)
  #extracting the player name from player_info data frame
  player_name = bothtrim(player_info %>%  html_node("h1") %>%  html_text())
  #extracting the player's date of birth (if available) from player_info data frame
  player_dob=player_info %>%  html_nodes(".ciPlayerinformationtxt:nth-child(2) span") %>%  html_text()
  player_dob = gsub('\\n', ' ', player_dob)
  #creating the end index for using in substring operation
  stop_index=gregexpr(pattern =',',player_dob)
  #extracting the player's birth month and date from the player's DOB
  player_dob_mmdd=bothtrim(substr(x=player_dob,start = 1,stop = stop_index[[1]][1]-1))
  #extracting the player's birth year from the player's DOB
  player_dob_year=bothtrim(substr(x=player_dob,start = stop_index[[1]][1]+1,stop = stop_index[[1]][2]-1))
  #extracting the player's birth city from the player_info data frame
  player_dob_city=bothtrim(substr(x=player_dob,start = stop_index[[1]][2]+1,stop = stop_index[[1]][3]-1))
  #following ifelse block extract the player's birth state
  if(is.na(player_dob_city)){
    player_dob_city=bothtrim(substr(x=player_dob,start = stop_index[[1]][2]+1,stop = nchar(player_dob)))
    player_dob_state=player_dob_city
  }else{
    player_dob_state=bothtrim(substr(x=player_dob,start = stop_index[[1]][3]+1,stop = nchar(player_dob)))
  }
  #extracting the player's birth month from the mmdd data frame
  player_dob_mm=strsplit(player_dob_mmdd," ")[[1]][1]
  #extracting the player's birth date from the mmdd data frame
  player_dob_dd=strsplit(player_dob_mmdd," ")[[1]][2]
  #extracting whether the player is currently dead or alive
  mortal_status=player_info %>%  html_nodes(".ciPlayerinformationtxt:nth-child(3) b") %>%  html_text()
  #the conditional formatting based on the mortal status of the player
  if(mortal_status=="Died"){
    died_flag=1
    #if the player is dead, extracting 
    player_dod_info=player_info %>%  html_nodes(".ciPlayerinformationtxt:nth-child(3) span") %>%  html_text()
    player_dod = gsub('\\n', ' ', player_dod_info)
    stop_index=gregexpr(pattern =',',player_dod)
    player_dod_mmdd=bothtrim(substr(x=player_dod,start = 1,stop = stop_index[[1]][1]-1))
    player_dod_year=bothtrim(substr(x=player_dod,start = stop_index[[1]][1]+1,stop = stop_index[[1]][2]-1))
    player_dod_city=bothtrim(substr(x=player_dod,start = stop_index[[1]][2]+1,stop = stop_index[[1]][3]-1))
    player_dod_state=bothtrim(substr(x=player_dod,start = stop_index[[1]][3]+1,stop = nchar(player_dod)))
    # NOT AVAILABLE BLOCK STARTS HERE
    # player_dod_age=bothtrim(substr(x=player_dod,start = stop_index[[1]][4]+1,stop = nchar(player_dod)))
    # player_dod_mm=strsplit(player_dod_mmdd," ")[[1]][1]
    # player_dod_dd=strsplit(player_dod_mmdd," ")[[1]][2]
    # NOT AVAILABLE BLOCK ENDS HERE
    player_dod=paste(player_dod_mmdd , player_dod_year)
    player_age=NA
  }
  # if the player is still alive
  if(mortal_status=="Current age"){
    # death flag is ZERO
    died_flag=0
    # extracting player age information
    player_age=player_info %>%  html_nodes(".ciPlayerinformationtxt:nth-child(3) span") %>%  html_text()
    player_age = gsub('\\n', ' ', player_age)
    player_dod=NA
  }
  # attaching all the columns together to construct the output data frame
  p_info=cbind(player_name,player_dob_mm,player_dob_dd,player_dob_year,player_dob_city,player_dob_state,died_flag,player_dod,player_age)
  # returning the constructed output data frame
  return(p_info)
}
# creating an empty data frame
result1=data.frame()
# reading the input player IDs from the input file
# this file is created from ESPN Cricinfo website to get the list of all players
player_ids <- read.table("C:/Users/User/Desktop/blog/cricket/player_ids.txt", quote="\"", comment.char="")
lenthSeq=seq(1:length(player_ids[[1]]))
for(i in lenthSeq){
  # iterating the information process for each player
  result1=rbind(result1,getCricketData(player_ids[[1]][i]))
}
# constructing the column names for data storage purpose
colnames(result1)=c("name","birth_month","bith_day","birth_year",
                    "birth_city","birth_state","dead","death_date","age")
write(x=paste("Name",',',"Birth Month",',',"Birth Day",',',"Birth Year",',',
              "Birth City",',',"Birth State",',',"Dead",',',"Death Date",',',"Age"),
      file="C:\\Users\\user\\Desktop\\blog\\cricket\\output.csv")
# writing the actual data to the output CSV file
write(x = paste(result1$name,',',result1$birth_month,',',result1$bith_day,',',
                result1$birth_year,',',result1$birth_city,',',result1$birth_state,',',
                result1$dead,',',result1$death_date,',',result1$age),
      file="C:\\Users\\user\\Desktop\\blog\\cricket\\output.csv",
      append = T)
