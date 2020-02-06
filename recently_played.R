library(httr)
library(jsonlite)
library(plyr)

db.Path <- "C:/Users/Vesko/Documents/R Scripts/recently_played_di.rds"
if(file.exists(db.Path)) {
  channel_playlist <- readRDS(file = db.Path)
}

player <- "http://www.di.fm/webplayer3/config"
channels <- fromJSON(txt = player)$API$Config$channels
channels$created_at <- as.POSIXct(x = channels$created_at, format = "%Y-%m-%dT%H:%M:%S")
channels$updated_at <- as.POSIXct(x = channels$updated_at, format = "%Y-%m-%dT%H:%M:%S")
channels <- subset(x = channels, select = -c(images, similar_channels))

for(j in 1:24) {
  # print(paste0("j = ", j))
  playlist <- NULL
  for(i in 1:nrow(channels)) {
    # print(paste0("i = ", i))
    url <- paste0("http://www.di.fm/_papi/v1/di/track_history/channel/", channels$id[i], "?per_page=20")
    # print(paste0("url = ", url))
    temp <- fromJSON(txt = url)
    if(is.data.frame(temp)) {
      temp <- temp[temp$type == "track", ]
      temp$started <- as.POSIXct(temp$started, origin="1970-01-01")
      temp <- subset(x = temp, select = -c(ad, votes, images))
      # temp$ad <- NULL
      # temp$votes <- NULL
      # temp$images <- NULL
      playlist <- rbind.fill(playlist, temp)
      # rm(url, temp)
    } else {
      print(temp)
    }
    Sys.sleep(sample(x = 100:500, size = 1)/100)
  }
  print(paste0("adding ", nrow(playlist), "items..."))
  channel_playlist <- rbind.fill(channel_playlist, merge(x = channels, y = playlist, 
                                                         by.x = c("id", "network_id"), by.y = c("channel_id", "network_id")))
  
  saveRDS(object = channel_playlist, file = db.Path)
  
  if(is.null(channel_playlist)) {
    Sys.sleep(5 * 60 * 15)
  } else {
    Sys.sleep(median(channel_playlist$duration) * 15)
  }
}

abc <- readRDS(db.Path)
# sort(x = unique(tolower(abc$track)), decreasing = FALSE)
# sort(tolower(abc[duplicated(tolower(abc$track)), ]$track), decreasing = FALSE)
library(scales)
alltracks <- length(abc$track)
uniquetracks <- length(unique(tolower(abc$track)))
duplicatetracks <- length(tolower(abc[duplicated(tolower(abc$track)), ]$track))
tracksummary <- paste("Tracks Summary\nTotal count of tracks: ", alltracks, 
      "\nCount of unique tracks: ", uniquetracks, " (", percent(uniquetracks / alltracks), ")",
      "\nCount of duplicate tracks: ", duplicatetracks, " (", percent(duplicatetracks / alltracks), ")", 
      "\nAverage track duration: ", round(mean(abc$duration), digits = 2), " seconds",
      "\nTotal tracks duration: ", round(sum(abc$duration)/(60*60), digits = 2), " hours", 
      sep = "")

allartists <- length(abc$artist)
uniqueartists <- length(unique(tolower(abc$artist)))
duplicateartists <- length(tolower(abc[duplicated(tolower(abc$artist)), ]$artist))
artistsummary <- paste("Artists Summary\nTotal count of artists: ", allartists, 
      "\nCount of unique artists: ", uniqueartists, " (", percent(uniqueartists / allartists), ")",
      "\nCount of duplicate artists: ", duplicateartists, " (", percent(duplicateartists / allartists), ")", 
      sep = "")

cat(artistsummary)
cat(tracksummary)

sort(unique(tolower(abc$artist)))
sort(unique(tolower(abc$track)))

# search YouTube
# https://developers.google.com/youtube/v3/docs/search/list#try-it
#
API_URL <- "https://www.googleapis.com/youtube/v3/search"

# API key
# https://developers.google.com/console/help/new/#api-keys
API_KEY <- "AIzaSyDk0RNWCAjP8vQFokWxR7lSgS3ptL64yzE"

tracks.unique <- unique(tolower(abc$track))
yt_url <- list()
yt_title <- list()

for (i in 1:length(tracks.unique)) { 
  
  query <- NULL
  request <- NULL

  query <- tracks.unique[i]
  
  # Standard query parameters
  # https://developers.google.com/youtube/v3/docs/standard_parameters
  # https://developers.google.com/youtube/v3/docs/search/list
  request <- paste(API_URL, 
                   "?part=snippet", 
                   "&q=", URLencode(query, reserved = TRUE), 
                   "&type=video", 
                   "&videoDefinition=high", 
                   "&key=", API_KEY, 
                   sep = "")
  
  response <- fromJSON(txt = request)
  
  if(!is.null(response) && response$pageInfo$totalResults != 0 && length(response$items) >= 1) { 
    
    yt_url[i] <- paste("https://www.youtube.com/watch?v=", 
                            response$items[1, ]$id$videoId, 
                            sep = "")
    
    yt_title[i] <- response$items[1, ]$snippet$title
    
    # rest 
    Sys.sleep(0.5)
    
  } else {
    yt_url[i] <- "not found"
    yt_title[i] <- "not found"
  }
}


for (i in 1:length(yt_url)) {
  name <- paste('item:', i, sep = '')
  yt_url[i] <- yt_url[[name]]
  yt_title[i] <- yt_title[[name]]
}

# download tracks
setwd("E:/Music/Indie Dance")
for(i in 1:length(yt_url)) {
  
  system(paste("C:\\Users\\Vesko\\Downloads\\youtube-dl.exe -f bestaudio ", yt_url[[i]], sep = ""), intern = TRUE)
  
  # rest for a moment
  Sys.sleep(0.5)
  
}

# df contains the list of downloaded tracks
df <- data.frame(list.files())
names(df)[1] <- "file.name"
if (!require("stringr")) install.packages("stringr")
library(stringr)
df$file.type <- str_match(df$file.name, "\\.([a-z0-9]{3,4})$")[, 2]
df$video.id <- str_match(df$file.name, "-(.{11})\\.[a-z0-9]{3,4}$")[, 2]
df$video.name <- str_match(df$file.name, "(.*)-.{11}\\.[a-z0-9]{3,4}$")[, 2]

# ToDo: 
# compare df$video.id against yt_url's video id
# attempt to download the remaining yt_url's video ids 
yt_video.id <- list()
for(i in 1:length(yt_url)) {
  yt_video.id[i] <- str_match(yt_url[[i]], "=(.{11})$")[, 2]
}

df2 <- data.frame(yt_video.id, stringsAsFactors = FALSE)
df2 <- t(df2)
df2 <- data.frame(df2, row.names = NULL)
names(df2) <- "video.id"

vesko <- merge(x = df, y = df2, all.y = TRUE)
vesko <- vesko[is.na(vesko$file.name), ]
vesko <- vesko[!is.na(vesko$video.id), ]

# download remaining tracks
setwd("E:/Music/Indie Dance")
for(i in 1:nrow(vesko)) {
  
  system(paste("C:\\Users\\Vesko\\Downloads\\youtube-dl.exe -f bestaudio ", vesko$video.id[[i]], sep = ""), intern = TRUE)
  
  # rest for a moment
  Sys.sleep(0.5)
  
}


# get the list of downloaded tracks
downloaded.tracks.fullpath <- list.files(path = "E:\\Music\\Indie Dance", full.names = TRUE)
downloaded.tracks.filenames <- list.files(path = "E:\\Music\\Indie Dance")
# prepare the list of ffmpeg commands for extracting the bitrate
ffmpeg.commands <- paste("C:\\Users\\Vesko\\Downloads\\ffmpeg.exe -i ", 
                         downloaded.tracks.fullpath, 
                         sep = "\"")

# read bitrate
# loop through the list of downloaded tracks and build a bitrate list
my.function <- function(x) {
  library(stringr)
  y <- list()
  for(i in 1:length(x)) {
    z <- str_match(system(x[i], intern = TRUE), ", bitrate: (.*) kb/s")
    z <- as.numeric(z[!is.na(z[, 2]), ][2])
    y[i] <- z
  }
  return(y)
}
downloaded.tracks.bitrate <- my.function(ffmpeg.commands)
test.df <- data.frame(downloaded.tracks.bitrate)
#test.df <- t(test.df)

# convert downloaded tracks to MP3
library(stringr)
mp3.tracks.fullpath <- str_replace(downloaded.tracks.fullpath, "(.*\\.)(m4a|webm)$", "\\1mp3")
test <- data.frame(mp3.tracks.fullpath)
mp3.commands <- paste("C:\\Users\\Vesko\\Downloads\\ffmpeg.exe -i ", 
                      downloaded.tracks.fullpath, 
                      " -codec:a libmp3lame -b:a 192K ", 
                      mp3.tracks.fullpath, 
                      sep = "\"")
for(i in 2:length(mp3.commands)) {
  system(mp3.commands[i], intern = TRUE)
}

#paste(tmp, collapse = ", ")