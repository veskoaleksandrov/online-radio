library(jsonlite)

json_data <- fromJSON(readLines(con = "http://listen.di.fm/public2/", 
                                warn = FALSE))

track.info <- paste("#EXTINF:-1", 
                    json_data$name, 
                    sep = ",")

track.location <- paste("http://pub2.di.fm:80/di_", 
                    json_data$key, 
                    "_aacplus", 
                    sep = "")

playlist <- paste(track.info, 
                  track.location, 
                  sep = "\n")

writeLines(text = playlist, 
           con = "di.fm.m3u")