# http://archive.ics.uci.edu/ml/machine-learning-databases/msnbc-mld/description.txt
# Read SPMF output
SPMFOutput <- readLines(file("rgout.txt"))
SPMFOutput

strsplit(SPMFOutput[[1]], "#")[[1]]
class(SPMFOutput[[1]])
SPMFOutput[[2]]
for (i in 1:length(SPMFOutput)) {
#  SPMFOutput[i] <- strsplit(SPMFOutput[i], "#")
  SPMFOutput[i] <- gsub(" #CONF", ", Confidence ", SPMFOutput[i])
  SPMFOutput[i] <- gsub("10 ", "living", SPMFOutput[i])
  SPMFOutput[i] <- gsub("11 ", "business", SPMFOutput[i])
  SPMFOutput[i] <- gsub("11,", "business, ", SPMFOutput[i])
  SPMFOutput[i] <- gsub("12 ", "sports", SPMFOutput[i])
  SPMFOutput[i] <- gsub("13 ", "summary", SPMFOutput[i])
  SPMFOutput[i] <- gsub("14 ", "bbs", SPMFOutput[i])
  SPMFOutput[i] <- gsub("15 ", "travel", SPMFOutput[i])
  SPMFOutput[i] <- gsub("16 ", "msn-news", SPMFOutput[i])
  SPMFOutput[i] <- gsub("17 ", "msn-sports", SPMFOutput[i])
  SPMFOutput[i] <- gsub('1 ', 'frontpage', SPMFOutput[i])
  SPMFOutput[i] <- gsub("2 ", "news", SPMFOutput[i])
  SPMFOutput[i] <- gsub("2,", "news, ", SPMFOutput[i])
  SPMFOutput[i] <- gsub("3 ", "tech", SPMFOutput[i])
  SPMFOutput[i] <- gsub("4 ", "local", SPMFOutput[i])
  SPMFOutput[i] <- gsub("5 ", "opinion", SPMFOutput[i])
  SPMFOutput[i] <- gsub("6 ", "on-air", SPMFOutput[i])
  SPMFOutput[i] <- gsub("7 ", "misc", SPMFOutput[i])
  SPMFOutput[i] <- gsub("8 ", "weather", SPMFOutput[i])
  SPMFOutput[i] <- gsub("9 ", "health", SPMFOutput[i])

}
SPMFOutput
