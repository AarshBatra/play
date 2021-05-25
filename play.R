# Play is exhilirating. Dive in, tear apart, recreate, engross yourself in learning.

# useful libraries---------------------------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(stringr)
library(dplyr)
library(gganimate)
library(plotly)
library(sketchr)
library(tidyr)
library(stringr)
library(magrittr)
library(dplyr)
library(lubridate)

# test zone------------------------------------------------------------------------------------------------

plot(df$x, df$y)
par(new = TRUE)
plot(df$x, df$z)

add <- function(x = 5, y = 10){
  return(x+y)
}

testFunc <- function(x, y = c("a", "b", "c")){
  if (y == "a"){
    sprintf("y is a and x is %d", x)
  } else if (y == "b"){
    sprintf("y is b and x is %d", x)    
  } else {
    sprintf("y is c and x is %d", x)
  }
}

for (z in 1 : length(x)){
  file.create(x[z])
}

tmpDie1 <- sample(1 : 6, 1)
tmpDie2 <- sample(1: 6, 1)
sumOfDice <- tmpDie1 + tmpDie2

if(tmpDie1 == tmpDie2){
  tmpDie1 <- 6
  tmpDie2 <- 2
  sumOfDice <- tmpDie1 + tmpDie2
  
  if(tmpDie1 == tmpDie2){
    tmpDie1 <- sample(1 : 6, 1)
    tmpDie2 <- sample(1: 6, 1)
    sumOfDice <- tmpDie1 + tmpDie2
    
    if(tmpDie1 == tmpDie2){
     print("Go to Jail.") 
    }
  } else {
    f <- "blibla"
  } 
} else {
  g <- "aarsh"
}

for(c in 1 : ncol(masterDataTable)){
  if(c == 2 || c == 15 || c == 16){
    masterDataTable[, c] <- as.character(masterDataTable[, c])
  } else {
    masterDataTable[, c] <- as.numeric(masterDataTable[, c])
  }
}

#------------------------------------------------------------------------------------------------------------

# relatively efficient (to manual switching) way of converting '\' to '/' while setting directories-------------

correctPath <- function(){
  library(stringr)
  oldPath <- readline()
  newPath <- str_replace_all(oldPath, "\\\\", "/")
  setwd(newPath)
}

# web scraping with rvest---------------------------------------------------------

#  library(rvest)
# lego_movie <- read_html("http://www.imdb.com/title/tt1490017/")
# cast <- html_nodes(lego_movie, "#titleCast .itemprop")
# 
# best_movies_link <- read_html("https://www.scoopwhoop.com/50-Best-Hollywood-Movies-Of-All-Time/#.29q4tgzvr")
# best_movies <- html_nodes(best_movies_link, "#artcontent h2")
# best_movies_description <- html_nodes(best_movies_link, "p.sw-para")
# best_movies_description_list <-list()
# best_movies_description_list <- html_text(best_movies_description)
# best_movies_description_list[[5]]
# best_movies_snaps <- html_nodes(best_movies_link, "#artcontent img")
# img_links <- best_movies_snaps %>% 
#   html_attr("src")
# 
# i <- 1
# for(i in 1:length(img_links)){
#   download.file(img_links[i], sprintf("test%d.jpg", i), mode = "wb") 
# }
# 
# url_cubelelo <- read_html("https://www.cubelelo.com/classic-cubes/5x5")
# five_by_five_names <- html_nodes(url_cubelelo, "#content a")
# five_by_five_names <- html_text(five_by_five_names)
# five_by_five_names
# 
# five_by_five_prices <- html_nodes(url_cubelelo, "#content .pull-left")
# five_by_five_names <- html_text(five_by_five_names)

#---------------------------------------------------------------------------------------------------

# downloading torrents v1

# hyp_link_s2_std_all <- list()
# num_eps <- 10
# for (eps in 1 : num_eps){
#   tmplink <- read_html(sprintf("https://zooqle.unblocked.mx/tv/silicon-valley-1aql/2x%d.html?tg=3", eps))
#   hyp_link_s2_std_all[[eps]] <- html_nodes(tmplink, "tr:nth-child(1) .text-trunc .small") %>% html_attr("href")
# }
# 
# for (dl in 1 : num_eps){
#   tmplink <- read_html(hyp_link_s2_std_all[dl])
#   tmp_tor_dl_link <- html_nodes(tmplink, "#dlPanel li:nth-child(3) a") %>% html_attr("href")
#   download.file(tmp_tor_dl_link, sprintf("2x%d.torrent", dl))
# }
# 

# library(stringr)
# library(rvest)
# linkVec <- c()
# tmpLink <- read_html("http://sv4avadl.uploadt.com/Serial/Silicon/S01/")
# linkVec <- html_nodes(tmpLink, "a") %>% html_attr("href") 
# 
# domNameAdd <- c("http://sv4avadl.uploadt.com/Serial/Silicon/S01/") 
# numEps <- 8
# for (eps in 1 : (numEps + 1)){
#   linkVec[eps] <- str_c(domNameAdd, linkVec[eps])
#   if (eps == 1){
#     next
#   } else {
#     download.file(linkVec[eps], sprintf("s01_ep%d", (eps - 1)))
#   }
# }
# 

#---------------------------------------------------------------------------------------------------------

# sending e-mails with R 
 
 install.packages("sendmailR")
 install.packages("mailR")
 library(sendmailR)
 library(mailR)
 emailIdList <- c("substance.mona@gmail.com", "pranavbatra.ind@gmail.com", "aarshbatra.in@gmail.com")
 namesList <- c("Mona Batra", "Pranav Batra", "Aarsh Batra")
 
 for(i in 1 : length(emailIdList)){
   send.mail(from = "aarshbatra.in@gmail.com", to = emailIdList[i], subject = "Test e-mail from R",
             body = sprintf("Hi, %s: This is a test e-mail. Best, Aarsh.", namesList[i]),
             smtp = list(host.name = "smtp.gmail.com", port = 465,
                         user.name = "aarshbatra.in@gmail.com",
                         passwd = "enterPassword", ssl = TRUE),
             authenticate = TRUE,
             send = TRUE)  
 }
 
 

#---------------------------------------------------------------------------------------------------------------

# renaming files in the computer file system v1

# library(stringr)
# renameFilesIntoOrderv1 <- function(numSeas, numEpS1, numEpS2, numEpS3, numEpS4, numEpS5, cwd){
#   setwd(cwd)
#   newFileNamesBuild <- c(rep(1, times = numEpS1), rep(2, times = numEpS2), rep(3, times = numEpS3)
#                         , rep(4, times = numEpS4), rep(5, times = numEpS5))
#   for (i in 1 : numSeas){
#     if (i == 1){
#       for(j in 1 : numEpS1){
#         newFileNamesBuild[j] <- str_c(newFileNamesBuild[j], sprintf(".%d", j))
#       }
#     } else if (i == 2){
#         for(j in 1 : numEpS2){
#         newFileNamesBuild[j + numEpS1] <- str_c(newFileNamesBuild[j + numEpS1], sprintf(".%d", j))
#       }
#     } else if (i == 3){
#       for(j in 1 : numEpS3){
#         newFileNamesBuild[j + numEpS1 + numEpS2] <- str_c(newFileNamesBuild[j + numEpS1 + numEpS2], sprintf(".%d", j))
#       }
#     } else if (i == 4){
#       for(j in 1 : numEpS4){
#         newFileNamesBuild[j + numEpS1 + numEpS2 + numEpS3] <- str_c(newFileNamesBuild[j + numEpS1 + numEpS2 + numEpS3], 
#                                                                    sprintf(".%d", j))
#       }
#     } else if (i == 5){
#       for(j in 1 : numEpS5){
#         newFileNamesBuild[j + numEpS1 + numEpS2 + numEpS3 + numEpS4] <- str_c(newFileNamesBuild[j + numEpS1 + numEpS2 + numEpS3 + numEpS4], 
#                                                                    sprintf(".%d", j))
#       }
#     } 
#   }
#   
#   newFileNames <- newFileNamesBuild
#   currentFileNames <- list.files(cwd)
#   for (k in 1 : length(currentFileNames)){
#     if(length(currentFileNames) == length(newFileNames)){
#     file.rename(from = currentFileNames[k], to = newFileNames[k] )
#     } else{
#       print("error!")
#     }
#   }
#   return(newFileNames)
# }
# 
#-----------------------------------------------------------------------------------------------------------

# creating test files---------------------------------------------------------------------------------------

createTestFiles <- function(num_files, directoryPath){
  setwd(directoryPath)
  extensionVec <- c("bmp", "png", "csv", "avi", "mp4", "xml", "mkv")
  for (tstFile in 1 : num_files){
    exten <- sample(extensionVec, 1)
    file.create(sprintf("s%dep%d.%s", sample(1:10, 1), sample(1:10, 1), exten))
  }
  
}

# ----------------------------------------------------------------------------------------------------------

# renaming files in a computer system v2--------------------------------------------------------------------
library(stringr)
library(dplyr)
library(tidyverse)

renameFilesIntoOrderv2 <- function(cwd, numSeas, ...){
  argThreeOnwards <- list(...)
  setwd(cwd)
  exten <- c("")
  if (cwd == ""){
    print("Please enter your current working directory in the function call and try again.")
  }
  newFileNamesBuild <- c()
  count <- 1
  
  for (i in 1 : numSeas){
      newFileNamesBuild[count : (count + argThreeOnwards[[i]] - 1)] <- c(rep(i, times = argThreeOnwards[[i]]))
      newFileNamesBuild[count : (count + argThreeOnwards[[i]] - 1)] <- str_c(newFileNamesBuild[count : (count + argThreeOnwards[[i]] - 1)], 1 : argThreeOnwards[[i]], sep = ".")
      count <- count + argThreeOnwards[[i]]
  }
  
   currentFileNames <- list.files(cwd)
   indexList <- list()
   indexDataFinalTable <- tibble(fileName = NA, season = NA, episode = NA)
   for (l in 1 : length(currentFileNames)){
     indexList[[l]] <- str_extract_all(currentFileNames[l], "(\\d)+")
     indexDataFinalTable[l, ] <- c(currentFileNames[l], indexList[[l]][[1]][1], indexList[[l]][[1]][2])
   }
   
   indexDataFinalTable$season <- as.numeric(indexDataFinalTable$season)
   indexDataFinalTable$episode <- as.numeric(indexDataFinalTable$episode)
   indexDataFinalTable <- arrange(indexDataFinalTable, season, episode)
   
   for (k in 1 : length(indexDataFinalTable$fileName)){
     if(length(indexDataFinalTable$fileName) == length(newFileNamesBuild)){
       extensionVec <- "bmp|png|csv|avi|mp4|xml|mkv"
       exten <- str_extract(indexDataFinalTable$fileName[k], extensionVec) # beautiful
       if(!is.na(exten)){
         file.rename(from = indexDataFinalTable$fileName[k], to = sprintf("%s.%s",newFileNamesBuild[k], exten))  
       } else{
         file.rename(from = indexDataFinalTable$fileName[k], to = newFileNamesBuild[k])
       }
       
     } else{
       print("error!")
     }
   }
   
   ioObjects <- list(currentFileNames, newFileNamesBuild, exten)
  
  return(ioObjects)
 
  # avenues for improvement
   # 1. Automate the counting of episodes per season and seasons.
   # 2. (Done)Automatically Order correctly when reading in from 'list.files()'. 
   # 3. Individual files preffered over folders.
   # 4. Delete R history files.
   # 5. The season might not be the 'second' and episode might not be the 'third'.
   # 6. People might not download all episodes for all seasons. So, function must be more resilient to this concern.
   # 7. Bring any system of files with any level of entropy into order.
   # 8. A measure of before and after 'entropy' in the given directory.
   # 9. Give options for renaming files, e.g. '1.1' or 's1ep1'. Some other options based on how many variety of files are sorted.
}

#----------------------------------------------------------------------------------------------------------

# renaming files into order in the computer system v3------------------------------------------------------

library(dplyr)
library(stringr)
library(tidyverse)

renameFilesIntoOrderv3 <- function(absDirPath){
  setwd(absDirPath)
  filesInDir <- list.files(absDirPath)
  finalDataTable <- tibble(fileName = NA, season = NA, episode = NA, extension = NA)
  # res <- str_extract(filesInDir, "(s|seas|S|Seas|Season|season)(\\d)+([^ ABCFGHJKLMNQRTUVWXYZabcfghjklmnqrtuvwxyz.])+")
  interRes <- str_extract(filesInDir, "(s|seas|S|Seas|Season|season|se|Se)(\\d)+.*")
  finalRes <- str_extract_all(interRes, "(\\d)+")
  extenVec <- str_extract(filesInDir, "bmp|png|csv|avi|mp4|xml|mkv" )
  for(i in 1 : length(filesInDir)){
    finalDataTable[i, ] <- c(filesInDir[i], finalRes[[i]][1], finalRes[[i]][2], extenVec[i])
  }
  finalDataTable$season <- as.numeric(finalDataTable$season)
  finalDataTable$episode <- as.numeric(finalDataTable$episode)
  finalDataTable <- arrange(finalDataTable, season, episode) # Please note that the 'assignment' is crucial, if I had just written
                                                             # the 'arrange' statement and did not assign it to 'finalDataTable' then
                                                             # finalDataTable would be arranged but the arranged version will not be 
                                                             # stored into memory and hence we cannot access it for further operations
                                                             # This means that the data in 'finalDataTable' would still be 'unarranged'.
                                                             # By assigning the 'arrange' statement to finalDataTable, the finalDataTable
                                                             # is both arranged and stored into memory.
  
    newFileNames <- c()
    for(k in 1 : nrow(finalDataTable)){
      if(is.na(finalDataTable$season[k]) && is.na(finalDataTable$episode[k])){
        newFileNames[k] <- finalDataTable$fileName[k]
      } else {
        if(is.na(finalDataTable$extension[k])){
          newFileNames[k] <- str_c(finalDataTable$season[k], finalDataTable$episode[k], sep = ".")
        } else {
          newFileNames[k] <- str_c(finalDataTable$season[k], finalDataTable$episode[k],
                                   finalDataTable$extension[k], sep = ".")
        }
      }
    }
     
    for (j in 1 : nrow(finalDataTable)){
      if(nrow(finalDataTable) != length(newFileNames)){ 
        print("error!")
      } else {
        file.rename(finalDataTable$fileName[j], newFileNames[j])
      }
    }
    
    finalDataTable <- mutate(finalDataTable, newFileName = newFileNames)
    
    return(finalDataTable)
  
  # avenues for improvement
   # 1. ignore case in detecting 'seas', 'season', etc.
   # 2. better and efficient regular expressions(e.g. for the extensions).
   # 3. loose the for loop.
   # 4. aim for a 'The Organizer': Give it 'anything' and will organize it for you.
   # 5. look at the points from earlier version of the function.
   # 6. Make it work for folders.
   # 7. If already correctly named, leave unchanged. If files named as folders (i.e. without extension) either guess extension or provide a default app
   #    to play it. Remove extension, if any in folder names.
   # 8. Let user enter his/her own pattern and use that in list.files as a regular expression.   
   # 9. Look at The Flash '2014.4' folder error. Status: Taken care off. 
    
  # Imp Note: Versioning of code is crucial. Do not try to make all optimizations in version 1. This way your old mistakes get erased.
  # , rather write a quick dirty version 1 and then improve upon it in version 2, and so on, till you get an efficient system. This way
  # you will have documented your progress and see what things are 'not' to be done again.  
}


#----------------------------------------------------------------------------------------------------------
# renameFilesIntoOrderV3 <- function

# delete all files in the computer system------------------------------------------------------------------

killSwitch <- function (directoryPath){
  unlink(directoryPath, recursive = TRUE, force = TRUE)
  
}

#-----------------------------------------------------------------------------------------------------------

# recursive functions in R----------------------------------------------------------------------------------

# v1
library(tidyverse)
factorial <- function(n){
  if (n == 1){
    return(n)
  } else {
    return(n * factorial(n - 1))
  }
}

# v1
countDown <- function(n){ # may serve as a recipe for R fatal error due to very deep nesting in case n is big (e.g. > 1000).
  if(n == 0){             # this can be used to screw up someone's computer for a few seconds.
    return(n)
  } else {
    return(append(n, countDown(n - 1)))
  }
}
counter <- 0
sumCountDown <- function(n, x){
  # counter <- 0
  if(n == (x*(x+1))/2){
    counter <- counter + 1
    return(n)
  } else {
    counter <- counter + 1
    return(append(n, sumCountDown(sum(n, x - counter), x)))
  }
}


printName <- function(name){
  nameSplit <- str_split(name, "")
  nameLength <- length(nameSplit[[1]])
  return(printName(nameSplit[length(nameSplit) - (length(nameSplit - 1))]))
}




#-----------------------------------------------------------------------------------------------------------

# print n nested for loops----------------------------------------------------------------------------------

x <- c(1:10)
y <-sprintf(" for (i in x){...
  
    }")

z <- rep(y, times = 5)

for (i in x){
  print(y)
}

#----------------------------------------------------------------------------------------------------------

# lock permutations print v1
library(stringr)
lockPermutations <- function(inputChoiceVec, keyLength = 4){
inputChoiceVec <- as.numeric(inputChoiceVec)
permutations <- c(NA)

for(i in inputChoiceVec){
  for (j in inputChoiceVec){
    for (k in inputChoiceVec){
      for(l in inputChoiceVec){
        permutations <- append(permutations, str_c(i, j, k, l, sep = ""))
      }
    }
  }
}
permutations <- permutations[2 : length(permutations)]
numPermutations <- length(permutations)
returnObj <- list(permutations, numPermutations)
return(returnObj)
}

#-------------------------------------------------------------------------------------------

# vectorizing a non-vectorized function-----------------------------------------------------



#-------------------------------------------------------------------------------------------

# pixelify----------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------------

# keep the files inside a folder and get rid of the folder----------------------------------


#-------------------------------------------------------------------------------------------

# simulating physical systems---------------------------------------------------------------



#-------------------------------------------------------------------------------------------

# infinite loops and recursion--------------------------------------------------------------

sumCountDown <- function(n){
  if(n == 9){
    return(n)
  } else {
    return(append(n, sumCountDown(n + (n - 1))))
  }
}

#-------------------------------------------------------------------------------------------

# webscrape various pages, convert each page to pdf, combine all into a single pdf----------



#--------------------------------------------------------------------------------------------

# simulating games like chess and monopoly---------------------------------------------------
 # see Monopoly simulation folder
 


#--------------------------------------------------------------------------------------------

# send at a randomly chosen point in time within a certain period and at certain
# intervals reminder via e-mail--------------------------------------------------------------



#--------------------------------------------------------------------------------------------

# brute force attack algorithm---------------------------------------------------------------

#--------------------------------------------------------------------------------------------

# take in data from a physical random process and use it as a seed to generate random numbers
# e.g. monopoly rule book banging on the clip of the clipboard-------------------------------





#--------------------------------------------------------------------------------------------

# Access phone via your computer, use R to access phone camera-------------------------------

#--------------------------------------------------------------------------------------------

# An algorithm to find the shortest way from one point on the map to another given current network of road systemss-----------------

# references: Edsger Dijkstra, implement without referencing.
# ways to tackle
 # 1. The path with the least variance from a center fixed line 



#---------------------------------------------------------------------------------------------

# An algorithm to find the longest way from one point on the map to anothe given you can visit each city once-----------------

# references: Edsger Dijkstra, implement without referencing.
# ways to tackle




#---------------------------------------------------------------------------------------------

# Cash counter Algorithm v1---------------------------------------------------------------------------------
cashBalanceDen <- function(billAmt, cashRecieved){
cashBalance <- cashRecieved - billAmt  
library(tidyverse)
library(dplyr)
denominations <- c(2000, 500, 200, 100, 50, 20, 10, 5, 2, 1)
counter <- c(rep(0, times = length(denominations)))

for (den in 1 : length(denominations)){
  if(cashBalance %% denominations[den] != cashBalance){
    if(cashBalance %% denominations[den] == 0){
      counter[den] <- counter[den] + cashBalance %/% denominations[den]
      cashBalance <- cashBalance - cashBalance
      break
    } else {
      counter[den] <- counter[den] + cashBalance %/% denominations[den]
      cashBalance <- cashBalance - ((cashBalance %/% denominations[den]) * denominations[den])
      cashRecieved <- cashBalance
      billAmt <- 0
      cashBalanceDen(billAmt, cashRecieved) # beautiful recursion
    }
  } else {
    next
  }
}
denCount <- tibble(x = denominations, y = counter)
return(denCount)

# additions to be made to the algorithm
 # alternative solutions (use the initial solution and from that calculate alternative solutions).
 # inventory
 # user interface (ask question on terminal)
}

#----------------------------------------------------------------------------------------------

# tic tac toe---------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------

# Animations: Linear transformations, etc.-----------------------------------------------
 # plotly, tweenr, gganimate, graphics packages looks promising
 
#---------------------------------------------------------------------------------------------

# generating fractals-------------------------------------------------------------------------

# Sierpinski triangle version 1
currentPlot <- c()
vertex1 <- c(0, 0)
vertex2 <- c(30, 0)
vertex3 <- c(15, 40)
xMax <- max(vertex1[1], vertex2[1], vertex3[1])
xMin <- min(vertex1[1], vertex2[1], vertex3[1])
yMax <- max(vertex1[2], vertex2[2], vertex3[2])
yMin <- min(vertex1[2], vertex2[2], vertex3[2])

plot(c(xMin - 1, xMax + 1), c(yMin - 1, yMax + 1), type = "n")

sierpinskiTriangle <- function(vertex1, vertex2, vertex3, delta, midPointCounter){
  if((length(vertex1) + length(vertex2) + length(vertex3)) != 6){
    print("Error: Please make sure that each vertex has exactly 2 coordinates.")
  } else{
    if(delta == 0){
      return(currentPlot)
    } else {
      # xMax <- max(vertex1[1], vertex2[1], vertex3[1])
      # xMin <- min(vertex1[1], vertex2[1], vertex3[1])
      # yMax <- max(vertex1[2], vertex2[2], vertex3[2])
      # yMin <- min(vertex1[2], vertex2[2], vertex3[2])
      xCoords <- c(vertex1[1], vertex2[1], vertex3[1])
      yCoords <- c(vertex1[2], vertex2[2], vertex3[2])
    #  plot(c(xMin - 1, xMax + 1), c(yMin - 1, yMax + 1), type = "n")
      currentPlot <- polygon(xCoords, yCoords)
      par(new = TRUE)
      vertex1Tmp <- c((vertex1[1] + vertex2[1])/ 2, (vertex1[2] + vertex2[2])/2)
      vertex2Tmp <- c((vertex2[1] + vertex3[1])/ 2, (vertex2[2] + vertex3[2])/2)
      vertex3Tmp <- c((vertex1[1] + vertex3[1])/ 2, (vertex1[2] + vertex3[2])/2)
      vertex1 <- vertex1Tmp
      vertex2 <- vertex2Tmp
      vertex3 <- vertex3Tmp
      delta <- delta - 1
      sierpinskiTriangle(vertex1, vertex2, vertex3, delta)
    }
  }
}

# Sierpinski triangle chaos game version (R bloggers)

p <- c(0, 500, 1000)
q <- c(0, 1000, 0)
x11()
par(mar = rep(0, 4))
plot(p, q, col= "red", pch = 15, cex = 1, axes = FALSE)

# Random starting point
x <- sample(0:1000, 1)
y <- sample(0:1000, 1)

# Chaos game
for (i in 1:10000) {
  Sys.sleep(.001)
  n <- sample(1:3, 1)
  x <- floor(x + (p[n] - x) / 1.7)
  y <- floor(y + (q[n] - y) / 1.7)
  points(x, y, pch = 15, cex = 0.5)
}

# Sierpinski triangle (TurtleGraphics package: https://cran.r-project.org/web/packages/TurtleGraphics/vignettes/TurtleGraphics.pdf) 
library(TurtleGraphics)
turtle_init()
turtle_forward(distance = 30)
turtle_backward(distance = 10)
turtle_right(angle = 90)
turtle_forward(distance = 10)
turtle_left(angle = 135)
turtle_forward(dist=14)
turtle_left(angle=90)
turtle_forward(dist=14)
turtle_left(angle=135)
turtle_forward(dist=10)
turtle_show()
turtle_right(90)
turtle_up()
turtle_right(90)


# general chaos game function 
chaosGame <- function(xCoords, yCoords, iterations, divDistBy, sleepFor, colour){
  x11()
  par(mar = rep(0, times = 4))
  plot(xCoords, yCoords, col = "red", cex = 1, pch = 15, axes = FALSE)
  
  # random starting point
  xStart <- sample(1 : max(xCoords), 1)
  yStart <- sample(1 : max(yCoords), 1)
  
  # chaos game
  for(i in 1 : iterations){
    Sys.sleep(sleepFor)
    rndInitVer <- sample(1 : length(xCoords), 1)
    xStart <- (xStart + xCoords[rndInitVer])/divDistBy
    yStart <- (yStart + yCoords[rndInitVer])/divDistBy
    if(colour == TRUE){
      points(xStart, yStart, cex = 0.5, pch = 15, col = sample(colours(), 1)) 
    } else {
      points(xStart, yStart, cex = 0.5, pch = 15)      
    }

  }
}

#---------------------------------------------------------------------------------------------

# genarate n-gon------------------------------------------------------------------

#---------------------------------------------------------------------------------------------

# Frobenius numbers (chicken mc nuggets),  calculate if one can make a number greater than 43 with 6, 9, 20.


#----------------------------------------------------------------------------------------------------------

# adding up all the 'digits' from 1 to 1 million (v1)
library(stringr)
library(tidyverse)
addDigitsV1 <- function(number){
  x <- 0
  tmp <- 0
  numLength <- 0
  numStrip <- list()
  for (i in 1 : number){
    numStrip <- str_split(i, "")
    numLength <- length((str_split(i, ""))[[1]])
    for(j in 1 : numLength){
      tmp <- tmp + as.numeric(numStrip[[1]][j])
    }
    x <- x + tmp
    tmp <- 0
  }
  return(x)
}

# addDigits v2 (using gauss's trick: clever + superfast compared to addDigitsV1, in case of number = 10000, approx 200x faster. Great!!, upcoming v3)
addDigitsV2 <- function(number){
  sumNumberMinusOne <- 0
  sumNumber <- 0
  numPairs <- number/2
  splNum <- str_split(number - 1, "")
  for(m in 1 : length(splNum[[1]])){
    sumNumberMinusOne <- sumNumberMinusOne + as.numeric(splNum[[1]][m])
  }
  
  intermedNum <- numPairs * (sumNumberMinusOne)
  splitNumber <- str_split(number, "")
  for(l in 1 : length(splitNumber[[1]])){
    sumNumber <- sumNumber + as.numeric(splitNumber[[1]][l]) 
  }
  finalNum <- intermedNum + sumNumber
  return(finalNum)
}

preV1 <- Sys.time()
addDigits(10000)
postV1 <- Sys.time()
diffV1 <- postV1 - preV1

preV2 <- Sys.time()
addDigitsV2(10000)
postV2 <- Sys.time()
diffV2 <- postV2 - preV2

    # make digit sum data table for 1 to 'n' digits
    digSumData <- function(num){
      digSumTable <- tibble(x = NA, y = NA)
      for(j in 1 : num){
        digSumTable[j, 1] <- j
        digSumTable[j, 2] <- addDigits(j)
      }
      return(digSumTable)
    }
ggplot(data = tab) + geom_point(mapping = aes(x = x, y = y, col = y))

# Sierpinski Triangle (R turtle graphics package code)---------------------------------------------------
drawTriangle <- function(points) {
  turtle_setpos(points[1,1], points[1,2])
  turtle_col(sample(colors(), 1))
  turtle_goto(points[2,1], points[2,2])
  turtle_col(sample(colors(), 1))
  turtle_goto(points[3,1], points[3,2])
  turtle_col(sample(colors(), 1))
  turtle_goto(points[1,1], points[1,2])
}
getMid <- function(p1, p2)
  (p1+p2)*(1/3)
sierpinski <- function(points, degree){
  drawTriangle(points)
  if (degree > 0) {
    p1 <- matrix(c(points[1,], getMid(points[1,], points[2,]),
                   getMid(points[1,], points[3,])), nrow=3, byrow=TRUE)
    sierpinski(p1, degree-1)
    p2 <- matrix(c(points[2,], getMid(points[1,], points[2,]),
                   getMid(points[2,], points[3,])), nrow=3, byrow=TRUE)
    sierpinski(p2, degree-1)
    p3 <- matrix(c(points[3,], getMid(points[3,], points[2,]),
                   getMid(points[1,], points[3,])), nrow=3, byrow=TRUE)
    sierpinski(p3, degree-1)
  }
  invisible(NULL)
}
turtle_init(520, 500, "clip")
turtle_do({
  p <- matrix(c(10, 10, 510, 10, 250, 448), nrow=3, byrow=TRUE)
  turtle_col("red")
  sierpinski(p, 6)
  turtle_setpos(250, 448)
})


#-----------------------------------------------------------------------

# Koch fractal----------------------------------------------------------
library(TurtleGraphics)
koch <- function(s=50, n=6) { # if want to clearly understand/recall how recursion works in R, try: turtle_init() -> turtle_backward(40) -> koch(50, 2) and then analyze results.
  if (n <= 1) {
    turtle_forward(s)
    print(sprintf("turtle_forward(%f)", s))
  }
  else {
    koch(s/3, n-1)
    print(sprintf("koch(%f, %f)", s/3, n-1))
    turtle_left(60)
    print(sprintf("turtle_left(60)"))
    koch(s/3, n-1)
    print(sprintf("koch(%f, %f)", s/3, n-1))
    turtle_right(120)
    print(sprintf("turtle_right(120)"))
    koch(s/3, n-1)
    print(sprintf("koch(%f, %f)", s/3, n-1))
    turtle_left(60)
    print(sprintf("turtle_left(60)"))
    koch(s/3, n-1)
    print(sprintf("koch(%f, %f)", s/3, n-1))
  }
}
turtle_init()
turtle_setpos(50, 5)
turtle_do(koch(50, 3))

# Sierpinski's carpet-------------------------------------------------
drawSquare <- function(points){
  turtle_setpos(points[1, 1], points[1, 2])
  turtle_goto(points[2, 1], points[2, 2])
  turtle_goto(points[3, 1], points[3, 2])
  turtle_goto(points[4, 1], points[4, 2])
  turtle_goto(points[1, 1], points[1, 2])
}

drawSquarePartial <- function(points){
  turtle_setpos(points[1, 1], points[1, 2])
  turtle_up()
  turtle_goto(points[2, 1], points[2, 2])
  turtle_down()
  turtle_goto(points[3, 1], points[3, 2])
  turtle_up()
  turtle_goto(points[4, 1], points[4, 2])
  turtle_down()
  turtle_goto(points[1, 1], points[1, 2])
}


goToFracDist <- function(p1, p2, frac){
  if((length(p1) != 2) || (length(p2) != 2)){
    print("Each point should be of the form c(x, y) where x and y are real numbers.")
  } else {
    if(p1[1] == p2[1]){
      tmp <- (p1 + p2) * (frac)
      tmp <- c(p1[1], tmp[2])
      return(tmp)
    } else {
      tmp <- (p1 + p2) * (frac)
      tmp <- c(tmp[1], p1[2])
      return(tmp)
    }
  }
}

sierpinskiCarpet <- function(points, degree){
  drawSquarePartial(points)
  if(degree > 0 ){
    p1 <- matrix(c(goToFracDist(points[1, ], points[2, ], 1/3),
                   goToFracDist(points[1, ], points[2, ], 2/3),
                   (goToFracDist(points[1, ], points[2, ], 2/3))[1] + 0,
                   (goToFracDist(points[1, ], points[2, ], 2/3))[2] + (1/3*(points[2, 1] - points[1, 1])), 
                   (goToFracDist(points[1, ], points[2, ], 1/3))[1] + 0,
                   (goToFracDist(points[1, ], points[2, ], 1/3))[2] + (1/3*(points[2, 1] - points[1, 1]))),
                   nrow = 4, byrow = TRUE)
    sierpinskiCarpet(p1, degree - 1)
    p2 <- matrix(c(goToFracDist(points[2, ], points[3, ], 1/3),
                   goToFracDist(points[2, ], points[3, ], 2/3),
                   (goToFracDist(points[2, ], points[3, ], 2/3))[1] - (1/3*(points[2, 1] - points[1, 1])),
                   (goToFracDist(points[2, ], points[3, ], 2/3))[2] + 0, 
                   (goToFracDist(points[2, ], points[3, ], 1/3))[1] - (1/3*(points[2, 1] - points[1, 1])),
                   (goToFracDist(points[2, ], points[3, ], 1/3))[2] + 0),
                 nrow = 4, byrow = TRUE)
    
    sierpinskiCarpet(p2, degree - 1)
    
    p3 <- matrix(c(goToFracDist(points[3, ], points[4, ], 1/3),
                   goToFracDist(points[3, ], points[4, ], 2/3),
                   (goToFracDist(points[3, ], points[4, ], 2/3))[1] + 0,
                   (goToFracDist(points[3, ], points[4, ], 2/3))[2] - (1/3*(points[2, 1] - points[1, 1])), 
                   (goToFracDist(points[3, ], points[4, ], 1/3))[1] + 0,
                   (goToFracDist(points[3, ], points[4, ], 1/3))[2] - (1/3*(points[2, 1] - points[1, 1]))),
                 nrow = 4, byrow = TRUE)
    
    sierpinskiCarpet(p3, degree - 1)
    
    p4 <- matrix(c(goToFracDist(points[4, ], points[1, ], 1/3),
                   goToFracDist(points[4, ], points[1, ], 2/3),
                   (goToFracDist(points[4, ], points[1, ], 2/3))[1] + (1/3*(points[2, 1] - points[1, 1])),
                   (goToFracDist(points[4, ], points[1, ], 2/3))[2] + 0, 
                   (goToFracDist(points[4, ], points[1, ], 1/3))[1] + (1/3*(points[2, 1] - points[1, 1])),
                   (goToFracDist(points[4, ], points[1, ], 1/3))[2] + 0),
                 nrow = 4, byrow = TRUE)
    
    sierpinskiCarpet(p4, degree - 1)
    
  } else {
    invisible(NULL)
  }
}

turtle_init(520, 500, "clip")
points <- matrix(c(50, 50, 450, 50, 450, 450, 50, 450), nrow=4, byrow=TRUE)
drawSquare(points)
drawSquarePartial(points)



turtle_init(520, 500, "clip")
points <- matrix(c(50, 50, 450, 50, 450, 450, 50, 450), nrow=4, byrow=TRUE)
drawSquarePartial(points)
# turtle_right(45)
# turtle_up()
# turtle_forward(400*(sqrt(2)/3))
# turtle_left(45)
# turtle_down()
# turtle_forward(400/3)
# turtle_right(90)
# turtle_forward(400/3)
# turtle_right(90)
# turtle_forward(400/3)
# turtle_right(90)
# turtle_forward(400/3)

#----------------------------------------------------------------------

# Fractal tree (Turtle graphics vignette code)--------------------------

#----------------------------------------------------------------------

# 3D Sierpinski's carpet (cube), Sierpinski's triangle (tetrahedron), other fractals, spirals-----

#-----------------------------------------------------------------------

# write quicksort, mergesort, etc using recursion------

#--------------------------------------------------

# General converter from decimal to other number systems using recursion------
library(stringr)
binToDecConverterV2 <- function(binaryInput){ # status: R approximates 100111111111111 to be 1e+14, which is a problem: Fix it, otherwise fine.
  decAns <- 0
  tmpList <- str_split(as.character(binaryInput), pattern = "")
  if(str_detect(str_c(tmpList[[1]], collapse = ""), pattern =  "e")){
    tmpList[[1]] <- str_c(tmpList[[1]], collapse = "")
    numberAfterE <- str_extract(tmpList[[1]], "(\\+|-)\\d+")
    numberAfterE <- as.numeric(str_extract(numberAfterE, "\\d+"))
    numbersBeforeEAfterDot <- str_extract(tmpList[[1]], "(\\.(\\d+)e)|(\\de)")
    numbersBeforeEAfterDot <- str_extract(numbersBeforeEAfterDot, "\\d+")
    
    numberBeforeEAfterDot <- str_extract(numbersBeforeEAfterDot, "\\d")
    
    lengthNumBeforeEAfterDot <- str_split(numbersBeforeEAfterDot, pattern = "")
    lengthNumBeforeEAfterDot <- length(lengthNumBeforeEAfterDot[[1]])
    
    if(lengthNumBeforeEAfterDot == 1 && !(is.na(lengthNumBeforeEAfterDot))){
      tmpList[[1]] <- c("1", rep("0", times = numberAfterE))
      tmpList[[1]] <- as.numeric(tmpList[[1]])
      for(i in (1 : length(tmpList[[1]]))){
        decAns <- decAns + ((2^(length(tmpList[[1]]) - i)) * (tmpList[[1]][i]))
        
      }
      
    } else {
      tmpList[[1]] <- c("1", rep(numberBeforeEAfterDot, times = numberAfterE))
      tmpList[[1]] <- as.numeric(tmpList[[1]])
      for(i in (1 : length(tmpList[[1]]))){
        decAns <- decAns + ((2^(length(tmpList[[1]]) - i)) * (tmpList[[1]][i]))
        
      }
      
    }
    
  } else {
    tmpList[[1]] <- as.numeric(tmpList[[1]])
    for(i in (1 : length(tmpList[[1]]))){
      decAns <- decAns + ((2^(length(tmpList[[1]]) - i)) * (tmpList[[1]][i]))
      
    }
    
  }
  return(decAns)
}

genConvToDecV1 <- function(){
  
}

decToBinConverterV1 <- function(decInput){
  residual <- decInput
  highestPower <- floor(log2(decInput))
  binAns <- c(rep(0, times = highestPower + 1))
  for(i in highestPower : 0){
    if(2^(i) > residual){
      next
    } else {
      binAns[i + 1] <- 1
      residual <- residual - (2^i)
      if(residual == 0){
        break
      }
    }
  }
  return(rev(binAns))
}

genConvFromDecV1 <- function(decInput, baseNum){ # converts from decimal to other bases (not that general, upcoming both way converter.)
  if(decInput == 0 || baseNum == 0){
    return(0)
  } else {
    if(baseNum == 1){
      print("In base 1, there are infinite ways to represent the decInput.\n
            One of them is the following:")
      return(rep(1, times = decInput))
    }
    residual <- decInput
    highestPower <- floor(logb(decInput, base = baseNum))
    newBaseAns <- c(rep(0, times = highestPower + 1))
    for(i in highestPower : 0){
      if(baseNum^i > residual){
        next
      } else {
        tmpDivisor <- residual %/% (baseNum^i)
        residual <- residual %% (baseNum^i)
        newBaseAns[i + 1] <- tmpDivisor
        if(residual == 0){
          break
        }
      }
    }
    return(rev(newBaseAns))
    
  }
}    
  
genConvFromDecV2 <- function(decInput, baseNum){ # Uses letters A-z, A = 10, B = 11, ..., Z = 35.
                                                 # Status: R approximates large numbers, fix it.    
  if(decInput == 0 || baseNum == 0){
    return(0)
  } else {
    if(baseNum == 1){
      print("In base 1, there are infinite ways to represent the decInput.\n
            One of them is the following:")
      return(rep(1, times = decInput))
    } else if (baseNum > 10){
      residual <- decInput
      highestPower <- floor(logb(decInput, base = baseNum))
      newBaseAns <- c(rep(0, times = highestPower + 1))
      for(i in highestPower : 0){
        if(baseNum^i > residual){
          next
        } else {
          tmpDivisor <- residual %/% (baseNum^i)
          residual <- residual %% (baseNum^i)
          if(tmpDivisor >= 10){
            newBaseAns[i + 1] <- LETTERS[(tmpDivisor %% 10) + 1]
          } else {
            newBaseAns[i + 1] <- tmpDivisor 
          }
          if(residual == 0){
            break
          }
        }
      }
      
    } else {
      residual <- decInput
      highestPower <- floor(logb(decInput, base = baseNum))
      newBaseAns <- c(rep(0, times = highestPower + 1))
      for(i in highestPower : 0){
        if(baseNum^i > residual){
          next
        } else {
          tmpDivisor <- residual %/% (baseNum^i)
          residual <- residual %% (baseNum^i)
          newBaseAns[i + 1] <- tmpDivisor
          if(residual == 0){
            break
          }
        }
      }
    }
    
    return(rev(newBaseAns))
    
  }
}    

# calculator for any number system---------------------------------------

#------------------------------------------------------------------------

# Fibonacci sequence using recursion------------------

#-----------------------------------------------------

# Calculating relative distances for different scales of the universe (e.g. How far would an ant's london will be from Gandhi Nagar?)-------

#---------------------------------------------------------------------------------

# Converting a photograph to rgb values/binary representation/hexadecimal representation------------------

#--------------------------------------------------------------------
                                 
# progrmattic Art, e.g.  ##----------------------------------------
#                         # 
#                        ####            
#------------------------------------------------------------------
# 3D plot in in R-------------------------------------------------

#--------------------------------------------------

# Cryptography-----------------------------------

#-------------------------------------------------

# Monte Carlo Methods-----------------------
 # refer: https://www.r-bloggers.com/probability-and-monte-carlo-methods/
 # refer MonteCarlo package
#-----------------------------------------

# Mandelbrot set, Filled Julia set--------------------

 # refer: http://rtricks.blogspot.com/2007/04/mandelbrot-set-with-r-animation.html
 #         https://leonjessen.wordpress.com/2015/11/03/plot-the-mandelbrot-set-using-r/ (contains explanation of code).
#-----------------------------------------------------

# Animation in general------------------------------
 # Useful functions: image, write.gif 
#----------------------------------------------------

# Candle burning problem---------------------

#---------------------------------------------


# frequency analysis [Faulty]
library(readr)
library(stringr)
library(ggplot2)
library(tidyverse)
library(dplyr)

freqAnalysis <- function(pathToDocument){
  sampleDoc <- readtext::readtext(file = pathToDocument)
  alphFreq <- tibble(alphabet = LETTERS)
  for(i in 1 : length(LETTERS)){
    alphFreq[i, 2] <- str_count(sampleDoc$text, LETTERS[i])
  }
  colnames(alphFreq)[2] <- c("freq")
  print(ggplot(data = alphFreq) + geom_col(mapping = aes(x = alphabet, y = freq)) + ggtitle(pathToDocument)) # use print when 
                                                                                # you want to print the 
                                                                                # graph directy from a 
                                                                                # function call  
  totalNumberOfAlphabets <- sum(alphFreq$freq)
  return(totalNumberOfAlphabets)
}

# prompting user for input, processing that, returning output-------------------



# Outputting beautiful Regression tables in PDF/word/html/LaTeX ------------------------------------------
# Markdown code ---
# If want to see output in console, use, type = "text" in 'stargazer' command.
# If want to see output in HTML, use, type ="html", and out = "fileName.html"
# If want to see output in PDF, use below R Markdown code. Using, type = "latex" and out = "fileName.pdf" alone...
# ... may not work in R Script, it will output only LaTeX code which one can then only run in MikTeX console...
# ... (I am trying to figure out the specifics for this)

#   title: "test"
# author: "Aarsh Batra"
# date: "11 September 2018"
# output: pdf_document
# ---
#   
#   ```{r setup, include=FALSE}
# knitr::opts_chunk$set(echo = TRUE)
# library(stargazer)
# ```
# 
# ```{r outputTable, results = "asis"}
# source("mod3Code.R")
# stargazer(model1, model2, model3, model4, type = "latex", column.sep.width = "1pt", font.size = "scriptsize", no.space = TRUE)
# Comment: I uses 'font.size' to shrink the output so that it can fit on the pdf (I am figuring out
# a better way to do this). 
# ```

#------------------------- Genoeconomics Practice code---------------------------------------------------------------------------

#--references
# 1. https://genome.sph.umich.edu/wiki/Code_Sample:_Generating_QQ_Plots_in_R
# 2. https://data.library.virginia.edu/understanding-q-q-plots/
# 3. https://rdrr.io/cran/gap/man/qqunif.html ('gap' Genetic analysis package).
# 4. http://www.cookbook-r.com/Graphs/Q-Q_plot/
# 5. https://homepage.divms.uiowa.edu/~luke/classes/STAT4580/qqpp.html

#-- Questions

# Q1. In our analysis, how would you interpret the 45 degree line? Qualitatively, what does the
#     deviation of a data trend from the 45 degree line in your graph signify? 

# ans.The 45 degree line is the reference line. If all points in a QQ plot lie on the 45 degree line
#     this would imply that the distribution of the experimental CDF of p-values is exactly the same as
#     the theoretical  distributuion (in our case the, uniform distribution) we are comparing to. So, the 
#     45 degree line implies that the distribution of experimental values is 'exactly' the same as the 
#     theoretical distribution.


# Q2. In a few sentences, provide a justification for why we should never see monotonic decreases
#     in the trend for any QQ plot, regardless of the data of being compared.

#  ans. The data on both x and y axis is ascendingly ordered. As we go from left to right on the x axis (or bottom to top
#       on the y axis) in a QQ plot, we are covering a larger and larger fraction of the dataset. So, for
#       e. g. if a point lies at a distance 'd' (let's call the point 'd') from the origin on the x axis, and 
#       let us assume that 'x%' of the data lies to the left of 'd'. Then as we go an additional unit to the right of 'd'
#       'd + 1'. Then, it is necessary that the percentage of data that lies to the left of 'd + 1' is
#       greater than or equal to 'x%'. Similar explanation applies to the y axis.

#       Given the above argument, if we have a point (x, y) on the xy-plane where 'x' corresponds to a 
#       point 'd'(with x% of data to the left of d on x axis) on the x axis. 'y' corresponds to a point 'c' (with y% of data 
#       below c on the y axis). Then a montonous decrease would imply going South East to (x, y). This means 
#       going to the right of 'd' on the x axis and below 'c' on the y axis.



# Code
#Fake sample data
my.pvalues = runif(100)

library(pwr)

# Calculate expectations
exp.pvalues <- (rank(my.pvalues, ties.method="first")+.5)/(length(my.pvalues)+1)

#Make plot
df <- data.frame(exp.pvalues.lab = exp.pvalues, my.pvalues.lab = my.pvalues)
ggplot(data = df) + geom_jitter(mapping = aes(x = -log10(exp.pvalues.lab), y = -log10(my.pvalues.lab))) + geom_abline(intercept = 0, slope = 1)
abline(0, 1, col = "blue" , lwd = 2)

#--------------
df <- data.frame(x = x, y = -log10(x))
plt <- ggplot(data = df) + geom_jitter(mapping = aes(x = x, y = y))
ggplot(data = df) + geom_jitter(mapping = aes(x = x, y = y)) + geom_abline(intercept = 0, slope = 1)

# Adds legend manually
ggplot(data = df) + geom_point(mapping = aes(x = x, y = y, colour = "ExperimentalValues")) + geom_point(mapping = aes(x = x^2, y = y^2, colour = "TheoreticalValues")) + geom_abline(slope = 1, intercept = 0, mapping = aes(colour = "ReferenceLine")) + scale_colour_manual(name = "", values = c(TheoreticalValues = "red", ExperimentalValues = "blue", ReferenceLine = "black"))
ggplot(data = df) + geom_point(mapping = aes(x = x, y = y, colour = "red")) + geom_point(mapping = aes(x = x^2, y = y^2, colour = "blue")) + geom_abline(slope = 1, intercept = 0, colour = "black")

# simulation, variability in Normal distribution plots
loopNum <- 1000
plt <- list()
tb <- tibble(expData = rnorm(1000)) 
plt[[1]] <- ggplot(data = tb)
for(i in 1 : loopNum){
  plt[[i + 1]] <- plt[[i]] + geom_qq(mapping = aes(sample = expData))
}

#----------------------------------------------------------------------------------------------

# Hypothesis testing---------------------------------------------------------------------

# R code Dafss pset8.

#multivariable regression
multi <- lm(lwage ~ yrs_school + ttl_exp, data = nlsw88)
summary(multi) # show results
anova_unrest <- anova(multi)

#Restricted model
nlsw88$newvar <- nlsw88$yrs_school + 2*nlsw88$ttl_exp
restricted <- lm(lwage ~ newvar, data = nlsw88)
summary(restricted) # show results
anova_rest <- anova(restricted)

#Test
statistic_test <- (((anova_rest$`Sum Sq`[2]-anova_unrest$`Sum Sq`[3])/1)
                   /((anova_unrest$`Sum Sq`[3])/anova_unrest$Df[3]))
statistic_test
pvalue <- df(statistic_test, 1, anova_unrest$Df[3])
pvalue

matrixR <- c(0, -2, 1)
linearHypothesis(multi, matrixR)

# Power Calculations---------------------------------------------------------------------

# refer: https://moderndata.plot.ly/power-curves-r-plotly-ggplot2/
# refer ?pwr.t2n.test()

#--------------------------------------------------------------------------------------

# t distriution simulation----------------------------------------------------------------

sampleSize <- 10
iter <- 1000000
meanVec <- c()
for (i in 1 : iter){
  tmp <- sample(rt(100, 9), sampleSize)
  meanVec[i] <- mean(tmp)
}

df <- tibble(meanVal = meanVec)
df <- mutate(df, tVal = (meanVal - (mean(meanVal)))/(sd(meanVal)/(sqrt(sampleSize))))
hist(df$tVal)
p <- ggplot(data = df) + geom_histogram(mapping = aes(x = tVal))
plotly::ggplotly(p) # interactive plot
#------------------------------------------------------------------------------------------

#-- controlling Google sheets with R 'googlesheets' package-------------------------------------------------------------
# 1. refer vignette: https://cran.r-project.org/web/packages/googlesheets/vignettes/basic-usage.html

install.packages("googlesheets")
library(googlesheets)
library(dplyr)

sheets <- googlesheets::gs_ls()
class(sheets)
View(sheets)
sheets %>% glimpse()
gs_gap() %>% gs_copy(to = "Gapminder")

gs_ls() # Step 1: list all sheets
playSheet <- gs_title("Play") # Step 2: Register sheet. Sheets need to be registered before they can be used.
gs_ws_ls(playSheet) # Step 3: inspect a sheet
wsToDataFrame <- playSheet %>% gs_read(ws = "Sheet1") # Step 4: read ws data into a R data frame for manipulation
View(wsToDataFrame) # Step 5: View R data frame
wsToDataFrame <- wsToDataFrame %>% select(X7:X12) # Step 6: Manipulate R Data Frame.
playSheet <- playSheet %>% gs_ws_new(ws_title = "dataSubset", input = wsToDataFrame) # Add the new data as a new worksheet to the worksheet registered in step 2.


#------------------------------------------------------------------------------------------

# Aproximating Pi with Squares, Circles, Random points and Counting-------------------------


#------------------------------------------------------------------------------------------

# How many square root hits does it take to get any number to 1?---------------------------

sqRootHitToOne <- function(numbers){
  
  originalNumVec <- numbers
  counterVec <- c(rep(0, times = length(numbers)))
  returnList <- list()
  for (i in 1 : length(numbers)){
    if(numbers[i] <= 1){
      message("Please retry and enter only positive real number(s) that are greater than 1.")
      break
    } else {
      while(numbers[i] > 1){
        numbers[i] <- sqrt(numbers[i])
        counterVec[i] <- counterVec[i] + 1
      } 
      
    }
    
  }
  
  returnList[[1]] <- originalNumVec
  returnList[[2]] <- numbers
  returnList[[3]] <- counterVec
  return(returnList)

}

tb <- tibble(x = sample(c(1 : 100000), 1000))
View(tb)
tb <- mutate(tb, col = sqRootHitToOne(x))

# naming the columns of a unnamed matrix------------------------------------------------------

library(dplyr)
library(tidyverse)
library(stringr)

dt <- rnorm(1000000, 0, 1)
df <- matrix(dt, 1000, 1000)
View(df)
hist(diag(df))

View(df)

colnames(df)
cN <- paste(c(1:1000))
colnames(df) <- cN
colnames(df)

#-------------------------------------------------------------------------------------------------

# summing the diagonal of a matrix that goes from bottom left to top right-------------------------

sumOfDiag <- 0
for(i in 1 : nrow(df)){
  tmp <- df[i, (ncol(df) - (i - 1))]
  sumOfDiag <- sumOfDiag + tmp
}
#-------------------------------------------------------------------------------------------------

#-- Gambling Simulations + animations (code works, have fun!)

# Setup: If 'H' gain 2 dollars, if tails loose 1 dollar. Start with 0 dollars. 1 is 'H', 0 is 'T'.

# function for generating simulation data
gamblingSim <- function(nIter, hWinAmt, tLooseAmt){
  
  acBalance <- c(rep(0, times = nIter))
  
  for(i in 1 : nIter){
    toss <- sample(c(0, 1), 1)
    if(i == 1){
      if(toss == 1){
        acBalance[i] <- acBalance[i] + hWinAmt
      } else {
        acBalance[i] <- acBalance[i] - tLooseAmt
      }
    } else {
      if(toss == 1){
        acBalance[i] <- acBalance[i - 1] + hWinAmt
      } else {
        acBalance[i] <- acBalance[i - 1] - tLooseAmt
      }
      
    }
  }
  
  return(acBalance)
}

returnLastTerm <- function(dataset){
  return(dataset[nrow(dataset)])
}

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

# visualization 1: a simple ggplot for toss v/s current balance
returnData <- gamblingSim(100, 2, 1)
df <- tibble(numberOfTosses = c(1 : nIter), accountBalance = returnData)
ggplot(data = df) + geom_line(mapping = aes(x = numberOfTosses, y = accountBalance))

# animation nobs
  # refer: 1. https://plot.ly/r/animations/
  #        2. https://plot.ly/r/cumulative-animations/
nToss <- 100
nAnimFrames <- 1000 
nIter <- 100
winAmt <- 10
looseAmt <- 1
counter <- 0

# animation Dataset generation  
animationData <- tibble(accountBalance = c(rep(0, times = (nIter * nAnimFrames))))
frameNumbers <- c(rep(0, times = (nIter * nAnimFrames)))
for(j in 1 : nAnimFrames){
  animationData$accountBalance[(counter + 1) : (counter + 100)] <- gamblingSim(nIter, winAmt, looseAmt)
  frameNumbers[(counter + 1) : (counter + 100)] <- c(rep(j, times = nIter))
  counter <- counter + 100
}
animationData <- animationData %>% mutate(frameNum = frameNumbers)
# animationData <- animationData %>% mutate(numberOfCoinTosses = c(1 : (nIter * nToss)))


View(animationData)
# plt <- plot_ly(animationData, x = ~numberOfCoinTosses, y = ~accountBalance, frame = ~frameNumbers)
# plt

# choosing the statistic to be animated (final for each frameNum)
byFrame <- dplyr::group_by(animationData, frameNum)
animationDataGroupedByFrame <- summarize(byFrame, finalAcBalByFrame = accountBalance[nIter])
View(animationDataGroupedByFrame)
plt1 <- plot_ly(animationDataGroupedByFrame, x = ~frameNum, y = ~finalAcBalByFrame, frame = ~frameNum) 
plt2 <- ggplot(data = animationDataGroupedByFrame, mapping = aes(x = frameNum, y = finalAcBalByFrame)) + geom_path() + transition_states(frameNum, transition_length = 1, state_length = 1)
d <- animationDataGroupedByFrame %>% accumulate_by(~frameNum)
plt3 <- plot_ly(d, x = ~frameNum, y = ~finalAcBalByFrame, frame = ~frameNum) 
plt3
plt2
plt4 <- d %>%
  plot_ly(
    x = ~frameNum, 
    y = ~finalAcBalByFrame,
    frame = ~frame, 
    type = 'scatter',
    mode = 'lines', 
    line = list(simplyfy = F)
  ) %>% 
  layout(
    xaxis = list(
      title = "simNum",
      zeroline = F
    ),
    yaxis = list(
      title = "finalAcBal",
      zeroline = F
    )
  ) %>% 
  animation_opts(
    frame = 100, 
    transition = 0, 
    redraw = FALSE
  ) %>%
  animation_slider(
    hide = T
  ) %>%
  animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  )
plt4
ggplotly(plt3)

#-- More animations using gganimate-----------------------------------------------------------------
# devtools::install_github('thomasp85/gganimate'); available only on GitHub as on October, 20, 2018
# refer: https://www.r-graph-gallery.com/animation/
library(gganimate)
library(gapminder)

plt <- ggplot(data = gapminder) + geom_point(mapping = aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) + facet_wrap(~continent) + scale_x_log10() + transition_time(year)
plt
#-----------------------------------------------------------------------------------------------

remainderWhenDivByThree <- c()
for(i in 1 : 1000){
  remainderWhenDivByThree[i] <- i %% 3
}

#-- solution(simulation) for MIT DEDP Micoreconomics course-Week7-HW-7.2.6 (Coding it turned my mood from pathetic to Fantastic)------------------------
library(sendmailR) # for updating via e-mail when data is ready
library(mailR) # for updating via e-mail when data is ready
bAmt <- 100
nFirmsVec <- seq(1, 1000, 2)
A <- matrix()
B <- matrix()
price <- c(rep(0, times = length(nFirmsVec)))
for (k in 1 : length(nFirmsVec)){
  A <- matrix(0, nFirmsVec[k], nFirmsVec[k])
  B <- matrix(c(rep(100, times = nFirmsVec[k])), nFirmsVec[k], 1)
  for(i in 1 : nFirmsVec[k]){
    for (j in 1 : nFirmsVec[k]){
      if(i == j){
        A[i, j] <- 1
      } else {
        A[i, j] <- 0.5
      }
    }
  }
  ans <- solve(A, B)
  price[k] <- 4 - (0.01*(length(ans)*ans[1]))
  

  # send e-mail when data is ready  
  # if(k == length(nFirmsVec)){
  #   send.mail(from = "aarshbatra.in@gmail.com", to = "aarshbatra.in@gmail.com", subject = "Test e-mail from R",
  #             body = print("Hi, Aarsh: Your animation data is ready! Best, Aarsh."),
  #             smtp = list(host.name = "smtp.gmail.com", port = 465,
  #                         user.name = "aarshbatra.in@gmail.com",
  #                         passwd = "enterPassword", ssl = TRUE),
  #             authenticate = TRUE,
  #             send = TRUE)
  # }
  # 
}

df <- tibble(numberOfFirms = nFirmsVec, eqPrice = price, frameNum = c(1 : length(nFirmsVec)))
ggplot(data = df) + geom_line(mapping = aes(x = numberOfFirms, y = eqPrice))
df1 <- df %>% accumulate_by(~frameNum) # accumulate_by is a function written (not by me) in play.R (see above). 
df1 %>% 
  plot_ly(x = ~numberOfFirms,
          y = ~eqPrice,
          frame = ~frame,
          type = "scatter",
          mode = "lines", 
          line = list(simplyfy = F)) %>%
  animation_opts(
    frame = 50, 
    transition = 0, 
    redraw = FALSE
  )

    
#------------------------------------------------------------------------------------

# sketchR 
fileName <- 1
fileType <- ".jpg"
imPath <- c(str_c("C:/Users/Aarsh Batra/Desktop/GitHub/play/", fileName,  fileType))

outputSketch <- function(fileName, fileType, lw, sty){
  imPath <- c(str_c("C:/Users/Aarsh Batra/Desktop/GitHub/play/", fileName,  fileType))
  sampleImage <- im_load(imPath)
  sketchedSampleImage <- sketch(sampleImage, lineweight = lw, style = sty)
  return(sketchedSampleImage)
}

im_save(foo, "14v2", path = "C:/Users/Aarsh Batra/Desktop/GitHub/play")

# cash-counter algorithm v2 (assuming denominations 1, 2,  5, 10, 50, 100, 200, 500, 2000)

# cash in stock

denominations <- c(1, 2, 5, 10, 20, 50, 100, 200, 500, 2000)
cashInStock <- c(10, 20, 30, 10, 15, 20, 0, 10, 19, 20)
returnCashDenmVec <- c(rep(0, times = length(denominations)))

returnChange <- function(nd1, nd2, nd3, nd4, nd5, nd6, nd7, nd8, nd9, nd10, cogs){
  
  amountReceived <- sum(c(nd1, nd2, nd3, nd4, nd5,
                      nd6, nd7, nd8, nd9, nd10) * denominations)
  
  foo <- denominations
  if(cogs > amountReceived){
    print(stringr::str_c("You need to pay", cogs - amountReceived, "more Rs", sep = " "))
  } else if (cogs == amountReceived) {
    print("Thank you for shopping with us!")
  } else {
    cashInStock <- cashInStock + c(nd1, nd2, nd3, nd4, nd5, nd6, nd7, nd8, nd9, nd10)
    returnCash <- amountReceived - cogs
    print(str_c("returnCash: ", returnCash)) # debugging
    denomDistfrmRetCash <- returnCash - denominations
    print(str_c("denomDistFromRetCash: ", denomDistfrmRetCash)) # debugging
    closDenInd <- which(denomDistfrmRetCash == min(
      denomDistfrmRetCash[denomDistfrmRetCash >= 0]))
    print(str_c("closeDenInd: ", closDenInd)) # debugging
    for(i in closDenInd : 1){
      if(cashInStock[i] > 0){
        currFreqReq <- returnCash %/% (denominations[i])
        if(currFreqReq == 0){
          next
        } else {
          remCash <- returnCash %% (denominations[i])
          if(currFreqReq >= cashInStock[i]){
            if(cashInStock[i] == 0){
              next
            } else {
              returnCashDenmVec[i] <-  cashInStock[i]
              cashInStock[i] <- cashInStock[i] -  cashInStock[i]
              returnCash <- remCash
              if(returnCash == 0){
                break
              } 
            }
            
          } else {
            returnCashDenmVec[i] <- currFreqReq
            cashInStock[i] <- cashInStock[i] - currFreqReq
            returnCash <- remCash
            if(returnCash == 0){
              break
            }
          }
          
        }
      }
    }
    dfToReturn <- tibble(denom = denominations, numOfNotesReturn = returnCashDenmVec,
                         cashLeftOverInCounter = cashInStock)
    return(dfToReturn)    

    
  }
}

# ggplot (Mastering Software Development in R)--------------------

# install.packages("titanic") # If you don't have the package installed
library(titanic)
data("titanic_train", package = "titanic")
titanic <- titanic_train

library(faraway)
data("worldcup")

library(ggplot2)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggthemes)

irisData <- datasets::iris
irisData %>% ggplot(mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + geom_point() +
  ggtitle("Sepal Length v/s Sepal Width by Species") + xlab("Sepal Length") + ylab("Sepal Width")

worldcup %>% ggplot() + geom_point(mapping = aes(x = Time, y = Passes)) + 
  facet_grid(Team ~ Position)

obj1 <- worldcup %>% dplyr::group_by(Team, Position) %>% summarise(AvgPasses = mean(Passes, na.rm = TRUE)) %>% dplyr::arrange(Team, AvgPasses)

obj1_filter_lrgAvgPasses <- obj1 %>% filter((AvgPasses > 150 | AvgPasses < 30)) %>% mutate(Label = str_c(Team, Position, sep = ", "))

obj1 %>% ggplot() + geom_point(mapping = aes(x = Position, y = AvgPasses, color = Position)) + geom_hline(yintercept = c(40, 80)) + 
  facet_wrap(~Team, ncol = 6) + geom_text(data = obj1_filter_lrgAvgPasses, mapping = aes(x = Position, y = AvgPasses, label = Label), size = 2.5) + coord_flip() + theme_few() +
  ggtitle("Avg Passes for each position by Team")

# Converting a number to string----------------------------
# assuming 3 comma system

getTotalNumOfPos <- function(num){
  if(str_detect(num, "\\+") == TRUE){
    numAfterE <- str_extract(num, "\\+(.)+")
    numAfterE <- as.numeric(str_extract(numAfterE, "[^\\+]+"))
    return(numAfterE + 1)
  } else {
    return(str_count(num))
  }
}

convertToWord <- function(num){ # This only converts for numbers 1 to 100
  convTibble <- tibble(
    num = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17
          , 18, 19, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 
          600, 700, 800, 900, 1000), 
    numInWords = c("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", 
          "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", 
          "sixteen", "seventeen", "eighteen", "nineteen", "twenty", "thirty", 
          "forty", "fifty", "sixty", "seventy", "eighty", "ninety", "One hundred", 
          "Two Hundred", "Three Hundred", "Four Hundred", "Five Hundred", "Six Hundred", 
          "Seven Hundred", "Eight Hundred", "Nine Hundred", "One Thousand")
  )
  if((num %in% convTibble$num) == TRUE){
    tmpInd1 <- which(convTibble$num == num)
    return(convTibble$numInWords[tmpInd1])
  } else {
    lenOfNum <- length(unlist(str_split(num, "")))
    if(lenOfNum == 2){
      splitNum <- as.numeric(unlist(str_split(num, "")))
      
      splitNumTensPartNum <- splitNum[1] * 10 
      tmpInd2 <- which(convTibble$num == splitNumTensPartNum)
      tensPartInWords  <- convTibble$numInWords[tmpInd2]
      
      splitNumOnesPartNum <- splitNum[2] * 1
      tmpInd3 <- which(convTibble$num == splitNumOnesPartNum)
      onesPartInWords  <- convTibble$numInWords[tmpInd3]
      
      return(str_c(tensPartInWords, onesPartInWords, sep = " ")) 
      
    } else if(lenOfNum == 3){
      splitNum <- as.numeric(unlist(str_split(num, "")))
      
      splitNumHundredsPartNum <- splitNum[1] * 100 
      tmpInd4 <- which(convTibble$num == splitNumHundredsPartNum)
      hundredsPartInWords  <- convTibble$numInWords[tmpInd4]
      
      newNum <- num - splitNumHundredsPartNum
      
      if((newNum %in% convTibble$num) == TRUE){
        tmpInd1 <- which(convTibble$num == newNum)
        onesTensPartInWords  <- convTibble$numInWords[tmpInd1]
      } else {
        lenOfNum <- length(unlist(str_split(newNum, "")))
        if(lenOfNum == 2){
          splitNum <- as.numeric(unlist(str_split(newNum, "")))
          
          splitNumTensPartNum <- splitNum[1] * 10 
          tmpInd2 <- which(convTibble$num == splitNumTensPartNum)
          tensPartInWords  <- convTibble$numInWords[tmpInd2]
          
          splitNumOnesPartNum <- splitNum[2] * 1
          tmpInd3 <- which(convTibble$num == splitNumOnesPartNum)
          onesPartInWords  <- convTibble$numInWords[tmpInd3]
          
          onesTensPartInWords <- str_c(tensPartInWords, onesPartInWords, sep = " ")
          
          return(str_c(hundredsPartInWords, onesTensPartInWords, sep = " "))
      
    } else {
      stop("Please enter a number in the range 0 to 999")
    }
  }
    return(str_c(hundredsPartInWords, onesTensPartInWords, sep = " "))    
}
 }
}

# biggestDenOfNum
# numOfPos
# nameOfEachPosVec <- c("one", "ten")
# numAtEachPos


# R Whatsapp-----------------------
history <- system.file("extdata", "sample.txt", package = "rwhatsapp")
chat <- rwa_read(history)


## Cryptography


# Frequency Analysis-------------------

cipherText <- "PCQVMJYPDLBYKLYSOKBXBJXWXVBXVZCJPOEYPDKBXBJYUXJLBJOOKCPKCPLBOLBCMKXPVXPVIYJKLPYDBL
QBOPKBOBXVOPVOVLBOLXROCISXXJMIKBOJCKOXPVEYKKOVLBODJCMPVZOICJOBYSKXUYPDDJOXLEYPDICJXLBCMK
XPVXPVCPOPYDBLKYBXNOZOOPJOACMPLYPDLCUCMLBOIXZROKCIFXKLXDOKXPVLBORODOPVKCIXPAYOPLEYPDK
SXUYSXEOKCZCRVXKLCAJXNOXIXNCMJCIUCMJSXGOKLUOFYRCDMOLXROKIJCSLBOLBCMKXPVXPVCPOPYDBLK"

cipherTextVec <- unlist(str_split(cipherText, ""))

lengthCipherText <- length(cipherTextVec)

lettersFreqInCipherText <- tibble(letter = LETTERS, freq = 
                                    c(rep(NA, times = length(LETTERS)))) 
for(i in 1 :  length(LETTERS)){
  tmpSum <- sum(str_detect(cipherTextVec, LETTERS[i]))
  lettersFreqInCipherText$freq[i] <- tmpSum 
}

lettersFreqInCipherText <- lettersFreqInCipherText %>% 
  mutate(percGivenTotalLetters = (freq/lengthCipherText)*100) %>%
  arrange(desc(percGivenTotalLetters))

# encipher, decipher

cryptAnalysis <- function(string, cipherType = "caesar", typeOfOper = "encrypt"){
  print("What do you want to do? Type 'encrypt' for encrypting and 'decrypt' for
  decrypting. Please enter your answer below:" )
  typeOfOper <- readline()
  if((typeOfOper %in% c("encrypt", "decrypt")) == FALSE){
    stop("Please enter 'encrypt' for encrypting and 'decrypt' for decrypting.")
  } else {
    if(cipherType == "caesar" && typeOfOper == "encrypt"){
      print("Enter a value for caesar shift")
      shiftBy <- readline()
      shiftBy <- as.numeric(shiftBy)
      stringToLower <- stringr::str_to_lower(string)
      stringToLowerVec <- unlist(stringr::str_split(stringToLower, ""))
      cipherTib <- tibble(plainText = stringToLowerVec, indexOfPlainText = NA, 
                          indexOfCipherText = NA)
      for(i in 1 : length(stringToLowerVec)){
        if(stringToLowerVec[i] == " "){
          cipherTib$indexOfPlainText[i] <- NA
        } else {
          cipherTib$indexOfPlainText[i] <- which(letters %in% stringToLowerVec[i]) 
        }
      }
      cipherTib$indexOfPlainText <- as.numeric(cipherTib$indexOfPlainText)
      cipherTib$indexOfCipherText <- (cipherTib$indexOfPlainText + shiftBy)
      cipherTib$indexOfCipherText <- cipherTib$indexOfCipherText %% 26
      cipherTib$indexOfCipherText[cipherTib$indexOfCipherText == 0] <- 26
      cipherTib$cipherAlphabet <- letters[cipherTib$indexOfCipherText]
      cipherTib$cipherAlphabet <- tidyr::replace_na(cipherTib$cipherAlphabet, " ")
      cipherTextReturn <- str_c(cipherTib$cipherAlphabet, collapse =  " ")
      retlist <- list(a = cipherTextReturn, b = cipherTib)
      return(retlist)
      
      
    } else if(cipherType == "caesar" && typeOfOper == "decrypt"){
      
      print("Enter a value by which you performed the caesar shift")
      shiftBy <- readline()
      shiftBy <- as.numeric(shiftBy)
      stringToLower <- stringr::str_to_lower(string)
      stringToLowerVec <- unlist(stringr::str_split(stringToLower, ""))
      cipherTib <- tibble(cipherText = stringToLowerVec, indexOfCipherText = NA, 
                          indexOfPlainText = NA)
      for(i in 1 : length(stringToLowerVec)){
        if(stringToLowerVec[i] == " "){
          cipherTib$indexOfCipherText[i] <- NA
        } else {
          cipherTib$indexOfCipherText[i] <- which(letters %in% stringToLowerVec[i]) 
        }
      }
      cipherTib$indexOfCipherText <- as.numeric(cipherTib$indexOfCipherText)
      cipherTib$indexOfPlainText <- (cipherTib$indexOfCipherText - shiftBy) + 26
      cipherTib$indexOfPlainText <- cipherTib$indexOfPlainText %% 26
      cipherTib$indexOfPlainText[cipherTib$indexOfPlainText == 0] <- 26
      cipherTib$decipherAlphabet <- letters[cipherTib$indexOfPlainText]
      cipherTib$decipherAlphabet <- tidyr::replace_na(cipherTib$decipherAlphabet, " ")
      decipherTextReturn <- str_c(cipherTib$decipherAlphabet, collapse =  " ")
      retlist <- list(a = decipherTextReturn, b = cipherTib)
      return(retlist)
      
      
      
      
    }
    
  }
}
# Test change on Line 1914
