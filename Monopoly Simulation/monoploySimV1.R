# Monopoly simulation------------------------------------------

# improvents to be made
 # 1. add feature: three time double roll -> go to jail
 # 2. render pre written templates
 # 3. add feature: auctioning
 # 4. add feature: mortgaging
 #  4.1  mortgaging if not enough cash to pay debts, for now allowed to continue
 # 5. add feature: building house + hotels

setwd("C:/Users/Aarsh/Desktop/Monopoly Simulation")

# libraries
library(tidyverse)
library(magrittr)
library(stringr)
library(dplyr)
library(xlsx)

# initlializing parameters-------------------------------------- 

numRounds <- 10000 # sample(50 : 150, 1) 
numPlayers <- 4
initialCash <- 1500000
initialPosition <- 1
outOfJailSignal <- FALSE
masterDataTable <- read.xlsx("masterDataTable.xlsx", sheetName = "1.masterDataTable")
for(c in 1 : ncol(masterDataTable)){
  if(c == 2 || c == 15 || c == 16){
    masterDataTable[, c] <- as.character(masterDataTable[, c])
  } else {
    masterDataTable[, c] <- as.numeric(masterDataTable[, c])
  }
}

playerInfo <- list(list(position = initialPosition, cash = initialCash, property = c(), tracePath = c(initialPosition), debt = c(0, 0, 0, 0), houses = c(), hotels = c()), 
 list(position = initialPosition, cash = initialCash, property = c(), tracePath = c(initialPosition), debt = c(0, 0, 0, 0), houses = c(), hotels = c()),
 list(position = initialPosition, cash = initialCash, property = c(), tracePath = c(initialPosition), debt = c(0, 0, 0, 0), houses = c(), hotels = c()),
 list(position = initialPosition, cash = initialCash, property = c(), tracePath = c(initialPosition), debt = c(0, 0, 0, 0), houses = c(), hotels = c()))

communityChestOptions <- c("pay500k", "recieve1M", "goToJail")
chanceOptions <- c("pay250k", "performanceAwardRecieve2M", "getOutOfJail")
jailBailOptions <- c("pay100k", "pay1M")

# main loop starts here-------------------------------------------------------------------------------------------
for(roundNum in 1 : numRounds){
  for(plyrNumPayFrom in 1 : numPlayers){ # pay debts as of this round, else continue
    for(plyrNumPayTo in 1 : numPlayers){
      if(playerInfo[[plyrNumPayFrom]]$debt[plyrNumPayTo] != 0){ # debt != 0 
        if(playerInfo[[plyrNumPayFrom]]$cash >= playerInfo[[plyrNumPayFrom]]$debt[plyrNumPayTo]){ # have enough cash to pay debt
          playerInfo[[plyrNumPayTo]]$cash <- playerInfo[[plyrNumPayTo]]$cash + playerInfo[[plyrNumPayFrom]]$debt[plyrNumPayTo]
          playerInfo[[plyrNumPayFrom]]$cash <- playerInfo[[plyrNumPayFrom]]$cash - playerInfo[[plyrNumPayFrom]]$debt[plyrNumPayTo]
          playerInfo[[plyrNumPayFrom]]$debt[plyrNumPayTo] <- playerInfo[[plyrNumPayFrom]]$debt[plyrNumPayTo] - playerInfo[[plyrNumPayFrom]]$debt[plyrNumPayTo]
        } else{ # not enough cash to pay debt, pay as much as you can. (for now allowed to continue, in later versions -> can sell prop, mortgage, etc.)
          playerInfo[[plyrNumPayTo]]$cash <- playerInfo[[plyrNumPayTo]]$cash <- playerInfo[[plyrNumPayFrom]]$cash
          playerInfo[[plyrNumPayFrom]]$cash <- playerInfo[[plyrNumPayFrom]]$cash - playerInfo[[plyrNumPayFrom]]$cash
          playerInfo[[plyrNumPayFrom]]$debt[plyrNumPayTo] <- playerInfo[[plyrNumPayFrom]]$debt[plyrNumPayTo] - playerInfo[[plyrNumPayFrom]]$cash
         }
      } else { # debt = 0
        next
      }
    }
  }
  for(plyrNum in 1 : numPlayers){
   tmpDie1 <- sample(1 : 6, 1)
   tmpDie2 <- sample(1: 6, 1) 
   sumOfDice <- tmpDie1 + tmpDie2 
   playerInfo[[plyrNum]]$position <- playerInfo[[plyrNum]]$position + sumOfDice
   
  if((playerInfo[[plyrNum]]$position) > 40){ 
  playerInfo[[plyrNum]]$position <- playerInfo[[plyrNum]]$position%%40 # 41 becomes 1 (modular arithmetic)
  playerInfo[[plyrNum]]$tracePath <- append(playerInfo[[plyrNum]]$tracePath, playerInfo[[plyrNum]]$position)
  playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash + 2000 # pass go bonus 2M
        
  if (playerInfo[[plyrNum]]$position == masterDataTable$index[5]){
    playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - 2000000 # income tax
  } else if (playerInfo[[plyrNum]]$position %in% c(masterDataTable$index[3], masterDataTable$index[18],
                                                         masterDataTable$index[34])){ # community chest
    communityChestOptionsChoose <- sample(communityChestOptions, 1) 
    if (communityChestOptionsChoose == communityChestOptions[1]){
      playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - 500000
    } else if ((communityChestOptionsChoose == communityChestOptions[2])){
      playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash + 1000000
    } else {
      playerInfo[[plyrNum]]$position <- masterDataTable$index[11]
      playerInfo[[plyrNum]]$tracePath <- append(playerInfo[[plyrNum]]$tracePath, playerInfo[[plyrNum]]$position)
      next
    }
    
         
  } else if(playerInfo[[plyrNum]]$position %in% c(masterDataTable$index[8], masterDataTable$index[23],
                                                        masterDataTable$index[37])){ # chance
    chanceOptionsChoose <- sample(chanceOptions, 1)
    if (chanceOptionsChoose == chanceOptions[1]){
      playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - 250000
    } else if (chanceOptionsChoose == chanceOptions[2]){
      playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash + 2000000
    } else {
      playerInfo[[plyrNum]]$property <- append(playerInfo[[plyrNum]]$property, "getOutOfJailPass")
    }
          
          
  } else if (playerInfo[[plyrNum]]$position  == masterDataTable$index[39]){ # super tax
      playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - 1000000 
          
  } else if (playerInfo[[plyrNum]]$position == masterDataTable$index[11]){ # just visiting jail
      if ((playerInfo[[plyrNum]]$tracePath[length(playerInfo[[plyrNum]]$tracePath) - 1] != masterDataTable$index[31]) || (outOfJailSignal == TRUE)){
        playerInfo[[plyrNum]]$position <- playerInfo[[plyrNum]]$position # remain where you are
        outOfJailSignal <- FALSE
        next 
      } else {
        if("getOutOfJailPass" %in% playerInfo[[plyrNum]]$property){
          outOfJailSignal <- TRUE
          next
        } else {
          jailBailOptionsChoose <- sample(jailBailOptions, 1)
          if(jailBailOptionsChoose == jailBailOptions[1]){
            playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - 100000
          } else {
            playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - 1000000
          }
          
        }
      }
          
  } else if (playerInfo[[plyrNum]]$position == masterDataTable$index[21]) { # free parking
      playerInfo[[plyrNum]]$position <- playerInfo[[plyrNum]]$position # remain where you are
      next
          
  } else if (playerInfo[[plyrNum]]$position == masterDataTable$index[31]){ # go to jail
      playerInfo[[plyrNum]]$position <- masterDataTable$index[11]
      playerInfo[[plyrNum]]$tracePath <- append(playerInfo[[plyrNum]]$tracePath, playerInfo[[plyrNum]]$position)
      next
    
          
  } else if (playerInfo[[plyrNum]]$position == masterDataTable$index[1]){ # if on 'go', proceed, as already got 2M above.
      next
    
  } else { # buy/sell/take rent etc
    if(masterDataTable$status[playerInfo[[plyrNum]]$position] == "avl"){ # is property available?
      if(playerInfo[[plyrNum]]$cash >= masterDataTable$slotFaceValue[playerInfo[[plyrNum]]$position]) { # is cash available?
        tmpBuyOrNot <- sample(0 : 1, 1)
        if(tmpBuyOrNot == 0){ # auctioning code will go here, for now if the current plyr do not want to buy, no auction 
          next
        } else { # buy it, pay cash, add this prop to the player's list of properties, status -> "sold"
          playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$slotFaceValue[playerInfo[[plyrNum]]$position]
          playerInfo[[plyrNum]]$property <- append(playerInfo[[plyrNum]]$property, 
                                                   masterDataTable$slotName[playerInfo[[plyrNum]]$position])
          masterDataTable$status[playerInfo[[plyrNum]]$position] <- "sold"
        }
      } else { # not enough cash, for now property mortgages is not allowed, to be added in future.
        next
      } 
    } else if (masterDataTable$status[playerInfo[[plyrNum]]$position] == "sold") { # prop not avl, find owner + pay rent
      landlordIndex <-  NA
      for(i in 1 : numPlayers){
        if(masterDataTable$slotName[playerInfo[[plyrNum]]$position] %in% playerInfo[[i]]$property){
          landlordIndex <- i
        } else {
          next
        }
      }
        if(str_detect(masterDataTable$slotName[playerInfo[[plyrNum]]$position], "br.")){ # prop brown?
          if(sum(str_detect(playerInfo[[landlordIndex]]$property, "br.")) == 2){ # brown + full set?
            if (playerInfo[[plyrNum]]$cash >= masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position]){
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # pay rent
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # recieve rent  
            } else {
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
              playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
            }
            
          } else {
            if (playerInfo[[plyrNum]]$cash >= masterDataTable$rent[playerInfo[[plyrNum]]$position]){
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rent[playerInfo[[plyrNum]]$position] # pay rent
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rent[playerInfo[[plyrNum]]$position] # recieve rent  
            } else {
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
              playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rent[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
            }
          }
        } else if (str_detect(masterDataTable$slotName[playerInfo[[plyrNum]]$position], "st.")){ # prop station?
          if(sum(str_detect(playerInfo[[landlordIndex]]$property, "st.")) == 4){ # station + full set?
            if (playerInfo[[plyrNum]]$cash >= 4*(masterDataTable$rent[playerInfo[[plyrNum]]$position])){
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - (4*masterDataTable$rent[playerInfo[[plyrNum]]$position]) # pay rent
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + 4*(masterDataTable$rent[playerInfo[[plyrNum]]$position]) # recieve rent  
            } else {
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
              playerInfo[[plyrNum]]$debt[landlordIndex] <- 4*(masterDataTable$rent[playerInfo[[plyrNum]]$position]) - playerInfo[[plyrNum]]$cash # debt
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
            }
            
          } else if (sum(str_detect(playerInfo[[landlordIndex]]$property, "st.")) == 3) { # station + 3 set?
            if (playerInfo[[plyrNum]]$cash >= 3*(masterDataTable$rent[playerInfo[[plyrNum]]$position])){
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - 3*(masterDataTable$rent[playerInfo[[plyrNum]]$position]) # pay rent
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + 3*(masterDataTable$rent[playerInfo[[plyrNum]]$position]) # recieve rent  
            } else {
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
              playerInfo[[plyrNum]]$debt[landlordIndex] <- 3*(masterDataTable$rent[playerInfo[[plyrNum]]$position]) - playerInfo[[plyrNum]]$cash # debt 
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
            }
            
            
          } else if (sum(str_detect(playerInfo[[landlordIndex]]$property, "st.")) == 2){ # station + 2 set?
            if (playerInfo[[plyrNum]]$cash >= 2*(masterDataTable$rent[playerInfo[[plyrNum]]$position])){
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - 2*(masterDataTable$rent[playerInfo[[plyrNum]]$position]) # pay rent
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + 2*(masterDataTable$rent[playerInfo[[plyrNum]]$position]) # recieve rent  
            } else {
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
              playerInfo[[plyrNum]]$debt[landlordIndex] <- 2*(masterDataTable$rent[playerInfo[[plyrNum]]$position]) - playerInfo[[plyrNum]]$cash # debt 
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
            }
            
            
          } else { # one station
            if (playerInfo[[plyrNum]]$cash >= 1*(masterDataTable$rent[playerInfo[[plyrNum]]$position])){
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - 1*(masterDataTable$rent[playerInfo[[plyrNum]]$position]) # pay rent
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + 1*(masterDataTable$rent[playerInfo[[plyrNum]]$position]) # recieve rent  
            } else {
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
              playerInfo[[plyrNum]]$debt[landlordIndex] <- 1*(masterDataTable$rent[playerInfo[[plyrNum]]$position]) - playerInfo[[plyrNum]]$cash # debt 
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
            }
            
          }
        } else if(str_detect(masterDataTable$slotName[playerInfo[[plyrNum]]$position], "lb.")){ # prop light blue?
          if(sum(str_detect(playerInfo[[landlordIndex]]$property, "lb.")) == 3){ # light blue + full set?
            if (playerInfo[[plyrNum]]$cash >= masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position]){
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # pay rent
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # recieve rent  
            } else {
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
              playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
            }
            
          } else {
            if (playerInfo[[plyrNum]]$cash >= masterDataTable$rent[playerInfo[[plyrNum]]$position]){
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rent[playerInfo[[plyrNum]]$position] # pay rent
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rent[playerInfo[[plyrNum]]$position] # recieve rent  
            } else {
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
              playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rent[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt 
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
            }
            
          }
        } else if(str_detect(masterDataTable$slotName[playerInfo[[plyrNum]]$position], "mj.")){ # prop majenta?
          if(sum(str_detect(playerInfo[[landlordIndex]]$property, "mj.")) == 3){ # majenta + full set?
            if (playerInfo[[plyrNum]]$cash >= masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position]){
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # pay rent
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # recieve rent  
            } else {
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
              playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
            }
            
          } else {
            if (playerInfo[[plyrNum]]$cash >= masterDataTable$rent[playerInfo[[plyrNum]]$position]){
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rent[playerInfo[[plyrNum]]$position] # pay rent
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rent[playerInfo[[plyrNum]]$position] # recieve rent  
            } else {
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
              playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rent[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt 
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
            }
            
          }
          
        } else if(str_detect(masterDataTable$slotName[playerInfo[[plyrNum]]$position], "ut.")){ # prop utility?
          utilityDie1Roll <- sample(1 : 6, 1) # utility die 1 roll
          utilityDie2Roll <- sample(1 : 6, 1) # utility die 2 roll
          sumOfUtilityDieRolls <- utilityDie1Roll + utilityDie2Roll
          
          if(sum(str_detect(playerInfo[[landlordIndex]]$property, "ut.")) == 2){ # all utilities (2)?
            if (playerInfo[[plyrNum]]$cash >= (10*sumOfUtilityDieRolls*10000)){ # pay 10*sumOfDie*10000
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - (10*sumOfUtilityDieRolls*10000) # pay rent
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + (10*sumOfUtilityDieRolls*10000) # recieve rent
            } else { # pay whatever available
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
              playerInfo[[plyrNum]]$debt[landlordIndex] <-  (10*sumOfUtilityDieRolls*10000) - playerInfo[[plyrNum]]$cash # debt
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
            } 
          } else { # pay 4*sumOfDie*10000
            if (playerInfo[[plyrNum]]$cash >= (4*sumOfUtilityDieRolls*10000)){ # pay 4*sumOfDie*10000
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - (4*sumOfUtilityDieRolls*10000) # pay rent
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + (4*sumOfUtilityDieRolls*10000) # recieve rent
            } else { # pay whatever available
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
              playerInfo[[plyrNum]]$debt[landlordIndex] <-  (4*sumOfUtilityDieRolls*10000) - playerInfo[[plyrNum]]$cash # debt
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
            } 
            
          }
          
        } else if(str_detect(masterDataTable$slotName[playerInfo[[plyrNum]]$position], "or.")){ # prop orange?
          if(sum(str_detect(playerInfo[[landlordIndex]]$property, "or.")) == 3){ # orange + full set?
            if (playerInfo[[plyrNum]]$cash >= masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position]){
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # pay rent
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # recieve rent  
            } else {
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
              playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
            }
            
          } else {
            if (playerInfo[[plyrNum]]$cash >= masterDataTable$rent[playerInfo[[plyrNum]]$position]){
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rent[playerInfo[[plyrNum]]$position] # pay rent
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rent[playerInfo[[plyrNum]]$position] # recieve rent  
            } else {
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
              playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rent[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt 
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
            }
            
          }
        }  else if(str_detect(masterDataTable$slotName[playerInfo[[plyrNum]]$position], "rd.")){ # prop red?
          if(sum(str_detect(playerInfo[[landlordIndex]]$property, "rd.")) == 3){ # red + full set?
            if (playerInfo[[plyrNum]]$cash >= masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position]){
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # pay rent
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # recieve rent  
            } else {
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
              playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
            }
            
          } else {
            if (playerInfo[[plyrNum]]$cash >= masterDataTable$rent[playerInfo[[plyrNum]]$position]){
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rent[playerInfo[[plyrNum]]$position] # pay rent
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rent[playerInfo[[plyrNum]]$position] # recieve rent  
            } else {
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
              playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rent[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt 
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
            }
            
          }
        }  else if(str_detect(masterDataTable$slotName[playerInfo[[plyrNum]]$position], "yl.")){ # prop yellow?
          if(sum(str_detect(playerInfo[[landlordIndex]]$property, "yl.")) == 3){ # yellow + full set?
            if (playerInfo[[plyrNum]]$cash >= masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position]){
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # pay rent
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # recieve rent  
            } else {
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
              playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
            }
            
          } else {
            if (playerInfo[[plyrNum]]$cash >= masterDataTable$rent[playerInfo[[plyrNum]]$position]){
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rent[playerInfo[[plyrNum]]$position] # pay rent
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rent[playerInfo[[plyrNum]]$position] # recieve rent  
            } else {
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
              playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rent[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt 
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
            }
            
          }
        }  else if(str_detect(masterDataTable$slotName[playerInfo[[plyrNum]]$position], "gr.")){ # prop green?
          if(sum(str_detect(playerInfo[[landlordIndex]]$property, "gr.")) == 3){ # green + full set?
            if (playerInfo[[plyrNum]]$cash >= masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position]){
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # pay rent
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # recieve rent  
            } else {
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
              playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
            }
            
          } else {
            if (playerInfo[[plyrNum]]$cash >= masterDataTable$rent[playerInfo[[plyrNum]]$position]){
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rent[playerInfo[[plyrNum]]$position] # pay rent
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rent[playerInfo[[plyrNum]]$position] # recieve rent  
            } else {
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
              playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rent[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt 
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
            }
            
          }
        }  else if(str_detect(masterDataTable$slotName[playerInfo[[plyrNum]]$position], "db.")){ # prop dark blue?
          if(sum(str_detect(playerInfo[[landlordIndex]]$property, "db.")) == 2){ # dark blue + full set?
            if (playerInfo[[plyrNum]]$cash >= masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position]){
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # pay rent
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # recieve rent  
            } else {
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
              playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
            }
            
          } else {
            if (playerInfo[[plyrNum]]$cash >= masterDataTable$rent[playerInfo[[plyrNum]]$position]){
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rent[playerInfo[[plyrNum]]$position] # pay rent
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rent[playerInfo[[plyrNum]]$position] # recieve rent  
            } else {
              playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
              playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rent[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt 
              playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
            }
            
          }
        }   
    }
  }  
     
  
       
 } else { 
   playerInfo[[plyrNum]]$tracePath <- append(playerInfo[[plyrNum]]$tracePath, playerInfo[[plyrNum]]$position)
   
   if (playerInfo[[plyrNum]]$position == masterDataTable$index[5]){
     playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - 2000000 # income tax
   } else if (playerInfo[[plyrNum]]$position %in% c(masterDataTable$index[3], masterDataTable$index[18],
                                                    masterDataTable$index[34])){ # community chest
     communityChestOptionsChoose <- sample(communityChestOptions, 1) 
     if (communityChestOptionsChoose == communityChestOptions[1]){
       playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - 500000
     } else if ((communityChestOptionsChoose == communityChestOptions[2])){
       playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash + 1000000
     } else {
       playerInfo[[plyrNum]]$position <- masterDataTable$index[11]
       playerInfo[[plyrNum]]$tracePath <- append(playerInfo[[plyrNum]]$tracePath, playerInfo[[plyrNum]]$position)
       next
     }
     
     
   } else if(playerInfo[[plyrNum]]$position %in% c(masterDataTable$index[8], masterDataTable$index[23],
                                                   masterDataTable$index[37])){ # chance
     chanceOptionsChoose <- sample(chanceOptions, 1)
     if (chanceOptionsChoose == chanceOptions[1]){
       playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - 250000
     } else if (chanceOptionsChoose == chanceOptions[2]){
       playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash + 2000000
     } else {
       playerInfo[[plyrNum]]$property <- append(playerInfo[[plyrNum]]$property, "getOutOfJailPass")
     }
     
     
   } else if (playerInfo[[plyrNum]]$position  == masterDataTable$index[39]){ # super tax
     playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - 1000000 
     
   } else if (playerInfo[[plyrNum]]$position == masterDataTable$index[11]){ # just visiting jail
     if ((playerInfo[[plyrNum]]$tracePath[length(playerInfo[[plyrNum]]$tracePath) - 1] != masterDataTable$index[31]) || (outOfJailSignal == TRUE)){
       playerInfo[[plyrNum]]$position <- playerInfo[[plyrNum]]$position # remain where you are
       outOfJailSignal <- FALSE
       next 
     } else {
       if("getOutOfJailPass" %in% playerInfo[[plyrNum]]$property){
         outOfJailSignal <- TRUE
         next
       } else {
         jailBailOptionsChoose <- sample(jailBailOptions, 1)
         if(jailBailOptionsChoose == jailBailOptions[1]){
           playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - 100000
         } else {
           playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - 1000000
         }
         
       }
     }
     
   } else if (playerInfo[[plyrNum]]$position == masterDataTable$index[21]) { # free parking
     playerInfo[[plyrNum]]$position <- playerInfo[[plyrNum]]$position # remain where you are
     next
     
   } else if (playerInfo[[plyrNum]]$position == masterDataTable$index[31]){ # go to jail
     playerInfo[[plyrNum]]$position <- masterDataTable$index[11]
     playerInfo[[plyrNum]]$tracePath <- append(playerInfo[[plyrNum]]$tracePath, playerInfo[[plyrNum]]$position)
     next
     
     
   } else if (playerInfo[[plyrNum]]$position == masterDataTable$index[1]){ # if on 'go', proceed, as already got 2M above.
     next
     
   } else { # buy/sell/take rent etc
     if(masterDataTable$status[playerInfo[[plyrNum]]$position] == "avl"){ # is property available?
       if(playerInfo[[plyrNum]]$cash >= masterDataTable$slotFaceValue[playerInfo[[plyrNum]]$position]) { # is cash available?
         tmpBuyOrNot <- sample(0 : 1, 1)
         if(tmpBuyOrNot == 0){ # auctioning code will go here, for now if the current plyr do not want to buy, no auction 
           next
         } else { # buy it, pay cash, add this prop to the player's list of properties, status -> "sold"
           playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$slotFaceValue[playerInfo[[plyrNum]]$position]
           playerInfo[[plyrNum]]$property <- append(playerInfo[[plyrNum]]$property, 
                                                    masterDataTable$slotName[playerInfo[[plyrNum]]$position])
           masterDataTable$status[playerInfo[[plyrNum]]$position] <- "sold"
         }
       } else { # not enough cash, for now property mortgages is not allowed, to be added in future.
         next
       } 
     } else if (masterDataTable$status[playerInfo[[plyrNum]]$position] == "sold") { # prop not avl, find owner + pay rent
       landlordIndex <-  NA
       for(i in 1 : numPlayers){
         if(masterDataTable$slotName[playerInfo[[plyrNum]]$position] %in% playerInfo[[i]]$property){
           landlordIndex <- i
         } else {
           next
         }
       }
       if(str_detect(masterDataTable$slotName[playerInfo[[plyrNum]]$position], "br.")){ # prop brown?
         if(sum(str_detect(playerInfo[[landlordIndex]]$property, "br.")) == 2){ # brown + full set?
           if (playerInfo[[plyrNum]]$cash >= masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position]){
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # pay rent
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # recieve rent  
           } else {
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
             playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
           }
           
         } else {
           if (playerInfo[[plyrNum]]$cash >= masterDataTable$rent[playerInfo[[plyrNum]]$position]){
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rent[playerInfo[[plyrNum]]$position] # pay rent
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rent[playerInfo[[plyrNum]]$position] # recieve rent  
           } else {
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
             playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rent[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
           }
         }
       } else if (str_detect(masterDataTable$slotName[playerInfo[[plyrNum]]$position], "st.")){ # prop station?
         if(sum(str_detect(playerInfo[[landlordIndex]]$property, "st.")) == 4){ # station + full set?
           if (playerInfo[[plyrNum]]$cash >= 4*(masterDataTable$rent[playerInfo[[plyrNum]]$position])){
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - (4*masterDataTable$rent[playerInfo[[plyrNum]]$position]) # pay rent
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + 4*(masterDataTable$rent[playerInfo[[plyrNum]]$position]) # recieve rent  
           } else {
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
             playerInfo[[plyrNum]]$debt[landlordIndex] <- 4*(masterDataTable$rent[playerInfo[[plyrNum]]$position]) - playerInfo[[plyrNum]]$cash # debt
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
           }
           
         } else if (sum(str_detect(playerInfo[[landlordIndex]]$property, "st.")) == 3) { # station + 3 set?
           if (playerInfo[[plyrNum]]$cash >= 3*(masterDataTable$rent[playerInfo[[plyrNum]]$position])){
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - 3*(masterDataTable$rent[playerInfo[[plyrNum]]$position]) # pay rent
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + 3*(masterDataTable$rent[playerInfo[[plyrNum]]$position]) # recieve rent  
           } else {
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
             playerInfo[[plyrNum]]$debt[landlordIndex] <- 3*(masterDataTable$rent[playerInfo[[plyrNum]]$position]) - playerInfo[[plyrNum]]$cash # debt 
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
           }
           
           
         } else if (sum(str_detect(playerInfo[[landlordIndex]]$property, "st.")) == 2){ # station + 2 set?
           if (playerInfo[[plyrNum]]$cash >= 2*(masterDataTable$rent[playerInfo[[plyrNum]]$position])){
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - 2*(masterDataTable$rent[playerInfo[[plyrNum]]$position]) # pay rent
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + 2*(masterDataTable$rent[playerInfo[[plyrNum]]$position]) # recieve rent  
           } else {
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
             playerInfo[[plyrNum]]$debt[landlordIndex] <- 2*(masterDataTable$rent[playerInfo[[plyrNum]]$position]) - playerInfo[[plyrNum]]$cash # debt 
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
           }
           
           
         } else { # one station
           if (playerInfo[[plyrNum]]$cash >= 1*(masterDataTable$rent[playerInfo[[plyrNum]]$position])){
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - 1*(masterDataTable$rent[playerInfo[[plyrNum]]$position]) # pay rent
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + 1*(masterDataTable$rent[playerInfo[[plyrNum]]$position]) # recieve rent  
           } else {
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
             playerInfo[[plyrNum]]$debt[landlordIndex] <- 1*(masterDataTable$rent[playerInfo[[plyrNum]]$position]) - playerInfo[[plyrNum]]$cash # debt 
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
           }
           
         }
       } else if(str_detect(masterDataTable$slotName[playerInfo[[plyrNum]]$position], "lb.")){ # prop light blue?
         if(sum(str_detect(playerInfo[[landlordIndex]]$property, "lb.")) == 3){ # light blue + full set?
           if (playerInfo[[plyrNum]]$cash >= masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position]){
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # pay rent
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # recieve rent  
           } else {
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
             playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
           }
           
         } else {
           if (playerInfo[[plyrNum]]$cash >= masterDataTable$rent[playerInfo[[plyrNum]]$position]){
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rent[playerInfo[[plyrNum]]$position] # pay rent
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rent[playerInfo[[plyrNum]]$position] # recieve rent  
           } else {
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
             playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rent[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt 
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
           }
           
         }
       } else if(str_detect(masterDataTable$slotName[playerInfo[[plyrNum]]$position], "mj.")){ # prop majenta?
         if(sum(str_detect(playerInfo[[landlordIndex]]$property, "mj.")) == 3){ # majenta + full set?
           if (playerInfo[[plyrNum]]$cash >= masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position]){
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # pay rent
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # recieve rent  
           } else {
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
             playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
           }
           
         } else {
           if (playerInfo[[plyrNum]]$cash >= masterDataTable$rent[playerInfo[[plyrNum]]$position]){
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rent[playerInfo[[plyrNum]]$position] # pay rent
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rent[playerInfo[[plyrNum]]$position] # recieve rent  
           } else {
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
             playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rent[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt 
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
           }
           
         }
         
       } else if(str_detect(masterDataTable$slotName[playerInfo[[plyrNum]]$position], "ut.")){ # prop utility?
         utilityDie1Roll <- sample(1 : 6, 1) # utility die 1 roll
         utilityDie2Roll <- sample(1 : 6, 1) # utility die 2 roll
         sumOfUtilityDieRolls <- utilityDie1Roll + utilityDie2Roll
         
         if(sum(str_detect(playerInfo[[landlordIndex]]$property, "ut.")) == 2){ # all utilities (2)?
           if (playerInfo[[plyrNum]]$cash >= (10*sumOfUtilityDieRolls*10000)){ # pay 10*sumOfDie*10000
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - (10*sumOfUtilityDieRolls*10000) # pay rent
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + (10*sumOfUtilityDieRolls*10000) # recieve rent
           } else { # pay whatever available
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
             playerInfo[[plyrNum]]$debt[landlordIndex] <-  (10*sumOfUtilityDieRolls*10000) - playerInfo[[plyrNum]]$cash # debt
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
           } 
         } else { # pay 4*sumOfDie*10000
           if (playerInfo[[plyrNum]]$cash >= (4*sumOfUtilityDieRolls*10000)){ # pay 4*sumOfDie*10000
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - (4*sumOfUtilityDieRolls*10000) # pay rent
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + (4*sumOfUtilityDieRolls*10000) # recieve rent
           } else { # pay whatever available
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
             playerInfo[[plyrNum]]$debt[landlordIndex] <-  (4*sumOfUtilityDieRolls*10000) - playerInfo[[plyrNum]]$cash # debt
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
           } 
           
         }
         
       } else if(str_detect(masterDataTable$slotName[playerInfo[[plyrNum]]$position], "or.")){ # prop orange?
         if(sum(str_detect(playerInfo[[landlordIndex]]$property, "or.")) == 3){ # orange + full set?
           if (playerInfo[[plyrNum]]$cash >= masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position]){
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # pay rent
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # recieve rent  
           } else {
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
             playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
           }
           
         } else {
           if (playerInfo[[plyrNum]]$cash >= masterDataTable$rent[playerInfo[[plyrNum]]$position]){
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rent[playerInfo[[plyrNum]]$position] # pay rent
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rent[playerInfo[[plyrNum]]$position] # recieve rent  
           } else {
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
             playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rent[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt 
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
           }
           
         }
       }  else if(str_detect(masterDataTable$slotName[playerInfo[[plyrNum]]$position], "rd.")){ # prop red?
         if(sum(str_detect(playerInfo[[landlordIndex]]$property, "rd.")) == 3){ # red + full set?
           if (playerInfo[[plyrNum]]$cash >= masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position]){
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # pay rent
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # recieve rent  
           } else {
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
             playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
           }
           
         } else {
           if (playerInfo[[plyrNum]]$cash >= masterDataTable$rent[playerInfo[[plyrNum]]$position]){
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rent[playerInfo[[plyrNum]]$position] # pay rent
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rent[playerInfo[[plyrNum]]$position] # recieve rent  
           } else {
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
             playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rent[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt 
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
           }
           
         }
       }  else if(str_detect(masterDataTable$slotName[playerInfo[[plyrNum]]$position], "yl.")){ # prop yellow?
         if(sum(str_detect(playerInfo[[landlordIndex]]$property, "yl.")) == 3){ # yellow + full set?
           if (playerInfo[[plyrNum]]$cash >= masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position]){
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # pay rent
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # recieve rent  
           } else {
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
             playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
           }
           
         } else {
           if (playerInfo[[plyrNum]]$cash >= masterDataTable$rent[playerInfo[[plyrNum]]$position]){
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rent[playerInfo[[plyrNum]]$position] # pay rent
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rent[playerInfo[[plyrNum]]$position] # recieve rent  
           } else {
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
             playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rent[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt 
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
           }
           
         }
       }  else if(str_detect(masterDataTable$slotName[playerInfo[[plyrNum]]$position], "gr.")){ # prop green?
         if(sum(str_detect(playerInfo[[landlordIndex]]$property, "gr.")) == 3){ # green + full set?
           if (playerInfo[[plyrNum]]$cash >= masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position]){
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # pay rent
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # recieve rent  
           } else {
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
             playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
           }
           
         } else {
           if (playerInfo[[plyrNum]]$cash >= masterDataTable$rent[playerInfo[[plyrNum]]$position]){
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rent[playerInfo[[plyrNum]]$position] # pay rent
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rent[playerInfo[[plyrNum]]$position] # recieve rent  
           } else {
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
             playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rent[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt 
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
           }
           
         }
       }  else if(str_detect(masterDataTable$slotName[playerInfo[[plyrNum]]$position], "db.")){ # prop dark blue?
         if(sum(str_detect(playerInfo[[landlordIndex]]$property, "db.")) == 2){ # dark blue + full set?
           if (playerInfo[[plyrNum]]$cash >= masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position]){
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # pay rent
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] # recieve rent  
           } else {
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
             playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rentWithColourSet[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
           }
           
         } else {
           if (playerInfo[[plyrNum]]$cash >= masterDataTable$rent[playerInfo[[plyrNum]]$position]){
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - masterDataTable$rent[playerInfo[[plyrNum]]$position] # pay rent
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + masterDataTable$rent[playerInfo[[plyrNum]]$position] # recieve rent  
           } else {
             playerInfo[[landlordIndex]]$cash <- playerInfo[[landlordIndex]]$cash + playerInfo[[plyrNum]]$cash # recieve cash (not all)
             playerInfo[[plyrNum]]$debt[landlordIndex] <- masterDataTable$rent[playerInfo[[plyrNum]]$position] - playerInfo[[plyrNum]]$cash # debt 
             playerInfo[[plyrNum]]$cash <- playerInfo[[plyrNum]]$cash - playerInfo[[plyrNum]]$cash # pay as much as you have, rest is debt
           }
           
         }
       }   
     }
   }  
   
 }
}
}


