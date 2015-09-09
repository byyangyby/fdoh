#Figure 2 script 

###############
# This will be the model created with Joe using variable and fixed cost 1.12.15
# FUNCTION WITH NICK, CUC, JOE IN NOVEMBER 2014
###############

# turn off scientific notation
options(scipen=999)
# 
# #####
# # SET WORKING DIRECTORY CONDITIONAL TO SYSTEM
# #####
# if ( Sys.info()["sysname"] == "Linux" ){
#   cf <- "/home/joebrew/Documents/controlflu"
#   linux_path <- "/media/joebrew/JB/fdoh/private/"
#   acps_path <-  "/media/joebrew/JB/fdoh/private/acps/" #"E:/fdoh/private/acps/"
# } else if( Sys.info()["user"] == "BrewJR"){
#   cf <- "C:/Users/BrewJR/Documents/controlflu"
#   linux_path <- "E:/fdoh/private/"
#   acps_path <-  "E:/fdoh/private/acps/"
# } else if( Sys.info()["user"] == "C"){
#   cf <- "C:/Users/C/Documents/controlflu"
#   linux_path <- NULL
#   acps_path <-  NULL
# }
# setwd(cf) 

#####
# READ IN AND CLEAN MEDICAID DATA
# (purpose: eventually calculate # and % of VFC kids for EACH county)
#####
# Read in data
# medicaid <-read.csv("Medicaid1.csv",
#                     stringsAsFactors = FALSE)
# 
# # Remove all commas
# for (i in 2:ncol(medicaid)){
#   medicaid[,i] <- as.numeric(gsub(",", "", medicaid[,i]))
# }
# 
# #####
# # MAKE SOME CALCULATIONS IN medicaid
# #####
# 
# # first make a duplicate of medicaid
# enrolled <- medicaid 
# 
# #loop through column 2- 12, replacing each column with 83.40% of itself 
# for (i in 2:ncol(enrolled)){
#   enrolled[,i] <- enrolled[,i]*.834 #based on data http://www.medicaid.gov/Medicaid-CHIP-Program-Information/By-State/florida.html
# }
# 
# ######
# # Read in and clean uninsured data
# ######
# 
# # read in un
# un <- read.csv("data/SLIV Billing/uninsured.csv",
#                stringsAsFactors = FALSE)
# 
# # get rid of some columns 
# un <- un[,c("Name", "Demographic.Group..Number", "Uninsured..Number", "Uninsured..MOE", "Uninsured...")]
# 
# # rename those coumns
# names(un) <- c("county", "total", "unin", "moe", "percent_unin")
# 
# #clean up county names 
# un$county <- gsub(" County, FL", "",un$county)
# 
# # make un$county all caps
# un$county <- toupper(un$county)
# 
# # Remove commas from columns 2:5 and make those numeric
# for (i in 2:ncol(un)){
#   un[,i] <- as.numeric(gsub(",","",un[,i]))
# }
# 
# ######
# # MERGE ALL 3 DATAFRAMES TOGETHER
# ######
# 
# # but first, clarify column names in enrolled and medicaid
# names(enrolled)[-1] <- paste0("e", names(enrolled)[-1])
# names(medicaid)[-1] <- paste0("m", names(medicaid)[-1])
# 
# library(dplyr)
# 
# # first merge enrolled and medicaid
# temp <- left_join(x=enrolled,
#                   y=medicaid,
#                   by="county")
# ## now merge temp with un       **left_join is function and joins two data sets (X & y, by variable)
# temp2 <- left_join(x=temp,
#                    y=un,
#                    by="county")
# 
# 
# # take out the garbage
# florida <- temp2
# rm(enrolled, medicaid, temp, temp2, un)
# 
# #order by county
# florida <- florida[order(florida$county),]
# 
# #make composite 5-18 (for now)
# florida$m6.18 <- florida$mx6.10 +florida$mx11.18
# florida$e6.18 <- florida$ex6.10 +florida$ex11.18
# 
# #####
# # READ IN SCHOOLS DATA
# #####
# public <- read.csv("data/SLIV Billing/totmem.csv")
# 
# # Make public_students column
# public$public_students <-public$TOTAL.MEMBERSHIP
# 
# #only taking district total 
# public <- public[which(public$SCHOOL.NAME =="DISTRICT TOTAL"),]
# 
# # clean up and group by county
# public$county <- public$DISTRICT.NAME
# 
# # public <- public %>% 
# #   group_by(county) %>%
# #   summarise(public_students = sum(TOTAL.MEMBERSHIP, na.rm = TRUE))
# 
# # Subset to only include these two columns: county, public_students
# public <-public[,c("public_students", "county")]
# 
# # left join public back into florida
# florida <-left_join(x=florida,
#                     y=public,
#                     by="county")
# 
# # Read in census data
# census <- read.csv("data/florida_details.csv",
#                    skip = 1)
# 
# # Clean up census names
# names(census) <- gsub("[.]", "", names(census))
# census$county <- census$AreaName
# census <- tbl_df(census)
# # Get the age distribution for Alachua County
# x <- census %>%
#   filter(RaceEthnicity == "Total") %>%
#   select(county, TotalUnder1Year:Total110YearsandOlder)
# names(x)[c(2:3,
#            102:104)] <- c("Total0Years", "Total1Years",
#                           "Total100-104Years", "Total105-109Years", "Total110+Years")
# census <- x
# rm(x)
# 
# # In census, make a column with the sum of 6-18 year olds population
# census$total6.18 <-  NA
# for (i in 1:nrow(census)){
#   census$total6.18[i] <- 
#     sum(census[i, 8:20], na.rm = TRUE)
# }
# 
# # Subset census to only those columns we need
# census <- census[,c("county","total6.18")]
# 
# # clean up county names in census (get rid of "county" and make all uppercase)
# census$county <-gsub(" County","",census$county)
# census$county <-toupper(census$county)
# 
# # Get rid of the florida row in census
# census<- census[which(census$county!="FLORIDA"),]
# 
# # Left join census into florida
# florida <- left_join(x=florida,
#                      y=census,
#                      by="county")
# 
# # Take out the trash
# rm(census,public)
# 
# #clarifying column names
# florida$ttl0.19 <-florida$total
# florida$total <- NULL
# 
# # Keep ony columns we want
# florida <- florida[,c("county", "unin", "moe", "percent_unin", "m6.18",
#                       "e6.18", "public_students", "total6.18", "ttl0.19")]
# 
# # clean up names
# florida$unin0.19 <- florida$unin
# florida$unin <- NULL
# 
# florida$total0.19 <- florida$ttl0.19
# florida$ttl0.19 <- NULL
# 
# 
# # Crete a percent_medicaid column
# florida$percent_medic <- florida$m6.18/ florida$total6.18 *100
# 
# ## making % private
# florida$percent_priv <- 100 -florida$percent_unin- florida$percent_medic
# 
# # create number of students in each cat
# florida <- florida %>%
#   mutate(medic_students=public_students*percent_medic/100,
#          priv_students=public_students*percent_priv/100,
#          unin_students=public_students*percent_unin/100)
# 
# # for example's sake, let's make a dataframe of just alacua county
# alachua <- florida[which(florida$county=="ALACHUA"),]
# 
# #####
# #### Simple Cost model 
# #### 1.13.15
# 
# 
# ##############
# # WRITE 4 functions for calculating a program's cost and revenue
###############
# 1. simp_cost (fixed and variable NON-VACCINE costs)
# 2. simp_rev (total gross program revenue)
# 3. simp_vac (vaccine only costs)
# 4. net_rev (a wrapper for the previous 3, calculating TOTAL NET REVENUE)

# 1. simp_cost() ##########
simp_cost <-function(n,
                     imm_rate = 100,
                     start_up = 47135.69){
  variable <- n * 4.52 * (imm_rate * 0.01)
  total_cost <- start_up+variable
  return(total_cost)
}

joe_simp_cost <-function(n,
                         imm_rate = 100){
  cost <- n * 6.25 * (imm_rate * 0.01)
  return(cost)
}



# 2. simp_rev() ############
simp_rev <-function(n_private, # number of kids with private
                    n_nonprivate,
                    suc_bill_private =80.35,#this is defualt of AC program
                    suc_bill_nonprivate =64.92,
                    reim_private = 47.05,
                    reim_nonprivate = 5,
                    imm_rate_private = 100,
                    imm_rate_nonprivate = 100){ #deafault
  
  # private revenue
  private <- # total private revenue is equal to
    n_private * # number of privately insured kids
    (suc_bill_private/100) * # times the percentage of private kids who will be suc billed
    reim_private  * # times the reimbursement rate
    (imm_rate_private/100) # times the private immunization rate
  
  # public revenue
  public <- #total public revenue
    n_nonprivate *
    (suc_bill_nonprivate/100) *
    reim_nonprivate *
    (imm_rate_nonprivate/100)
  
  # total revenue
  total <- private+public
  
  # spit back at the user
  return(total)
} 
####################


# 3. simp_vac ############### 
#making function for vaccination cost of program 
simp_vac <-function(n_private, 
                    n_nonprivate,
                    imm_rate_private,
                    imm_rate_nonprivate,
                    vac_cost_private=17.50, #dfault
                    vac_cost_nonprivate=0){ #dfault 
  
  # private vaccine cost
  private <-n_private *
    (imm_rate_private/100) *
    vac_cost_private
  
  # public vaccine cost
  public <-n_nonprivate *
    (imm_rate_nonprivate/100) *
    vac_cost_nonprivate
  
  # total revenue
  total <- private+public
  
  # spit back at the user
  return(total)
  
}

# WRAP FUNCTIONS INTON ONE MASTER NET REV FUNCTION
# only using this for mapping
net_rev <- function(number_private = 20000,
                    number_nonprivate = 20000,
                    ir = 100,
                    collaborative = TRUE,
                    reim_np = 5,
                    reim_p = 47.05,
                    suc_np = 64.92,
                    suc_p = 80.35){
  
  if(collaborative){
    adjusted_start_up <- (number_private + number_nonprivate) * 1.53
  } else {
    adjusted_start_up <- (number_private + number_nonprivate) * 3.23
    
  }
  
  cost <- joe_simp_cost(n = number_private + number_nonprivate,
                    imm_rate = ir) 
  
  vac_cost <- simp_vac(n_private = number_private, n_nonprivate = number_nonprivate,
                       imm_rate_private = ir,
                       imm_rate_nonprivate = ir)
  
  rev <- simp_rev(n_private = number_private, n_nonprivate = number_nonprivate,
                  imm_rate_private = ir,
                  imm_rate_nonprivate = ir,
                  reim_nonprivate = reim_np,
                  reim_private = reim_p,
                  suc_bill_private = suc_p,
                  suc_bill_nonprivate = suc_np)
  
  net_profit <- rev - vac_cost - cost
  return(net_profit)
}
# 
# ###################################################################################
# ################################
# # LINE CHARTS FOR CUC (2015-01-20)
# ################################
# 
# ######
# # THIS IS VFC, COLLABORATIVE Cost 
# ######
# 
# # define range for imm rates
# ir <- 0:100
# 
# # Make the plot canvas onto which we'll add lines
# plot(x = c(0,100),
#      y = c(-5, 10),
#      type = "n",
#      xlab = "Immunization Rate (%)", 
#      ylab = "Net Revenue per Dose ($)",
#      cex.lab = 1, 
#      cex.axis = 0.7)
# # Add a zero line
# abline(h = 0)
# 
# # define different line types, colors, reim rates and imm_rates for each line
# # line_types <- c(2, 3, 4)
# # colors <- c("darkred", "darkorange", "darkgreen", "blue")
# # imm <- c(67, 70, 80, 90)
# # reim <- c(5, 10, 15)
# 
# # LOOP TO DRAW LINES
# range_reim <- c(100, 100, 100, 100, 100, 100)
# #range_reim <- seq(60, 100, length = 6)
# range_amount <- c(2.5, 3.25, 5, 10, 15, 20)
# range_colors <- c("black", "grey", "darkblue", 
#                   "dodgerblue3", "darkcyan", "cadetblue")
# 
# # colorRampPalette takes >= 2 colors, plus length of 
# # palette on outside
# directional_colors <- rev(
#   colorRampPalette(c("darkblue", "green", "darkorange"))(6))
# 
# range_lty <- c(1: 6)
# range_lwd <- c(1,2,3,1,2,3)
# for (i in 1:length(range_reim)){
#   for (j in 1:length(range_amount)){
#     
#     
#     # Draw a line for VFC, with reimbursement being $5
#     temp_vac <- simp_vac(n_private = 0, 
#                          n_nonprivate = alachua$unin_students + alachua$medic_students,
#                          imm_rate_private = ir,
#                          imm_rate_nonprivate = ir,
#                          vac_cost_private=17.50, #dfault
#                          vac_cost_nonprivate=0)
#     temp_cost <- simp_cost(n = alachua$unin_students + alachua$medic_students,
#                            imm_rate = ir)
#     temp_rev <- simp_rev(n_private = 0, 
#                          n_nonprivate = alachua$unin_students + alachua$medic_students,
#                          suc_bill_private =80.35,
#                          suc_bill_nonprivate = range_reim[i],
#                          reim_private = 37,
#                          reim_nonprivate = range_amount[j],
#                          imm_rate_private = ir,
#                          imm_rate_nonprivate = ir)
#     
#     temp_net <- ((temp_rev - temp_cost - temp_vac)/40000)
#     lines(x = 0:100, 
#           y = temp_net,
#           col = directional_colors[j], #range_colors[j],
#           lty = range_lty[j],
#           lwd = range_lwd[j] )
#     
#     if(max(temp_net)> 0){
#       break_even <- which(temp_net == min(temp_net[which(temp_net > 0)]))
#     } else{
#       break_even <- NA
#     }
#     # Print break_even points
#     print(paste("range amount", range_amount[j], ":\n",
#                 break_even))
#   }
#   
# }
# 
# legend ( x = "topleft",
#          lty = range_lty,
#          lwd = range_lwd,
#          col= directional_colors,#range_colors,
#          legend = paste0("$",(range_amount)), #rev(range_amount)), 
#          cex= 0.8,
#          bty = "n",
#          ncol = 3)
# 
# title (main = "Collaborative Approach: VFC")
# ############################################################################
# ############################################################################
# ##########################################################################################3
# #############
# ############# VFC All Inclusive 
# 
# 
# # define range for imm rates
# ir <- 0:100
# 
# # Make the plot canvas onto which we'll add lines
# plot(x = c(0,100),
#      y = c(-5, 10),
#      type = "n",
#      xlab = "Immunization Rate (%)", 
#      ylab = "Net Revenue per Dose ($)",
#      cex.lab = 1,
#      cex.axis = 0.7)
# # Add a zero line
# abline(h = 0)
# 
# 
# # LOOP TO DRAW LINES
# range_reim <- c(100, 100, 100, 100, 100, 100)
# range_amount <- c(2.5, 3.25, 5, 10, 15, 20)
# #range_colors <- c("black", "grey", "darkblue", "dodgerblue3", "darkcyan", "cadetblue")
# directional_colors <- rev(
#   colorRampPalette(c("darkblue", "green", "darkorange"))(6))
# 
# 
# range_lty <- c(1:6)
# range_lwd <- c(1,2,3,1,2,3) #* change different 
# for (i in 1:length(range_reim)){
#   for (j in 1:length(range_amount)){
#     
#     
#     # Draw a line for VFC, with reimbursement being $5
#     temp_vac <- simp_vac(n_private = 0, 
#                          n_nonprivate = alachua$unin_students + alachua$medic_students,
#                          imm_rate_private = ir,
#                          imm_rate_nonprivate = ir,
#                          vac_cost_private=17.50, #dfault
#                          vac_cost_nonprivate=0)
#     temp_cost <- simp_cost(n = alachua$unin_students + alachua$medic_students,
#                            imm_rate = ir,
#                            start_up = 92219) #place holder for cost 
#     temp_rev <- simp_rev(n_private = 0, 
#                          n_nonprivate = alachua$unin_students + alachua$medic_students,
#                          suc_bill_private =80.35,
#                          suc_bill_nonprivate = range_reim[i],
#                          reim_private = 37,
#                          reim_nonprivate = range_amount[j],
#                          imm_rate_private = ir,
#                          imm_rate_nonprivate = ir)
#     
#     temp_net <- ((temp_rev - temp_cost - temp_vac)/40000)
#     lines(x = 0:100, 
#           y = temp_net,
#           col = directional_colors[j],
#           lty = range_lty[j],
#           lwd = range_lwd[j])
#   }
# }
# legend ( x = "topleft",
#          lty = range_lty,
#          lwd = range_lwd,
#          col= directional_colors,#range_colors,
#          legend = paste0("$",(range_amount)), #rev(range_amount)), 
#          cex= 0.8,
#          bty = "n",
#          ncol = 3)
# 
# title (main = "All-Inclusive Approach: VFC")
# ############################################################################3
# #############################################################333
# ###
# ## Private Collab
# 
# # define range for imm rates
# ir <- 0:100
# 
# # Make the plot canvas onto which we'll add lines
# plot(x = c(0,100),
#      y = c(-5, 10),
#      type = "n",
#      xlab = "Immunization Rate (%)", 
#      ylab = "Net Revenue per Dose ($)",
#      cex.lab = 1,
#      cex.axis = 0.7)
# # Add a zero line
# abline(h = 0)
# 
# # define different line types, colors, reim rates and imm_rates for each line
# # line_types <- c(2, 3, 4)
# # colors <- c("darkred", "darkorange", "darkgreen", "blue")
# # imm <- c(67, 70, 80, 90)
# # reim <- c(5, 10, 15)
# 
# # LOOP TO DRAW LINES
# range_reim <- c(100, 100, 100, 100, 100, 100)
# range_amount <- c(23.5, 37, 47, 50, 55, 60)
# #range_colors <- c("black", "grey", "darkblue", "dodgerblue3", "darkcyan", "cadetblue")
# 
# directional_colors <- rev(
#   colorRampPalette(c("darkblue", "green", "darkorange"))(6))
# 
# range_lty <- c(1:6)
# range_lwd <- c(1,2,3,1,2,3)
# for (i in 1:length(range_reim)){
#   for (j in 1:length(range_amount)){
#     
#     
#     # Draw a line for VFC, with reimbursement being $5
#     temp_vac <- simp_vac(n_private = alachua$priv_students, 
#                          n_nonprivate = 0,
#                          imm_rate_private = ir,
#                          imm_rate_nonprivate = ir,
#                          vac_cost_private=17.50, #dfault
#                          vac_cost_nonprivate=0)
#     temp_cost <- simp_cost(n = alachua$priv_students,
#                            imm_rate = ir)
#     temp_rev <- simp_rev(n_private = alachua$priv_students, 
#                          n_nonprivate = 0,
#                          suc_bill_private = range_reim[i],
#                          suc_bill_nonprivate = 0,
#                          reim_private = range_amount[j],
#                          reim_nonprivate = 0,
#                          imm_rate_private = ir,
#                          imm_rate_nonprivate = ir)
#     
#     temp_net <- ((temp_rev - temp_cost - temp_vac)/40000)
#     lines(x = 0:100, 
#           y = temp_net,
#           col = directional_colors[j],
#           lty = range_lty[j],
#           lwd = range_lwd[j])
#   }
# }
# legend ( x = "bottomleft",
#          lty = range_lty,
#          lwd = range_lwd,
#          col= directional_colors,#range_colors,
#          legend = paste0("$",(range_amount)), #rev(range_amount)), 
#          cex= 0.8,
#          bty = "n",
#          ncol = 3)
# 
# 
# title (main = "Collaborative Approach: Non-VFC")
# 
# #########################################################
# # PRIVATE All inclusive
# #########################################
# ########################################
# ########################################
# # PRIVATE All inclusive
# 
# # define range for imm rates
# ir <- 0:100
# 
# # Make the plot canvas onto which we'll add lines
# plot(x = c(0,100),
#      y = c(-5, 10),
#      type = "n",
#      xlab = "Immunization Rate (%)", 
#      ylab = "Net Revenue per Dose ($)",
#      cex.lab = 1,
#      cex.axis = 0.7)
# # Add a zero line
# abline(h = 0)
# 
# # define different line types, colors, reim rates and imm_rates for each line
# # line_types <- c(2, 3, 4)
# # colors <- c("darkred", "darkorange", "darkgreen", "blue")
# # imm <- c(67, 70, 80, 90)
# # reim <- c(5, 10, 15)
# 
# # LOOP TO DRAW LINES
# range_reim <- c(100, 100, 100, 100, 100, 100)
# range_amount <- c(23.5, 37, 47, 50, 55, 60)
# #range_colors <- c("black", "grey", "darkblue", "dodgerblue3", "darkcyan", "cadetblue")
# directional_colors <- rev(
#   colorRampPalette(c("darkblue", "green", "darkorange"))(6))
# range_lty <- c(1: 6)
# range_lwd <- c(1,2,3,1,2,3)
# for (i in 1:length(range_reim)){
#   for (j in 1:length(range_amount)){
#     
#     
#     # Draw a line for VFC, with reimbursement being $5
#     temp_vac <- simp_vac(n_private = alachua$priv_students, 
#                          n_nonprivate = 0,
#                          imm_rate_private = ir,
#                          imm_rate_nonprivate = ir,
#                          vac_cost_private=17.50, #dfault
#                          vac_cost_nonprivate=0)
#     temp_cost <- simp_cost(n = alachua$priv_students,
#                            imm_rate = ir,
#                            start_up = 100000) #place holder
#     temp_rev <- simp_rev(n_private = alachua$priv_students, 
#                          n_nonprivate = 0,
#                          suc_bill_private = range_reim[i],
#                          suc_bill_nonprivate = 0,
#                          reim_private = range_amount[j],
#                          reim_nonprivate = 0,
#                          imm_rate_private = ir,
#                          imm_rate_nonprivate = ir)
#     
#     temp_net <- ((temp_rev - temp_cost - temp_vac)/40000)
#     lines(x = 0:100, 
#           y = temp_net,
#           col = directional_colors[j],
#           lty = range_lty[j],
#           lwd = range_lwd[j])
#   }
# }
# legend ( x = "bottomleft",
#          lty = range_lty,
#          lwd = range_lwd,
#          col= directional_colors,#range_colors,
#          legend = paste0("$",(range_amount)), #rev(range_amount)), 
#          cex= 0.8,
#          bty = "n",
#          ncol = 3)
# 
# 
# title (main = "All-Inclusive Approach: Non-VFC")
# ##########################################################
# #############################################################3
# #########################################
# ## Both, Collab
# 
# # define range for imm rates
# ir <- 0:100
# 
# # Make the plot canvas onto which we'll add lines
# plot(x = c(0,100),
#      y = c(-5, 10),
#      type = "n",
#      xlab = "Immunization Rate (%)", 
#      ylab = "Net Revenue per Dose ($)",
#      cex.lab = 1,
#      cex.axis = 0.7)
# # Add a zero line
# abline(h = 0)
# 
# # define different line types, colors, reim rates and imm_rates for each line
# # line_types <- c(2, 3, 4)
# # colors <- c("darkred", "darkorange", "darkgreen", "blue")
# # imm <- c(67, 70, 80, 90)
# # reim <- c(5, 10, 15)
# 
# # LOOP TO DRAW LINES
# range_reim <- c(100, 100, 100, 100, 100, 100) 
# range_amount <- c(23.5, 37, 47, 50, 55, 60)
# range_amount_public <- c(3.25, 3.25, 3.25, 3.25 , 3.25, 3.25)
# #range_colors <- c("black", "grey", "darkblue", "dodgerblue3", "darkcyan", "cadetblue")
# directional_colors <- rev(
#   colorRampPalette(c("darkblue", "green", "darkorange"))(6))
# 
# range_lty <- c(1:6)
# range_lwd <- c(1,2,3,1,2,3)
# for (i in 1:length(range_reim)){
#   for (j in 1:length(range_amount)){
#     
#     
#     # Draw a line for VFC, with reimbursement being $5
#     temp_vac <- simp_vac(n_private = alachua$priv_students, 
#                          n_nonprivate = alachua$unin_students + alachua$medic_students,
#                          imm_rate_private = ir,
#                          imm_rate_nonprivate = ir,
#                          vac_cost_private=17.50, #dfault
#                          vac_cost_nonprivate=0)
#     temp_cost <- simp_cost(n = alachua$priv_students + alachua$unin_students + alachua$medic_students,
#                            imm_rate = ir)
#     temp_rev <- simp_rev(n_private = alachua$priv_students, 
#                          n_nonprivate = alachua$unin_students + alachua$medic_students,
#                          suc_bill_private = range_reim[i],
#                          suc_bill_nonprivate = range_reim[i],
#                          reim_private = range_amount[j],
#                          reim_nonprivate = range_amount_public[j],
#                          imm_rate_private = ir,
#                          imm_rate_nonprivate = ir)
#     
#     temp_net <- ((temp_rev - temp_cost - temp_vac)/40000)
#     lines(x = 0:100, 
#           y = temp_net,
#           col = directional_colors[j],
#           lty = range_lty[j],
#           lwd = range_lwd[j])
#   }
# }
# legend ( x = "bottomleft",
#          lty = range_lty,
#          lwd = range_lwd,
#          col= directional_colors,#range_colors,
#          legend = paste0("$",(range_amount)), #rev(range_amount)), 
#          cex= 0.8,
#          bty = "n",
#          ncol = 3)
# 
# 
# title (main = "Collaborative Approach: VCF & Non-VFC at $3.25") 
# #########################################################3
# ######################################################
# #####################################################################################
# #################################################################################3
# ### Both All Inclusive
# 
# # define range for imm rates
# ir <- 0:100
# 
# # Make the plot canvas onto which we'll add lines
# plot(x = c(0,100),
#      y = c(-5, 10),
#      type = "n",
#      xlab = "Immunization Rate (%)", 
#      ylab = "Net Revenue per Dose ($)",
#      cex.lab = 1,
#      cex.axis = 0.7)
# # Add a zero line
# abline(h = 0)
# 
# # define different line types, colors, reim rates and imm_rates for each line
# # line_types <- c(2, 3, 4)
# # colors <- c("darkred", "darkorange", "darkgreen", "blue")
# # imm <- c(67, 70, 80, 90)
# # reim <- c(5, 10, 15)
# 
# # LOOP TO DRAW LINES
# range_reim <- c(100, 100, 100, 100, 100, 100)
# range_amount <- c(23.5, 37, 47, 50, 55, 60)
# range_amount_public <- c(3.25, 3.25, 3.25, 3.25 , 3.25, 3.25)
# #range_colors <- c("black", "grey", "darkblue", "dodgerblue3", "darkcyan", "cadetblue")
# directional_colors <- rev(
#   colorRampPalette(c("darkblue", "green", "darkorange"))(6))
# range_lty <- c(1:6)
# range_lwd <- c(1,2,3,1,2,3)
# for (i in 1:length(range_reim)){
#   for (j in 1:length(range_amount)){
#     
#     
#     # Draw a line for VFC, with reimbursement being $5
#     temp_vac <- simp_vac(n_private = alachua$priv_students, 
#                          n_nonprivate = alachua$unin_students + alachua$medic_students,
#                          imm_rate_private = ir,
#                          imm_rate_nonprivate = ir,
#                          vac_cost_private=17.50, #dfault
#                          vac_cost_nonprivate=0)
#     temp_cost <- simp_cost(n = alachua$priv_students + alachua$unin_students + alachua$medic_students,
#                            imm_rate = ir,
#                            start_up = 100000) #plAce holder 
#     temp_rev <- simp_rev(n_private = alachua$priv_students, 
#                          n_nonprivate = alachua$unin_students + alachua$medic_students,
#                          suc_bill_private = range_reim[i],
#                          suc_bill_nonprivate = range_reim[i],
#                          reim_private = range_amount[j],
#                          reim_nonprivate = range_amount_public[j],
#                          imm_rate_private = ir,
#                          imm_rate_nonprivate = ir)
#     
#     temp_net <- ((temp_rev - temp_cost - temp_vac)/40000)
#     lines(x = 0:100, 
#           y = temp_net,
#           col = directional_colors[j],
#           lty = range_lty[j],
#           lwd = range_lwd[j])
#   }
# }
# legend ( x = "bottomleft",
#          lty = range_lty,
#          lwd = range_lwd,
#          col= directional_colors,#range_colors,
#          legend = paste0("$",(range_amount)),  
#          cex= 0.8,
#          bty = "n",
#          ncol = 3)
# 
# 
# title (main = "All-Inclusive Approach: VFC & Non-VFC at $3.25")
# 
# #######################################################
# #####################################################
# ##########################################################
# #############################################################3
# #########################################
# ## Both, Collab at 5 VFC 
# 
# # define range for imm rates
# ir <- 0:100
# 
# # Make the plot canvas onto which we'll add lines
# plot(x = c(0,100),
#      y = c(-5, 10),
#      type = "n",
#      xlab = "Immunization Rate (%)", 
#      ylab = "Net Revenue per Dose ($)",
#      cex.lab = 1,
#      cex.axis = 0.7)
# # Add a zero line
# abline(h = 0)
# 
# # define different line types, colors, reim rates and imm_rates for each line
# # line_types <- c(2, 3, 4)
# # colors <- c("darkred", "darkorange", "darkgreen", "blue")
# # imm <- c(67, 70, 80, 90)
# # reim <- c(5, 10, 15)
# 
# # LOOP TO DRAW LINES
# range_reim <- c(100, 100, 100, 100, 100, 100) 
# range_amount <- c(23.5, 37, 47, 50, 55, 60)
# range_amount_public <- c(5, 5, 5, 5 , 5, 5)
# #range_colors <- c("black", "grey", "darkblue", "dodgerblue3", "darkcyan", "cadetblue")
# directional_colors <- rev(
#   colorRampPalette(c("darkblue", "green", "darkorange"))(6))
# 
# range_lty <- c(1:6)
# range_lwd <- c(1,2,3,1,2,3)
# for (i in 1:length(range_reim)){
#   for (j in 1:length(range_amount)){
#     
#     
#     # Draw a line for VFC, with reimbursement being $5
#     temp_vac <- simp_vac(n_private = alachua$priv_students, 
#                          n_nonprivate = alachua$unin_students + alachua$medic_students,
#                          imm_rate_private = ir,
#                          imm_rate_nonprivate = ir,
#                          vac_cost_private=17.50, #dfault
#                          vac_cost_nonprivate=0)
#     temp_cost <- simp_cost(n = alachua$priv_students + alachua$unin_students + alachua$medic_students,
#                            imm_rate = ir)
#     temp_rev <- simp_rev(n_private = alachua$priv_students, 
#                          n_nonprivate = alachua$unin_students + alachua$medic_students,
#                          suc_bill_private = range_reim[i],
#                          suc_bill_nonprivate = range_reim[i],
#                          reim_private = range_amount[j],
#                          reim_nonprivate = range_amount_public[j],
#                          imm_rate_private = ir,
#                          imm_rate_nonprivate = ir)
#     
#     temp_net <- ((temp_rev - temp_cost - temp_vac)/40000)
#     lines(x = 0:100, 
#           y = temp_net,
#           col = directional_colors[j],
#           lty = range_lty[j],
#           lwd = range_lwd[j])
#   }
# }
# legend ( x = "bottomleft",
#          lty = range_lty,
#          lwd = range_lwd,
#          col= directional_colors,#range_colors,
#          legend = paste0("$",(range_amount)), #rev(range_amount)), 
#          cex= 0.8,
#          bty = "n",
#          ncol = 3)
# 
# 
# title (main = "Collaborative Approach: VCF & Non-VFC at $5") 
# ############################################################
# #######################################################
# #All-Inclusive @ 5 
# 
# # define range for imm rates
# ir <- 0:100
# 
# # Make the plot canvas onto which we'll add lines
# plot(x = c(0,100),
#      y = c(-5, 10),
#      type = "n",
#      xlab = "Immunization Rate (%)", 
#      ylab = "Net Revenue per Dose ($)",
#      cex.lab = 1,
#      cex.axis = 0.7)
# # Add a zero line
# abline(h = 0)
# 
# # define different line types, colors, reim rates and imm_rates for each line
# # line_types <- c(2, 3, 4)
# # colors <- c("darkred", "darkorange", "darkgreen", "blue")
# # imm <- c(67, 70, 80, 90)
# # reim <- c(5, 10, 15)
# 
# # LOOP TO DRAW LINES
# range_reim <- c(100, 100, 100, 100, 100, 100)
# range_amount <- c(23.5, 37, 47, 50, 55, 60)
# range_amount_public <- c(5, 5, 5, 5 , 5, 5)
# #range_colors <- c("black", "grey", "darkblue", "dodgerblue3", "darkcyan", "cadetblue")
# directional_colors <- rev(
#   colorRampPalette(c("darkblue", "green", "darkorange"))(6))
# range_lty <- c(1:6)
# range_lwd <- c(1,2,3,1,2,3)
# for (i in 1:length(range_reim)){
#   for (j in 1:length(range_amount)){
#     
#     
#     # Draw a line for VFC, with reimbursement being $5
#     temp_vac <- simp_vac(n_private = alachua$priv_students, 
#                          n_nonprivate = alachua$unin_students + alachua$medic_students,
#                          imm_rate_private = ir,
#                          imm_rate_nonprivate = ir,
#                          vac_cost_private=17.50, #dfault
#                          vac_cost_nonprivate=0)
#     temp_cost <- simp_cost(n = alachua$priv_students + alachua$unin_students + alachua$medic_students,
#                            imm_rate = ir,
#                            start_up = 100000) #plAce holder 
#     temp_rev <- simp_rev(n_private = alachua$priv_students, 
#                          n_nonprivate = alachua$unin_students + alachua$medic_students,
#                          suc_bill_private = range_reim[i],
#                          suc_bill_nonprivate = range_reim[i],
#                          reim_private = range_amount[j],
#                          reim_nonprivate = range_amount_public[j],
#                          imm_rate_private = ir,
#                          imm_rate_nonprivate = ir)
#     
#     temp_net <- ((temp_rev - temp_cost - temp_vac)/40000)
#     lines(x = 0:100, 
#           y = temp_net,
#           col = directional_colors[j],
#           lty = range_lty[j],
#           lwd = range_lwd[j])
#   }
# }
# legend ( x = "bottomleft",
#          lty = range_lty,
#          lwd = range_lwd,
#          col= directional_colors,#range_colors,
#          legend = paste0("$",(range_amount)),  
#          cex= 0.8,
#          bty = "n",
#          ncol = 3)
# 
# 
# title (main = "All-Inclusive Approach: VFC & Non-VFC at $5")
# ##############################################################
# ##############################################################
# ## Both, Collab at 5 VFC 
# 
# # define range for imm rates
# ir <- 0:100
# 
# # Make the plot canvas onto which we'll add lines
# plot(x = c(0,100),
#      y = c(-5, 10),
#      type = "n",
#      xlab = "Immunization Rate (%)", 
#      ylab = "Net Revenue per Dose ($)",
#      cex.lab = 1,
#      cex.axis = 0.7)
# # Add a zero line
# abline(h = 0)
# 
# # define different line types, colors, reim rates and imm_rates for each line
# # line_types <- c(2, 3, 4)
# # colors <- c("darkred", "darkorange", "darkgreen", "blue")
# # imm <- c(67, 70, 80, 90)
# # reim <- c(5, 10, 15)
# 
# # LOOP TO DRAW LINES
# range_reim <- c(100, 100, 100, 100, 100, 100) 
# range_amount <- c(23.5, 37, 47, 50, 55, 60)
# range_amount_public <- c(10, 10, 10, 10 , 10, 10)
# #range_colors <- c("black", "grey", "darkblue", "dodgerblue3", "darkcyan", "cadetblue")
# directional_colors <- rev(
#   colorRampPalette(c("darkblue", "green", "darkorange"))(6))
# 
# range_lty <- c(1:6)
# range_lwd <- c(1,2,3,1,2,3)
# for (i in 1:length(range_reim)){
#   for (j in 1:length(range_amount)){
#     
#     
#     # Draw a line for VFC, with reimbursement being $5
#     temp_vac <- simp_vac(n_private = alachua$priv_students, 
#                          n_nonprivate = alachua$unin_students + alachua$medic_students,
#                          imm_rate_private = ir,
#                          imm_rate_nonprivate = ir,
#                          vac_cost_private=17.50, #dfault
#                          vac_cost_nonprivate=0)
#     temp_cost <- simp_cost(n = alachua$priv_students + alachua$unin_students + alachua$medic_students,
#                            imm_rate = ir)
#     temp_rev <- simp_rev(n_private = alachua$priv_students, 
#                          n_nonprivate = alachua$unin_students + alachua$medic_students,
#                          suc_bill_private = range_reim[i],
#                          suc_bill_nonprivate = range_reim[i],
#                          reim_private = range_amount[j],
#                          reim_nonprivate = range_amount_public[j],
#                          imm_rate_private = ir,
#                          imm_rate_nonprivate = ir)
#     
#     temp_net <- ((temp_rev - temp_cost - temp_vac)/40000)
#     lines(x = 0:100, 
#           y = temp_net,
#           col = directional_colors[j],
#           lty = range_lty[j],
#           lwd = range_lwd[j])
#   }
# }
# legend ( x = "bottomleft",
#          lty = range_lty,
#          lwd = range_lwd,
#          col= directional_colors,#range_colors,
#          legend = paste0("$",(range_amount)), #rev(range_amount)), 
#          cex= 0.8,
#          bty = "n",
#          ncol = 3)
# 
# 
# title (main = "Collaborative Approach: VCF & Non-VFC at $10") 
# ############################################################
# #######################################################
# #All-Inclusive @ 5 
# 
# # define range for imm rates
# ir <- 0:100
# 
# # Make the plot canvas onto which we'll add lines
# plot(x = c(0,100),
#      y = c(-5, 10),
#      type = "n",
#      xlab = "Immunization Rate (%)", 
#      ylab = "Net Revenue per Dose ($)",
#      cex.lab = 1,
#      cex.axis = 0.7)
# # Add a zero line
# abline(h = 0)
# 
# # define different line types, colors, reim rates and imm_rates for each line
# # line_types <- c(2, 3, 4)
# # colors <- c("darkred", "darkorange", "darkgreen", "blue")
# # imm <- c(67, 70, 80, 90)
# # reim <- c(5, 10, 15)
# 
# # LOOP TO DRAW LINES
# range_reim <- c(100, 100, 100, 100, 100, 100)
# range_amount <- c(23.5, 37, 47, 50, 55, 60)
# range_amount_public <- c(10, 10, 10, 10, 10, 10)
# #range_colors <- c("black", "grey", "darkblue", "dodgerblue3", "darkcyan", "cadetblue")
# directional_colors <- rev(
#   colorRampPalette(c("darkblue", "green", "darkorange"))(6))
# range_lty <- c(1:6)
# range_lwd <- c(1,2,3,1,2,3)
# for (i in 1:length(range_reim)){
#   for (j in 1:length(range_amount)){
#     
#     
#     # Draw a line for VFC, with reimbursement being $5
#     temp_vac <- simp_vac(n_private = alachua$priv_students, 
#                          n_nonprivate = alachua$unin_students + alachua$medic_students,
#                          imm_rate_private = ir,
#                          imm_rate_nonprivate = ir,
#                          vac_cost_private=17.50, #dfault
#                          vac_cost_nonprivate=0)
#     temp_cost <- simp_cost(n = alachua$priv_students + alachua$unin_students + alachua$medic_students,
#                            imm_rate = ir,
#                            start_up = 100000) #plAce holder 
#     temp_rev <- simp_rev(n_private = alachua$priv_students, 
#                          n_nonprivate = alachua$unin_students + alachua$medic_students,
#                          suc_bill_private = range_reim[i],
#                          suc_bill_nonprivate = range_reim[i],
#                          reim_private = range_amount[j],
#                          reim_nonprivate = range_amount_public[j],
#                          imm_rate_private = ir,
#                          imm_rate_nonprivate = ir)
#     
#     temp_net <- ((temp_rev - temp_cost - temp_vac)/40000)
#     lines(x = 0:100, 
#           y = temp_net,
#           col = directional_colors[j],
#           lty = range_lty[j],
#           lwd = range_lwd[j])
#   }
# }
# legend ( x = "bottomleft",
#          lty = range_lty,
#          lwd = range_lwd,
#          col= directional_colors,#range_colors,
#          legend = paste0("$",(range_amount)),  
#          cex= 0.8,
#          bty = "n",
#          ncol = 3)
# 
# title (main = "All-Inclusive Approach: VFC & Non-VFC at $10")