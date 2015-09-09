# server.R

#load in packages
library(maps)
library(classInt)
library(RCurl)
library(xtable)
library(maps)
library(RColorBrewer)

#source the model file
source("~/ShinyApps/slivModel/model.R")


shinyServer(
  function(input, output) {
    
    
    
    output$text2 <- renderText({ 
      paste("Program costs: $",
            round(CostFun(totMem = sum(dfSchool$totMem[which(dfSchool$district == input$var)], na.rm=TRUE), 
                          vfcPer = weighted.mean(dfSchool$vfcPer[which(dfSchool$district == input$var)],
                                                 dfSchool$totMem[which(dfSchool$district == input$var)],
                                                 na.rm=TRUE),
                          immRate = input$immRate,
                          privateVacCost = 17.5,
                          delivCost = .05,
                          billCost = 5.23,
                          printCost = .1,
                          storageCost = .05,
                          nursePerHour = 25,
                          randp=input$randp), 
                  digits=2)
            
      )
    })
    
    output$text3 <- renderText({ 
      paste("Program gross revenue: $",
            round(RevFun(totMem = sum(dfSchool$totMem[which(dfSchool$district == input$var)], na.rm=TRUE), 
                         vfcPer = weighted.mean(dfSchool$vfcPer[which(dfSchool$district == input$var)],
                                                dfSchool$totMem[which(dfSchool$district == input$var)],
                                                na.rm=TRUE),
                         immRate = input$immRate,
                         privDenRate = 100 - input$sucBill,
                         privUnbillableRate = 5.123104,
                         vfcDenRate = 20,
                         privAvgRe = 39.39,
                         vfcAvgRe = 5), 
                  digits=2)
            
      )
    })
    
    output$text4 <- renderText({ 
      paste("Net surplus/deficit: $",
            round(round(RevFun(totMem = sum(dfSchool$totMem[which(dfSchool$district == input$var)], na.rm=TRUE), 
                               vfcPer = weighted.mean(dfSchool$vfcPer[which(dfSchool$district == input$var)],
                                                      dfSchool$totMem[which(dfSchool$district == input$var)],
                                                      na.rm=TRUE),
                               immRate = input$immRate,
                               privDenRate = 100 - input$sucBill,
                               privUnbillableRate = 5.123104,
                               vfcDenRate = 20,
                               privAvgRe = 39.39,
                               vfcAvgRe = 5), 
                        digits=2) - 
                    round(CostFun(totMem = sum(dfSchool$totMem[which(dfSchool$district == input$var)], na.rm=TRUE), 
                                  vfcPer = weighted.mean(dfSchool$vfcPer[which(dfSchool$district == input$var)],
                                                         dfSchool$totMem[which(dfSchool$district == input$var)],
                                                         na.rm=TRUE),
                                  immRate = input$immRate,
                                  privateVacCost = 17.5,
                                  delivCost = .05,
                                  billCost = 5.23,
                                  printCost = .1,
                                  storageCost = .05,
                                  nursePerHour = 25,
                                  randp=input$randp), 
                          digits=2), digits=2)
            
      )
    })
    
    output$text5 <- renderText({ 
      paste(
        "Number of students:",
        sum(dfSchool$totMem[which(dfSchool$district == input$var)], na.rm=TRUE)
      )
    })
    
    output$text6 <- renderText({ 
      paste(
        "Percent with private health insurance:",
        paste(round(100 - weighted.mean(dfSchool$vfcPer[which(dfSchool$district == input$var)],
                                        dfSchool$totMem[which(dfSchool$district == input$var)], 
                                        na.rm=TRUE), digits=2)),
        "%"
        
      )
    })
    
    
    output$text6b <- renderText({ 
      paste(
        "Number of schools:",
        nrow(dfSchool[which(dfSchool$district == input$var),])
        
      )
    })
    
    
    output$text7 <- renderText({ 
      paste(
        "Vaccine purchase cost: $",
        
        round(VacFun(totMem = sum(dfSchool$totMem[which(dfSchool$district == input$var)], na.rm=TRUE), 
                     vfcPer = weighted.mean(dfSchool$vfcPer[which(dfSchool$district == input$var)],
                                            dfSchool$totMem[which(dfSchool$district == input$var)],
                                            na.rm=TRUE),
                     immRate = input$immRate,
                     privateVacCost = 17.5), 
              digits=2)
        
      )
    })
    
    
    
    
    output$text8 <- renderText({ 
      paste(
        "Other program costs: $",
        
        round(OtherCostFun(totMem = sum(dfSchool$totMem[which(dfSchool$district == input$var)], na.rm=TRUE), 
                           vfcPer = weighted.mean(dfSchool$vfcPer[which(dfSchool$district == input$var)],
                                                  dfSchool$totMem[which(dfSchool$district == input$var)],
                                                  na.rm=TRUE),
                           immRate = input$immRate,
                           delivCost = .05,
                           billCost = 5.23,
                           printCost = .1,
                           storageCost = .05,
                           nursePerHour = 25,
                           randp=input$randp), 
              digits=2)
        
        
      )
    })
    
    
    
    
    output$text8b <- renderText({ 
      paste(
        "Total costs: $",
        
        round(CostFun(totMem = sum(dfSchool$totMem[which(dfSchool$district == input$var)], na.rm=TRUE), 
                      vfcPer = weighted.mean(dfSchool$vfcPer[which(dfSchool$district == input$var)],
                                             dfSchool$totMem[which(dfSchool$district == input$var)],
                                             na.rm=TRUE),
                      immRate = input$immRate,
                      privateVacCost = 17.5,
                      delivCost = .05,
                      billCost = 5.23,
                      printCost = .1,
                      storageCost = .05,
                      nursePerHour = 25,
                      randp=input$randp), 
              digits=2)
        
      )
    })
    
    
    output$text9 <- renderText({ 
      paste("Revenue from billing private insurers: $",
            round(privateRevFun(totMem = sum(dfSchool$totMem[which(dfSchool$district == input$var)], na.rm=TRUE), 
                                vfcPer = weighted.mean(dfSchool$vfcPer[which(dfSchool$district == input$var)],
                                                       dfSchool$totMem[which(dfSchool$district == input$var)],
                                                       na.rm=TRUE),
                                immRate = input$immRate,
                                privDenRate = 100 - input$sucBill,
                                privUnbillableRate = 5.123104,
                                vfcDenRate = 20,
                                privAvgRe = 39.39,
                                vfcAvgRe = 5), 
                  digits=2))
      
    })
    
    output$text10 <- renderText({ 
      paste("Revenue for VFC administration: $",
            round(vfcRevFun(totMem = sum(dfSchool$totMem[which(dfSchool$district == input$var)], na.rm=TRUE), 
                            vfcPer = weighted.mean(dfSchool$vfcPer[which(dfSchool$district == input$var)],
                                                   dfSchool$totMem[which(dfSchool$district == input$var)],
                                                   na.rm=TRUE),
                            immRate = input$immRate,
                            privDenRate = 100 - input$sucBill,
                            privUnbillableRate = 5.123104,
                            vfcDenRate = 20,
                            privAvgRe = 39.39,
                            vfcAvgRe = 5), 
                  digits=2))
    })
    
    
    output$text11 <- renderText({ 
      paste("Total gross revenue: $",
            round(RevFun(totMem = sum(dfSchool$totMem[which(dfSchool$district == input$var)], na.rm=TRUE), 
                         vfcPer = weighted.mean(dfSchool$vfcPer[which(dfSchool$district == input$var)],
                                                dfSchool$totMem[which(dfSchool$district == input$var)],
                                                na.rm=TRUE),
                         immRate = input$immRate,
                         privDenRate = 100 - input$sucBill,
                         privUnbillableRate = 5.123104,
                         vfcDenRate = 20,
                         privAvgRe = 39.39,
                         vfcAvgRe = 5), 
                  digits=2))
    })
    
    
    output$text12 <- renderText({ 
      paste("The following is interpolated from",
            "Weycker D, Edelsberg J, Halloran ME, et al.",
            "Population-wide benefits of routine vaccination of children against influenza.",
            "Vaccine 2005; 23(10): 1284-93.")
    })
    
    output$text13 <- renderText({ 
      paste("(Figures have been adjusted for inflation and population growth, 
            and assume a 5% non-SLIV pediatric immunization rate)")
    })
    
    output$text14 <- renderText({ 
      paste(" ")
    })
    
    
    
    
    
    
    
    output$plot1 <- renderPlot({
      
      par(mfrow=c(1,2))
      myMap$color <- ifelse(myMap$county == input$var,
                            "blue",
                            "grey")
      
      map("county", "florida", border="darkgrey", fill=TRUE, col=myMap$color)
      
      
      barplot(
        c(round(CostFun(totMem = sum(dfSchool$totMem[which(dfSchool$district == input$var)], na.rm=TRUE), 
                        vfcPer = weighted.mean(dfSchool$vfcPer[which(dfSchool$district == input$var)],
                                               dfSchool$totMem[which(dfSchool$district == input$var)],
                                               na.rm=TRUE),
                        immRate = input$immRate,
                        privateVacCost = 17.5,
                        delivCost = .05,
                        billCost = 5.23,
                        printCost = .1,
                        storageCost = .05,
                        nursePerHour = 25,
                        randp=input$randp), 
                digits=0), 
          round(RevFun(totMem = sum(dfSchool$totMem[which(dfSchool$district == input$var)], na.rm=TRUE), 
                       vfcPer = weighted.mean(dfSchool$vfcPer[which(dfSchool$district == input$var)],
                                              dfSchool$totMem[which(dfSchool$district == input$var)],
                                              na.rm=TRUE),
                       immRate = input$immRate,
                       privDenRate = 100 - input$sucBill,
                       privUnbillableRate = 5.123104,
                       vfcDenRate = 20,
                       privAvgRe = 39.39,
                       vfcAvgRe = 5), 
                digits=0)), 
        names.arg=c("Costs", "Revenue"),
        border="darkgrey",
        col=adjustcolor(c("darkred", "darkgreen"))
      )      
    })
    
    
    
    output$plot2 <- renderPlot({
      
      par(mfrow=c(1,2))
      
      
      #CASES AVERTED
      plot(1:2, 1:2,
           xlim=c(0,100),
           ylim=c(0,max(fl$cases[which(fl$county == input$var)])),
           type="n",
           xlab="SLIV Immunization rate",
           ylab="Influenza cases (annual)")
      
      for (i in 1:100){
        points(x = i,
               y = max(fl$cases[which(fl$county == input$var)])  - CasesFun(county = input$var, 
                                                                            immRate = i),
               pch=16,
               cex=0.5,
               col=adjustcolor("black", alpha.f=0.5))
      }
      
      points(x = input$immRate,
             y = max(fl$cases[which(fl$county == input$var)])  - CasesFun(county = input$var, 
                                                                          immRate = input$immRate),
             pch=16,
             cex=2,
             col="red")
      
      abline(h = max(fl$cases[which(fl$county == input$var)])  - CasesFun(county = input$var, 
                                                                          immRate = input$immRate),
             col=adjustcolor("black", alpha.f=0.2))
      
      abline(v=input$immRate,
             col=adjustcolor("black", alpha.f=0.2))
      
      text(x = ifelse(input$immRate < 80,
                      input$immRate + 10,
                      input$immRate - 10),
           y = ifelse(input$immRate < 20,
                      (max(fl$cases[which(fl$county == input$var)])  - CasesFun(county = input$var, 
                                                                                immRate = input$immRate)) *0.75,
                      (max(fl$cases[which(fl$county == input$var)])  - CasesFun(county = input$var, 
                                                                                immRate = input$immRate)) *1.5),
           labels=paste0(round(CasesFun(county = input$var, 
                                        immRate = input$immRate), digits=0),
                         " cases\n averted"))
      
      title(main="Cases")
      
      
      #LIVES SAVED
      ##### 
      plot(1:2, 1:2,
           xlim=c(0,100),
           ylim=c(0,max(fl$deaths[which(fl$county == input$var)])),
           type="n",
           xlab="SLIV Immunization rate",
           ylab="Influenza deaths (annual)")
      
      for (i in 1:100){
        points(x = i,
               y = max(fl$deaths[which(fl$county == input$var)])  - DeathsFun(county = input$var, 
                                                                              immRate = i),
               pch=16,
               cex=0.5,
               col=adjustcolor("black", alpha.f=0.5))
      }
      
      points(x = input$immRate,
             y = max(fl$deaths[which(fl$county == input$var)])  - DeathsFun(county = input$var, 
                                                                            immRate = input$immRate),
             pch=16,
             cex=2,
             col="red")
      
      abline(h = max(fl$deaths[which(fl$county == input$var)])  - DeathsFun(county = input$var, 
                                                                            immRate = input$immRate),
             col=adjustcolor("black", alpha.f=0.2))
      
      abline(v=input$immRate,
             col=adjustcolor("black", alpha.f=0.2))
      
      text(x = ifelse(input$immRate < 80,
                      input$immRate + 10,
                      input$immRate - 10),
           y = ifelse(input$immRate < 20,
                      (max(fl$deaths[which(fl$county == input$var)])  - DeathsFun(county = input$var, 
                                                                                  immRate = input$immRate)) *0.75,
                      (max(fl$deaths[which(fl$county == input$var)])  - DeathsFun(county = input$var, 
                                                                                  immRate = input$immRate)) *1.5),
           labels=paste0(round(DeathsFun(county = input$var, 
                                         immRate = input$immRate), digits=0),
                         " lives \n saved"))
      
      title(main="Deaths")
      
      title(main=input$var, outer=TRUE, line=-1)
      
      
    })
    
    
    output$plot3 <- renderPlot({
      
      par(mfrow= c(1,2))
      
      #hospitalizations
      ##### 
      plot(1:2, 1:2,
           xlim=c(0,100),
           ylim=c(0,max(fl$hospitalizations[which(fl$county == input$var)])),
           type="n",
           xlab="SLIV Immunization rate",
           ylab="Influenza hospitalizations (annual)")
      
      for (i in 1:100){
        points(x = i,
               y = max(fl$hospitalizations[which(fl$county == input$var)])  - HospitalizationsFun(county = input$var, 
                                                                                                  immRate = i),
               pch=16,
               cex=0.5,
               col=adjustcolor("black", alpha.f=0.5))
      }
      
      points(x = input$immRate,
             y = max(fl$hospitalizations[which(fl$county == input$var)])  - HospitalizationsFun(county = input$var, 
                                                                                                immRate = input$immRate),
             pch=16,
             cex=2,
             col="red")
      
      abline(h = max(fl$hospitalizations[which(fl$county == input$var)])  - HospitalizationsFun(county = input$var, 
                                                                                                immRate = input$immRate),
             col=adjustcolor("black", alpha.f=0.2))
      
      abline(v=input$immRate,
             col=adjustcolor("black", alpha.f=0.2))
      
      text(x =ifelse(input$immRate < 80,
                     input$immRate + 10,
                     input$immRate - 10),
           y = ifelse(input$immRate < 20,
                      (max(fl$hospitalizations[which(fl$county == input$var)])  - HospitalizationsFun(county = input$var, 
                                                                                                      immRate = input$immRate)) *0.75,
                      (max(fl$hospitalizations[which(fl$county == input$var)])  - HospitalizationsFun(county = input$var, 
                                                                                                      immRate = input$immRate)) *1.5),
           labels=paste0(round(HospitalizationsFun(county = input$var, 
                                                   immRate = input$immRate), digits=0),
                         " \n hospitalizations \n averted"))      
      
      title(main="Hospitalizations")
      
      
      
      #COST REDUCTIONS
      
      
      #DIRECT COSTS
      
      ##### 
      plot(1:2, 1:2,
           xlim=c(1,100),
           ylim=c(1,max(fl$indirectCosts[which(fl$county == input$var)])),
           type="n",
           xlab="SLIV Immunization rate",
           ylab="Costs (annual)",
           cex.axis=0.7)
      
      for (i in 1:100){
        points(x = i,
               y = max(fl$directCosts[which(fl$county == input$var)])  - DirectCostsFun(county = input$var, 
                                                                                        immRate = i),
               pch=16,
               cex=0.5,
               col=adjustcolor("red", alpha.f=0.5))
      }
      
      points(x = input$immRate,
             y = max(fl$directCosts[which(fl$county == input$var)])  - DirectCostsFun(county = input$var, 
                                                                                      immRate = input$immRate),
             pch=16,
             cex=2,
             col="red")
      
      abline(h = max(fl$directCosts[which(fl$county == input$var)])  - DirectCostsFun(county = input$var, 
                                                                                      immRate = input$immRate),
             col=adjustcolor("black", alpha.f=0.2))
      
      abline(v=input$immRate,
             col=adjustcolor("black", alpha.f=0.2))
      
      text(x = ifelse(input$immRate < 80,
                      input$immRate + 10,
                      input$immRate - 10),
           y = ifelse(input$immRate < 20,
                      (max(fl$directCosts[which(fl$county == input$var)])  - DirectCostsFun(county = input$var, 
                                                                                            immRate = input$immRate)) *0.75,
                      (max(fl$directCosts[which(fl$county == input$var)])  - DirectCostsFun(county = input$var, 
                                                                                            immRate = input$immRate)) *1.5),
           labels=paste0("$" ,round(DirectCostsFun(county = input$var, 
                                                   immRate = input$immRate)/1000000, digits=2),
                         " million saved \n in direct costs"),
           cex=0.8)
      
      
      #INDIRECT COSTS
      
      for (i in 1:100){
        points(x = i,
               y = max(fl$indirectCosts[which(fl$county == input$var)])  - IndirectCostsFun(county = input$var, 
                                                                                            immRate = i),
               pch=16,
               cex=0.5,
               col=adjustcolor("blue", alpha.f=0.5))
      }
      
      
      points(x = input$immRate,
             y = max(fl$indirectCosts[which(fl$county == input$var)])  - IndirectCostsFun(county = input$var, 
                                                                                          immRate = input$immRate),
             pch=16,
             cex=2,
             col="blue")
      
      text(x = ifelse(input$immRate < 80,
                      input$immRate + 10,
                      input$immRate - 10),
           y = ifelse(input$immRate < 20,
                      (max(fl$indirectCosts[which(fl$county == input$var)])  - IndirectCostsFun(county = input$var, 
                                                                                                immRate = input$immRate)) *0.75,
                      (max(fl$indirectCosts[which(fl$county == input$var)])  - IndirectCostsFun(county = input$var, 
                                                                                                immRate = input$immRate)) *1.5),
           labels=paste0("$" ,round(IndirectCostsFun(county = input$var, 
                                                     immRate = input$immRate)/1000000, digits=2),
                         " million saved \n in indirect costs"),
           cex=0.8)
      
      
      title(main="Costs of flu")
      
    })
    
    
    
    
    
    output$plot4 <- renderPlot({
      
      par(mar=c(6,4,1,1))
      par(oma=c(8,1,1,1))
      
      temp <- dfSchool[which(dfSchool$district == input$var),]
      temp <- temp[order(temp$totMem),]
      
      barplot(temp$totMem,
              yaxt="n",
              xaxt="n",
              names.arg=NA)
      barplot(temp$totMem*temp$vfcPer/100,
              names.arg=temp$School,
              cex.names=ifelse(nrow(temp)>100,
                               0.1,
                               0.5),
              las=3,
              add=TRUE,
              col=adjustcolor("darkred", alpha.f=0.4),
              border=NA,
              ylab="Students")
      
      legend(x="topleft",
             fill=adjustcolor("darkred", alpha.f=0.5),
             border="darkgrey",
             legend="VFC eligible",
             bty="n")
      
      
    })
    
    
    output$plot5 <- renderPlot({
      
      par(mfrow=c(1,2))
      par(mar=c(5,3,5,3))
      par(oma=c(0,0,0,0))
      
      pie(c(
        VacFun(totMem = sum(dfSchool$totMem[which(dfSchool$district == input$var)], na.rm=TRUE), 
               vfcPer = weighted.mean(dfSchool$vfcPer[which(dfSchool$district == input$var)],
                                      dfSchool$totMem[which(dfSchool$district == input$var)],
                                      na.rm=TRUE),
               immRate = input$immRate,
               privateVacCost = 17.5),
        OtherCostFun(totMem = sum(dfSchool$totMem[which(dfSchool$district == input$var)], na.rm=TRUE), 
                     vfcPer = weighted.mean(dfSchool$vfcPer[which(dfSchool$district == input$var)],
                                            dfSchool$totMem[which(dfSchool$district == input$var)],
                                            na.rm=TRUE),
                     immRate = input$immRate,
                     delivCost = .05,
                     billCost = 5.23,
                     printCost = .1,
                     storageCost = .05,
                     nursePerHour = 25,
                     randp=input$randp)),
        labels=c("Vaccine purchase", 
                 "Other costs"),
        main="Program costs"
      )
      
      
      #REVENUE
      pie(c(privateRevFun(totMem = sum(dfSchool$totMem[which(dfSchool$district == input$var)], na.rm=TRUE), 
                          vfcPer = weighted.mean(dfSchool$vfcPer[which(dfSchool$district == input$var)],
                                                 dfSchool$totMem[which(dfSchool$district == input$var)],
                                                 na.rm=TRUE),
                          immRate = input$immRate,
                          privDenRate = 100 - input$sucBill,
                          privUnbillableRate = 5.123104,
                          vfcDenRate = 20,
                          privAvgRe = 39.39,
                          vfcAvgRe = 5),
            vfcRevFun(totMem = sum(dfSchool$totMem[which(dfSchool$district == input$var)], na.rm=TRUE), 
                      vfcPer = weighted.mean(dfSchool$vfcPer[which(dfSchool$district == input$var)],
                                             dfSchool$totMem[which(dfSchool$district == input$var)],
                                             na.rm=TRUE),
                      immRate = input$immRate,
                      privDenRate = 100 - input$sucBill,
                      privUnbillableRate = 5.123104,
                      vfcDenRate = 20,
                      privAvgRe = 39.39,
                      vfcAvgRe = 5)),
          labels=c("Billing\nprivate insurers",
                   "Billing\nfor VFC\nadministration"),
          main="Revenue sources")
      
      
    })
    
    
    
    
    # Generate an HTML table view of the data
    output$table1 <- renderTable({
      
      
      
      dfSchool$Cost <- round(CostFun(totMem = dfSchool$totMem, 
                                     vfcPer = dfSchool$vfcPer,
                                     immRate = input$immRate,
                                     privateVacCost = 17.5,
                                     delivCost = .05,
                                     billCost = 5.23,
                                     printCost = .1,
                                     storageCost = .05,
                                     nursePerHour = 25,
                                     randp=input$randp), 
                             digits=0)
      
      
      dfSchool$Revenue <- round(RevFun(totMem = dfSchool$totMem, 
                                       vfcPer = dfSchool$vfcPer,
                                       immRate = input$immRate,
                                       privDenRate = 100 - input$sucBill,
                                       privUnbillableRate = 5.123104,
                                       vfcDenRate = 20,
                                       privAvgRe = 39.39,
                                       vfcAvgRe = 5), 
                                digits=0)
      
      
      dfSchool$priv <- 100 - dfSchool$vfcPer
      
      
      ##############################
      ### Make dfSchool2
      ##############################
      dfSchool2 <- dfSchool[,c("district",
                               "School",
                               "totMem",
                               "vfcPer",
                               "priv",
                               "Cost",
                               "Revenue")]
      
      dfSchool2$vfcPer <- paste0(round(dfSchool2$vfcPer, digits=1), "%")
      dfSchool2$priv <- paste0(round(dfSchool2$priv, digits=1), "%")
      
      dfSchool2$totMem <- gsub(".00","",as.character(dfSchool2$totMem))
      
      
      colnames(dfSchool2) <-
        c("District", "School", "Students", "Percent VFC", 
          "Percent Private", "Cost",
          "Revenue")
      
      dfSchool2$Net <- round(dfSchool2$Revenue - dfSchool2$Cost, digits=0)
      
      dfSchool2$Revenue <- paste0("$", dfSchool2$Revenue)
      dfSchool2$Cost <- paste0("$", dfSchool2$Cost)
      dfSchool2$Net <- paste0("$", dfSchool2$Net)
      
      
      dfSchool2 <- dfSchool2[order(dfSchool2$School),]
      
      
      
      data.frame(dfSchool2[which(dfSchool2$District == input$var),])
    })
    
    
    
    }
)