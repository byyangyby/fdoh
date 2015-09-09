# ui.R


shinyUI(pageWithSidebar(
  
  
  
  
  headerPanel("School Located Influenza Vaccination for Florida"),  
  
  sidebarPanel(
    
    "",
    
    tags$img(src="https://docs.google.com/drawings/d/1z5OtlqZ8ex1QhHDtsCOvYMM2WgFsYgD1onjSi-Kus40/pub?w=534&h=89", height="100px"),
    
    br(),
    br(),
    
    
    
    
    helpText("Select your county and intended immunization rate."),
    helpText(paste("Then scroll through the tabs in the upper-left to see the potential impact",
                   "and details of a SLIV program in your county.")),
    
    
    selectInput("var", 
                label = "Choose a county",
                choices = c("ALACHUA","BAKER","BAY","BRADFORD",
                            "BREVARD","BROWARD","CALHOUN","CHARLOTTE",
                            "CITRUS","CLAY","COLLIER","COLUMBIA",
                            "DESOTO","DIXIE","DUVAL",
                            "ESCAMBIA",
                            "FLAGLER","FRANKLIN",
                            "GADSDEN","GILCHRIST",
                            "GLADES","GULF","HAMILTON","HARDEE",
                            "HENDRY","HERNANDO","HIGHLANDS","HILLSBOROUGH",
                            "HOLMES","INDIAN RIVER","JACKSON","JEFFERSON",
                            "LAFAYETTE","LAKE","LEE","LEON","LEVY",
                            "LIBERTY","MADISON","MANATEE","MARION",
                            "MARTIN","MIAMI-DADE","MONROE","NASSAU",
                            "OKALOOSA","OKEECHOBEE","ORANGE","OSCEOLA",
                            "PALM BEACH","PASCO","PINELLAS","POLK",
                            "PUTNAM","SANTA ROSA","SARASOTA","SEMINOLE",
                            "ST. JOHNS","ST. LUCIE","SUMTER","SUWANNEE",
                            "TAYLOR","UNION","VOLUSIA",
                            "WAKULLA","WALTON","WASHINGTON"),
                selected = "ALACHUA"),
    
    
    # IMMUNIZATION RATE
    sliderInput("immRate", 
                label=strong("Projected immunization rate:"), 
                min=0, max=100, value=30),
    helpText("(usually 10-60%)", align="center"),
    br(),
    
    # SUCCESSFUL BILLING RATE    
    sliderInput("sucBill", 
                label = strong("Successful (private) billing rate"),
                min = 0, max = 100, value = 88),
    helpText("(80-90% if you pre-emptively arrange contracts)"),
    br(),
    
    
    # REVIEW AND PURSUE COSTS
    sliderInput("randp", 
                label=strong("\'Review and pursue\' cost per enrolled student"), 
                min=0, max=5, value=1),
    
    helpText(paste(
      "\'Review and pursue\' refers to the (time-consuming) act of",
      "contacting families with an unreturned consent form to ensure that",
      "they had a chance to consider having their child immunized.",
      "In our experience, achieving an immunization rate of greater than 30%,",
      "requires that $1-2 per enrolled student be spent on",
      "\'Review and pursue\' activites."
    ),
    helpText("___________"),
    helpText("Joseph.Brew@FloridaHealth.gov")
    
    )
    
  ),
  
  mainPanel(
    
    tabsetPanel(
      tabPanel("Overview",
               
               h3(paste("Investing in SLIV programs can",
                        "save lives and money.")),
               
               textOutput("text2"),
               textOutput("text3"),
               textOutput("text4"),
               plotOutput("plot1"),
               
               
               p(paste("Seasonal influenza has tremendous human and economic costs.",
                       "In an average year, more than 2 million Floridians fall ill to flu,",
                       "resulting in greater than 8000 hospitalizations and 2600 deaths.",
                       "Flu costs the state of Florida approximately a billion dollars in direct",
                       "and indirect economic costs.")),
               
               p(paste("Flu strains our health care system, decreases worker productivity,",
                       "increases school absenteeism and causes unnecessary human suffering and death.",
                       "No other vaccine-preventable illness exerts anywhere near flu's toll in the United States.")),
               
               p(paste("Schools offer the best platform by which a community can protect itself against flu.",
                       "Not only do SLIV programs improve access to a recommended health service,",
                       "immunizing children protects the most vulnerable from flu's worst effects",
                       "(<5 and >65 years-old)."))
               
               
               
      ),
      tabPanel("Health impact",
               
               h3(paste("Protecting children from the flu also protects the entire community.")),
               
               textOutput("text12"),
               textOutput("text13"),
               textOutput("text14"),
               plotOutput("plot2"),
               plotOutput("plot3")
      ),
      
      tabPanel("Costs and revenue",
               
               h3(paste("Most SLIV programs pay for themselves.")),
               
               
               plotOutput("plot5"),
               h3("Costs"),
               textOutput("text7"),
               textOutput("text8"),
               textOutput("text8b"),
               
               h3("Revenue"),
               textOutput("text9"),
               textOutput("text10"),
               textOutput("text11"),
               
               h4("Details"),
               p("\'Other program costs\' are vaccine storage and delivery, 
                 billing administration, 
                 printing consent forms,
                 nurse wages and \'reviewing and pursuing\' unreturned consent forms")
               ),
      
      
      tabPanel("School breakdown",
               
               h3(paste("Partnering with schools is crucial to SLIV success.")),
               
               
               plotOutput("plot4"), 
               textOutput("text5"),
               textOutput("text6"),
               textOutput("text6b"),
               tableOutput("table1")
      ),
      
      tabPanel("Details",
               
               h3(em("Control Flu:"),paste("A model for economic sustainability.")),
               
               h4(paste("Background")),
               
               
               p("Alachua County began its SLIV program, \"Control Flu,\" in 2006.",
                 "It has run continuously since 2009, and immunizes approximately 10,000 to 13,000 schoolchildren annually.",
                 "Approximately 42% of all Alachua schoolchildren (pre-K through 12th) are immunized by \"Control Flu,\"",
                 "whereas an additional 21% receive vaccine from local providers. ",
                 "This high pediatric immunization rate (62.7%) contributes to a signficantly lower ",
                 "incidence of emergency cases of influenza-like illness in Alachua County compared ",
                 "to other counties in the State. "),
               
               p("In addition to protecting the health of the community, \"Control Flu\" has",
                 "demonstrated that SLIV programs are economically sustainable, and can even generate a surplus.",
                 "Despite the fact that \"Control Flu\" is made up of a diversity of community",
                 "partners (FDOH-Alachua, the University of Florida, the Emerging Pathogens",
                 "Institute, Alachua County Public Schools and many local organizations), the costs modelled here assume that",
                 "the CHD will absorb all immunization-related expenses (vaccine purchase,",
                 "storage, delivery, administration and billing); in reality, if community partners",
                 "are involved, costs could be significantly lower."),
               
               h4(paste("Costs")),
               
               p("The components of a SLIV program's total cost are vaccine purchase, vaccine storage",
                 "vaccine delivery, vaccine administration (nurse person-hours),",
                 "billing administration, consent form review, record-keeping (clerical person-hours),",
                 "printing and other material costs."),
               
               p(
                 "An additional component, \"review and pursue\" (the act of reviewing consent forms and",
                 "following up with the families of unreturned forms to ensure that they had a chance to",
                 "consider immunization), is not necessary for a SLIV program to function.  However,",
                 "Alachua's experience (and that of other counties in Florida) suggests that in order to",
                 "achieve immunization rates greater than 30%, \"review and pursue\" is necessary,",
                 "and should be funded on the order of $1-3 per enrolled student."
               ),
               
               p( 
                 "The overall cost of these components is a function of the number of students in a school, ",
                 "the percentage with private health insurance, and the hourly rate of SLIV program employees. ",
                 "For simplicity's sake, this website calculates most costs based on Alachu'a experience."
               ),
               
               p("Model parameters were based directly off Alachua's documented experience when possible,",
                 "and estimated indirectly when necessary. Vaccine purchase was set at $17.50 (private) and",
                 "$0.00 (VFC).  Vaccine storage was estimated to be $0.05 per vaccine (with no additional",
                 "purchase or manpower requirements for a typical CHD). Vaccine delivery was estimated at",
                 "$0.05 per vaccine.  Vaccine administration (nurse wages per vaccine) was set at",
                 "$0.95 per vaccine, based on the experience of a team of non-volunteer nurses at FDOH-Alachua",
                 "(at an average rate of $25.00 per hour)."
               ),
               
               p("Billing, review and record-keeping was set at $5.23 per vaccine",
                 "(person-hours), and printing / other material costs were set at $0.10."),
               
               
               
               
               h4(paste("Revenue")),
               
               p("The components of a SLIV program's revenue are simply reimbursement for VFC",
                 "administration and private vaccine purchase and administration.",
                 "The overall revenue generated by these components is also a function of the",
                 "number of students in a school, the percentage with private health insurance,",
                 "as well as the average reimbursement rate from private insurers and the successful",
                 "billing rate of those insurers."
               ),
               
               p("Average reimbursement for private vaccines was $39.39 (including both purchase",
                 "and administration).  The denial rate for private vaccines was approximately 12.5%.",
                 "The reimbursement to CHDs for VFC vaccines is $5.00.",
                 "19.12% of students who receive the VFC vaccine are estimated to be\"unbillable\"",
                 "(no public insurance coverage whatsoever)",
                 "and approximately 5.12% of children who receive a private vaccine are",
                 "also \"unbillable.\""
                 
               ),
               
               h4(paste("Data sources")),
               
               p("School enrollment data was obtained from the Florida Department of Education.",
                 "The percentage of children eligible to receive the VFC vaccine was estimated to be approximately",
                 "85% the number of students on free/reduced lunch (based on Alachua's experience)."     
               ),
               
               h4(paste("Further information")),
               
               
               p(
                 "Full model details and assistance are available upon request to a county",
                 "which requires a more customized approach (ie, their",
                 "storage, administrative, purchase or billing costs are likely to be significantly", 
                 "different from Alachua's.)")
               
               
               
               
      ),
      
      tabPanel("Obtaining High Immunization Rates",
               
               
               
               p(paste("The first year of a program that distributes parental consent forms and immunizes those children who have signed forms achieve",
                       "rates usually in the 15 - 25% range for elementary and middle schools, and lower rates for high schools. After several years of",
                       "community awareness these rates reach the 30 - 40% range (Alabama, Hawaii and Gainesville private elementary schools).",
                       "These rates are achieved with minimal disruption and involvement of school personnel,",
                       "whether the immunization is provided by private companies or health departments. Achieving higher rates requires extra effort.")),
               
               p(paste("In Alachua County we have achieved 60% immunization rates in some elementary and middle schools and another 10 - 15% in physicians' offices",
                       ", thus achieving the greater than 70% rate, predicted by mathematical modeling studies,",
                       "to be sufficient to prevent flu outbreaks in the community (Longini IM, Jr. A Theoretic Framework to Consider the Effect of Immunizing Schoolchildren Against Influenza: Implications for Research. Pediatrics 2012; 129;S63-S67.). Two strategies have led to this result.",
                       "The first is using a consent form that asks the parent to identify whether or not they want their child immunized, and requests that the form be returned irrespective of the answer.",
                       "The second is to have the forms reviewed and to pursue non-returned forms.",
                       "The optimal way to do this probably varies widely depending on community resources.",
                       "Alachua is unique in having a nurse in every school, plus nursing and public health student volunteers. Were a county not to have volunteers, then \"Review and Pursue\" activities could be costly",
                       "(which is why we've included a \"Review and Pursue\" slider on the main page).  One possibility for funding these activities would be to appoint a part-time flu program coordinator, selected by the principal, and paid depending on the number of students the coordinator is responsbile for.",
                       "Such funds would help involved the schools in the program, and avoid the program being seen as an unfunded mandate. Another possibility is to reward teachers and/or classes that achieve 100% form return rates.",
                       "Other possibilities include mobilizing local volunteers, such as parents and/or Medical Reserve Corps members to coordinate the program. ",
                       "Given appropriate funding, the University of Florida Emerging Pathogens Institute is prepared to create an online training program for such coordinators.")),
               
               p(paste("To raise high school rates we have developed a one 50 minute class activity, taught by biology teachers, that almost doubles students intent to be immunized.",
                       "It includes a "Community Immunity card game" that simulates a flu outbreak without and with 50% immunization rates.",
                       "Students learn that getting immunized not only protect them, but their friend and family as well.  ",
                       "The game and associated power point presentations are available at www.controlflu.com."))
               
               
               
      )   
      
               )
    
    
    
    
      )
  ))