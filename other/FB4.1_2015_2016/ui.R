require(shiny)
parms <- read.csv("Parameters_official.csv") ###  Read parameter values from .csv file
Diet_prop <- read.csv("inputs/Main_Inputs/Diet_prop.csv",head=TRUE) ### Detects number of prey types

#choicesSpecies <- setNames(1:nrow(parms),parms$Species) ### Allows the consumption output to be broken down by species for individual fish
choicePreyJoules <- names(Diet_prop[,-1])
choicePreyJoules
choicePreyJoules <- paste("Cons",choicePreyJoules,"J", sep = " ")
choicePreyJoules

choicePreyGrams <- names(Diet_prop[,-1])
choicePreyGrams <- paste("Cons",choicePreyGrams,"g", sep = " ")
choicePreyGrams
choicePreyPopJoules <- names(Diet_prop[,-1]) ### Allows the consumption output to be broken down by species for the population
choicePreyPopJoules <- paste("Cons Pop",choicePreyPopJoules,"J", sep = " ")
choicePreyPopJoules
choicePreyPopGrams <- names(Diet_prop[,-1])
choicePreyPopGrams <- paste("Cons Pop",choicePreyPopGrams,"g", sep = " ")
choicePreyPopGrams

shinyUI(
  fluidPage(
  titlePanel('Fish Bioenergetics 4.0'),
  navbarPage('',selected = 'Initial Settings',
                   tabPanel(
                     img(src = "FB4.jpg", height = 50, width = 50)),
  tabPanel("Initial Settings", ### Initial Settings tab
           sidebarLayout(
             sidebarPanel(
      textInput('Simulation','Simulation Name',value = 'test_sim'),
      selectInput('input_folder','File Input Folder',folder_list,"inputs/Main_Inputs"),
      #uiOutput('spec_ui'),
      selectInput("spec",  ### Species selection
                  label = "Species",
                  choices = c("",choicesSpecies),
                  selected = choicesSpecies[1]),
      
      numericInput("ID", ### Intial day input
                   label = "Initial Day",
                   value = 1),
      
      numericInput("FD", ### Final day input
                   label = "Final Day",
                   value = 10),
      
      numericInput("InW", ### Intial weight input
                   label = "Initial Weight (g)", 
                   value = 1),
      
      numericInput("oxycal", ### Oxycalorific Coefficient default value
                   label = "Oxycalorific Coefficient (J/g O2)", 
                   value = 13560),
           
      radioButtons("fitto", ### Fitting options
                   label = h4("Fit to:"), 
                   choices = list("Final Weight (g of wet predator body weight)" = "Weight",
                                  "Consumption (g of total prey wet weight)" = "Consumption",
                                  "Ration (g of daily wet prey weight)" = "Ration_prey",
                                  "Ration (% of daily wet predator weight)" = "Ration",
                                  "p-value (proportion of Cmax)"="p-value"),                                 
                   selected = 'Weight'),
 
      numericInput("FinW", ### Creates a box for the fitting value, which is dependent on the selection made above
                   label = 'fitting value', 
                   value = 5),
      
                 checkboxInput("nut", ### Checkbox to decide if nutrient regeneration should be accounted for
                               label = h4("Nutrient Regeneration"),
                               value = FALSE),
      checkboxInput("contaminant", ### Checkbox to decide if population mortality should be accounted for
                                             label = h4("Contaminant Accumulation"),
                                             value = FALSE),
                               radioButtons("cont_acc", ### Radio buttons to decide on which contaminant accumulation to use
                                            label = "Contaminant Analysis Model",
                                            choices = list("Net Assimilation Efficiency (Model 1)" = 1,
                                                           "Gross Assimilation Efficiency + Temperature- and Size-Dependent Depuration (Model 2)" = 2),
                                            selected = 1), ### Numeric input for the initial predator contaminant concentration
                               numericInput("init_pred_conc",
                                            label = "Initial Predator Concentration (ug/g)",
                                            value = NA),
                 checkboxInput("pop_mort", ### Checkbox to decide if population mortality should be accounted for
                               label = h4("Mortality"),
                               value = FALSE),

                 numericInput("ind", ### Number of individuals in the population; default is 1
                              label = "Initial Population Size",
                              value = 1),
      checkboxInput("spawn", ### Checkbox to decide if spawning should be accounted for
                                            label = h4("Spawning"),
                                            value = FALSE)
                 
      
      
),
mainPanel(
  fluidRow(
  #tabsetPanel(
            #tabPanel('New Simulation',tableOutput('settings'),
            #         actionButton("update", "Update All Simulation Table")),    
   #         tabPanel('Simulations Settings',
                    column(9,  fileInput("sim_file", "Upload Simulation CSV file",
                                         accept = c(
                                           "text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv")
                    )),
                     column(12,tags$h3('New Simulation'),
                     tableOutput('settings'),
                     column(4,actionButton("update", "Add")),
                     #column(2),
                     column(4,actionButton("remove", "Remove")),
                     column(4,downloadButton("downloadData_sim",'Save Simulation CSV')),
                     
                     column(12,tags$h3('Simulation Table')),
     
                     
                     column(12,tableOutput('settings_app'))
            #         ))
          )))  

)),

tabPanel("Simulation Parameters",
         tabsetPanel(
         tabPanel('Model Output',
                  #textOutput('temp_text'),
                  column(11),
                  column(1,div(style="height: 40px;",downloadButton('downloadData_model', 'Download'))), ### Button that allows to download the tabulated output
                  
                  column(12,tableOutput('Model_table'))),
         tabPanel('Parameters',
                  column(11),
                  column(1,div(style="height: 40px;",downloadButton('downloadData_parameters', 'Download'))), ### Button that allows to download the tabulated output
                  
                  column(12,
                  tableOutput("multi_parameter_table"))),
         #tabPanel("Summary",tableOutput("multi_summary_table")),
         #uiOutput('failed_ui')
         tabPanel('Failed Simulations', tableOutput('failed_model_text'))
         
         )),

  
#   tabPanel("Input Files", ### Input files tab
#   tabsetPanel(
#   tabPanel("Temperature",plotOutput("temp")), ### Temperature tab 
#   tabPanel("Diet Proportions", plotOutput("diet_prop")), ### Diet proportions tab 
#   tabPanel("Prey Energy Density",plotOutput("prey_ED")), ### Prey energy density tab
#   tabPanel("Predator Energy Density",plotOutput("pred_ED")), ### Predator energy density tab
#   tabPanel("Indigestible Prey",plotOutput("indigest_prey")) ### Indigestible prey tab
#   )
# ),    
#   navbarMenu("Sub-Models", ### Sub-Models tab
#   tabPanel("Population", ### Population section
#            sidebarLayout(
#              sidebarPanel(
#            checkboxInput("pop_mort", ### Checkbox to decide if population mortality should be accounted for
#                          label = h4("Mortality"), 
#                          value = FALSE),
#            
#            numericInput("ind", ### Number of individuals in the population; default is 1
#                         label = "Initial Population Size",
#                         value = 1)),
#            mainPanel(
#              plotOutput("mort"), ### plot output for mortality probabilities
#              plotOutput("pop") ### plot output for the number of individuals in the population
#            )
#            )
#            ),
#   tabPanel("Reproduction", ### Reproduction section
#            sidebarLayout(
#              sidebarPanel(
#            checkboxInput("spawn", ### Checkbox to decide if spawning should be accounted for
#                         label = h4("Spawning"), 
#                         value = FALSE)),
#            mainPanel(
#              plotOutput("repro") ### plot output for spawning events
#            )
#            )
#            ),
#              
#   tabPanel("Nutrient Regeneration", ### Nutrient regeneration section
#            sidebarLayout(
#              sidebarPanel(
#            checkboxInput("nut", ### Checkbox to decide if nutrient regeneration should be accounted for
#                          label = h4("Nutrient Regeneration"), 
#                          value = FALSE)
#            ),
#            mainPanel(
#              tabsetPanel(
#                tabPanel("Phosphorous Assimilation Efficiency",plotOutput("phos_ae")), ### plot for Phosphorous assimilation efficiency
#                tabPanel("Prey Phosphorous Concentration",plotOutput("prey_phos_conc")), ### plot for Prey Phosphorous Concentration
#                tabPanel("Predator Phosphorous Concentration",plotOutput("pred_phos_conc")), ### plot for Predator Phosphorous Concentration
#                tabPanel("Nitrogen Assimilation Efficiency",plotOutput("nit_ae")), ### plot for Nitrogen Assimilation Efficiency
#                tabPanel("Prey Nitrogen Concentration",plotOutput("prey_nit_conc")), ### plot for Prey Nitrogen Concentration
#                tabPanel("Predator Nitrogen Concentration",plotOutput("pred_nit_conc")) ### plot for Predator Nitrogen Concentration
#              )
#            )
#            )
#   ),
#   
#   tabPanel("Contaminant Accumulation",
#            sidebarLayout(
#              sidebarPanel(
#            checkboxInput("contaminant", ### Checkbox to decide if population mortality should be accounted for
#                          label = h4("Contaminant Accumulation"), 
#                          value = FALSE),
#            radioButtons("cont_acc", ### Radio buttons to decide on which contaminant accumulation to use
#                         label = "Contaminant Analysis Model", 
#                         choices = list("Net Assimilation Efficiency (Model 1)" = 1,
#                                        "Gross Assimilation Efficiency + Temperature- and Size-Dependent Depuration (Model 2)" = 2),
#                         selected = 1), ### Numeric input for the initial predator contaminant concentration
#            numericInput("init_pred_conc",
#                         label = "Initial Predator Concentration (ug/g)",
#                         value = NA)
# ),
# mainPanel(
#   tabsetPanel(
#     tabPanel("Contaminant Assimilation Efficiency",plotOutput("cont_ae")), ### plot for Contaminant assimilation efficiency
#     tabPanel("Prey Contaminant Concentration",plotOutput("prey_cont_conc")), ### plot prey contaminant concentration
#     tabPanel("Contaminant Transfer Efficiency",plotOutput("trans_eff")) ### plot contaminant transfer efficiency
#   )
# )
# )
# )),
  
tabPanel("Output", ### Output tab  
    tabsetPanel(selected = 'Selected Output',
      tabPanel('Summary',
               column(11),
               column(1,div(style="height: 40px;",downloadButton('downloadData_summary', 'Download Summary Table'))), ### Button that allows to download the tabulated output
               
               column(12,tableOutput("multi_summary_table"))),
      tabPanel('Full Table', dataTableOutput('full_output_table')),
      tabPanel('Selected Output',
           sidebarLayout(
          sidebarPanel(   
            
      h3("Table Output"), ### Table output tab
      
      uiOutput('Simulation_ui'),
      
      numericInput("int", ### Nemeric input for the duration of the time intervals
                   label = "Interval (days)", 
                   value = 1),
      
      
      # selectizeInput(
      #   "var1", "Individual Variables (click in box for options)", choices = list("Day"="Day", ### Variables by individuals
      #                                      "Temperature (C)"="Temperature.C",
      #                                      "Weight (g)"="Weight.g",
      #                                      "Net Production (g)"="Net.Production.g",
      #                                      "Net Production (J)"="Net.Production.J",
      #                                      "Cumulative Net Production (g)"="Cum.Net.Production.g",
      #                                      "Cumulative Net Production (J)"="Cum.Net.Production.J",
      #                                      "Gross Production (g)"="Gross.Production.g",
      #                                      "Gross Production (J)"="Gross.Production.J",
      #                                      "Cumulative Gross Production (g)"="Cum.Gross.Production.g",
      #                                      "Cumulative Gross Production (J)"="Cum.Gross.Production.J",
      #                                      "Specific Growth Rate (g/g/d)"="Specific.Growth.Rate.g.g.d",
      #                                      "Specific Growth Rate (J/g/d)"="Specific.Growth.Rate.J.g.d",
      #                                      "Specific Consumption Rate (g/g/d)"="Specific.Consumption.Rate.g.g.d",
      #                                      "Specific Consumption Rate (J/g/d)"="Specific.Consumption.Rate.J.g.d",
      #                                      "Consumption (g)"="Consumption.g",
      #                                      "Consumption (J)"="Consumption.J",
      #                                      "Cumulative Consumption (g)"="Cum.Cons.g",
      #                                      "Cumulative Consumption (J)"="Cum.Cons.J",
      #                                      "Consumption of Prey (g)"=choicePreyGrams,
      #                                      "Consumption of Prey (J)"=choicePreyJoules,
      #                                      "Specific Egestion Rate (J/g/d)"="Specific.Egestion.Rate.J.g.d",
      #                                      "Specific Excretion Rate (J/g/d)"="Specific.Excretion.Rate.J.g.d",
      #                                      "Specific Respiration Rate (J/g/d)"="Specific.Respiration.Rate.J.g.d",
      #                                      "Specific SDA Rate (J/g/d)"="Specific.SDA.Rate.J.g.d",
      #                                      "Initial Predator Energy Density (J/g)"="Initial.Predator.Energy.Density.J.g",
      #                                      "Final Predator Energy Density (J/g)"="Final.Predator.Energy.Density.J.g",
      #                                      "Mean Prey Energy Density (J/g)"="Mean.Prey.Energy.Density.J.g",
      #                                      "Gametic Production (g)"="Gametic.Production.g",
      #                                      "Gametic Production (J)"="Cum.Gametic.Production.J"),
      #                                      multiple = TRUE,selected = c("Day","Temperature.C","Weight.g")),
      # 
      # selectizeInput(
      #   "var2", "Population Variables", choices = list(
      #                                                                 "Population Number"="Population.Number", ### Variables by population
      #                                                                 "Population Biomass (g)"="Population.Biomass.g",
      #                                                                 "Consumption by Population (g)"="Cons.Pop.g",
      #                                                                 "Consumption by Population (J)"="Cons.Pop.J",
      #                                                                 "Cumulative Consumption by Population (g)"="Cum.Cons.Pop.g",
      #                                                                 "Cumulative Consumption by Population (J)"="Cum.Cons.Pop.J",
      #                                                                 "Population Consumption of Prey (g)"=choicePreyPopGrams,
      #                                                                 "Population Consumption of Prey (J)"=choicePreyPopJoules,
      #                                                                 "Mortality"="Mortality.number",
      #                                                                 "Mortality Biomass (g)"="Mortality.g"),
      #   multiple = TRUE,selected = c()),
      # 
      # selectizeInput(
      #   "var3", "Nutrient Regeneration Variables", choices = list( 
      #                                                                 "Nitrogen Egestion (g)"="Nitrogen.Egestion.g", ### Nutrient regeneration variables 
      #                                                                 "Phosphorous Egestion (g)"="Phosphorous.Egestion.g",
      #                                                                 "N:P Egestion (mass ratio)"="N.to.P.Egestion",
      #                                                                 "Nitrogen Excretion (g)"="Nitrogen.Excretion.g",
      #                                                                 "Phosphorous Excretion (g)"="Phosphorous.Excretion.g",
      #                                                                 "N:P Excretion (mass ratio)"="N.to.P.Excretion",
      #                                                                 "Nitrogen Consumption (g)"="Nitrogen.Consumption.g",
      #                                                                 "Phosphorous Consumption (g)"="Phosphorous.Consumption.g",
      #                                                                 "N:P Consumption (mass ratio)"="N.to.P.Consumption",
      #                                                                 "Nitrogen Growth (g)"="Nitrogen.Growth.g",
      #                                                                 "Phosphorous Growth (g)"="Phosphorous.Growth.g",
      #                                                                 "N:P Growth (mass ratio)"="N.to.P.Growth"),
      #   multiple = TRUE,selected = c()),
      # 
      # selectizeInput(
      #   "var4", "Contaminant Analysis Variables", choices = list(
      #                                                                 "Contaminant Clearance Rate (ug/d)"="Contaminant.Clearance.Rate.ug.d", ### Contaminant analysis variables
      #                                                                 "Contaminant Uptake (ug)"="Contaminant.Uptake.ug",
      #                                                                 "Contaminant Burden (ug)"="Contaminant.Burden.ug",
      #                                                                 "Contaminant Predator Concentration (ug/g)"="Contaminant.Predator.Concentration.ug.g"),
      #   multiple = TRUE,selected = c()),

      
      uiOutput('variable_column_ui'),
      uiOutput('population_column_ui'),
      uiOutput('nutrient_column_ui'),
      uiOutput('contaminant_column_ui'),
      uiOutput('consumption_column_ui')
      
      
      ),
           
      mainPanel(
        tabsetPanel(
          #tabPanel("Table",tableOutput("table")),
          #tabPanel('Full Table', dataTableOutput('full_output_table')),
          tabPanel('Table', 
                   column(10),
                   column(2,div(style="height: 40px;",downloadButton('downloadData', 'Download'))),
                   
                   column(12,dataTableOutput('multi_W1.p'))),
          

          tabPanel("Plot", 
                   
                   column(4,uiOutput('xaxis_ui')),
                   column(4,uiOutput('yaxis_ui')),
                   column(4,uiOutput('yaxis2_ui')),
                   

                   
                   # column(4,selectInput("yaxis", ### 1st y-axis variables for plot output
                   #             label = "Y Axis",
                   #             choices = list("Day"="Day", 
                   #                            "Temperature (C)"="Temperature.C",
                   #                            "Weight (g)"="Weight.g",
                   #                            "Population Number"="Population.Number",
                   #                            "Population Biomass (g)"="Population.Biomass.g",
                   #                            "Specific Growth Rate (J/g/d)"="Specific.Growth.Rate.J.g.d",
                   #                            "Specific Consumption Rate (J/g/d)"="Specific.Consumption.Rate.J.g.d",
                   #                            "Specific Egestion Rate (J/g/d)"="Specific.Egestion.Rate.J.g.d",
                   #                            "Specific Excretion Rate (J/g/d)"="Specific.Excretion.Rate.J.g.d",
                   #                            "Specific Respiration Rate (J/g/d)"="Specific.Respiration.Rate.J.g.d",
                   #                            "Specific SDA Rate (J/g/d)"="Specific.SDA.Rate.J.g.d",
                   #                            "Specific Consumption Rate (g/g/d)"="Specific.Consumption.Rate.g.g.d",
                   #                            "Specific Growth Rate (g/g/d)"="Specific.Growth.Rate.g.g.d",
                   #                            "Initial Predator Energy Density (J/g)"="Initial.Predator.Energy.Density.J.g",
                   #                            "Final Predator Energy Density (J/g)"="Final.Predator.Energy.Density.J.g",
                   #                            "Mean Prey Energy Density (J/g)"="Mean.Prey.Energy.Density.J.g",
                   #                            "Gross Production (g)"="Gross.Production.g",
                   #                            "Gross Production (J)"="Gross.Production.J",
                   #                            "Cumulative Gross Production (g)"="Cum.Gross.Production.g",
                   #                            "Cumulative Gross Production (J)"="Cum.Gross.Production.J",
                   #                            "Gametic Production (g)"="Gametic.Production.g",
                   #                            "Gametic Production (J)"="Cum.Gametic.Production.J",
                   #                            "Net Production (g)"="Net.Production.g",
                   #                            "Net Production (J)"="Net.Production.J",
                   #                            "Cumulative Net Production (g)"="Cum.Net.Production.g",
                   #                            "Cumulative Net Production (J)"="Cum.Net.Production.J",
                   #                            "Prey Tot Ind (g)"="Consumption.g",
                   #                            "Prey Tot Ind (J)"="Consumption.J",
                   #                            "Cumulative Prey Tot Ind (g)"="Cum.Cons.g",
                   #                            "Cumulative Prey Tot Ind (J)"="Cum.Cons.J",
                   #                            "Prey Tot Pop (g)"=" Cons.Pop.g",
                   #                            "Prey Tot Pop (J)"="Cons.Pop.J",
                   #                            "Cumulative Prey Tot Pop (g)"="Cum.Cons.Pop.g",
                   #                            "Cumulative Prey Tot Pop (J)"="Cum.Cons.Pop.J",
                   #                            "Mortality"="Mortality.number",
                   #                            "Mortality (g)"="Mortality.g", 
                   #                            "Nitrogen Egestion (g)"="Nitrogen.Egestion.g",
                   #                            "Phosphorous Egestion (g)"="Phosphorous.Egestion.g",
                   #                            "N to P Egestion (mass ratio)"="N.to.P.Egestion",
                   #                            "Nitrogen Excretion (g)"="Nitrogen.Excretion.g",
                   #                            "Phosphorous Excretion (g)"="Phosphorous.Excretion.g",
                   #                            "N to P Excretion (mass ratio)"="N.to.P.Excretion",
                   #                            "Nitrogen Consumption (g)"="Nitrogen.Consumption.g",
                   #                            "Phosphorous Consumption (g)"="Phosphorous.Consumption.g",
                   #                            "N to P Consumption (mass ratio)"="N.to.P.Consumption",
                   #                            "Nitrogen Growth (g)"="Nitrogen.Growth.g",
                   #                            "Phosphorous Growth (g)"="Phosphorous.Growth.g",
                   #                            "N to P Growth (mass ratio)"="N.to.P.Growth",
                   #                            "Contaminant Clearance Rate (ug/d)"="Contaminant.Clearance.Rate.ug.d",
                   #                            "Contaminant Uptake (ug)"="Contaminant.Uptake.ug",
                   #                            "Contaminant Burden (ug)"="Contaminant.Burden.ug",
                   #                            "Contaminant Predator Concentration (ug/g)"="Contaminant.Predator.Concentration.ug.g"),
                   #             selected = "Temperature.C", multiple=FALSE)),
                   # 
                   # column(4,selectInput("yaxis2", ### 2nd y-axis variables for plot output
                   #             label = "Y Axis 2",
                   #             choices = list('None' = 'None',
                   #                            "Day"="Day", 
                   #                            "Temperature (C)"="Temperature.C",
                   #                            "Weight (g)"="Weight.g",
                   #                            "Population Number"="Population.Number",
                   #                            "Population Biomass (g)"="Population.Biomass.g",
                   #                            "Specific Growth Rate (J/g/d)"="Specific.Growth.Rate.J.g.d",
                   #                            "Specific Consumption Rate (J/g/d)"="Specific.Consumption.Rate.J.g.d",
                   #                            "Specific Egestion Rate (J/g/d)"="Specific.Egestion.Rate.J.g.d",
                   #                            "Specific Excretion Rate (J/g/d)"="Specific.Excretion.Rate.J.g.d",
                   #                            "Specific Respiration Rate (J/g/d)"="Specific.Respiration.Rate.J.g.d",
                   #                            "Specific SDA Rate (J/g/d)"="Specific.SDA.Rate.J.g.d",
                   #                            "Specific Consumption Rate (g/g/d)"="Specific.Consumption.Rate.g.g.d",
                   #                            "Specific Growth Rate (g/g/d)"="Specific.Growth.Rate.g.g.d",
                   #                            "Initial Predator Energy Density (J/g)"="Initial.Predator.Energy.Density.J.g",
                   #                            "Final Predator Energy Density (J/g)"="Final.Predator.Energy.Density.J.g",
                   #                            "Mean Prey Energy Density (J/g)"="Mean.Prey.Energy.Density.J.g",
                   #                            "Gross Production (g)"="Gross.Production.g",
                   #                            "Gross Production (J)"="Gross.Production.J",
                   #                            "Cumulative Gross Production (g)"="Cum.Gross.Production.g",
                   #                            "Cumulative Gross Production (J)"="Cum.Gross.Production.J",
                   #                            "Gametic Production (g)"="Gametic.Production.g",
                   #                            "Gametic Production (J)"="Cum.Gametic.Production.J",
                   #                            "Net Production (g)"="Net.Production.g",
                   #                            "Net Production (J)"="Net.Production.J",
                   #                            "Cumulative Net Production (g)"="Cum.Net.Production.g",
                   #                            "Cumulative Net Production (J)"="Cum.Net.Production.J",
                   #                            "Prey Tot Ind (g)"="Consumption.g",
                   #                            "Prey Tot Ind (J)"="Consumption.J",
                   #                            "Cumulative Prey Tot Ind (g)"="Cum.Cons.g",
                   #                            "Cumulative Prey Tot Ind (J)"="Cum.Cons.J",
                   #                            "Prey Tot Pop (g)"=" Cons.Pop.g",
                   #                            "Prey Tot Pop (J)"="Cons.Pop.J",
                   #                            "Cumulative Prey Tot Pop (g)"="Cum.Cons.Pop.g",
                   #                            "Cumulative Prey Tot Pop (J)"="Cum.Cons.Pop.J",
                   #                            "Mortality"="Mortality.number",
                   #                            "Mortality (g)"="Mortality.g", 
                   #                            "Nitrogen Egestion (g)"="Nitrogen.Egestion.g",
                   #                            "Phosphorous Egestion (g)"="Phosphorous.Egestion.g",
                   #                            "N to P Egestion (mass ratio)"="N.to.P.Egestion",
                   #                            "Nitrogen Excretion (g)"="Nitrogen.Excretion.g",
                   #                            "Phosphorous Excretion (g)"="Phosphorous.Excretion.g",
                   #                            "N to P Excretion (mass ratio)"="N.to.P.Excretion",
                   #                            "Nitrogen Consumption (g)"="Nitrogen.Consumption.g",
                   #                            "Phosphorous Consumption (g)"="Phosphorous.Consumption.g",
                   #                            "N to P Consumption (mass ratio)"="N.to.P.Consumption",
                   #                            "Nitrogen Growth (g)"="Nitrogen.Growth.g",
                   #                            "Phosphorous Growth (g)"="Phosphorous.Growth.g",
                   #                            "N to P Growth (mass ratio)"="N.to.P.Growth",
                   #                            "Contaminant Clearance Rate (ug/d)"="Contaminant.Clearance.Rate.ug.d",
                   #                            "Contaminant Uptake (ug)"="Contaminant.Uptake.ug",
                   #                            "Contaminant Burden (ug)"="Contaminant.Burden.ug",
                   #                            "Contaminant Predator Concentration (ug/g)"="Contaminant.Predator.Concentration.ug.g"),
                   #             selected = "Weight.g", multiple=FALSE)),
                   column(4,radioButtons('show_legend','Show Legend',c(T,F),selected = F,inline = T)),
                   #column(4,numericInput('scale_y2','Scale Y Axis',value = 1)),
                   column(4,uiOutput('y_scale_ui')),
                   
                   
                   
                   
                   plotOutput("g_plot"))
        ))
         

)
)
)
) 
 ### FB4 logo
)))