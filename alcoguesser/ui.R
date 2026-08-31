# Main UI for alcoguesser (single page as it's a simple app)
fluidPage(
  useShinyjs(),
  
  # Intro text at the top of the page
  
  h2("Alcoguesser prototype (experimental branch)"),
  h3("v0.2"),
  p("This application is designed to allow you to input your specific gravity (SG) readings over time to predict when the batch is ready for distillation or bottling."),
  p("The model assumes first-order kinetics for change in SG over time and uses a simulation of 1000 experiments to obtain uncertainty estimates"),
  
  # Preparing the input data table
  
  h2("Data input"),
  p("Use the interactive table below to add your data points or paste in data from Excel. Feel free to use whatever units for time, just be consistent! (eg, only days for all timepoints)."),
  p("You will need to enter at least 4 measurements for this application to work. However, 6+ points may be required for the model to be fitted"),
  p("Your first measurement should be time = 0 if you took a measurement immediately prior to adding the yeast"),
  p("To add or remove rows, right click on a row. NOTE: Time values must be unique! (ie, you can't have two measurements with the same time). Any rows with blank values will be ignored."),
  textInput("time_unit", label = "Unit for time", value = "days"),
  rHandsontableOutput("sg_table"),
  
  # Prediction side of things
  
  h2("Predictions"),
  h3("Model settings"),
  p("You can change the target SG here. More advanced options can be found later"),
  numericInput("target_sg", label = "Desired target SG", value = 0.990, step = 0.001),
  actionButton("fit_model", label = "Fit Model"),
  
  h3("Model outputs"),
  textOutput("pred_time"),
  plotOutput("plot"),
  p("For those who wish to see the model coefficients:"),
  textOutput("model_coeff"),
  
  h3("Advanced model settings"),
  p("WARNING: here be dragons. Changing these default values is not recommended! Best-fit parameters will be found during model fitting."),
  numericInput("init_fg", label = "Estimated final SG", value = 0.95, step = 0.001),
  numericInput("init_a", label = "Estimated offset", value = 0.15, step = 0.001),
  numericInput("init_k", label = "Estimated rate constant", value = 0.04, step = 0.001),
  
  h3("Changelog"),
  p("v0.2: Tightened lower and upper search constraints for the nls fitting algo")

)
