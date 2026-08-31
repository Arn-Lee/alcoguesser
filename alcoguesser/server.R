# Server-side code for alcoguesser
# NOTE: ground truth is stored in sg_data using rhandsontable to provide a clean UI
# The data pipeline sorts values by time, drops NA rows and performs basic validation

function(input, output, session) {
  
  # Setting up ground truth table (sg_data), starting with dummy data from global.R
  
  sg_data = reactiveVal(init_df)
  
  # Exposing table to UI
  
  output$sg_table = renderRHandsontable({
    rhandsontable(sg_data()) %>% 
      hot_col("time", type = "numeric", format = "0.0") %>% # enforcing numeric type for time, plus allows 1 dp for time
      hot_col("sg", type = "numeric", format = "0.000") %>% # enforcing numeric type for sg, plus changing display format to 3 dp
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) # locking down column edits
  })
  
  # Retrieving updated table from UI
  
  observeEvent(input$sg_table, {
    sg_data(hot_to_r(input$sg_table)) # NOTE: need to use hot_to_r for rhandsontable to work properly
  })
  
  # Model fitting
  
  # First, retrieve ground truth table and validate data
  validated_data = reactive({
    raw_data = sg_data() # Retrieving data
    raw_data = raw_data %>% drop_na() # Drops rows with missing values
    raw_data = raw_data %>% arrange(time) # Reorganising by time, just in case
    validate( # Adding validation rules (as shown by error messages)
      need(all(raw_data$time >= 0), "Time values cannot be negative"),
      need(all(raw_data$sg > 0), "SG values must be greater than 0"),
      need(nrow(raw_data) >= 4, "The model requires at least 4 readings"),
      need(!anyDuplicated(raw_data$time), "Time values must be unique!")
      )
    raw_data # returning the table
  })
  
  # Preparing model (using nonlinear least squares)
  
  model = eventReactive(input$fit_model, {
    nls(sg ~ fg + A * exp(-k * time), # Setting the formula used for the model itself (ie, generic first-order kinetics)
        data = validated_data(),
        # Setting up starting params below
        start = list(
          fg = input$init_fg,
          A = input$init_a,
          k = input$init_k),
        control = nls.control(maxiter = 250, warnOnly = TRUE), # warnOnly should be useful to at least attempt a fit, extra iterations to improve chances of finding a fit
        algorithm = "port",
        lower = c(fg = 0.95, A = 0, k = 0), # fg shouldn't be below 0.95, others should be positive
        upper = c(fg = 1.1, A = 0.3, k = 10) # should prevent exploration of exceptionally high vals, but leaving k effectively unconstrained
        )
  })
  
  # Preparing MC simulation
  
  mc_sims = reactive({
    # Prep prior to sim
    req(model()) # Prevents calc until the model is prepped at least once
    coefs = coef(model()) # Extract model coefs
    vcov_matrix = vcov(model()) # Prep variance-covariance matrix
    # Actual sim
    sims = MASS::mvrnorm(n = 1000, mu = coefs, Sigma = vcov_matrix) # Set up sims
    pred_times = apply(sims, 1, function(x){
      fg = x["fg"]
      A = x["A"]
      k = x["k"]
      if (k <= 0 || A <= 0 || input$target_sg <= fg) {
        return(NA) # Discard impossible parameters
        } 
      -log((input$target_sg - fg) / A) / k  # Make prediction here based on params
    })
    # Filtering out NAs from invalid predictions plus invalid sims as well
    valid_pred = !is.na(pred_times)
    pred_times = pred_times[valid_pred]
    sims = sims[valid_pred, ]
    req(length(pred_times) > 0) # NOTE: guards against situations where there's no valid predictions
    # Get median + lower/upper CI bounds (plus Q1/Q3 estimates as well), simulations and the predicted times as list
    list(
      median = median(pred_times),
      lower = quantile(pred_times, 0.025),
      upper = quantile(pred_times, 0.975),
      q1 = quantile(pred_times, 0.25),
      q3 = quantile(pred_times, 0.75),
      pred_times = pred_times,
      sims = sims
    )
  })
  
  # Printing time prediction
  
  output$pred_time = renderText({
    req(model()) # Prevents rendering until the model is prepped at least once
    predicted = mc_sims() # Get simulation results
    paste("The batch will be ready in", # preparing string for print
          round(predicted$median, 1), 
          input$time_unit,
          "with a 50% prediction interval of",
          round(predicted$q1, 1),
          "to",
          round(predicted$q3, 1),
          input$time_unit,
          "95% CI:",
          round(predicted$lower, 1),
          "to",
          round(predicted$upper, 1),
          input$time_unit
          )
  })
  
  # Preparing parameter sets for lower and upper CIs
  
  ci_bounds = reactive({
    # Retrieve data/sim results
    pred_times = mc_sims()$pred_times
    sims = mc_sims()$sims
    # Get low and high CI values
    lower_target = quantile(pred_times, 0.025)
    upper_target = quantile(pred_times, 0.975)
    # Get positions/indices associated with said low/high CI values
    idx_low = which.min(abs(pred_times - lower_target))
    idx_high = which.min(abs(pred_times - upper_target))
    # Return list of lower and upper params
    list(
      lower = sims[idx_low, ],
      upper = sims[idx_high, ]
      )
  })
  
  # Drawing plot using ggplot
  
  output$plot = renderPlot({
    req(model()) # Prevents rendering until the model is prepped at least once
    # Grabbing data
    point_data = validated_data()
    predicted = mc_sims()
    ci_bounds = ci_bounds()
    sims = mc_sims()$sims
    # Preparing prediction dataframe (to plot the model + CIs); note: max duration is fixed to 1.5x median duration (long tail otherwise!)
    time_grid = seq(min(point_data$time), predicted$median * 1.5, length.out = 200)
    # Using sapply to predict lower, upper and median
    pred_mat = sapply(
      seq_len(nrow(sims)),
      function(i){
        fg = sims[i, "fg"]
        A = sims[i, "A"]
        k = sims[i, "k"]
        fg + A * exp(-k * time_grid)
      }
    )
    # Saving to df for plotting
    ci_points = data.frame(
      time = time_grid,
      lower = apply(pred_mat, 1, quantile, 0.025),
      median = apply(pred_mat, 1, median),
      upper = apply(pred_mat, 1, quantile, 0.975))
    # Actual plot
    plot = ggplot(point_data, aes(x = time, y = sg)) +
      geom_point() +
      geom_hline(yintercept = input$target_sg, col = "red") +
      geom_line(data = ci_points, aes(x = time, y = median), col = "blue") +
      geom_ribbon(data = ci_points, aes(x = time, ymin = lower, ymax = upper), alpha = 0.2, fill = "lightblue", inherit.aes = FALSE) +
      labs(
        x = paste0("Time (", input$time_unit, ")"),
        y = "Specific Gravity",
        title = "SG over time",
        subtitle = "Red line indicates desired SG, blue indicates model predictions (line for median + shading for 95% CI)"
      )
    plot
  })
  
  # Getting model coeffs (for those who care)
  
  output$model_coeff = renderText({
    req(model()) # Prevents rendering until the model is prepped at least once
    coefs = coef(model())
    paste("The model coefficients are", 
          round(coefs["fg"], 4), "for final gravity", 
          round(coefs["A"], 4), "for A and", 
          round(coefs["k"], 4), "for the rate constant.")
  })
  
}
