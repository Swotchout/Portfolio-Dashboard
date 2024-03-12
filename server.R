library(forecast)
library(shiny)
library(ggplot2)
library(reshape2)
library(cowplot)
library(ggcorrplot)
library(scales)
library(quantmod)
library(dplyr)
library(TTR)
library(plotly)
library(timetk)
library(tidyr)
library(forcats)
        
theme_set(theme_half_open())


shinyServer(function(input, output, session) {
  
  readPortfolio = eventReactive(input$go, {
    # Handling_data -----------------------------------------------------------
    base_tickers <- c('^SSMI', 'GC=F', 'BTC-USD', '^GSPC', 'CSBGC0.SW', '^TNX') # tckr lst to dwnld
    #base_ticker_names <- c('S&P 500', 'Swiss Bond', 'US-Bond', 'Gold', 'SMI', 'Bitcoin')
    
    # Add additional tickers
    additional_tickers <- strsplit(gsub(" ", "", input$additional_tickers), ",")[[1]]
    if (length(additional_tickers) == 1 && additional_tickers == "") {
      additional_tickers <- NULL
    }
    
    tics <- c(base_tickers, additional_tickers)
    
    e <- new.env() # create a new environment for data
    getSymbols(tics, from=input$date_range[1], to=input$date_range[2], env = e) # dwnld
    TICS <- do.call(merge, lapply(e, Ad)) # merge data in xts obj using lapply instead of eapply
    #TICS$CHFUSD.X.Adjusted <- na.locf(TICS$CHFUSD.X.Adjusted) # complete data for FX
    TICS <- na.omit(TICS)
    colnames(TICS) <- gsub("\\.Adjusted", "", colnames(TICS))
    #TICS <- TICS[,-4] # Remove FX col
    #colnames(TICS) <- c(base_ticker_names, additional_tickers) # Include additional ticker names
    
    # Create the data frame with 'Date' as the first column
    df = data.frame(Date = index(TICS), coredata(TICS))
    #colnames(df) <- c("Date", colnames(TICS)))
    
    
    if (input$hasMarket) {
      if (!input$market %in% colnames(df)) {
        stop("The entered market index does not exist")
      }
    }
    if (ncol(df) < 3) stop("Portfolio can not have less than 2 assets!")
    return(df)
  })
  
  
  #####################################################################################
  
  observeEvent(input$easy_invest_button, {
    updateNavbarPage(session, "navbar", selected = "Easy-Invest")
  })
  
  # Reagiere auf Klicks des Pro-Invest-Buttons
  observeEvent(input$pro_invest_button, {
    updateNavbarPage(session, "navbar", selected = "Pro-Invest")
  })
  
  
    #######################################################################################
    tics <-  eventReactive(input$submit |  input$tics, {
      set.seed(0815)
      library(tidyquant)
      tick <- c('^SSMI', 'GC=F', 'BTC-USD', '^GSPC', 'CSBGC0.SW', '^TNX')
      start_date <- input$time_range[1]
      end_date <- input$time_range[2]
      
      price_data <- tq_get(tick,
                           from = start_date,
                           to = end_date, 
                           get = 'stock.prices')
        
        log_ret_tidy <- price_data %>%
          group_by(symbol) %>%
          tq_transmute(select = adjusted,
                       mutate_fun = periodReturn,
                       period = 'daily',
                       col_rename = 'ret',
                       type = 'log')
        
        log_ret_xts <- log_ret_tidy %>%
          spread(symbol, value = ret) %>%
          tk_xts()
        
        log_ret_xts <- na.omit(log_ret_xts)
        colnames(log_ret_xts) <- tick
        mean_ret <- colMeans(log_ret_xts)
        y_mean_ret <- mean_ret * 260
        
        cov_mat <- cov(log_ret_xts) * 260
        
        num_port <- 10000
        all_wts <- matrix(nrow = num_port,
                          ncol = length(tick))
        
        port_returns <- vector('numeric', length = num_port)
        port_risk <- vector('numeric', length = num_port)
        sharpe_ratio <- vector('numeric', length = num_port)
        
        for (i in seq_along(port_returns)) {
          
          
          wts <- runif(length(tick))
          wts <- wts/sum(wts)
          
          all_wts[i,] <- wts
          
          port_ret <- sum(wts * mean_ret)
          port_ret <- ((port_ret + 1)^252) - 1
          
          port_returns[i] <- port_ret
          
          port_sd <- sqrt(t(wts) %*% (cov_mat  %*% wts))
          port_risk[i] <- port_sd
          
          sr <- port_ret/port_sd
          sharpe_ratio[i] <- sr
          
        }
        
        portfolio_values <- tibble(Return = port_returns,
                                   Risk = port_risk,
                                   SharpeRatio = sharpe_ratio)
        
        all_wts <- tk_tbl(all_wts)
        colnames(all_wts) <- tick
        
        portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))
        
        min_var <- portfolio_values[which.min(portfolio_values$Risk),]
        max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]
        
        p <- portfolio_values[portfolio_values$SharpeRatio >= 0.4, ]
        p5 <- p[p$Return > 0.045 & p$Return < 0.05, ]
        p05 <- p5[which.max(p5$SharpeRatio),]
        
        p8 <- p[p$Return > 0.075 & p$Return < 0.08, ]
        p08 <- p8[which.max(p8$SharpeRatio),]
        
        p16 <- p[p$Return > 0.155 & p$Return < 0.16, ]
        p16 <- p16[which.max(p16$SharpeRatio),]
        
        resultplots <- list(log_ret_xts = log_ret_xts, 
                       mean_ret = mean_ret, 
                       y_mean_ret = y_mean_ret, 
                       cov_mat = cov_mat, 
                       all_wts = all_wts, 
                       portfolio_values = portfolio_values, 
                       min_var = min_var, 
                       max_sr = max_sr, 
                       p05 = p05, 
                       p08 = p08, 
                       p16 = p16)
        
        return(resultplots)
      })
  
    # Backtesting -------------------------------------------------------------
  myfunction <- eventReactive(input$submit |  input$tics, {
      mf <- tics()
      ret_min_var <- mf$log_ret_xts %*% as.numeric(mf$min_var[1:6])
      ret_max_sr <- mf$log_ret_xts %*% as.numeric(mf$max_sr[1:6])
      ret_p05 <- mf$log_ret_xts %*% as.numeric(mf$p05[1:6])
      ret_p08 <- mf$log_ret_xts %*% as.numeric(mf$p08[1:6])
      ret_p16 <- mf$log_ret_xts %*% as.numeric(mf$p16[1:6])
      Time <- index(mf$log_ret_xts)
      
      performance <- (cumsum(data.frame(x = cbind(ret_min_var, ret_max_sr, ret_p05, ret_p08, ret_p16)))*100)+100
      performance <- cbind(Time, performance)
      
      colnames(performance) <- c('Time', 'Minimum Varianz Portfolio', 'Tangential Portfolio',
                                 'Portfolio 5 % Rendite', 'Portfolio 8 % Rendite', 'Portfolio 16 % Rendite')
      
      # Reshape data for plotting
      df_performance_long <- reshape2::melt(performance, id.vars = "Time", variable.name = "Portfolio", value.name = "Returns")
      
      return(df_performance_long)
      
    })

  ############################################Prediction########################
  

  
  ###########################################
  
  
  
  #############################################
    # Create the ggplot
  
    output$bt <- renderPlotly({
      mff <- myfunction()
      Portfolio <- mff$Portfolio
      plot_combined <- ggplot(mff$df_performance_long, aes(x = mff$Time, y = mff$Returns, color = Portfolio)) +
        geom_line() +
        theme_bw() +
        labs(title = "Portfolio Performance",
             x = "Jahr",
             y = "Portfoliowert in %")
      
      p <- plotly::ggplotly(plot_combined)
      
      # Legendenposition anpassen
      p <- p %>% layout(legend = list(orientation = "h", x = 0.0, y = -0.15))
      
      # Größe des Plots anpassen
      p <- p %>% layout(height = 500, width = 700)
      
      p
    })
 
    ##################################################################### function returnbox und volatility box ##############

    output_table <- reactive({
      df_performance <- myfunction()
      df_summary <- df_performance %>%
        group_by(Portfolio) %>%
        summarize('Rendite %' = (last(Returns) - 100),
                  'Volatilität %' = sd(Returns))
      
      return(df_summary)
    })
    
    output$portfolio_summary <- renderTable({
      output_table()
    })
    
    
      ######################################### output efficient frontier ################################
      
    output$ef <- renderPlotly({
      if (is.null(tics())) return(NULL)
      #values for plots
      withProgress({
        setProgress(message = "Rendering plot...", detail = "Please wait...")
      vfp <- tics()
      p <- vfp$portfolio_values %>%
        ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
        geom_point() +
        theme_classic() +
        scale_y_continuous(labels = scales::percent) +
        scale_x_continuous(labels = scales::percent) +
        labs(x = 'Jährliches Risiko',
             y = 'Jährliche Rendite',
             title = "Portfolio Optimierung") +
       
        geom_point(aes(x = Risk,
                       y = Return), data = vfp$min_var, color = ifelse(result()$risiko < 4, 'red',"gray"), size = 4) +
        geom_point(aes(x = Risk,
                       y = Return), data = vfp$max_sr, color = ifelse(result()$risiko < 13 & result()$risiko > 9, 'red',"gray"), size = 4) +
        geom_point(aes(x = Risk,
                       y = Return), data = vfp$p05, color = ifelse(result()$risiko < 7 & result()$risiko > 3, 'red',"gray"), size = 4) +
        geom_point(aes(x = Risk,
                       y = Return), data = vfp$p08, color = ifelse(result()$risiko < 10 & result()$risiko > 6, 'red',"gray"), size = 4) +
        geom_point(aes(x = Risk,
                       y = Return), data = vfp$p16, color = ifelse(result()$risiko > 12, 'red',"gray"), size = 4)
      
      
      ggplotly(p)
      })
    })
    
    ###########portfolio names#################################################
    
    print_portfolio <- function(risiko) {
      if (is.na(risiko)) {
        "Füllen Sie bitte zuerst den Fragebogen aus !"
      } else if (risiko < 4) {
        "Empfehlung: Minimum Varianz Portfolio"
      } else if (risiko < 13 & risiko > 9) {
        "Empfehlung: Tangential Portfolio"
      } else if (risiko < 7 & risiko > 3) {
        "Empfehlung: 5 % Rendite Portfolio"
      } else if (risiko < 10 & risiko > 6) {
        "Empfehlung: 8 % Rendite Portfolio"
      } else if (risiko > 12) {
        "Empfehlung: 16 % Rendite Portfolio"
      }
    }
    
    output$print_output <- renderText({
      risiko <- result()$risiko
      print_portfolio(risiko)
    })
    
    

    ######################################### output weights ################################

    output$plotPie <- renderPlotly({
      res <- result()
      tics_result <- tics()
      risk_value <- res$risiko
      selected_portfolio <- NULL
      
      if (is.na(risk_value)) {
        return("Füllen Sie bitte zuerst den Fragebogen aus !")
      } else if (risk_value < 4) {
        selected_portfolio <- tics_result$min_var
      } else if (risk_value > 3 & risk_value < 7) {
        selected_portfolio <- tics_result$p05
      } else if (risk_value > 6 & risk_value < 10) {
        selected_portfolio <- tics_result$p08
      } else if (risk_value > 9 & risk_value < 13) {
        selected_portfolio <- tics_result$max_sr
      } else if (risk_value > 12) {
        selected_portfolio <- tics_result$p16
      }
      
      portfolio_weights <- selected_portfolio
      colnames(portfolio_weights) <- c("S&P 500","SMI","US-Bond", "Bitcoin", "Swiss Bond","Gold")
      weights <- as.numeric(portfolio_weights)
      weight_percents <- weights / sum(weights)
      
      # Scale the weights so that they sum to 1 (100%)
      scaled_weights <- weight_percents / sum(weight_percents)
      
      # Remove the asset with the name "NULL" if it exists
      non_null_indices <- which(colnames(portfolio_weights) != "null")
      labels <- colnames(portfolio_weights)[non_null_indices]
      values <- scaled_weights[non_null_indices]
      
      plot_ly(labels = labels, values = values, type = "pie", textinfo = "label+percent", insidetextorientation = "radial", hole = 0.3) %>%
        layout(title = "Empfohlene Gewichtung",
               showlegend = T,
               legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.3),
               colorway = RColorBrewer::brewer.pal(n = length(labels), "Spectral")) %>%
        config(displayModeBar = F)
    })
    

    ##################################################

      result <- eventReactive(input$submit |  input$tics, {
        list(
          risiko = (as.numeric(input$frage1) + as.numeric(input$frage2) + as.numeric(input$frage3) + as.numeric(input$frage4)),
          diversifikation = (as.numeric(input$frage5) + as.numeric(input$frage6)),
          volatilitaet = (as.numeric(input$frage7) + as.numeric(input$frage8))
        )})
 
      # Output Risikobereitschaft
    output$RISK_gauge <- renderPlotly({
      if (is.null(result())) return(NULL)
      
      value <- result()$risiko/16*100
      
      gauge <- list(
        shape = "angular",
        bgcolor = "#F1F1F1",
        bordercolor = "#F1F1F1",
        axis = list(
          range = list(0, 100),
          tickmode = "array",
          tickvals = c(0, 25, 50, 75, 100),
          ticktext = c("0%", "25%", "50%", "75%", "100%"),
          tickfont = list(color = "black", size = 12),
          tickwidth = 2,
          tickcolor = "#A9A9A9",
          showline = FALSE,
          showticklabels = TRUE,
          showgrid = FALSE,
          gridcolor = "#A9A9A9",
          gridwidth = 1,
          zeroline = FALSE
        ),
        steps = list(
          list(range = c(0, 25), color = "#FFC300"),
          list(range = c(25, 50), color = "#FF5733"),
          list(range = c(50, 75), color = "#C70039"),
          list(range = c(75, 100), color = "#900C3F")
        ),
        threshold = list(
          line = list(color = "black", width = 2),
          thickness = 0.75,
          value = value
        ),
        bar = list(
          color = "black",
          thickness = 0.5
        ),
        borderwidth = 2,
        bordercolor = "black"
      )
      
      degrees <- -45 + value * 1.8
      
      pointer <- list(
        type = "scatter",
        x = c(0, 0.45, 0.5, 0.55, 0),
        y = c(0.2, 0.2, 0.5, 0.2, 0.2),
        fill = "toself",
        fillcolor = "black",
        line = list(color = "black")
      )
      
      text <- paste0(value, "%")
      
      plot_ly(
        type = "indicator",
        mode = "gauge+number",
        value = value,
        gauge = gauge,
        domain = list(x = c(0, 1), y = c(0, 1)),
        pointer = pointer,
        text = text,
        textposition = "center"
      ) %>%
        layout(
          title = "Risikobereitschaft",
          font = list(color = "black", family = "Helvetica"),
          paper_bgcolor = "white",
          plot_bgcolor = "white"
        )
    })
    
    
    
    # Output Diversifikation
    output$DIVERS_gauge <- renderPlotly({
      if (is.null(result())) return(NULL)
      
      value <- result()$diversifikation/8*100
      
      gauge <- list(
        shape = "angular",
        bgcolor = "#F1F1F1",
        bordercolor = "#F1F1F1",
        axis = list(
          range = list(0, 100),
          tickmode = "array",
          tickvals = c(0, 25, 50, 75, 100),
          ticktext = c("0%", "25%", "50%", "75%", "100%"),
          tickfont = list(color = "black", size = 12),
          tickwidth = 2,
          tickcolor = "#A9A9A9",
          showline = FALSE,
          showticklabels = TRUE,
          showgrid = FALSE,
          gridcolor = "#A9A9A9",
          gridwidth = 1,
          zeroline = FALSE
        ),
        steps = list(
          list(range = c(0, 25), color = "lightcyan"),
          list(range = c(25, 50), color = "lightblue"),
          list(range = c(50, 75), color = "blue"),
          list(range = c(75, 100), color = "navy")
        ),
        threshold = list(
          line = list(color = "black", width = 2),
          thickness = 0.75,
          value = value
        ),
        bar = list(
          color = "black",
          thickness = 0.5
        ),
        borderwidth = 2,
        bordercolor = "black"
      )
      
      degrees <- -45 + value * 1.8
      
      pointer <- list(
        type = "scatter",
        x = c(0, 0.45, 0.5, 0.55, 0),
        y = c(0.2, 0.2, 0.5, 0.2, 0.2),
        fill = "toself",
        fillcolor = "black",
        line = list(color = "black")
      )
      
      text <- paste0(value, "%")
      
      plot_ly(
        type = "indicator",
        mode = "gauge+number",
        value = value,
        gauge = gauge,
        domain = list(x = c(0, 1), y = c(0, 1)),
        pointer = pointer,
        text = text,
        textposition = "center"
      ) %>%
        layout(
          title = "Diversifikation",
          font = list(color = "black", family = "Helvetica"),
          paper_bgcolor = "white",
          plot_bgcolor = "white"
        )
    })
    
    
    
    # Output ESG
    output$VOL_gauge <- renderPlotly({
      if (is.null(result())) return(NULL)
      
      value <- result()$volatilitaet/8*100
      
      gauge <- list(
        shape = "angular",
        bgcolor = "#F1F1F1",
        bordercolor = "#F1F1F1",
        axis = list(
          range = list(0, 100),
          tickmode = "array",
          tickvals = c(0, 25, 50, 75, 100),
          ticktext = c("0%", "25%", "50%", "75%", "100%"),
          tickfont = list(color = "black", size = 12),
          tickwidth = 2,
          tickcolor = "#A9A9A9",
          showline = FALSE,
          showticklabels = TRUE,
          showgrid = FALSE,
          gridcolor = "#A9A9A9",
          gridwidth = 1,
          zeroline = FALSE
        ),
          steps = list(
            list(range = c(0, 25), color = "#99EDC3"),
            list(range = c(25, 50), color = "greenyellow"),
            list(range = c(50, 75), color = "green"),
            list(range = c(75, 100), color = "darkgreen")
        ),
        threshold = list(
          line = list(color = "black", width = 2),
          thickness = 0.75,
          value = value
        ),
        bar = list(
          color = "black",
          thickness = 0.5
        ),
        borderwidth = 2,
        bordercolor = "black"
      )
      
      degrees <- -45 + value * 1.8
      
      pointer <- list(
        type = "scatter",
        x = c(0, 0.45, 0.5, 0.55, 0),
        y = c(0.2, 0.2, 0.5, 0.2, 0.2),
        fill = "toself",
        fillcolor = "black",
        line = list(color = "black")
      )
      
      text <- paste0(value, "%")
      
      plot_ly(
        type = "indicator",
        mode = "gauge+number",
        value = value,
        gauge = gauge,
        domain = list(x = c(0, 1), y = c(0, 1)),
        pointer = pointer,
        text = text,
        textposition = "center"
      ) %>%
        layout(
          title = "Nachhaltigkeit (ESG)",
          font = list(color = "black", family = "Helvetica"),
          paper_bgcolor = "white",
          plot_bgcolor = "white"
        )
    })
    
    
    calculateDailyReturns = reactive({
        data = readPortfolio()
        df = data[, !(names(data) == 'Date')]
        df0 = df
        df0[nrow(df)+1,] = rep(0, ncol(df))
        df0 = df0[-1,]
        returns_df  = 100*(df0/df-1)
        returns_df = returns_df[1:nrow(df)-1,]
        returns_df[['Date']] = data[2:nrow(df), 1]
        return(returns_df)
    })
    
    MeanReturns = reactive({
        dailyReturns = calculateDailyReturns()
        if(input$hasMarket){
            dailyReturns = dailyReturns[, names(dailyReturns) != input$market]}
        returns_df = dailyReturns[, !(names(dailyReturns) %in% c('Date'))]
        returns = colMeans(returns_df)
        return(returns)
    })
    
    ExpMeanReturns = reactive({
        dailyReturns = calculateDailyReturns()
        if(input$hasMarket){
            dailyReturns = dailyReturns[, names(dailyReturns) != input$market]}
        returns_df = dailyReturns[, !(names(dailyReturns) %in% c('Date'))]
        expReturns = returns_df * (1:nrow(returns_df))
        returns = colSums(expReturns) / sum(1:nrow(returns_df))
        return(returns)
    })
    
    CAPMReturns = reactive({
        validate(need(input$hasMarket, "Market index is needed for CAPM!"),
                 need(input$rf,"Enter the risk free return rate!"))
        dailyReturns = calculateDailyReturns()
        returns_df = dailyReturns[, names(dailyReturns) != 'Date']
        marketReturns = returns_df[[input$market]]
        returns_df = returns_df[, names(returns_df) != input$market]
        betas = cov(returns_df, marketReturns)/var(marketReturns)
        riskFree = as.numeric(input$rf)
        returns = riskFree + betas * (mean(marketReturns) - riskFree)
        return(returns)
    })
    
    calculateReturns = eventReactive(input$go,{
        switch(input$returnModel,
               "MH" = MeanReturns(),
               "EWMH" = ExpMeanReturns(),
               "CAPM" = CAPMReturns())
        })
    
    covRisk = reactive({
        df = calculateDailyReturns()
        r = c('Date')
        if (input$hasMarket) r = c('Date', input$market)
        df = df[, !(names(df) %in% r)]
        return(cov(df))
    })
    
    ExpCovRisk = reactive({
        df = calculateDailyReturns()
        r = c('Date')
        if (input$hasMarket) r = c('Date', input$market)
        df = df[, !(names(df) %in% r)]
        weights = (1/sum(1:nrow(df)))*(1:nrow(df))
        WC = cov.wt(df, weights)
        return(WC[[1]])
    })
    
    calculateRisks = eventReactive(input$go,{
        switch(input$riskModel,
               "Cov" = covRisk(),
               "ExpCov" = ExpCovRisk())
    })
    
    visRisk = eventReactive(input$go,{
        Risks = calculateRisks()
        l = c(min(Risks)-0.02, max(Risks)+0.02)
        ggcorrplot(Risks, hc.order = FALSE, outline.col = "white", lab = TRUE,
                   colors = c("#6D9EC1", "white", "orange"), legend.title = '') + 
            scale_fill_gradient2(breaks = l, limit = l, low = "darkseagreen1", high = "firebrick1")
    })
    
    visReturn = eventReactive(input$go,{
        Returns = data.frame(calculateReturns())
        names(Returns) = c('returns')
        ggplot(data=Returns, aes(x=returns, y=rownames(Returns))) + 
            geom_bar(colour="white", stat="identity", width = 0.8, fill="lightblue") +
            geom_text(aes(label = sprintf("%.4f", returns), y=rownames(Returns)),
                      size = 5, hjust = 1.2) +
            guides(fill=FALSE) + labs(title='', x='Daily Returns (%)', y='')
    })
    
    output$plotRisk = renderPlot({
        visRisk()
    })
    
    output$plotReturn = renderPlot({
        visReturn()
    })
    
    output$assetCheckBox = renderUI({
        df = readPortfolio()
        assets = colnames(df[, !(names(df) %in% c('Date', input$market))])
        checkboxGroupInput('assetsGroup', 'Auswahl der anzuzeigenden Assets:', 
                           choices = assets, selected = c(assets[1]))
    })
    
    output$plots_P = renderPlot({
        df_P = readPortfolio()
        d_P = df_P[, names(df_P) %in% c(input$assetsGroup, 'Date')]
        df_melt = melt(d_P, id.vars="Date", value.name="Price", variable.name='Assets')
        ggplot(df_melt, aes(x=Date, y=Price, group=Assets, color=Assets)) + geom_line() + 
            labs(title='', x='Date', y='Price') + theme(legend.position="bottom",
                                                        legend.title = element_blank()) +
            scale_x_date(breaks = breaks_pretty(10))
    })
    
    output$plotInfo_P = renderText({
        xy_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste0("Date = ", as.Date(e$x, origin = "1970-01-01"), "\t", "Price = ", 
                   round(e$y, 4), "\n")
        }
        xy_range_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste0("Min Date = ", as.Date(e$xmin, origin = "1970-01-01"), "\t",
                   "Min Price = ", round(e$ymin, 4), "\n",
                   "               Max Date = ", as.Date(e$xmax, origin = "1970-01-01"), "\t",
                   "Max Price = ", round(e$ymax, 4))
        }
        
        paste0(
            "Click:         ", xy_str(input$plot_click_P),
            "Double Click:  ", xy_str(input$plot_dblclick_P),
            "Hover:         ", xy_str(input$plot_hover_P),
            "Area:          ", xy_range_str(input$plot_brush_P)
        )
    })
    
    output$plots_R_daily = renderPlot({
        df_R = calculateDailyReturns()
        d_R = df_R[, names(df_R) %in% c(input$assetsGroup, 'Date')]
        d_Rmelt = melt(d_R, id.vars="Date", value.name="Returns", variable.name='Assets')
        ggplot(d_Rmelt, aes(x=Date, y=Returns, group=Assets, color=Assets)) + 
            geom_line() + labs(title='', x='Date', y='Daily Returns (%)') + 
            theme(legend.position="bottom", legend.title = element_blank()) +
            scale_x_date(breaks = breaks_pretty(10))
    })
    
    output$plotInfo_R_daily = renderText({
        xy_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste0("Date = ", as.Date(e$x, origin = "1970-01-01"), "\t", "Return(%) = ", 
                   round(e$y, 4), "\n")
        }
        xy_range_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste0("Min Date = ", as.Date(e$xmin, origin = "1970-01-01"), "\t",
                   "Min Return(%) = ", round(e$ymin, 4), "\n",
                   "               Max Date = ", as.Date(e$xmax, origin = "1970-01-01"), "\t",
                   "Max Return(%) = ", round(e$ymax, 4))
        }
        
        paste0(
            "Click:         ", xy_str(input$plot_click_R_daily),
            "Double Click:  ", xy_str(input$plot_dblclick_R_daily),
            "Hover:         ", xy_str(input$plot_hover_R_daily),
            "Area:          ", xy_range_str(input$plot_brush_R_daily)
        )
    })
    
    output$plots_R_hist = renderPlot({
        df_R = calculateDailyReturns()
        d_R = df_R[, names(df_R) %in% c(input$assetsGroup, 'Date')]
        d_Rmelt = melt(d_R, id.vars="Date", value.name="Returns", variable.name='Assets')
        ggplot(d_Rmelt, aes(Returns, fill=Assets)) + geom_histogram(binwidth=0.25) +
            theme(legend.position = 'bottom', legend.title = element_blank()) +
            labs(title='', x='Daily Returns (%)', y='Frequency')
    })
    
    output$plotInfo_R_hist = renderText({
        xy_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste0("Return(%) = ", round(e$x, 4), "\t", "Frequency = ", round(e$y),"\n")
        }
        xy_range_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste0("Min Return(%) = ", round(e$xmin, 4), "\t",
                   "Min Frequency = ", round(e$ymin), "\n",
                   "               Max Return(%) = ", round(e$xmax, 4),"\t",
                   "Max Frequency = ", round(e$ymax))
        }
        
        paste0(
            "Click:         ", xy_str(input$plot_click_R_hist),
            "Double Click:  ", xy_str(input$plot_dblclick_R_hist),
            "Hover:         ", xy_str(input$plot_hover_R_hist),
            "Area:          ", xy_range_str(input$plot_brush_R_hist)
        )
    })
    
    output$efficientFrontier = renderPlot({
        returns = calculateReturns()
        risks_cov = calculateRisks()
        risks_cov_inv = solve(risks_cov)
        vec1 = matrix(1, nrow = length(returns))
        
        alpha = as.numeric(t(vec1) %*% risks_cov_inv %*% vec1)
        beta = as.numeric(t(vec1) %*% risks_cov_inv %*% returns)
        gamma = as.numeric(t(returns) %*% risks_cov_inv %*% returns)
        delta = alpha * gamma - beta * beta
        
        risks = sqrt(diag(risks_cov))
        
        p1 = stat_function(fun=function(z) 
            beta/alpha+sqrt((beta/alpha)^2-(gamma-delta*z^2)/(alpha)), color='darkcyan', size=2)
        p2 = stat_function(fun=function(z) 
            beta/alpha-sqrt((beta/alpha)^2-(gamma-delta*z^2)/(alpha)), color='aquamarine', size=2)
        
        p = switch (input$FrontierChoice, 
                'eff' = p1,
                'ieff' = p2)
        
        if (input$returnModel == 'CAPM') {
            ggtext = geom_text(aes(label=rownames(returns)) ,hjust=0, vjust=2, size = 4.5)
        }
            else {ggtext = geom_text(aes(label=names(returns)) ,hjust=0, vjust=2, size = 4.5)}
        
        P = ggplot(data.frame(x = risks, y = returns), aes(x, y)) +
            geom_point(size = 3, color = 'deepskyblue3') + ggtext +
            scale_x_continuous(limits = c(0, max(risks)*1.2)) +
            scale_y_continuous(limits = c(-max(returns), max(returns)*2)) +
            labs(title='', x='Daily Risk (%)', y='Daily Return (%)')
        
        if (input$portfolioGO) {
            p_user_xy = userPortfolioPoint()
            p_user = geom_point(aes(x = p_user_xy[1], y = p_user_xy[2]),
                                color='darkorchid1', pch=23, size = 6, fill='darkorchid1')}
        
        if (input$portfolioGO & input$FrontierChoice=='both') return(P+p1+p2+p_user)
        else if (input$portfolioGO & input$FrontierChoice!='both') return(P+p+p_user)
        else if (input$portfolioGO==FALSE & input$FrontierChoice=='both') return(P+p1+p2)
        else {return(P+p)}
    })
    
    output$plotInfo_EF = renderText({
        xy_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste0("Risk(%) = ", round(e$x, 2), "\t", "\t", "Return(%) = ", round(e$y, 2),"\n")
        }
        xy_range_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste0("Min Risk(%) = ", round(e$xmin, 2), "\t",
                   "Min Return(%) = ", round(e$ymin, 2), "\n",
                   "               Max Risk(%) = ", round(e$xmax, 2),"\t",
                   "Max Return(%) = ", round(e$ymax, 2))
        }
        
        paste0(
            "Click:         ", xy_str(input$plot_click_EF),
            "Double Click:  ", xy_str(input$plot_dblclick_EF),
            "Hover:         ", xy_str(input$plot_hover_EF),
            "Area:          ", xy_range_str(input$plot_brush_EF)
        )
    })
    
    output$userWeights = renderUI({
        asset_names = colnames(readPortfolio())
        returns = calculateReturns()
        r = c('Date')
        if (input$hasMarket) {
            r = c('Date', input$market)
            asset_names = asset_names[!(asset_names %in% r)]}
        else {asset_names = names(returns)}
        n_assets = length(asset_names)
        n_half1 = round(n_assets/2)+1
        n_half2 = n_half1+1
        fluidRow(
            column(6, lapply(1:n_half1, function(i) {
            numericInput(paste0('w_', i), asset_names[i], 0)
                })),
            column(6, lapply(n_half2:n_assets, function(i) {
                numericInput(paste0('w_', i), asset_names[i], 0)
            }))
            )
        })
    
     userPortfolioPoint = eventReactive(input$portfolioGO,{
        returns = calculateReturns()
        risks = calculateRisks()
        n_assets = length(returns)
        w_assets = unlist(lapply(1:n_assets, function(i) {
            eval(parse(text = paste0('input$w_', i)))}))
        w_assets = w_assets/sum(w_assets)
        portfolio_return = sum(returns*w_assets)
        sig = 0
        for (i in 1:n_assets) {
            for (j in 1:n_assets) {
                sig = sig + w_assets[i] * w_assets[j] * risks[i, j]
                }
            }
        portfolio_risk = sqrt(sig)
        portfolio = c(portfolio_risk, portfolio_return)
        return(portfolio)
    })
    
    output$userPortfolio = renderText({
        x = userPortfolioPoint()
        paste0('Portfolio Return: ', round(x[2], 4), '\n',
               'Portfolio Risk: ', round(x[1], 4))
    })

})

