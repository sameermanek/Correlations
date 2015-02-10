
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

source('functions.R')
shinyServer(function(input, output) {

  # I only need to create the correlations and do the reshaping once. No real need to be reactive, but oh well.
  getData <- reactive({
    dt.data <- retrieveData()
  })
  reshapeData <- reactive({
    dt.data <- getData()
    dt.data.wide <- data.table:::dcast.data.table(data.table:::melt.data.table(dt.data, id.vars = c('date','name')), date ~ name)
    return(dt.data.wide)
  })
  
  getCorrelations <- reactive({
    dt.data.wide <- reshapeData()
    mat.cor <- cor(dt.data.wide[, -1, with = FALSE], use = "pairwise.complete.obs")
    return(mat.cor)
  })
  
  getPValues <- reactive({
    mat.cor <- getCorrelations()
    dt.data.wide <- reshapeData()
    # let's just run pairwise linear regressions, because this is what I'm going to plot later.
    mat.p <- vapply(1:ncol(mat.cor), function(x) {
      vapply(1:nrow(mat.cor), function(y) {
        if(x >= y) return(NA_real_)
        lm.fit <- lm(dt.data.wide[, y+1, with = FALSE][[1]] ~ dt.data.wide[, x+1, with = FALSE][[1]])
        if(length(coefficients(lm.fit)) == 2 && !is.na(coefficients(lm.fit)[2])) return(coefficients(summary(lm.fit))[2,4]) else return(1)
        
      }, numeric(1))
    }, numeric(ncol(mat.cor)))
    rownames(mat.p) <- rownames(mat.cor)
    colnames(mat.p) <- colnames(mat.cor)
    return(mat.p)
  })
  
  getSigPairs <- reactive({
    mat.p <- getPValues()
    mat.cor <- getCorrelations()

    # now get the index pairs of stat. sig. relationships.
    lst.index <- lapply(1:nrow(mat.p), function(x) lapply(1:ncol(mat.p), function(y) if(x > y) return(c(x,y)) else return(NA_real_)))
    lst.index <- unlist(lst.index, recursive = FALSE)
    lst.index <- lst.index[vapply(lst.index, function(x) !is.na(mat.p[x[1],x[2]]) && mat.p[x[1],x[2]] <= 0.05, logical(1))]
    # Should I also apply some restriction to the correlation coefficient? Let me try to find more good relationships first.
    
    lst.index <- sample(lst.index)
    return(lst.index)
  })
  
  output$scatterPlot <- renderPlot({
    lst.index <- getSigPairs()
    dt.data.wide <- reshapeData()
    num.relationship <- (input$nextPlot %% length(lst.index)) + 1
    chr.x <- colnames(dt.data.wide)[lst.index[[num.relationship]][1] + 1]
    chr.y <- colnames(dt.data.wide)[lst.index[[num.relationship]][2] + 1]
    # restrict to where I have both
    dt.data.wide <- dt.data.wide[, c(chr.x, chr.y), with = FALSE]
    dt.data.wide <- dt.data.wide[complete.cases(dt.data.wide)]
    # now plot.
    a <- ggplot(dt.data.wide, aes_string(x = paste0('`',chr.x,'`'), y = paste0('`',chr.y,'`'))) + 
      geom_point() + 
      stat_smooth(method = 'lm') + 
      labs(x = chr.x, y = chr.y) + 
      theme_bw()
    a
  })
  output$linePlot <- renderPlot({
    lst.index <- getSigPairs()
    dt.data <- getData()
    dt.data.wide <- reshapeData()
    num.relationship <- (input$nextPlot %% length(lst.index)) + 1
    chr.x <- colnames(dt.data.wide)[lst.index[[num.relationship]][1] + 1]
    chr.y <- colnames(dt.data.wide)[lst.index[[num.relationship]][2] + 1]
    
    a <- ggplot(dt.data[name %in% c(chr.x, chr.y)], aes(as.Date(date), value, colour = name)) + 
      geom_line() + 
      facet_grid(name ~ ., scale = 'free_y') + 
      theme_bw() + 
      labs(x = 'Date', y = 'Value\n(See Right Axis)', colour = '')
    a
  })
  
  output$summaryTable <- renderDataTable({
    lst.index <- getSigPairs()
    dt.data.wide <- reshapeData()
    num.relationship <- (input$nextPlot %% length(lst.index)) + 1
    chr.x <- colnames(dt.data.wide)[lst.index[[num.relationship]][1] + 1]
    chr.y <- colnames(dt.data.wide)[lst.index[[num.relationship]][2] + 1]
    
    lm.fit <- lm(as.formula(paste0('`',chr.y,'` ~ `', chr.x,'`')), dt.data.wide)
    df.output <- data.frame(`Statistical Parameter` = c('R^2','p-value'),
                            `Value` = c(summary(lm.fit)$r.squared, summary(lm.fit)$coefficients[2,4] ),
                            check.names = FALSE,
                            stringsAsFactors = FALSE)
    return(df.output)
    
  })
      
})