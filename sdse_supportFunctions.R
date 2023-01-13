repLimit = 100
trackData <- vroom("trackData_byGenre_shiny.csv")%>% 
  filter(genre != "necrogrind")

cols <- wes_palette("Darjeeling1",n=6,type = "continuous")
cols[5] <- "#EFCA08"
paramList <- c("tempo","acousticness","danceability","energy","key","loudness",
               "speechiness","valence")
genreSublist<-c("underground_hip_hop","reggae","punk","latin",
                "reggaeton","grime","drill",
                "trap","k-pop","pop_rap","pop_punk","viral_pop","bangla_pop")

# genreCompData_1 <- trackData %>%
#   filter(genre == "reggaeton") %>%
#   arrange(desc(popularity))
# 
# genreCompData_2 <- trackData %>%
#   filter(genre == "bangla_pop") %>%
#   arrange(tempo)

# data <- generate_data(genreCompData_1, inputData=NA, draws = 1000,feat="key")
# genre_2_data <- generate_data(genreCompData_2, inputData=NA, draws = 1000,feat="key")
# test<-getGenreText(genre1data = genreCompData_1)
# genreTextRepeat(test,6,"genreText")
getGenreText <- function(genre1data,genre2data=data.frame()){
  #browser()
  if(nrow(genre2data)==0){
    output <- paste("You are sampling from <B>",genre1data$genre[1],
                    "</B>. The most popular track in this genre is <B>",
                    genre1data$song[1],"</B> by <B>",genre1data$artist[1],"</B>",sep="")
  }
  else{
    genre1 = str_to_title(str_replace_all(genre1data$genre[1],"_"," "))
    genre2 = str_to_title(str_replace_all(genre2data$genre[1],"_"," "))
    genre1_song = str_to_title(genre1data$song[1])
    genre2_song = str_to_title(genre2data$song[1])
    genre1_artist = str_to_title(genre1data$artist[1])
    genre2_artist = str_to_title(genre2data$artist[1])
    output<-paste("You are sampling from <B>", genre1, "</B> and <B>", genre2,
                  "</B>. The most popular track for <B>",genre1, "</B> is <B>", genre1_song,
                  "</B> by <B>", genre1_artist,"</B>. The most popular track for <B>", genre2,
                  "</B> is <B>", genre2_song, "</B> by <B>", genre2_artist,"</B>.",sep="")
  }
  return(output)
}

genreTextRepeat <- function(genreText,reps,ID){
  lapply(1:reps,function(i){
    outputID<-paste(ID,i,sep="_")
    output[[outputID]] = genreText
  })
}

generate_data <- function(samplingData,inputData,draws = 1,feat = "tempo"){
  #browser()
  outputData <- sample(samplingData[[feat]],size = draws, replace =TRUE)
  if(all(is.na(inputData))){
    currentGenre = samplingData$genre[1]
    return(c(currentGenre,feat,outputData))
  }
  outputData<-c(inputData,outputData)
  return(outputData)
}

data_prep <- function(data,repLimit){
  #browser()
  tempData <- data.frame(click = seq(1:nrow(data)),
                         data)
  
  clicks <- rep(seq(1:repLimit),
                times = ceiling(nrow(tempData)/repLimit))
  
  reps <- rep(seq(1:ceiling(nrow(tempData)/repLimit)),
              each = repLimit)
  
  tempData$click = clicks[1:nrow(tempData)]
  tempData$rep = reps[1:nrow(tempData)]
  return(tempData)
}

generate_histogram <- function(data, repLimit, means = F,
                               plotCols = cols[3],genreTitle =F){
  #browser()
  if(all(is.na(data))){
    inputData <- data.frame(samples = data)
    tempData <- data_prep(inputData,repLimit = repLimit)
    plotParams = paramGenerator("tempo")
  }
  
  if(all(is.na(data))==FALSE){
    currentGenre = data[1]
    plotParams = paramGenerator(data[2])
    inputData <- data.frame(samples = as.numeric(data[-c(1,2)]))
    tempData <- data_prep(inputData,repLimit = repLimit)
  }

  if(means == T){
    currentData <- tempData %>% 
      group_by(rep) %>% 
      filter(max(click)==repLimit) %>% 
      summarize(samples = mean(samples))
    
    plot<-plot_ly(x = ~currentData$samples, type = "histogram",
                  xbins = list(start = plotParams$min, end = plotParams$max, size = plotParams$binWidth),
                  marker = list(color = cols[6],
                                line = list(width = 1, color = "black"))) %>% 
      layout(title = paste("Sampling distribution when N = ",repLimit),
             yaxis = list(title = "# of Sample Means",
                          range=c(0,30)),
             xaxis = list(zeroline= F,
                          title = plotParams$title,
                          range=c(plotParams$min,plotParams$max),
                          dtick=plotParams$tickInt,
                          ticks = "outside",tickwidth=1,tickcolor = "black",ticklength=2))
  }
  
  if(means == F){
    plotTitle = "Sample Data"
    if(genreTitle & length(data)>1){
      plotTitle = paste("Sample Data for",
                        str_to_title(str_replace_all(data[1],"_"," ")))
    }
    currentRep = max(tempData$rep)

    currentData <- tempData %>% 
      filter(rep == currentRep)
    
    currentClick = sum(is.na(currentData$samples)==FALSE)

    
    plot<-plot_ly(x = ~currentData$samples, type = "histogram",
                  xbins = list(start = plotParams$min, 
                               end = plotParams$max, size = plotParams$binWidth),
                  marker = list(color = plotCols,
                                line = list(width = 1, color = "black"))) %>% 
      layout(title = plotTitle,
             yaxis = list(range=c(0,30),
                          title = "# of Data Points"),
             xaxis = list(zeroline= F,
                          title = plotParams$title,
                          range=c(plotParams$min,plotParams$max),
                          dtick=plotParams$tickInt,
                          ticks = "outside",tickwidth=1,tickcolor = "black",ticklength=2),
             annotations = list(x = plotParams$max*.9, y = 28, 
                                showarrow = F,
                                text = paste(currentClick,"data points drawn")))
  }

  return(plot)
}

generate_timeseries <- function(data, repLimit){
  #browser()
  if(all(is.na(data))){
    inputData <- data.frame(samples = data)
    tempData <- data_prep(inputData,repLimit = repLimit)
  }
  
  if(all(is.na(data))==FALSE){
    currentGenre = data[1]
    inputData <- data.frame(samples = as.numeric(data[-c(1,2)]))
    tempData <- data_prep(inputData,repLimit = repLimit)
  }
  
  currentRep = max(tempData$rep)
  
  tempData <- tempData %>% 
    group_by(rep) %>% 
    mutate(avg = cummean(samples))
  
  greyData <- tempData %>% 
    filter(rep < currentRep)
  
  currentData <- tempData %>% 
    filter(rep == currentRep)
  
  plot <- plot_ly(data = greyData,
                  x = ~click, y = ~avg, split = ~factor(rep),
                  opacity = 0.25,
                  showlegend=F,
                  line = list(color = "grey"),
                  type = "scatter", mode = "lines") %>% 
    add_trace(data = currentData,
              x = ~click, y= ~avg,
              opacity = 1,
              line = list(color = cols[6]),
              marker = list(color = cols[6]),
              type = "scatter", mode = "lines+markers") %>% 
    layout(title = "Cumulative Sample Mean",
           xaxis = list(title = list(text = "# of Data Points",
                                     xanchor = "center",
                                     font = list(size = 14,
                                                 family = "Arial")),
                        zeroline = F),
           yaxis = list(title = list(text = "Sample Mean",
                                     xanchor = "center",
                                     font = list(size = 14,
                                                 family = "Arial")),
                        zeroline = F))
  return(plot)
}

generate_gc_timeseries <- function(genre_1_data,
                                   genre_2_data,
                                   repLimit){
  #browser()
  if(all(is.na(genre_1_data))){
    inputData<-data.frame(genre_1_data = genre_1_data,
                          genre_2_data = genre_2_data)
    tempData <- data_prep(inputData,repLimit = repLimit)
  }
  
  if(all(is.na(genre_1_data))==FALSE){
    inputData<-data.frame(genre_1_data = as.numeric(genre_1_data[-c(1,2)]),
                          genre_2_data = as.numeric(genre_2_data[-c(1,2)]))
    tempData <- data_prep(inputData,repLimit = repLimit)
  }
  
  tempData <- tempData %>% 
    group_by(rep) %>% 
    mutate(avg_1 = cummean(genre_1_data),
           avg_2 = cummean(genre_2_data)) %>% 
    gather(genre,avg,avg_1,avg_2)
  
  tempData$genre[tempData$genre == "avg_1"] = genre_1_data[1]
  tempData$genre[tempData$genre == "avg_2"] = genre_2_data[1]
  
  currentRep = max(tempData$rep)
  
  greyData <- tempData %>% 
    filter(rep < currentRep)
  
  currentData <- tempData %>% 
    filter(rep == currentRep)
  
  plot <- greyData %>%
    group_by(rep,genre) %>%
    plot_ly(x = ~click, y = ~avg, color = ~genre,
            colors = c(cols[5],cols[6]),
            type = "scatter", mode = "lines",
            opacity = 0.25,
            showlegend = F) %>%
    add_trace(data = currentData,
              x = ~click, y= ~avg, color = ~genre,
              colors = c(cols[5],cols[6]),
              opacity = 1,
              type = "scatter", mode = "lines+markers",
              showlegend = T) %>%
    layout(title = "Cumulative Sample Mean",
           xaxis = list(title = list(text = "# of Data Points",
                                     xanchor = "center",
                                     font = list(size = 14,
                                                 family = "Arial")),
                        zeroline = F),
           yaxis = list(title = list(text = "Sample Mean",
                                     xanchor = "center",
                                     font = list(size = 14,
                                                 family = "Arial")),
                        zeroline = F))
  return(plot)
}

paramGenerator<-function(audioFeature){
  if(audioFeature == "tempo"){
    min = 50
    max = 200
    binWidth = 5
    tickInt = 10
    title = "Beats Per Minute (BPM)"
  }
  
  if(audioFeature == "key"){
    min = 0
    max = 12
    binWidth = 0.5
    tickInt = 1
    title = "Numeric Key"
  }
  
  if(audioFeature == "loudness"){
    min = -60
    max = 6
    binWidth = 1
    tickInt = 6
    title = "Loudness"
  }
  
  if(audioFeature == "year"){
    min = 1850
    max = 2022
    binWidth = 2
    tickInt = 10
    title = "Release Year"
  }
  
  if(audioFeature %in% c("acousticness","speechiness","danceability","energy")){
    min = 0
    max = 1
    binWidth = 0.01
    tickInt = 0.1
    title = audioFeature
  }
  output<-data.frame(min,max,title,binWidth,tickInt)
  return(output)
}

generate_gc_histogram <- function(genre_1_data,
                                  genre_2_data,
                                  repLimit){
  #browser()
  if(all(is.na(genre_1_data))){
    inputData<-data.frame(genre_1_data = genre_1_data,
                          genre_2_data = genre_2_data)
    tempData <- data_prep(inputData,repLimit = repLimit)
    plotParams <- paramGenerator("tempo")
  }
  
  if(all(is.na(genre_1_data))==FALSE){
    inputData<-data.frame(genre_1_data = as.numeric(genre_1_data[-c(1,2)]),
                          genre_2_data = as.numeric(genre_2_data[-c(1,2)]))
    tempData <- data_prep(inputData,repLimit = repLimit)
    plotParams <- paramGenerator(genre_1_data[2])
  }
  
  tempData <- tempData %>% 
    group_by(rep) %>% 
    filter(max(click)==repLimit) %>% 
    summarize(avg_1 = mean(genre_1_data),
              avg_2 = mean(genre_2_data))
  
  plot<-plot_ly(xbins = list(start = plotParams$min, end = plotParams$max, size = plotParams$binWidth)) %>% 
    add_histogram(x = ~tempData$avg_2,color = I(cols[5]),name = genre_2_data[1],alpha = 0.5) %>% 
    add_histogram(x = ~tempData$avg_1,color= I(cols[6]),name = genre_1_data[1],alpha = 0.5) %>% 
    layout(barmode = "overlay",
           title = paste("Sampling distribution when N =",repLimit),
           yaxis = list(title = "# of Sample Means",
                        range=c(0,30)),
           xaxis = list(zeroline= F,
                        title = plotParams$title,
                        range=c(plotParams$min,plotParams$max),
                        dtick=plotParams$tickInt,
                        ticks = "outside",tickwidth=1,tickcolor = "black",ticklength=2))
  return(plot)
}

# genreCompData_1 <- trackData %>%
#   filter(genre == "reggaeton") %>%
#   arrange(desc(popularity))
# 
# genreCompData_2 <- trackData %>%
#   filter(genre == "bangla_pop") %>%
#   arrange(tempo)
# 
#genre_1_data <- generate_data(genreCompData_1, inputData=NA, draws = 100,feat="tempo")
#genre_2_data <- generate_data(genreCompData_2, inputData=NA, draws = 100,feat="tempo")

generate_ci_histogram <- function(genre_1_data,
                                  genre_2_data){
  
  if(all(is.na(genre_1_data))){
    plotParams <- paramGenerator("tempo")
    range = NA
    curve_1 <- NA
    curve_1_ci <- NA
    range_1_ci = NA
    curve_2 <- NA
    curve_2_ci <- NA
    range_2_ci <- NA
    testText = " "
    testPosition = 15
    genre_1_name <- NA
    genre_2_name <- NA
  }
  
  if(all(is.na(genre_1_data))==FALSE){
    plotParams <- paramGenerator(genre_1_data[2])
    genre_1_name <- genre_1_data[1]
    genre_2_name <- genre_2_data[1]
    genre_1_data <- as.numeric(genre_1_data[-c(1,2)])
    genre_2_data <- as.numeric(genre_2_data[-c(1,2)])
    avg_1 = mean(genre_1_data)
    avg_2 = mean(genre_2_data)
    se_1 = sd(genre_1_data)/sqrt(length(genre_1_data))
    se_2 = sd(genre_2_data)/sqrt(length(genre_2_data))
    #Calculate p-value based on mean and SD of sample data
    test<-round(tsum.test(mean.x = avg_1, s.x = sd(genre_1_data), n.x = length(genre_1_data),
                          mean.y = avg_2, s.y = sd(genre_2_data), n.y = length(genre_2_data))$p.value,3)
    
    testText = paste("p value =",test)
    if(test < 0.001){
      testText =paste("p value < 0.001")}
    
    #Plot estimated sample distributions based on mean and SE of sample data
    range = seq(plotParams$min,plotParams$max,length=1001)
    curve_1 <- dnorm(x=range,mean = avg_1,sd  = se_1)
    curve_1_int <- curve_1[1:(length(range)-1)]*diff(range)
    curve_1_ci <- curve_1[cumsum(curve_1_int)<0.975 & cumsum(curve_1_int)>0.025]
    range_1_ci = range[cumsum(curve_1_int)<0.975 & cumsum(curve_1_int)>0.025]
    curve_2 <- dnorm(x=range,mean = avg_2,sd  = se_2)
    curve_2_int <- curve_2[1:(length(range)-1)]*diff(range)
    curve_2_ci <- curve_2[cumsum(curve_2_int)<0.975 & cumsum(curve_2_int)>0.025]
    range_2_ci <- range[cumsum(curve_2_int)<0.975 & cumsum(curve_2_int)>0.025]
    testPosition = max(curve_1,curve_2)*101
  }
  plot<-plot_ly() %>% 
    #NOTE: Values are multiplied by 101 to convert probabilities in to the number of data points 
    #with a given value. This essentially turns the PDF into a histogram. This approach was used
    #(as opposed to just plotting a histogram) because Plotly does not seem to offer any sort of
    #density plot. So multiplying by 101 is just kind of a hack.
    add_trace(x = ~range_1_ci,y = ~curve_1_ci*101,type="scatter",mode="lines",fill="tozeroy",
              showlegend = T, name = str_to_title(str_replace_all(genre_1_name,"_"," ")),
              fillcolor = "rgba(239, 202, 8, 0.5)",
              opacity = 0.5,
              line = list(width = 1,
                          color = "white"))%>%
    add_trace(x = ~range_2_ci,y = ~curve_2_ci*101,type="scatter",mode="lines",fill="tozeroy",
              showlegend = T, name = str_to_title(str_replace_all(genre_2_name,"_"," ")),
              fillcolor = "rgba(91, 188, 214, 0.5)",
              line = list(width = 1,
                          color = "white")) %>%
    add_trace(x = ~range,y = ~curve_1*101,type="scatter",mode="lines",
              alpha = 0.5,
              showlegend = F,
              line = list(width = 1,
                          dash = "dot",
                          color = "black")) %>%
      add_trace(x = ~range,y = ~curve_2*101,type="scatter",mode="lines",
                alpha = 0.5,
                showlegend = F,
                line = list(width = 1,
                            dash = "dot",
                            color = "black")) %>%
      layout(title = paste("Estimated Sampling distribution when N = 100"),
             yaxis = list(title = "Estimated # of Data Points"),
             legend = list(x = 0.05,
                           y = 0.95),
             xaxis = list(zeroline= F,
                          title = plotParams$title,
                          range=c(plotParams$min,plotParams$max),
                          dtick=plotParams$tickInt,
                          ticks = "outside",tickwidth=1,tickcolor = "black",ticklength=2),
             annotations = list(x = plotParams$max*.9, y = testPosition,
                                showarrow = F,
                                text = testText))
    
  return(plot)
}

