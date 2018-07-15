# Given an even number ( greater than 2 ), return two prime numbers whose sum will be equal to given number
primeNumbersEqualEven <- function(num)
{
  if((num %% 2 == 0) & num > 2)
  {
    
  }
}

Notes <- read_xlsx("Z:/135_Fleet_Data_Mgmt/Pre_staged_eels/Updated OKC Spares Ready For Eel Build - Master List.xlsx", "Analysis")
write.xlsx(Notes, "Z:/135_Fleet_Data_Mgmt/Pre_staged_eels/Updated OKC Spares Ready For Eel Build - Master List.xlsx")

product <- function(a, b)
{
  return (a * b)
}
# return count of the entered integer if found in Vector 
num_count <- function(num, v){
  count <- 0
  for(ele in v){
    if(ele == num){
      count <- count + 1
    }
  }
  return(count)
}

bar_count <- function(pack){
  # Pack is the kg load to fill up
  amount.of.ones <- pack %% 5
  amount.of.fives <- (pack - amount.of.ones) / 5
  return(amount.of.ones + amount.of.fives)
}

# Pipe Operator
# %>%
df <- mtcars
# Nesting
result <- arrange(sample_n(filter(df, mpg > 20), size = 5), desc(mpg))

# Pipe operator (finally)
result<- df %>% filter(mpg > 20) %>%
  sample_n(size = 5) %>%
  arrange(desc(mpg))
result

avg.mpg.6cyl <- mtcars %>% 
  filter(cyl == 6) %>%
  select(mpg) %>%
  summarise(mean(mpg, na.rm = T))

select(mtcars, mpg, hp)

mtcars %>% 
  select(gear) %>%
  distinct()

# creating new colum using mutate()
mutate(mtcars, Performance = hp/wt)

# transmute() only returns columns mentioned in the function. Only those you want to keep
transmute(mtcars.mute, ingufu = hp)

summarise(mtcars, avg_mpg = mean(mpg))

# AVerage horse power for 6 cylinder vehicle()
mtcars %>%
  filter(cyl == 6) %>%
  summarise(avg_hp = mean(hp))


# gather collapse multiple columns
comp <- c(1,1,1,2,2,2,3,3,3)
yr <- c(1998:2000, 1998:2000, 1998:2000)
q1 <- runif(9, min = 0, max = 100)
q2 <- runif(9, min = 0, max = 100)
q3 <- runif(9, min = 0, max = 100)
q4 <- runif(9, min = 0, max = 100)

df <- data.frame(comp = comp, year = yr, Qtr1 = q1, Qtr2 = q2, Qtr3 = q3, Qtr4 = q4)
gather(df, Quarter, Revenue, Qtr1:Qtr4) # key = "key", value = "value" (key-value pairs in that order)

# spread 
stocks <- data.frame(
  time = as.Date("2009-02-01") + 0:9,
  x = rnorm(10, 0, 1),
  y = rnorm(10, 0, 2),
  z = rnorm(10, 0, 4)
)
stocks
# example of spread
stocks.gathered <- stocks %>% gather(stock, price, x:z) # The first 2 arguments represent columns for the newly created dataframe (stock, price)
stocks.gathered %>% spread(stock, price)
df <- data.frame(new.col = c(NA, "a-x", "b-y", "c-z"))
df <- separate(data = daf, col = new.col, into = c("abc", "xyz"), sep = "-")
# unite function combines multiple columns into one (see example below)
unite(daf, new.joined.col, abc, xyz)
unite(daf, new.joined.col, abc, xyz, sep = "---")

# ggplot2 popular data visualization package for R
# first 3 layers are: Data, Aesthetics and Geometries
# next 3 layers: facets, statistics and coordinates
# syntax: ggplot(data = mtcars) 
pl <- ggplot(data = mtcars, aes(x = mpg, y = hp))

# Data and Aesthetics
pl <- ggplot(movies, aes( x = rating))

# Adding Geometry
pl + geom_histogram()
pl2 <- pl + geom_histogram(binwidth = 0.1, color = "red", fill = "pink", alpha = 0) # alpha is used for transparency

pl3 <- pl2 + xlab("Movie Rating") + ylab("My Count") + ggtitle("My title")
print(pl3)

# Advanced Aesthetic layer
pl4 <- pl + geom_histogram(binwidth = 0.1, aes(fill = ..count..)) #the second argument adds the fill based on the count of the histogram

# scatter plot
## Data and Aesthetics
pl <- ggplot(mtcars, aes(x =wt, y = mpg))

# Geometry
print(pl + geom_point(aes(size = factor(cyl)))) # Use factor() on that column since we know it is a categorical variable
print(pl + geom_point(aes(shape = factor(cyl), color = factor(cyl)), size = 3))

pl2 <- pl + geom_point(aes(color = hp), size = 5)
pl3 <- pl2 + scale_color_gradient(low = "blue", high = "red")

#Barplots
  ##They help show count when working with categorical data
  ##Diference between bar plot and histogram is tht histogram is used for continuous data whereas bar plot is used for categorical data such as 
  ##in case below where the plot is done on categorical(class of vehicle). Also bar plots have space between them
df <- mpg
pl <- ggplot(df, aes(x = class))
pl + geom_bar(aes(fill = drv), position = "dodge")

#BoxPlot used to depict data using their quartile information 
df <- mtcars
pl <- ggplot(df, aes(x = factor(cyl), y = mpg))
print(pl + geom_boxplot(aes(fill = factor(cyl))) + theme_dark())

#Variable plotting
pl <- ggplot(movies, aes(x = year, y = rating))
pl2 <- pl + geom_bin2d() + scale_fill_gradient(high = "red", low = "green")
pl2 <- pl + geom_density2d()

#Coordinates and Faceting
  ##Coordinates allow us to resize our plots correctly
  ##Faceting allows to put several plots next to each other
pl <- ggplot(mpg, aes(x =displ, y = hwy)) + geom_point()
pl2 <- pl + coord_cartesian(xlim = c(1, 4), ylim = c(15, 30))
pl2 <- pl + coord_fixed(ratio = 1/3)

print(pl + facet_grid(. ~ cyl)) #syntax for facet_grid is what you wanna facet by Y axis, ~, what you want to facet by X axis
print(pl + facet_grid(drv ~ .)) #if you want to facet by one column

##ggthemes
pl <- ggplot(mtcars, aes(x = wt, y =mpg)) + geom_point()
theme_set(theme_minimal()) # this would set the theme for all plots vs
pl + theme_dark() # which would add the theme for individual plot

print(pl + theme_fivethirtyeight())

###############################################################################
#ggplot exercises
###############################################################################
pl <- ggplot(mpg, aes(x = hwy)) # data + column to use
pl1<- pl + geom_histogram(binwidth = 1.8, fill = "pink") # geometry
ggplotly(pl1)

#gsub exercises
submitted.Eel <- read.table("EEL_request.txt")
for(i in 1:length(submitted.Eel){
  clean.Eel <- gsub("\\;", " ", submitted.Eel[[1]])
  submitted.Eel[,i] <- clean.Eel
  i = i + 1
})
  
# separate_rows() if a variable contains multiple delimited values, this function separate values and places each one in its own row
# separate_rows() is part of tidyverse package 
  submitted.Eel <- read.table("EEL_request.txt")
  EELs <- separate_rows(submitted.Eel, V1, convert = TRUE)

  # plotly package for interactive visualization
  library(plotly)
  pl <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
  gpl <- ggplotly(pl) #Here we are converting a usual ggplot plot to a plotly plot (interactive)

  
# plot_ly() exercises from www.datacamp.com 
  # I would need diamond data in order to produce these plots
  str(diamonds)
  head(diamonds, 3)
  # A first scatterplot has been made for you
  plot_ly(diamonds, x = ~carat, y = ~price)
  
  # Replace ___ with the correct vector
  plot_ly(diamonds, x = ~carat, y = ~price, color = ~carat)
  
  # Replace ___ with the correct vector
  plot_ly(diamonds, x = ~carat, y = ~price, color = ~carat, size = ~carat)
  
  # Bar plot using plot_ly
  diamonds_bucket <- diamonds %>% count(cut, clarity)
  head(diamonds_bucket, 3)
  # Replace ___ with the correct vector
  plot_ly(diamonds_bucket, x = ~cut, y = ~n, type ="bar",  color = ~clarity) 
  
  # Mapping all commercial airports in the world
  # dataset is "airports" 
  g <- list(
    scope = 'world',
    showland = TRUE,
    landcolor = toRGB("gray95")
  )
  
  plot_geo(airports, lat = ~Latitude, lon = ~Longitude) %>%
    add_markers(
      text = ~paste(AirportID, City, Country, sep = "<br />"),
      color = ~Country, symbol = I("square"), size = I(3), hoverinfo = "text", colors = "Set1"
    ) %>%
    layout(
      title = 'Commercial Airports Worldwide', geo = g
    )

  # Apple Stock Price With Rangeslider  
  plot_ly(apple_stock_price, x = ~Date) %>%
    add_lines(y = ~AAPL.Adjusted, name = "Apple") %>% 
    rangeslider() %>% 
    layout(
      title = "Stock Price Apple",
      xaxis = list(title = "Date"),
      yaxis = list(title = "Price"))
  
  
  
# adding rows to dataframe
  df2 <- data.frame(col.name = 2000, col.name2 = "new")
  df <- data.frame(col.name = 1:10, col.name2 = letters[1:10])
  dfnew <- rbind(df, df2)
  
# adding column to data.frame
  df$newcol <- 2 *df$col.name
  
  df[, "newcol.copy"] <- df$newcol
  
# Renaming columns
  colnames(df) <- c("somename", ...)
  colnames(df)[1] <- "New Col Name"
  
# Selecting multiple rows from dataframe
  df[1:3, ]
  df[-2, ] # this selects everything but second row
  
# conditional selection
  mtcars[mtcars$mpg > 20, ]
  mtcars[mtcars$mpg > 20 & mtcars$cyl == 6, c("mpg", "cyl", "hp")] # specifying columns as well
  subset(mtcars, mpg > 20 & cyl == 6) # when using subset() no need to qualify the dataframe
  mtcars[, c(1, 2, 3)]
  mtcars[, c("mpg", "cyl")]
  
# Dealing with missing data
  is.na(mtcars) #returns logical dataframe
  any(is.na(df))
  df[is.na(df)] < 0 # replacing all null values with 0
  mtcars$mpg[is.na(mtcars$mpg)] <- mean(mtcars$mpg) # replacing all the null values with the mean of the data in that column (inpute)
  