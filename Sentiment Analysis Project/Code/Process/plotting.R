libs <- c("tidyverse",
          "rvest",
          "polite",
          "httr",
          "stringr",
          "lubridate",
          "xml2",
          "dplyr",
          "ggplot2",
          "scales",
          "gridExtra",
          "ggthemes",
          "TSstudio",
          "xts",
          "tseries",
          "forecast",
          "stargazer",
          "TTR",
          "plotly",
          "zoo",
          "readxl",
          'aTSA',
          'gtsummary',
          'skimr',
          'kableExtra',
          'stringi')
install.packages(setdiff(libs, rownames(installed.packages())))
lapply(libs, library, character.only = TRUE)

# Setup -------------------------------------------------------------------



df <- read.csv("/Users/zahrita/Documents/DissData/Output/corpusv3.csv")
df$date <- ymd(df$date)
head(df)

#summary stats 
df2 <- df %>%
  mutate(wrdcount = stri_count_words(articles))
xarticles_per_year <- df %>%
  mutate(year = year(ymd(date))) %>%
  group_by(year) %>%
  mutate(total = n()) %>%
  group_by(year, articles) %>%
  summarise(count = n(), total = first(total), .groups="drop") %>%
  distinct(total)

mean(articles_per_year$total)

summary_table <- df %>%
  summarise(
  total_obs = length(articles),
  average_word_count = mean(wrdcount),
  median_word_count = median(wrdcount),
  date_range = paste(min(date), "to", max(date)),
  articles_py = mean(total_obs))
kable(summary_table, format = "latex", booktabs = TRUE)



kable(summary_table, format = "latex", booktabs = TRUE)


str(df)
df_mean <- df %>%
  mutate(year = year(ymd(date))) %>%
  group_by(year) %>%
  summarise(nativism_mean = mean(predicted_class, na.rm = TRUE))
# Plotting -------------------------------------------------------------------------
l <- ggplot(df_mean, aes(x = year, y = nativism_mean)) +
  geom_line(color = "#008080", linewidth = 0.5 ) +
  theme_classic() +
  theme(
    axis.text = element_text(size= 11),
    axis.title = element_text(size = 11),
    plot.title = element_text(size = 13)
  ) +
  labs(
    title = "Flucuations of Nativism in Organized Labor: 1900-1988",
    x = "Year",
    y = "Average Natvism Per Year"
  )
l <- ggplotly(l)
l
l
# Nativsm and Immigration -------------------------------------------------------------------------
imm1 <- read_excel("~/Documents/DissData/Data/Inputs/Immigration/Ad1-2.xls")
scaledimm <- imm1 %>%
  select(-EmigrantsFromTheUnitedStates_Ad2_Number) %>%
  filter(Year != "(TQ)") %>%
  filter(Year >= "1900")%>%
  filter(Year <= "1988") %>%
  filter(Year != "1907") %>%
  filter(Year != "1955") %>%
  mutate(
    Year= as.numeric(Year),
    imm = scale(ImmigrantsToTheUnitedStates_Ad1_Number))

imm.ts <- ts(imm1$ImmigrantsToTheUnitedStates_Ad1_Number, end = 1998, frequency = 1)

autoplot(imm.ts)
imm1.ts <- window(imm.ts, start = 1900, end = 1988)
ggplotly(autoplot(imm1.ts))

imm1.ts <- ts(scaledimm$ImmigrantsToTheUnitedStates_Ad1_Number, end = 1988, frequency = 1)

scaledyear <- df_mean %>%
  mutate(nativism_mean = scale(nativism_mean))
mean.ts <- ts(scaledyear$nativism_mean, end = 1988, frequency = 1)

#Plotting 
scaledimm <- imm1 %>%
  select(-EmigrantsFromTheUnitedStates_Ad2_Number) %>%
  filter(Year != "(TQ)") %>%
  filter(Year >= "1900")%>%
  filter(Year <= "1988") %>%
  filter(Year != "1907") %>%
  filter(Year != "1955") %>%
  mutate(
    Year= as.numeric(Year),
    imm = scale(ImmigrantsToTheUnitedStates_Ad1_Number))

p = ggplot() +
  geom_line(data=scaledyear,
            aes(x = year, y = nativism_mean[,1], colour="nativism_mean"), 
                linewidth = 0.5,
          ) +
  geom_line(data = scaledimm,
            aes(x = Year, y = imm[,1], colour = "imm"),
            linewidth = 0.5,
            ) +
  scale_color_manual(name = "Legend", values = c("nativism_mean" = "darkblue", "imm" = "red"),
                     labels = c("Average Nativism", "Immigration")) +
  theme_classic() +
  theme(panel.grid = element_blank()) +
  labs(
    title = "Immigration to the US Plotted Against Yearly Average Nativism (scaled)",
    x = "Year",
    y = "Z-scaling",
  )
#____________COINTEGRATION TEST ________________
longrun1 <- lm(scaledyear$nativism_mean ~ scaledimm$ImmigrantsToTheUnitedStates_Ad1_Number)

test1 <- coint.test(mean.ts, imm1.ts, nlag=3, output = TRUE)
summary(test1)
stargazer(test1)
# Emplpoyment AND nativism -------------------------------------------------------------------------
employmentdf <- read_excel("~/Documents/DissData/Data/workforce data/Ba470-477.xls")
scaledemp <- employmentdf %>%
  select(Year, Unemployed_AsPercentageOf_CivilianLaborForce_Ba475_Percent) %>%
  filter(Year >= "1900")%>%
  filter(Year <= "1988") %>%
  filter(Year != "1907") %>%
  filter(Year != "1955") %>%
  mutate(
    Year = as.numeric(Year),
    emprate = scale(Unemployed_AsPercentageOf_CivilianLaborForce_Ba475_Percent))

emp.ts <- ts(scaledemp$Unemployed_AsPercentageOf_CivilianLaborForce_Ba475_Percent, end = 1988, frequency =  1)

p = ggplot() +
  geom_line(data=scaledyear,
            aes(x = year, y = nativism_mean[,1], colour="nativism_mean"), 
            linewidth = 0.5,
  ) +
  geom_line(data = scaledemp,
            aes(x = Year, y = emprate[,1], colour = "emprate"),
            linewidth = 0.5,
  ) +
  scale_color_manual(name = "Legend", values = c("nativism_mean" = "darkblue", "emprate" = "red"),
                     labels = c("Average Nativism", "Unemployment Rate")) +
  theme_classic() +
  theme(panel.grid = element_blank()) +
  labs(
    title = "Unemployment Rate Plotted Against Yearly Average Nativism (scaled)",
    x = "Year",
    y = "Z-scaling",
  )
#___________COINTEGRATION TEST 2---------------------
test2 <- coint.test(mean.ts, emp.ts, nlag=3, output = TRUE)
stargazer(test2)
 # Regression W/ GG --------------------------------------------------------

df_mean$date_num <- as.numeric(df_mean$year)
df$date_num <- as.numeric(df$date)
model <- lm(nativism_mean ~ date_num, data = df_mean)
summary(model)
stargazer(model)
model2 <- lm(predicted_class ~ date_num, data = df)
summary(model2)

df_mean$trend_line <- predict(model, newdata = df_mean)
# Replotting --------------------------------------------------------------
#with the trend line. 
dfmean1 <- df %>%
  filter(date >= as.Date("1900-01-01"), date <= as.Date("1930-12-31")) %>%
  group_by(month = floor_date(date, 'month')) %>%
  mutate(total = n()) %>%
  group_by(month, predicted_class) %>%
  summarise(count = n(), total = first(total), .groups="drop") %>%
  mutate(percentage = (count/total) * 100) %>%
  filter(predicted_class == 1) %>%
  ungroup() %>%
  select(-month)

p <- ggplot(df_mean, aes(x = year)) +
  geom_line(aes(y = nativism_mean), color = "darkblue", linewidth = 0.7 ) +
  geom_line(aes(y = trend_line), color = "darkred", linewidth=0.5)+
  labs(
    title = "Yearly Nativist Sentiment in Organized Labor: 1900-1988",
    x = "Year",
    y = "Average Natvist Articles"
  ) +
theme_classic() 
f <- ggplotly(p)
p

# Plotting -------------------------------------------------------------------------
p <- ggplot(df_mean1, aes(x = month, y = nativism_mean)) +
  geom_line(color = "#008080", linewidth = 0.5 ) +
  theme_classic() +
  theme(
    axis.text = element_text(size= 14),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 19)
  ) +
  labs(
    title = "Flucuations of Nativism in Organized Labor: 1900-1988",
    x = "Year",
    y = "Average Natvism Per Month"
  )
p
# Plotting -------------------------------------------------------------------------
dfmon1.ts <- ts(df_mean1$nativism_mean, )
ggplot(df_mean, aes(x = year, y = nativism_mean)) +
  geom_line(color = "#008080", linewidth = 0.5 ) +
  theme_classic() +
  theme(
    axis.text = element_text(size= 14),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 19)
  ) +
  labs(
    title = "Flucuations of Nativism in Organized Labor: 1900-1988",
    x = "Year",
    y = "Average Natvism Per Year"
  )


dfm1 <- df %>%
  filter(date >= as.Date("1900-01-01"), date <= as.Date("1930-12-31")) %>%
  group_by(month = floor_date(date, 'month')) %>%
  mutate(total = n()) %>%
  group_by(month, predicted_class) %>%
  summarise(count = n(), total = first(total), .groups="drop") %>%
  mutate(percentage = (count/total) * 100) %>%
  filter(predicted_class == 1) %>%
  arrange(month) 

dfm1.5 <- df %>%
  filter(date >= as.Date("1900-01-01"), date <= as.Date("1930-12-31")) %>%
  group_by(month = floor_date(date, 'month')) %>%
  mutate(total = n()) %>%
  group_by(month, predicted_class, articles) %>%
  summarise(count = n(), total = first(total), .groups="drop") %>%
  mutate(percentage = (count/total) * 100) %>%
  filter(predicted_class == 1) %>%
  arrange(month) 

dfm1.ts <- zoo(dfm1$percentage, order.by = dfm1$month)

dfm1sma <- SMA(dfm1.ts, window = 12)

p <-autoplot(dfm1sma, series = "12-month SMA") +
  labs(
    title = " Nativism in Organized Labor: 1900-1930",
    x = "Date",
    y = "Percentage of Natvist Articles Per Month"
  )
f <- ggplotly(p)
p
f

df10p <- data.frame(
  time = as.Date(zoo::index(dfm1sma)),
  val = as.numeric(dfm1sma)
)
h <- ggplot(df10p, aes(x = time, y = val)) +
  geom_line(colour = "blue")+
  annotate("rect", xmin=as.Date("1902-09-01"), xmax = as.Date("1904-08-01"), ymin=-Inf, ymax=Inf,
           alpha=0.15, fill="yellow") +
  annotate("rect", xmin=as.Date("1907-01-01"), xmax = as.Date("1907-12-01"), ymin=-Inf, ymax=Inf,
           alpha=0.15, fill="red") +
  annotate("rect", xmin=as.Date("1907-10-01"), xmax = as.Date("1908-06-01"), ymin=-Inf, ymax=Inf,
           alpha=0.15, fill="yellow") +
  annotate("rect", xmin=as.Date("1910-01-01"), xmax = as.Date("1912-01-01"), ymin=-Inf, ymax=Inf,
           alpha=0.15, fill="yellow") +
  annotate("rect", xmin=as.Date("1913-01-01"), xmax = as.Date("1914-12-01"), ymin=-Inf, ymax=Inf,
           alpha=0.15, fill="yellow") +
  annotate("rect", xmin=as.Date("1918-08-01"), xmax = as.Date("1921-07-01"), ymin=-Inf, ymax=Inf,
           alpha=0.15, fill="yellow") +
  annotate("rect", xmin=as.Date("1923-05-01"), xmax = as.Date("1924-06-01"), ymin=-Inf, ymax=Inf,
           alpha=0.15, fill="yellow") +
  annotate("rect", xmin=as.Date("1929-09-01"), xmax = as.Date("1930-12-12"), ymin=-Inf, ymax=Inf,
           alpha=0.15, fill="yellow") +
  annotate("text", x = as.Date("1903-09-01"), y = Inf, label = "Recession",
           vjust=2, size=3.5, fontface="bold") +
  annotate("text", x = as.Date("1907-06-01"), y = Inf, label = "Anti-Asian Riots",
           vjust=4, size=3.5, fontface="bold") +
  annotate("text", x = as.Date("1907-11-01"), y = Inf, label = "Great Panic",
           vjust=2, size=3.5, fontface="bold") +
  annotate("text", x = as.Date("1911-09-01"), y = Inf, label = "Panic of 1910-1911",
           vjust=2, size=3.5, fontface="bold") +
  annotate("text", x = as.Date("1914-05-01"), y = Inf, label = "Recession",
           vjust=2, size=3.5, fontface="bold") +
  annotate("text", x = as.Date("1919-09-01"), y = Inf, label = "Post-War Recession",
           vjust=2, size=3.5, fontface="bold") +
  annotate("text", x = as.Date("1923-09-01"), y = Inf, label = "Industrial Recession",
           vjust=2, size=3.5, fontface="bold") +
  annotate("text", x = as.Date("1929-09-01"), y = Inf, label = "Great Depression",
           vjust=2, size=3.5, fontface="bold") +
  coord_cartesian(clip = "off") +
  labs(title = "Nativism in Organized Labor: 1900-1930") +
  ylab("Monthly Nativist Percentage") +
  xlab("Year") +
  theme_classic() +
  theme(
    plot.margin = margin(20,10,10,10),
    plot.title = element_text(face = "bold", size = 15, hjust=0.5),
    axis.title = element_text(size = 10),
    panel.grid.minor = element_blank()
  )
f <- ggplotly(h)
f

dfm2 <- df %>%
  filter(date >= as.Date("1930-01-01"), date <= as.Date("1970-12-31")) %>%
  group_by(month = floor_date(date, 'month')) %>%
  mutate(total = n()) %>%
  filter(!month %in% as.Date(c("1953-09-01", "1956-06-01"))) %>%
  group_by(month, predicted_class) %>%
  summarise(count = n(), total = first(total), .groups="drop") %>%
  mutate(percentage = (count/total) * 100) %>%
  filter(predicted_class == 1) %>%
  arrange(month) 

dfm2.ts <- zoo(dfm2$percentage, order.by = dfm2$month)

dfm2sma <- SMA(dfm2.ts, window = 12)

p <-autoplot(dfm2sma, series = "12-month SMA") +
  labs(
    title = " Nativism in Organized Labor: 1930-1970",
    x = "Date",
    y = "Percentage of Natvist Articles Per Month"
  )
f <- ggplotly(p)
p
f


df10p2 <- data.frame(
  time = as.Date(zoo::index(dfm2sma)),
  val = as.numeric(dfm2sma)
)
h <- ggplot(df10p2, aes(x = time, y = val)) +
  geom_line(colour = "blue")+
  annotate("rect", xmin=as.Date("1930-12-01"), xmax = as.Date("1939-12-01"), ymin=-Inf, ymax=Inf,
           alpha=0.15, fill="yellow") +
  annotate("rect", xmin=as.Date("1941-12-01"), xmax = as.Date("1945-09-01"), ymin=-Inf, ymax=Inf,
           alpha=0.15, fill="red") +
  annotate("rect", xmin=as.Date("1953-07-01"), xmax = as.Date("1954-05-01"), ymin=-Inf, ymax=Inf,
           alpha=0.15, fill="yellow") +
  annotate("rect", xmin=as.Date("1954-06-01"), xmax = as.Date("1954-10-01"), ymin=-Inf, ymax=Inf,
           alpha=0.15, fill="red") +
  annotate("rect", xmin=as.Date("1960-04-01"), xmax = as.Date("1961-02-01"), ymin=-Inf, ymax=Inf,
           alpha=0.15, fill="yellow") +
  annotate("text", x = as.Date("1933-09-01"), y = Inf, label = "The Great Depression",
           vjust=2, size=3.5, fontface="bold") +
  annotate("text", x = as.Date("1942-11-01"), y = Inf, label = "WWII",
           vjust=2, size=3.5, fontface="bold") +
  annotate("text", x = as.Date("1953-11-01"), y = Inf, label = "Recession",
           vjust=4, size=3.5, fontface="bold") +
  annotate("text", x = as.Date("1955-10-01"), y = Inf, label = "Operation Wetback",
           vjust=2, size=3, fontface="bold") +
  annotate("text", x = as.Date("1960-10-01"), y = Inf, label = "Recession",
           vjust=4, size=3.5, fontface="bold") +
  coord_cartesian(clip = "off") +
  labs(title = "Nativism in Organized Labor: 1930-1970") +
  ylab("Monthly Nativist Percentage") +
  xlab("Year") +
  theme_classic() +
  theme(
    plot.margin = margin(20,10,10,10),
    plot.title = element_text(face = "bold", size = 15, hjust=0.5),
    axis.title = element_text(size = 10),
    panel.grid.minor = element_blank()
  )
h
f <- ggplotly(h)
f


dfm3 <- df %>%
  filter(date >= as.Date("1971-01-01"), date <= as.Date("1988-12-31")) %>%
  group_by(month = floor_date(date, 'month')) %>%
  mutate(total = n()) %>%
  group_by(month, predicted_class) %>%
  summarise(count = n(), total = first(total), .groups="drop") %>%
  mutate(percentage = (count/total) * 100) %>%
  filter(predicted_class == 1) %>%
  arrange(month) 

dfm3.5 <- df %>%
  filter(date >= as.Date("1971-01-01"), date <= as.Date("1988-12-31")) %>%
  group_by(month = floor_date(date, 'month')) %>%
  mutate(total = n()) %>%
  group_by(month, predicted_class, articles) %>%
  summarise(count = n(), total = first(total), .groups="drop") %>%
  mutate(percentage = (count/total) * 100) %>%
  filter(predicted_class == 1) %>%
  arrange(month) 
dfm3.ts <- zoo(dfm3$percentage, order.by = dfm3$month)

dfm3sma <- SMA(dfm3.ts, window = 6)

df10p3 <- data.frame(
  time = as.Date(zoo::index(dfm3sma)),
  val = as.numeric(dfm3sma)
)

h <- ggplot(df10p3, aes(x = time, y = val)) +
  geom_line(colour = "darkblue")+
  annotate("rect", xmin=as.Date("1973-12-01"), xmax = as.Date("1975-03-01"), ymin=-Inf, ymax=Inf,
           alpha=0.15, fill="yellow") +
  annotate("rect", xmin=as.Date("1981-07-01"), xmax = as.Date("1982-11-01"), ymin=-Inf, ymax=Inf,
           alpha=0.15, fill="yellow") +
  annotate("rect", xmin=as.Date("1980-07-01"), xmax = as.Date("1981-11-01"), ymin=-Inf, ymax=Inf,
           alpha=0.15, fill="red") +
  annotate("text", x = as.Date("1974-07-01"), y = Inf, label = "Recession",
           vjust=2, size=3.5, fontface="bold") +
  annotate("text", x = as.Date("1981-12-01"), y = Inf, label = "Recession",
           vjust=2, size=3.5, fontface="bold") +
  annotate("text", x = as.Date("1980-08-01"), y = Inf, label = "Reagan Campaign",
           vjust=4, size=3.5, fontface="bold") +
  coord_cartesian(clip = "off") +
  labs(title = "Nativism in Organized Labor: 1970-1988") +
  ylab("Monthly Nativist Percentage") +
  xlab("Year") +
  theme_classic() +
  theme(
    plot.margin = margin(20,10,10,10),
    plot.title = element_text(face = "bold", size = 13, hjust=0.5),
    axis.title = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

p <-autoplot(dfm3sma, series = "12-month SMA") +
  labs(
    title = " Nativism in Organized Labor: 1971-1988",
    x = "Date",
    y = "Percentage of Natvist Articles Per Month"
  )
f <- ggplotly(p)
p

autoplot(dfm1sma, ts.colour = "Blue") +
  labs(title = "Nativism Throughout 1912-1924") +
  ylab("Nativist Sentiment %") +xlab("Months")

dfm12 <- df %>%
  group_by(month = floor_date(date, 'month')) %>%
  mutate(total = n()) %>%
  group_by(month, predicted_label) %>%
  summarise(count = n(), total = first(total)) %>%
  mutate(percentage = (count/total) * 100) %>%
  filter(predicted_label == 1)

dfm12.ts <- ts(dfm12$percentage, start = c(1939), end = c(1950), frequency = 12)

dfm12sma <- SMA(dfm12.ts, window = 12)

autoplot(dfm12sma, ts.colour = "red") +
  labs(title = "Nativism Throughout 1939-1950") +
  ylab("Nativist Sentiment %") + xlab("Years")

