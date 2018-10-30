library(data.table)
library(plotly)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(googleVis)
# library(rsconnect)
library(scales)

# LOAD THE DATA
spotify.data <- fread(file = "./spotify_data.csv")
# MUTATE AND FORMAT  AND FIX THE DATA
spotify.data <- spotify.data %>%
  rename(Track.Name = `Track Name`) %>%
  mutate(Date = ymd(Date),
         Track.Name = ifelse(Track.Name == "Me RehÃºso", "Me Rehuso", Track.Name)) %>%
  filter(year(Date) == 2017, Track.Name != "")

# LOAD REGION CODES AND OTHER INFO
spotify.region.codes <- fread(file = "./spotify_region_codes.csv", stringsAsFactors = FALSE)
spotify.region.codes <- spotify.region.codes %>% 
  mutate(Region = tolower(Region))

# FIND THE BEST RANK POSITION FOR EVERY TRACK ON THE GLOBAL CHART
best.ranks <- spotify.data %>%
  filter(Region == "global") %>%
  group_by(Track.Name) %>%
  summarise(best.rank = min(Position))

# SUBSET SPECIFIC TOP TRACKS
number.ones <- best.ranks$Track.Name[which(best.ranks$best.rank == 1)]
my.number.ones <- number.ones[c(2,3,4,7,8,9)]

number.twos <- best.ranks$Track.Name[which(best.ranks$best.rank == 2)]
my.number.twos <- number.twos[c(10,12)]

number.threes <- best.ranks$Track.Name[which(best.ranks$best.rank == 3)]
my.number.threes <- number.threes[c(1,2,4)]

# DEFINE SONG AND REGION VECTORS FOR FUTURE USE IN UI
top.songs <- sort(c(my.number.ones,my.number.twos))
my.regions <- sort(c("ca","us","mx","br","ar","gb","fr"))

# GET WORLDWIDE #1'S ON THE LAST DAY OF 2017
latest.spotify.data <- spotify.data %>% 
                            filter(Date == as.Date("2017-12-31"),
                                   Position == 1,
                                   Region != "global")

# GET THE MOST STREAMED TRACKS BY COUNTRY DURING 2017
most.streamed.2017 <- spotify.data %>%
                          filter(Region != "global") %>%
                          group_by(Region, Track.Name) %>%
                          summarise(Streams = sum(Streams),
                                    log.Streams = log(sum(Streams))) %>%
                          top_n(1, Streams)

# FIND WHICH TRACKS REACHED THE TOP 10 AND ON WHICH DATE THEY EACH ENTERED THEIR 
# RESPECTIVE COUNTRIES TOP 10
best.ranks.region <- spotify.data %>%
  select(-URL, -Streams) %>%
  filter(Region != "global", Position <= 10) %>%
  group_by(Region, Track.Name) %>%
  summarise(top.rank.date = min(Date))

# USE THE LAST TABLE TO COUNT THE NUMBER OF DAYS IT TOOK FOR EACH OF THE TOP TEN 
# TRACKS TO ACTUALLY ENTER THE TOP 10 AND GET THE MEAN DAYS FOR EACH COUNTRY
# THIS IS FOR THE ADOPTION/RETENTION SCATTERPLOT
top.ten.region <- spotify.data %>%
  select(-URL) %>%
  left_join(., best.ranks.region, by = c("Region","Track.Name")) %>%
  filter(!is.na(top.rank.date)) %>%
  group_by(Track.Name, Region) %>%
  summarise(days.to.top = sum(ifelse(Date < top.rank.date,1,0)),
            days.after.top = sum(ifelse(Date > top.rank.date,1,0)),
            Streams = sum(as.numeric(Streams))) %>%
  group_by(Region) %>%
  summarise(mean.days.to.top = mean(days.to.top),
            mean.days.after.top = mean(days.after.top),
            number.of.tracks = n(),
            total.streams = sum(as.numeric(Streams))) %>%
  left_join(., select(spotify.region.codes, Region , Continent), by = "Region")

#  GET THE TRACKS THAT REACH THE TOP 10 PER COUNTRY
top.ten.track <- spotify.data %>%
  select(-URL) %>%
  left_join(., best.ranks.region, by = c("Region","Track.Name")) %>%
  filter(!is.na(top.rank.date), Region != "global") %>%
  group_by(Track.Name, Region) %>%
  summarise(days.to.top = sum(ifelse(Date < top.rank.date,1,0)),
            days.after.top = sum(ifelse(Date > top.rank.date,1,0)),
            Streams = sum(as.numeric(Streams))) %>%
  group_by(Track.Name) %>%
  summarise(mean.days.to.top = mean(days.to.top),
            mean.days.after.top = mean(days.after.top),
            total.streams = sum(as.numeric(Streams)))

# CALCULATE THE AMOUNT OF TOTAL STREAMS FOR EVERY TRACK WORLDWIDE
total.streams.2017 <- spotify.data %>%
  filter(Region != "global") %>%
  group_by(Track.Name) %>%
  summarise(Streams = sum(as.numeric(Streams)), first.appear = min(Date))

# FIND THE TOP 10 STREAMING COUNTRIES
top.countries <- spotify.data %>%
  filter(Region != "global") %>%
  group_by(Region) %>%
  summarise(Streams = sum(as.numeric(Streams))) %>%
  arrange(desc(Streams)) %>%
  top_n(10)

# GET DESCRIPTIVE STATS TO HELP SUBSET OUTLYING OBSERVATIONS
descriptive.stats <- spotify.data %>%
  filter(Region %in% top.countries$Region) %>%
  group_by(Region, Track.Name) %>%
  summarise(Streams = sum(as.numeric(Streams))) %>%
  group_by(Region) %>%
  summarise(median = median(Streams),
            q3 = quantile(Streams,0.75),
            iqr = IQR(Streams),
            p80 = quantile(Streams,0.8)) %>%
  mutate(upper.fence.1 = q3+iqr*1.5, upper.fence.2 = p80)

# GET DESCRIPTIVE STATS TO HELP SUBSET OUTLYING OBSERVATIONS FOR EVERY REGION
descriptive.stats.long <- spotify.data %>%
  # filter(Region %in% top.countries$Region) %>%
  group_by(Region, Track.Name) %>%
  summarise(Streams = sum(as.numeric(Streams))) %>%
  group_by(Region) %>%
  summarise(median = median(Streams),
            q3 = quantile(Streams,0.75),
            iqr = IQR(Streams),
            p80 = quantile(Streams,0.8)) %>%
  mutate(upper.fence.1 = q3+iqr*1.5, upper.fence.2 = p80)

# CALCULATE PROPORTION OF STREAMS GENERATED BY OUTLIERS BY COUNTRY
polarity.region <- as.data.frame(spotify.data %>%
      filter(Region != "global") %>%
      group_by(Region, Track.Name) %>%
      summarise(Streams = sum(as.numeric(Streams))) %>%
      left_join(.,select(descriptive.stats.long, Region, upper.fence = upper.fence.2), by = "Region") %>%
      mutate(stream.type = ifelse(Streams < upper.fence, "s.regular","s.outlier"),
             n.type = ifelse(Streams < upper.fence, "n.regular","n.outlier")) %>%
      group_by(Region, stream.type, n.type) %>%
      summarise(Streams = sum(Streams),n = n()) %>%
      spread(key = stream.type, value = Streams, fill = 0) %>%
      spread(key = n.type, value = n, fill = 0) %>%
      group_by(Region) %>%
      summarise(s.regular = sum(s.regular), s.outlier = sum(s.outlier),
                n.regular = sum(n.regular), n.outlier = sum(n.outlier)) %>%
      mutate(s.outlier.part = s.outlier/(s.regular+s.outlier),
             n.outlier.part = n.outlier/(n.regular+n.outlier)) %>%
      filter(n.outlier > 100))

# LOAD REGIONS CODE DATA
# spotify.region.codes <- fread(file = "./spotify_region_codes.csv", stringsAsFactors = FALSE)
# spotify.region.codes <- spotify.region.codes %>% 
#   mutate(Region = tolower(Region))
# 
# latest.spotify.data <- left_join(latest.spotify.data, spotify.region.codes)

# latest.spotify.data %>%
#   select(-URL) %>%
#   arrange(desc(Streams)) %>%
#   head(.)

# "Whats the story (morning glory)?"
# 
# "What was the most streamed song of 2017 by country?"
# "How long does it take for a track to reach/leave it's top position?"
#   "How does it change by country?"
# "Which countries are the best at adopting and retaining music?"





