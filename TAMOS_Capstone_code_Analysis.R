
# Analysis Code - Capstone Project
# Tyler Amos

# Define list of organizations
organizations <- c("ICC", "OHCHR", "HRW", "USSTATE", "FIDH", "CRISISGROUP", "AMNESTY")

# Country codes, e.g., "IR" for Iran. This list contains Iran, Israel, Turkey, and Egypt
country_code <- c("IR", "IS", "TU", "EG")


# Merge all tone data
require(stringr)
tone_all <- data.frame()
for (i in 1:7) {
  tone_working <- data.frame()
  tone_working <- read.delim(str_c(organizations[i], "tone_data_flat.txt"))
  tone_working[,9] <- organizations[i]
  tone_all <- rbind(tone_all, tone_working)
  rm(tone_working)
}

# Merge all location data
locations_all <- data.frame()
for (i in 1:7) {
  locations_working <- data.frame()
  locations_working <- read.delim(str_c(organizations[i], "locations_data_flat.txt"))
  locations_working[,9] <- organizations[i]
  locations_all <- rbind(locations_all, locations_working)
  rm(locations_working)
}
colnames(locations_all)[9]<- "source"

# Merge tone and location data, using gkgrecordid as key

tone_location <- merge(locations_all[,c(1,4,9)], tone_all[,c(1:7)], by = c("gkgrecordid"), all.x = FALSE, all.y = FALSE)

# Clean environment to free up space
rm(locations_all)
rm(tone_all)


# Take only complete cases
tone_location <- na.omit(tone_location)

# Select a stratified random sample to work with, extracting data for the country list above, equally divided among organizations.

# First, verify there are enough records from each source, for each country, to work with

table(tone_location$source[tone_location$flat_location_countrycode == "IR"])
table(tone_location$source[tone_location$flat_location_countrycode == "IS"])
table(tone_location$source[tone_location$flat_location_countrycode == "TU"])

# Second, in order to create our baseline group, we need to assign "EG" to all other countries

tone_location[!(tone_location$flat_location_countrycode %in% country_code),"flat_location_countrycode"] <- levels(tone_location$flat_location_countrycode)[53]

# There are very few ICC records to use for all the countries, thus ICC is not referenced 
# and dropped from the for loop below, 1200 records to be randomly selected from each country-source combination

require(dplyr)

tone_location_3country <- data.frame()
for (i in 1:4)
{
  tone_locationcntry <- tone_location[tone_location$flat_location_countrycode == country_code[i],]
  for (i in 2:7)
  {
    tone_locationorg <- sample_n(tone_locationcntry[tone_locationcntry$source == organizations[i],], 1200)
    tone_location_3country <- rbind(tone_location_3country, tone_locationorg)
  }
  rm(tone_locationorg)
  rm(tone_locationcntry)
}

# Clean up the environment by saving tone_location and removing unnecessary variables
require(readr)
write_delim(tone_location, "tone_location_all_countries.txt", delim = "\t")
rm(tone_location)

# Check for normality visually
# While not perfect, boxplots indicate data is for the most part normally distributed

boxplot(tone_location_3country$flat_tone_avtone
        ~ tone_location_3country$source, 
        main = "Average Tone across Sources")

boxplot(tone_location_3country$flat_tone_positive
        ~ tone_location_3country$source,
        main = "Positive Tone across Sources")

boxplot(tone_location_3country$flat_tone_negative
        ~ tone_location_3country$source,
        main = "Negative Tone across Sources")

# Descriptive statistics confirm the distributions are all fairly normal
summary(tone_location_3country$flat_tone_avtone)
summary(tone_location_3country$flat_tone_positive)
summary(tone_location_3country$flat_tone_negative)

# variance is fairly uniform across the countries
aggregate(tone_location_3country$flat_tone_avtone
          ~ tone_location_3country$flat_location_countrycode,
          FUN = var)

aggregate(tone_location_3country$flat_tone_positive
          ~ tone_location_3country$flat_location_countrycode,
          FUN = var)

aggregate(tone_location_3country$flat_tone_negative
          ~ tone_location_3country$flat_location_countrycode,
          FUN = var)

#relevel the country factor to make EG the reference, this will allow us to see the other countries, which use EG as a baseline
tone_location_3country$flat_location_countrycode <- relevel(tone_location_3country$flat_location_countrycode, ref = "EG")

# call broom package to make the ouput easy to read

install.packages("broom")
require(broom)

# try the model with just source as a predictor
model <- lm(flat_tone_avtone ~ source, data = tone_location_3country)
modelpos <- lm(flat_tone_positive ~ source, data = tone_location_3country)
modelneg <- lm(flat_tone_negative ~ source, data = tone_location_3country)

 # print key statistics of model
glance(model)
glance(modelpos)
glance(modelneg)
 
#try the model with just country
model <- lm(flat_tone_avtone ~ flat_location_countrycode,
                            data = tone_location_3country
        )

modelpos <- lm(flat_tone_positive ~ flat_location_countrycode,
            data = tone_location_3country
)

modelneg <- lm(flat_tone_negative ~ flat_location_countrycode,
            data = tone_location_3country
)

# printing key statistics
glance(model)
glance(modelpos)
glance(modelneg)

# model with source by country
model <- lm(flat_tone_avtone ~ source
            *flat_location_countrycode, 
            data = tone_location_3country
            )

glance(model)
summary(model)

modelpos <- lm(flat_tone_positive ~ source
            *flat_location_countrycode, 
            data = tone_location_3country
)

glance(modelpos)
summary(modelpos)

modelneg <- lm(flat_tone_negative ~ source
               *flat_location_countrycode, 
               data = tone_location_3country
)

glance(modelneg)
summary(modelneg)

# The model's r squared value shows ~ 12-15% of the variation in average tone is explained by source and location interaction. 

# Overall, the hypothesized predictor factors explain only a small amount of variation in tone. 

# Let's look at the fits. Residuals appear random - suggesting a good fit.
plot(residuals(model), main = "Residuals: Average Tone Predicted \n by Source and Country Code")
plot(residuals(modelpos), main = "Residuals: Positive Tone Predicted \n by Source and Country Code")
plot(residuals(modelneg), main = "Residuals: Negative Tone Predicted \n by Source and Country Code")


#If the hypothesized factors are not predicting a substantial amount of the 
#dependent variables let's try incorporating information on 
#specific events from the counts data.

# Clean up the environment and save the dataset
write_delim(tone_location_3country, "tone_location_3country.txt", delim = "\t")
rm(tone_location_3country)
rm(modelneg)
rm(modelpos)
rm(model)

require(stringr)

# import all the counts data
counts_all <- data.frame()
for (i in 2:7) {
  require(stringr)
  counts_working <- data.frame()
  counts_working <- read.delim(str_c(organizations[i], "counts_data_flat.txt"))
  counts_working[,12] <- organizations[i]
  counts_all <- rbind(counts_all, counts_working)
  rm(counts_working)
}

# Merge the dataset with the counts data

tone_location_3country <- read.delim("tone_location_3country.txt")

tone_location_counts_3country <- merge(tone_location_3country, counts_all[,c(1:3, 7)], by = c("gkgrecordid"), all.x = TRUE)

rm(counts_all)
rm(tone_location_3country)

#clear out any missing values
tone_location_counts_3country <- na.omit(tone_location_counts_3country)

#save the data before trimming to include just the variables needed
write_delim(tone_location_counts_3country, "tone_location_counts_3country.txt", delim = "\t")
t_l_c_3country <- tone_location_counts_3country[,c(2:7,10:12)]
rm(tone_location_counts_3country)

# Install biglm and call the package - it will be used later for the larger models

install.packages("biglm")
require(biglm)



# Try a model with just count type
model <- lm(t_l_c_3country$flat_tone_avtone ~ 
             t_l_c_3country$flat_count_counttype
)

modelpos <- lm(t_l_c_3country$flat_tone_positive ~ 
                 t_l_c_3country$flat_count_counttype
)

modelneg <- lm(t_l_c_3country$flat_tone_negative ~ 
                 t_l_c_3country$flat_count_counttype
)


# print summary statistics
glance(model)
glance(modelpos)
glance(modelneg)


# Try a model with location by count type
model <- lm(t_l_c_3country$flat_tone_avtone ~ 
              t_l_c_3country$flat_location_countrycode*
              t_l_c_3country$flat_count_counttype
)

modelpos <- lm(t_l_c_3country$flat_tone_positive ~ 
                 t_l_c_3country$flat_location_countrycode*
                 t_l_c_3country$flat_count_counttype
)

modelneg <- lm(t_l_c_3country$flat_tone_negative ~ 
              t_l_c_3country$flat_location_countrycode*
              t_l_c_3country$flat_count_counttype
)


# print summary statistics
glance(model)
glance(modelpos)
glance(modelneg)


# Try a model with source by location and count type

model <- lm(t_l_c_3country$flat_tone_avtone ~ 
             t_l_c_3country$source*
             t_l_c_3country$flat_location_countrycode+
             t_l_c_3country$flat_count_counttype
            )

glance(model)

rm(model)

modelpos <- lm(t_l_c_3country$flat_tone_positive ~ 
              t_l_c_3country$source*
              t_l_c_3country$flat_location_countrycode+
              t_l_c_3country$flat_count_counttype
)

glance(modelpos)
rm(modelpos)

modelneg <- lm(t_l_c_3country$flat_tone_negative ~ 
              t_l_c_3country$source*
              t_l_c_3country$flat_location_countrycode+
              t_l_c_3country$flat_count_counttype
)

glance(modelneg)
rm(modelneg)

# This model provides a substantially higher r squared value, ~ .27

# If we repeat the same model, but with count values as a function of count type
# we obtain a lower r-squared value, suggesting count value is not a valuable predictor.
model <- lm(t_l_c_3country$flat_tone_avtone ~ 
              t_l_c_3country$source*
              t_l_c_3country$flat_location_countrycode+
              t_l_c_3country$flat_count_counttype*
              t_l_c_3country$flat_count_count
            )
glance(model)
rm(model)

modelpos <- lm(t_l_c_3country$flat_tone_positive ~ 
              t_l_c_3country$source*
              t_l_c_3country$flat_location_countrycode+
              t_l_c_3country$flat_count_counttype*
              t_l_c_3country$flat_count_count
)
glance(modelpos)
rm(modelpos)

modelneg <- lm(t_l_c_3country$flat_tone_negative ~ 
                 t_l_c_3country$source*
                 t_l_c_3country$flat_location_countrycode+
                 t_l_c_3country$flat_count_counttype*
                 t_l_c_3country$flat_count_count
)
glance(modelneg)
rm(modelneg)


# A linear model with source and count type and location
model <- lm(flat_tone_avtone ~ 
             source+
             flat_count_counttype+
             flat_location_countrycode, data = t_l_c_3country
            )

modelpos <- lm(flat_tone_positive ~ 
              source+
              flat_count_counttype+
              flat_location_countrycode, data = t_l_c_3country
)

modelneg <- lm(flat_tone_negative ~ 
              source+
              flat_count_counttype+
              flat_location_countrycode, data = t_l_c_3country
)
# This model is as strong as source by location and count type
# suggesting there is no notable interaction between source, country code, and count type
glance(model)
rm(model)
glance(modelneg)
rm(modelneg)
glance(modelpos)
rm(modelpos)

# Let's try removing location data in model, and focusing on source by count type 
model <- lm(t_l_c_3country$flat_tone_avtone ~ 
             t_l_c_3country$source*
             t_l_c_3country$flat_count_counttype
            )

glance(model)
rm(model)

modelpos <- lm(t_l_c_3country$flat_tone_positive ~ 
              t_l_c_3country$source*
              t_l_c_3country$flat_count_counttype
)

glance(modelpos)
rm(modelpos)

modelneg <- lm(t_l_c_3country$flat_tone_negative ~ 
              t_l_c_3country$source*
              t_l_c_3country$flat_count_counttype
)

glance(modelneg)
rm(modelneg)

# This model is as strong or stronger than our previous best model, without using location data. 
glance(model)

# This suggests that source and count type are the principal contributors to average tone. 
# It also suggests that location is not a notable predictor for tone. 

# Another possibility is source interacting with count type, which is in turn interacting with location.
# This model is too large for the regular R environment to handle.
# We need to use the biglm package and process the data in smaller pieces.

# split out the data set
half <- trunc(nrow(t_l_c_3country) / 2)
write_delim(t_l_c_3country, "tlc3country.txt", delim = "\t")
write_delim(t_l_c_3country[1:half,], "tlc3country1.txt", delim = "\t")
half <- half+1
full <- nrow(t_l_c_3country)
write_delim(t_l_c_3country[half:full,], "tlc3country2.txt", delim = "\t")
rm(half)
rm(full)
rm(t_l_c_3country)

bigmodel <- biglm(flat_tone_avtone ~ 
                    source*
                    flat_count_counttype*
                    flat_location_countrycode, data = read.delim("tlc3country1.txt")
                  )

bigmodel <- update(bigmodel, moredata = read.delim("tlc3country2.txt"))

# This model does not provide any notable gain in r squared value over our previous best model.
glance(bigmodel)

# calculate the p-value manually

r <- sqrt(as.numeric(glance(bigmodel)[1]))
n <- as.numeric(bigmodel["n"])
t <- (r * (sqrt(n-2)))/ (sqrt(1-(r*r)))
2*pt(q = -abs(t), df = n-1) 
rm(r)
rm(n)
rm(t)
rm(bigmodel)
# p-value is very small


bigmodel <- biglm(flat_tone_positive ~ 
                    source*
                    flat_count_counttype*
                    flat_location_countrycode, data = read.delim("tlc3country1.txt")
)

bigmodel <- update(bigmodel, moredata = read.delim("tlc3country2.txt"))

# no substantial gains
glance(bigmodel)

# calculate the p-value manually

r <- sqrt(as.numeric(glance(bigmodel)[1]))
n <- as.numeric(bigmodel["n"])
rm(bigmodel)
t <- (r * (sqrt(n-2)))/ (sqrt(1-(r*r)))
2*pt(q = -abs(t), df = n-1) 
rm(r)
rm(n)
rm(t)


bigmodel <- biglm(flat_tone_negative ~ 
                    source*
                    flat_count_counttype*
                    flat_location_countrycode, data = read.delim("tlc3country1.txt")
)

bigmodel <- update(bigmodel, moredata = read.delim("tlc3country2.txt"))

# again, no substantial gains
glance(bigmodel)

# calculate the p-value manually

r <- sqrt(as.numeric(glance(bigmodel)[1]))
n <- as.numeric(bigmodel["n"])
rm(bigmodel)
t <- (r * (sqrt(n-2)))/ (sqrt(1-(r*r)))
2*pt(q = -abs(t), df = n-1) 
rm(r)
rm(n)
rm(t)


# It appears the best model is source by count type (average tone ~ source*count type)
