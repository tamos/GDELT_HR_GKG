
# Analysis October 31

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
table(tone_location$source[tone_location$flat_location_countrycode == "EG"])

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

boxplot(tone_location_3country$flat_tone_avtone ~ tone_location_3country$source)

# descriptive statistics tell us the distributions are all fairly similar

aggregate(tone_location_3country$flat_tone_avtone ~ tone_location_3country$flat_location_countrycode, FUN = summary)

# variance is fairly uniform across the countries
aggregate(tone_location_3country$flat_tone_avtone ~ tone_location_3country$flat_location_countrycode, FUN = var)

# given the above results, we can conclude the data is sufficiently normal

#relevel the country factor to make EG the reference, this will allow us to see the other countries, which use EG as a baseline
tone_location_3country$flat_location_countrycode <- relevel(tone_location_3country$flat_location_countrycode, ref = "EG")

# call broom package to make the ouput easy to read

install.packages("broom")
require(broom)

#try the model with just source as a predictor
model <- lm(flat_tone_avtone ~ source, data = tone_location_3country)

# print key statistics of model
glance(model)

#trying the model with just country gives smaller r squared
model <- lm(flat_tone_avtone ~ flat_location_countrycode,
                              data = tone_location_3country
            )

glance(model)

# model with source by country

model <- lm(flat_tone_avtone ~ source
            :flat_location_countrycode, 
            data = tone_location_3country
            )

glance(model)

# The model's r squared value shows ~ 16% of the variation in average tone is explained by source and location interaction. This is the highest figure yet, so we will save the coefficients and keep this combination of factors

write_delim(tidy(model), "avtone_sourcebycountry.txt", delim = "\t")

# Overall, the hypothesized predictor factors explain only a small amount of variation in tone. If we recall that tone was determined by simple counts of positive and negative words, ~ 16% variation is relatively minor. It would result in, for example, a difference of 160 words in a 1,000 word text.

# Let's look at the fit. Residuals appear random for the average tone model.
plot(residuals(model))



# try the model with the positive score, r squared is ~ .13
model_pos <- lm(flat_tone_positive ~ source:
                  flat_location_countrycode, 
                data = tone_location_3country
                )

glance(model_pos)

# try the model with negative score, r squared is ~ .12
model_neg <- lm(flat_tone_negative ~ source
                :flat_location_countrycode,
                data = tone_location_3country
                )

glance(model_neg)


# Residuals for the positive tone model are random
plot(residuals(model_pos))

# Residuals for the negative tone model are random
plot(residuals(model_neg))

# If the hypothesized factors are not predicting a substantial amount of the dependent variables let's try incorporating information on the specific events from the counts data.

# Clean up the environment and save the dataset
write_delim(tone_location_3country, "tone_location_3country.txt", delim = "\t")
rm(tone_location_3country)
rm(model_neg)
rm(model_pos)
rm(model)

# import all the counts data
counts_all <- data.frame()
for (i in 2:7) {
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

# Try a model with location by count type
model <- lm(t_l_c_3country$flat_tone_avtone ~ 
              t_l_c_3country$flat_location_countrycode:
              t_l_c_3country$flat_count_counttype
)
# This model has a low r squared ~ .02
glance(model)


# Try a linear model with count type by location and source

model <- lm(t_l_c_3country$flat_tone_avtone ~ 
             t_l_c_3country$source:
             t_l_c_3country$flat_location_countrycode+
             t_l_c_3country$flat_count_counttype
            )
# This model provides a substantially higher r squared value, ~ .27
glance(model)

# This is a strong model, so we will save these results
write_delim(tidy(model)), "avtone_sourcebylocationandcounttype.txt", delim = "\t") 

# If we repeat the same model, but with count values as a function of count type, we obtain a lower r squared value, suggesting count value is not a valuable predictor.
model <- lm(t_l_c_3country$flat_tone_avtone ~ 
              t_l_c_3country$source:
              t_l_c_3country$flat_location_countrycode+
              t_l_c_3country$flat_count_counttype:
              t_l_c_3country$flat_count_count
            )

glance(model)

# A linear model with source and count type and location
model <- lm(flat_tone_avtone ~ 
             source+
             flat_count_counttype+
             flat_location_countrycode, data = t_l_c_3country
            )
# This model is as strong as source by location and count type, suggesting there is no notable interaction between source and country code
glance(model)

# These results are notable, so let's save them
write_delim(tidy(model)), "avtone_sourceandcounttypeandlocation.txt", delim = "\t")

# Let's try removing location data in model, and focusing on source by count type 
model <- lm(t_l_c_3country$flat_tone_avtone ~ 
             t_l_c_3country$source:
             t_l_c_3country$flat_count_counttype
            )
# This model is as strong or stronger than our previous best model, without using location data. 
glance(model)

# This suggests that source and count type are the principal contributors to average tone. 
# It also suggests that location is not a notable predictor for average tone. 

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
                    source:
                    flat_count_counttype:
                    flat_location_countrycode, data = read.delim("tlc3country1.txt")
                  )

bigmodel <- update(bigmodel, moredata = read.delim("tlc3country2.txt"))

# This model does not provide any notable gain in r squared value over our previous best model.
glance(bigmodel)

# Try source and count type by count value, and location
rm(bigmodel)

bigmodel <- biglm(flat_tone_avtone ~ 
                    source+
                    flat_count_counttype*
                    flat_count_count+
                    flat_location_countrycode, data = read.delim("tlc3country1.txt")
                )

bigmodel <- update(bigmodel, moredata = read.delim("tlc3country2.txt"))

# This model gives an r squared of approximately ~.24, not an improvement over our previous best model.
glance(bigmodel)

# It appears the best model for average tone is source by count type (average tone ~ source:count type)

# Let's repeat the process with positive and negative values, as well as polarity, applying our best model.

# Load the required data

t_l_c_3country <- read.delim("tlc3country.txt")

# Recall that average tone is an aggregate measure of positive and negative tone.
# Using our best model so far for the aggregate, let's try positive, predicted with source by count type.

model <- lm(flat_tone_positive ~ 
              source:
              flat_count_counttype
            ,data = t_l_c_3country
          )

# Positive tone as predicted by source by count type has an r squared ~ .19
glance(model)

model <- lm(flat_tone_positive ~ 
              source:
              flat_location_countrycode+
              flat_count_counttype
            ,data = t_l_c_3country
          )
# Once again, incorporating location data does not result in stronger r squared values.
glance(model)

# negative as predicted by source by count type

model <- lm(flat_tone_negative ~ 
             source:
             flat_count_counttype
           ,data = t_l_c_3country
          )
# the r squared is ~ .3, the strongest model yet
glance(model)

# negative as predicted by source and count type, adding country
model <- lm(flat_tone_negative ~ 
             source+
             flat_count_locationcountrycode +
             flat_count_counttype
           , data = t_l_c_3country
        ) 
# as with the other models, the addition of location does not substantially affect the model
glance(model)

# polarity as predicted by source and count type

model <- lm(flat_tone_polarity ~ 
             source:
             flat_count_counttype
           ,data = t_l_c_3country
        )
# The r squared is ~ .29
glance(model)

# polarity as predicted by source and location
model <- lm(flat_tone_polarity ~ 
             source*
             flat_location_countrycode
           ,data = t_l_c_3country
          )
# with location r squared is ~ .27
glance(model)

# polarity as predicted by just source

model <- lm(flat_tone_polarity ~ 
             source
           ,data = t_l_c_3country
           )
# the model is not substantially different from the model including location
glance(model)


# - CONTROLLING FOR DATES

# before drawing conclusions, it is important to control for dates. If date references are equally distributed across
# organizations, we can assume there are no major time-related imbalances in the samples

dates_all <- data.frame()
require(stringr)
for (i in 2:7) {
  dates_working <- data.frame()
  dates_working <- read.delim(str_c(organizations[i], "enhanced_dates_flat.txt"))
  dates_working[,7] <- organizations[i]
  dates_all <- rbind(dates_all, dates_working)
  #rm(dates_working)
}
colnames(dates_all)[7] <- "source"

# date mentions have approximately the same means and medians across all organizations
aggregate(dates_all$flat_enhanceddate_year ~ as.factor(dates_all$V7), FUN = median)
plot(aggregate(dates_all$flat_enhanceddate_year ~ as.factor(dates_all$V7), FUN = mean), ) 

rm(dates_all)

