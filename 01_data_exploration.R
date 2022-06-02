## This is the file where I am working on all my data stuff for the project
## Forgive me, it's messy
## Author: Greg York
## Date Started: 6/2/2022

## Setting up first plot ---------------------
target <- "https://www.bls.gov/cps/cpsaat01.xlsx"
dir.create("bls_project")

destfile <- 
        "C:/Users/The White Stripes/Documents/R/bls_project/01_employmentstatus_2021.xlsx"
download.file(target, destfile)

# the file needed to be edited, the header was messed up
file <- "bls_project/1950-2021_general_employment_status_edited.xlsx"

employment_df <- read_excel(file)

## First Plot Code ------------------
employment_df <- employment_df[1:71,]

View(employment_df)

ggplot(data = employment_df) +
        geom_point(mapping = aes(x = Year, y = civilian_population_total))

ggplot(data = employment_df) +
        geom_point(mapping = aes(x = Year, y = civilian_population_total),
                   color = "yellow")+
        geom_point(mapping = aes(x = Year, y = labor_force_total), 
                   color = "blue")

ggplot(data = employment_df) +
        geom_col(mapping = aes(x = Year, y = labor_force_percent_of_population),
                 fill = Year)
employment_df %>% 
        filter(labor_force_percent_of_population == 
                       min(employment_df$labor_force_percent_of_population))

employment_df %>% 
        filter(labor_force_percent_of_population == 
                       max(employment_df$labor_force_percent_of_population))

employment_df[71,]


p <- ggplot(data = employment_df, 
       mapping = aes(x = Year, y = labor_force_percent_of_population, 
                     angle = 25)) +
        geom_line(aes(group=1), color="blue") +
        geom_point(size = 3, color= "red") +
        theme(axis.text.x = element_text(angle = 45)) +
        annotate("text", x=13,y=58.4, label="Lowest: 1963-1964") +
        annotate("text", x=48, y=67.4, label = "Highest: 1997-2000") +
        annotate("text", x=70, y=61.5, label = "2021")

p +
        geom_line(aes(x = Year, y = employed_percent_of_population, group=1),
                  color = "purple")

employment_df$Year <- as.integer(employment_df$Year)

# this is the plot I ended up using:
# I foolishly didn't change the year from chr to int or num,
# so I have to do that first
employment_df$Year <- as.integer(employment_df$Year)

ggplot(data = employment_df, aes(x = Year)) +
        geom_line(aes(y = labor_force_percent_of_population,
                      color = 'labor_force_percent_of_population',
                      group = 1), lwd = 2) +
        geom_line(aes(y = employed_percent_of_population,
                      color = 'employed_percent_of_population',
                      group = 1), lwd = 2) +
        geom_vline(xintercept = 2000, color = "purple", linetype = "longdash") +
        labs(y = "Percent of Population") +
        scale_color_discrete(name = "Historical Employment 1951-2021",
                labels=c("Employed", "In Labor Force")) +
        geom_text(mapping=aes(x=2000, y=67.4, label = "Year 2000")) +
        theme(axis.text.x = element_text(angle = 45),
              legend.position = c(.1,.9))

## second plot set-up ---------------
target <- "https://www.bls.gov/cps/cpsaat02.xlsx"


destfile <- 
        "C:/Users/The White Stripes/Documents/R/bls_project/02_employmentstatus_2021.xlsx"
download.file(target, destfile)

# the file needed to be edited, the header was messed up
# I have to work on downloading these files, they download corrupted
file <- 
        "bls_project/1981-2021_general_employment_status_sex_edited.xlsx"

employment_sex_df <- read_excel(file)

summary(employment_sex_df)
tail(employment_sex_df)

employment_sex_df <- employment_sex_df[1:82,]
ggplot(data = employment_sex_df, aes(x = Year)) +
        geom_line(aes(y = labor_force_percent_of_population,
                      color = Sex), lwd = 2) +
        geom_vline(xintercept = 2000, color = "purple", 
                   linetype = "longdash") +
        theme(axis.text.x = element_text(angle = 45),
              legend.position = c(.9,.9)) +
        labs(y = "Percent of Population") +
        scale_color_discrete(name = "Historical Employment 1981-2021 by Sex",
                             breaks=c('M','F'),
                             labels=c("Male", "Female"))

## third plot --------------------




