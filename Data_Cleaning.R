library(readr)
library(tidyverse)
library(dplyr)

df <- read_csv("2021 Western Carolina University Student Needs Survey_August 10, 2023_17.45.csv")


#Edit Rows
df <- df %>%
  slice(3 : n()) %>%
  setNames(as.character(df[1,]))


names(df) <- make.unique(names(df))


#Remove Identifiers
df <- df %>%
  select(-c(1:11))

colnames(df)[1] <- "Payment_Ways"

#Create new features based on Payment Ways
df$Work_Study <- ifelse(grepl("work-study", df$Payment_Ways, ignore.case = TRUE), 1, 0)
df$Student_Loans <- ifelse(grepl("student loans", df$Payment_Ways, ignore.case = TRUE), 1, 0)
df$Savings <- ifelse(grepl("savings", df$Payment_Ways, ignore.case = TRUE), 0, 1)
df$Pell_Grant <- ifelse(grepl("Pell Grant", df$Payment_Ways, ignore.case = TRUE), 1, 0)
df$Federal_Grants <- ifelse(grepl("grants from the federal or state government", df$Payment_Ways, ignore.case = TRUE), 1, 0)
df$Family_Support <- ifelse(grepl("I get help from family or friends", df$Payment_Ways, ignore.case = TRUE), 0, 1)
df$Credit_Cards <- ifelse(grepl("credit card", df$Payment_Ways, ignore.case = TRUE), 1, 0)

# Extracting potentially relevant features into cleaned_data
cleaned_data <- data.frame(row.names = seq_len(nrow(df)))

cleaned_data$Searching_Work <- df$`In the past 30 days have you been looking for work?`
cleaned_data$Have_Job <- df$`LAST WEEK, did you have a job where you worked for pay or profit? Include a job even if you were temporarily absent from it last week.`
cleaned_data$Number_Jobs <- df$`LAST WEEK, not including Federal Work-Study, how many jobs did you have?`
cleaned_data$Hours_Work <- df$`Thinking back to the last full week that began on a Monday and ended on a Sunday, for about how m... - Hours - Working for pay -`
cleaned_data$Commuting_Hours <- df$`Thinking back to the last full week that began on a Monday and ended on a Sunday, for about how m... - Hours - Commuting to or from work or school -`
cleaned_data$Sleeping <- df$`Thinking back to the last full week that began on a Monday and ended on a Sunday, for about how m... - Hours - Sleeping -`
cleaned_data$Leisure_Activites <- df$`Thinking back to the last full week that began on a Monday and ended on a Sunday, for about how m... - Hours - Leisure activities (for example, spending time with friends, watching TV or... - Leisure activities (for example, spending time with friends, watching TV or movies, using the internet for leisure, talking or texting on the phone) -`

cleaned_data$Scale_Food_Insecurity <- df$`On a scale from 1-10, with 1 being strongly disagree and 10 being strongly agree, please rate your agreement with the following statement:
In the last 12 months, there were times when I was hungry but didn't eat because there wasn't enough money for food`

cleaned_data$ClassWork_Hours <- df$`Thinking back to the last full week that began on a Monday and ended on a Sunday, for about how m... - Hours - Attending college classes, labs, or discussion sections either in person or... - Attending college classes, labs, or discussion sections either in person or online -`
cleaned_data$Studying_Hours <- df$`Thinking back to the last full week that began on a Monday and ended on a Sunday, for about how m... - Hours - Preparing for class by yourself or with others by studying, reading, writin... - Preparing for class by yourself or with others by studying, reading, writing, rehearsing, or doing other academic activities -`
cleaned_data$Qualify_Reduced_Lunch <- df$`In the past 12 months, has anyone in your family under age 18 received free or reduced price breakfast or lunch at school?`
cleaned_data$Worry_Food_Running_Out <- df$`Thinking about the last 12 months, how true would you say the following statements are? - I worried whether my food would run out before I got money to buy more`
cleaned_data$Afford_Balanced_Meals <- df$`Thinking about the last 12 months, how true would you say the following statements are? - I couldn’t afford to eat balanced meals`
cleaned_data$Cut_Size_Or_Skip_Meals <- df$`In the last 12 months, did you ever cut the size of your meals or skip meals because there wasn’t enough money for food?`
cleaned_data$How_Often_Skip_Meals <- df$`How often did this happen?`
cleaned_data$Food_Pantry_On_Campus <- df$`Is there a food pantry on campus?`
cleaned_data$On_Campus_Residence_Halls <- df$`Does your college have on-campus residence halls?`
cleaned_data$No_Place_Winter_Spring_Breaks <- df$`In the last 12 months, have you ever not known where you would stay during winter/spring breaks because the on-campus residence halls were closed?`
cleaned_data$Stayed_Others_Room <- df$`In the past 12 months, were there times when you stayed in someone else’s room in an on-campus residence hall because you didn’t have anywhere else to sleep?`
cleaned_data$Left_Staying_Admin_Rules <- df$`In the past 12 months, were there times you stayed in someone else’s room in an on-campus residence hall but had to leave because of administration rules?`
cleaned_data$Share_Residence <- df$`Do you share your residence with other people?`
cleaned_data$Rent_Mortgage_Hold <- df$`In the past 12 months, was there a rent or
mortgage increase that made it difficult to pay?`
cleaned_data$Home_Public_Housing <- df$`Is your home in a public housing project, owned by a local housing authority or other public agency?`
cleaned_data$Public_Housing_Voucher <- df$`Do you receive a public housing voucher, such as Section 8, to subsidize the cost of private housing?`
cleaned_data$Monthly_Income_Rent_Mortgage <- df$`Approximately how much of your total household monthly income do you spend on rent or mortgage?`
cleaned_data$Times_Moved <- df$`In the past 12 months, how many times have you moved?`
cleaned_data$Parent1_Education <- df$`What is the highest level of education completed by Parent 1?`
cleaned_data$Safety_Feeling <- df$`How safe do you feel where you currently live?`
cleaned_data$Left_Household_Unsafe <- df$`In the past 12 months, did you leave your household because you felt unsafe?`
cleaned_data$Thrown_Out_By_Household <- df$`In the past 12 months, were you thrown out of your home by someone else in the household?`
cleaned_data$Parent2_Education <- df$`What is the highest level of education completed by Parent 2?`
cleaned_data$Race_Ethnicity <- df$`How do you usually describe your race and/or ethnicity? (Select all that apply) - Selected Choice`
cleaned_data$US_Citizen_Resident <- df$`Are you a U.S. citizen or permanent resident?`
cleaned_data$Have_Children <- df$`Do you have any biological, adopted, step or foster children?`
cleaned_data$Been_In_Foster_Care <- df$`Have you ever been in foster care?`
cleaned_data$Age <- df$Age
cleaned_data$Gender <- df$Gender
cleaned_data$GPA <- df$GPA
cleaned_data$Student_Status <- df$student_full_part_time
cleaned_data$Work_Study <- df$Work_Study
cleaned_data$Student_Loans <- df$Student_Loans
cleaned_data$Savings <- df$Savings
cleaned_data$Pell_Grant <- df$Pell_Grant
cleaned_data$Federal_Grants <- df$Federal_Grants
cleaned_data$Family_Support <- df$Family_Support
cleaned_data$Credit_Cards <- df$Credit_Cards
cleaned_data$Sexual_Orientation <- df$`Do you consider yourself to be:`



colnames(cleaned_data)

#Turn Characters into Valid Format for Regression Analysis

# Convert "Yes"/"No" responses to 1/0 for Searching_Work, leaving NA as is
cleaned_data$Searching_Work <- ifelse(is.na(cleaned_data$Searching_Work), NA, ifelse(grepl("Yes", cleaned_data$Searching_Work, ignore.case = TRUE), 1, 0))

# Convert "Yes"/"No" responses to 1/0 for Have_Job, leaving NA as is
cleaned_data$Have_Job <- ifelse(is.na(cleaned_data$Have_Job), NA, ifelse(grepl("Yes", cleaned_data$Have_Job, ignore.case = TRUE), 1, 0))

# Remove non-numeric characters from Scale_Food_Insecurity, leaving NA as is
str(cleaned_data$Scale_Food_Insecurity)
cleaned_data$Scale_Food_Insecurity <- gsub("[^0-9]", "", cleaned_data$Scale_Food_Insecurity)


# Convert "Yes"/"No" responses to 1/0 for Qualify_Reduced_Lunch, leaving NA as is
cleaned_data$Qualify_Reduced_Lunch <- ifelse(is.na(cleaned_data$Qualify_Reduced_Lunch), NA, ifelse(grepl("Yes", cleaned_data$Qualify_Reduced_Lunch, ignore.case = TRUE), 1, 0))

# Convert "Never"/other responses to 0/1 for Worry_Food_Running_Out, leaving NA as is
cleaned_data$Worry_Food_Running_Out <- ifelse(is.na(cleaned_data$Worry_Food_Running_Out), NA, ifelse(grepl("Never", cleaned_data$Worry_Food_Running_Out, ignore.case = TRUE), 0, 1))

# Convert "Never"/other responses to 1/0 for Afford_Balanced_Meals, leaving NA as is
cleaned_data$Afford_Balanced_Meals <- ifelse(is.na(cleaned_data$Afford_Balanced_Meals), NA, ifelse(grepl("Never", cleaned_data$Afford_Balanced_Meals, ignore.case = TRUE), 0, 1))


# Convert "Yes"/"No" responses to 1/0 for Share_Residence, leaving NA as is
cleaned_data$Share_Residence <- ifelse(is.na(cleaned_data$Share_Residence), NA, ifelse(grepl("Yes", cleaned_data$Share_Residence, ignore.case = TRUE), 1, 0))

# Convert "Yes"/"No" responses to 1/0 for Home_Public_Housing, leaving NA as is
cleaned_data$Home_Public_Housing <- ifelse(is.na(cleaned_data$Home_Public_Housing), NA, ifelse(grepl("Yes", cleaned_data$Home_Public_Housing, ignore.case = TRUE), 1, 0))

# Convert "More"/other responses to 1/0 for Monthly_Income_Rent_Mortgage, leaving NA as is
cleaned_data$Monthly_Income_Rent_Mortgage <- ifelse(is.na(cleaned_data$Monthly_Income_Rent_Mortgage), NA, ifelse(grepl("More", cleaned_data$Monthly_Income_Rent_Mortgage, ignore.case = TRUE), 1, 0))


cleaned_data$Rent_Mortgage_Hold <- ifelse(is.na(cleaned_data$Rent_Mortgage_Hold), NA, ifelse(grepl("Yes", cleaned_data$Rent_Mortgage_Hold, ignore.case = TRUE), 1, 0))

# Modify Parent1_Education and convert to 1/0, leaving NA as is
cleaned_data$Parent1_Education <- ifelse(is.na(cleaned_data$Parent1_Education), NA, ifelse(grepl("Some college (but no college degree)", cleaned_data$Parent1_Education, ignore.case = TRUE), 0, cleaned_data$Parent1_Education))
cleaned_data$Parent1_Education <- ifelse(is.na(cleaned_data$Parent1_Education), NA, ifelse(grepl("degree", cleaned_data$Parent1_Education, ignore.case = TRUE), 0, 1))

cleaned_data$Parent2_Education <- ifelse(is.na(cleaned_data$Parent2_Education), NA, ifelse(grepl("Some college (but no college degree)", cleaned_data$Parent2_Education, ignore.case = TRUE), 0, cleaned_data$Parent2_Education))
cleaned_data$Parent2_Education <- ifelse(is.na(cleaned_data$Parent2_Education), NA, ifelse(grepl("degree", cleaned_data$Parent2_Education, ignore.case = TRUE), 0, 1))

cleaned_data$Parents_Education <- ifelse(cleaned_data$Parent1_Education == 1 | cleaned_data$Parent2_Education == 1, 1, 0)

# Modify Safety_Feeling and convert to 1/0, leaving NA as is
cleaned_data$Safety_Feeling <- ifelse(is.na(cleaned_data$Safety_Feeling), NA, ifelse(grepl("extremely", cleaned_data$Safety_Feeling, ignore.case = TRUE), 0, cleaned_data$Safety_Feeling))
cleaned_data$Safety_Feeling <- ifelse(is.na(cleaned_data$Safety_Feeling), NA, ifelse(grepl("very", cleaned_data$Safety_Feeling, ignore.case = TRUE), 0, cleaned_data$Safety_Feeling))
cleaned_data$Safety_Feeling <- ifelse(is.na(cleaned_data$Safety_Feeling), NA, ifelse(grepl("0", cleaned_data$Safety_Feeling, ignore.case = TRUE), 0, 1))



cleaned_data$Left_Household_Unsafe <- ifelse(is.na(cleaned_data$Left_Household_Unsafe), NA, ifelse(grepl("yes", cleaned_data$Left_Household_Unsafe, ignore.case = TRUE), 1, 0))
cleaned_data$Thrown_Out_By_Household <- ifelse(is.na(cleaned_data$Thrown_Out_By_Household), NA, ifelse(grepl("yes", cleaned_data$Thrown_Out_By_Household, ignore.case = TRUE), 1, 0))


cleaned_data$Have_Children <- ifelse(is.na(cleaned_data$Have_Children), NA, ifelse(grepl("yes", cleaned_data$Have_Children, ignore.case = TRUE), 1,0))


cleaned_data$Gender <- ifelse(is.na(cleaned_data$Gender), NA, ifelse(grepl("F", cleaned_data$Gender, ignore.case = TRUE), 0,1))

cleaned_data$Student_Status <- ifelse(is.na(cleaned_data$Student_Status), NA, ifelse(grepl("F", cleaned_data$Student_Status, ignore.case = TRUE), 0,1))

cleaned_data$Sexual_Orientation <- ifelse(is.na(cleaned_data$Sexual_Orientation), NA, ifelse(grepl("straight", cleaned_data$Sexual_Orientation, ignore.case = TRUE), 0,1))



cleaned_data <- cleaned_data %>%
  select(-c(Parent1_Education, Parent2_Education ,How_Often_Skip_Meals, Food_Pantry_On_Campus, Stayed_Others_Room, Public_Housing_Voucher, US_Citizen_Resident, Been_In_Foster_Care))

for (col in names(cleaned_data)) {
  cleaned_data[[col]] <- as.numeric(cleaned_data[[col]])
}

