


#import the DataSet and the  libraries 
install.packages("stringr")
install.packages("ggpolt2")
install.packages("dplyr")
install.packages("lubridate")
install.packages("patchwork")
install.packages("gganimate")
library(patchwork)
library(lubridate)
library(gganimate)
library(dplyr)
library(ggplot2) 
library(stringr) 

Data = read.csv("C:\\Users\\User\\OneDrive - Asia Pacific University\\Desktop\\coures\\year Two\\Semester one\\Programming For Data Analysis\\Assignment\\employee_attrition.csv", header = TRUE)

#=================================================================
#=================================================================
#=================================================================

#Cleaing Data
#view display the  data set 


View(Data)

#check  the  duplication  
sum( duplicated(data) == TRUE) 

#remove the duplication 
Data <- Data %>% group_by(EmployeeID)%>% summarise_all(list(~ last(.)))

#remover ensued columns 
Data <- select(Data,  -c("recorddate_key", "gender_short", "termtype_desc"))


# fixing a misspelling 
Data$termReason <- gsub("Resignaton", "Resignation", Data$termreason_desc)

#==================================================================================================================


#Data pre-prossing 

#Rename the  Columns
names(Data) <-  c("ID", "birthDate","hireDate","termDate", "age","serviceLength","city","department"    ,"job"
                    ,"store","gender","termReason","statusYear","status","businessUnit")



#charging the Date format 
Data$birthDate <- as.Date(Data$birthDate, format = "%m/%d/%Y")
Data$birthDate <- format(Data$birthDate, "%Y-%m-%d")

Data$hireDate <- as.Date(Data$hireDate, format = "%m/%d/%Y")
Data$hireDate <- format(Data$hireDate, "%Y-%m-%d")

Data$termDate <- as.Date(Data$termDate, format = "%m/%d/%Y")
Data$termDate <- format(Data$termDate, "%Y-%m-%d")





#Data explorations  
class(Data) 


# Finding unusual Data
unique(Data$job)
unique(Data$gender)
unique(Data$status)
unique(Data$businessUnit)
unique(Data$department)


#view the  structure of the  dataset
str(Data)

Data$store = as.character(Data$store)

#finding if data missing 

any(is.na(Data))


#countion the rows and the
nrow(Data)
ncol( Data)


#=================================================================
#=================================================================
#=================================================================

#Question one? 
#what  are  the  comman  reasons which made the  employee  to leave their  jobs?  


###############################################################################
# 1.1 Analysis 

termReason <- Data %>%
              select(termReason) %>%
             filter(termReason != "Not Applicable") %>%
             count(termReason) %>%
             rename(termReasonCount = n)



 ggplot(termReason, aes(x = "", y = termReasonCount, fill = termReason)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Termination Reasons",
       fill = "Reason",
       y = "") +
  theme(legend.position = "right") +
  geom_text(aes(label = scales::percent(termReasonCount/sum(termReasonCount))),
            position = position_stack(vjust = 0.5),
            size = 4)






###################################################################################

#1.2 Analysis 

# what is  the  relationship between the  people leaving company and thier age ? 

 terminated <- Data %>%
            select(age,status) %>%
            group_by(age)  %>% 
            filter(status == "TERMINATED") %>%
            summarise(countTerm = n())
 


ggplot(terminated, aes(x = age)) + geom_histogram(width =  0.1 , color= "black", aes(fill=..count..) , binwidth =1 ) + 
  scale_fill_gradient("count" , low = "red", high = "blue") + 
  labs(x = "Age" , y= "Number of Terminated Individuals") + 
  geom_text(stat = "count", aes(label = stat(count)), vjust = -1, color = "black", size = 3.5)
View( terminated)

#=========================================================================================================

#1.3 Analysis 
# top tea jobs with heighs number of the  terminations? 
#purpose  
#find out  if the  job of the  employee  consider  as main purpose of the  termination  
#result 
#  1 Meat Cutter    354
# Produce Clerk     332
# Cashier           248
# Dairy Person      190
#have  huge gape of termination  between them and other department  
#however this may occur  due  the  number of the  emplyees under this dpartment  

# Count the number of terminations in each job
JobTerm <- Data %>% select(job , status) %>% 
          filter(status  == "TERMINATED") %>%
          group_by(job) %>%
          summarise(Count = n()) %>%
          arrange(desc(Count))  %>% 
          top_n(10)


# reorder(job, Count)  extra feature ^^^^^^^^^^^ 

ggplot(JobTerm, aes(x = job,  y = Count, fill = job), width = 0.1) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5, color = "black", size = 2) +
  labs(title = "Top 10 Jobs with Highest Terminations",
       x = "Job", y = "Termination Count") 


#=======================================================================================================

#1.4 Analysis 
#number of the people terminated in each year?

# Calculate the number of terminations for each year
#purposes  
#understand the  termination pattern  thrown years 
#make  a prediction for the  future and find if there is a 
#specific year with higher termination number 
#result 
#sharp increases in the  termination number in 2014 
#followed by shaep decreeing 

#additional feature animated graph ^^^^^^^^^^^^^^^^^ 
#addtional  feature  adding the  points on the  line  graph  ^^^^^^^^

# Create the animation


termYears<- Data  %>%  select(status , termDate) %>%
  filter(status == "TERMINATED") %>%
  mutate(termYear = lubridate::year(termDate))


termYearsCounts <- termYears %>%
  group_by(termYear) %>%
  summarise(Count = n()) %>% arrange(desc(Count))


#addtional  feature  adding the  points on the  line  graph  ^^^^^^^^
#using animation
termYearsCounts %>%
  ggplot(aes(x = termYear, y = Count)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 3) +
  geom_text(aes(label = round(Count)), vjust = -1.5, color = "black") +
  labs(title = "Number of Terminations Each Year",
       x = "Year", y = "Termination Amount") +
  transition_reveal(termYear)




 

 
 




#=================================================================================

#1.5 Analysis 
# which gender tend to have  longer service  duration? 
#purpose  
#see if the one  of the genders tend to work always less duriong in the  compny 
#there for  we know  which geneder theat always less satisfied with the  job 
#result  
#the  result shows that both gender both genders score same highest lenght duration 
#26 years with same min services duration which is  0 for both genders 
#how  eve

lenOfSerGender <-  Data %>% select(gender, serviceLength) 

filteredData <- lenOfSerGender %>%
  filter(serviceLength < 2)

View(lenOfSerGender)

View(filteredData)


##addtional feature   scale_fill_manual(values = c("steelblue", "salmon")) ^^^
# scale_y_continuous addtional feature  

ggplot(lenOfSerGender, aes(x = gender, y = serviceLength, fill = gender)) +
  geom_boxplot() +
  scale_x_discrete(limits = c("Male", "Female"), expand = c(0.05, 0)) +
  labs(title = "Distribution of Length of Service by Gender",
       x = "Gender", y = "Length of Service") +
  scale_fill_manual(values = c("steelblue", "salmon")) +
  scale_y_continuous(breaks = seq(0, 30, by = 4), limits = c(0, 30)) +
  geom_text(aes(label = serviceLength), vjust = -0.5, color = "black", size = 2) 


#====================================================================================

#1.6 Analysis 
# the average  age  in each store  and the  number of the  termination? 
#pupose 
#investigate the  age avg age  in each store and the  termination number 
#result  
#  stores that have  a higher  age  avg (more  old people) tends  to loss more  employee
#however the reason why some stores have  more old people working 
#is can differ  either  the  area or when the  store  Firstly opened   
# age  range in each  store  number of the  tremination in each store



termStore <- Data %>%
  select(store, status  , age ) %>%
  filter(status == "TERMINATED") %>%
  group_by(store) %>%
  summarise(totalTerm = n(),
            AverageAge = mean(age, na.rm = TRUE)) %>%  
           arrange(desc(totalTerm))

View(termStore)
termStore %>% filter(AverageAge == min(termStore$AverageAge))
termStore %>% filter(totalTerm == min(termStore$totalTerm))


##addtion feature marg two line graphs 


ggplot(termStore, aes(x = store)) +
  geom_line(aes(y = totalTerm), color = "blue") +
  geom_point(aes(y = totalTerm), color = "blue", size = 3) +
  geom_line(aes(y = AverageAge), color = "red") +
  geom_point(aes(y = AverageAge), color = "red", size = 3) +
  geom_text(aes(y = totalTerm, label = totalTerm), vjust = -0.5, color = "blue", size = 2) +
  geom_text(aes(y = AverageAge, label = round(AverageAge, 2)), vjust = -1.5, color = 
              "red", size = 2) +
  labs(title = "Termination Number and Average Age by Store",
     x = "Store", y = "Termination Number / Average Age") 




#====================================================================================

#1.7
#what are the  jobs tend to have  a higher lenght of the  service ? 
#purpose  
#in which kind of jobs the  employee tends to stay more  
#result  
#in most of the higher level jobs  score  more  avg service  duration 
#and the  jobs with lower level  tends to have  less avg of the  serive  length 
# this can be related dirctly with amount  of the  salary 
#



jobLengthOfService <- Data %>%
  select(job , serviceLength)  %>%
  group_by(job) %>%
  summarise(meanServiceLength = round(mean(serviceLength), 2)) %>%
  arrange(desc(meanServiceLength))
  View(jobLengthOfService)

jobLengthOfService %>% top_n(10)
jobLengthOfService %>% tail(10)


ggplot(jobLengthOfService, aes(x = reorder(job, meanServiceLength), y = meanServiceLength)) +
  geom_bar(stat = "identity", fill = "Navy", width = 0.8) +
  labs(title = "Average Service Length by Job",
       x = "Job",
       y = "Average Service Length") +
  geom_text(aes(y = meanServiceLength, label =  meanServiceLength), vjust = -0.5, color = "black", size = 2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))




#=====================================================================================
#1.8 
# what are the  top 10 cities with heights termination rate  the  top 10 city with lowest  termination rate 
#purpose  found out  which city have  the  most number of the  termination which have  the  least 
#and to understand  
#result 
#there is relation  with number of the termination and the  city 
# and  vancover as  will victory city have  huge  termination number in 
#in comparison  to the  another cittis which may indecate  that those  cityies 
#consits of main resons that make  the  people left thier  job 

termCity <- Data %>%
  filter(status == "TERMINATED") %>%
  group_by(city) %>%
  summarise(Count = n()) %>% 
  arrange(desc(Count)) %>%
  
  topButtomtermCity <- bind_rows(
    termCity %>% top_n(10),
    termCity %>% tail(10)
  )

View(topButtomtermCity)

ggplot(termCity, aes(x = reorder(city, Count), y = Count, fill = Count)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Termination Count by City", x = "City", y = "Count") +
  scale_fill_gradient(low = "steelblue", high = "salmon") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#========================================================================================
#1.9 
# find the  realtion between the avarage age and city? 
#purpos 
#find out which cities have  heights number  elderly  people 
# to understand whether  or not  the  gender and the  age  is 
#the mean  reason for  exicessing the  number of  the  termination  ins some  
#result  
# the result shows that the  ocean fall is the  city with heights avarge  age  
#followed  by Dease Lake  and princeton  however those cities  have  law  termination  rate  
#which may indecate that age  is not  the  factor that make  some cities have  a higher teramination number the other  
#all the  exception  here it may because  those  cities have  few number of people  works  in the  store 
# so the  termiantion rate  my decress beacuse  the  descress in the number of the  works

CityByAge = Data %>%  
               select(city , age) %>% 
               group_by(city) %>% 
              summarise(age = round(mean(age), 2))


View(CityByAge )
# Define the city abbreviations

# Plot the graph with city abbreviations and legend
#extra feature  
#scale_y_discrete ^^^^^^^^^^^^^^^^^^^^^^
ggplot(CityByAge, aes(x = city, y = age, fill = city)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = age), vjust = -0.5, color = "black", size = 3) +
  labs(title = "Average Age by City", x = "City", y = "Average Age") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#===========================================================================================

#analysis 1.10
#what is the  number of males and femals in each  city? 
#purpos
#identify if the main  identfiy oif the  gender is the reason behind that citis with high trmination rate  
# result 
#the result  shows that there is  not a huge gape  beteen the number if the  male and the  female  in each city 
# however so ciies like  vancover have huge  number of the  amle  and the  feamle  
# in other  world employees , throgh the  pervious  analysis we identify vancover city 
#as city with heighest termination rate  and  noew  with  highest number of famle and  
#male and this may make  us to conclude  that the  number of the  employee is factore that make  
#some cities with the high termination  number  in comparsion to the  others 



#extra feature  with() function ^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Calculate the count of females and males in each city
CityByMale <- Data %>%
  select(gender, city) %>% 
  filter(gender == "Male") %>%
  group_by(city) %>%
  summarise(Male = n())

CityByFemale <- Data %>%
  select(gender, city) %>% 
  filter(gender == "Female") %>%
  group_by(city) %>%
  summarise(Female = n())

CityByGender <- merge(CityByMale, CityByFemale, by = "city", all = TRUE) 

View(CityByGender)


### replaceing NA values with zero since three is some cities with no either male or female  workers 
i = 0 
for (i in seq_along(CityByGender$Male)) {

  if (is.na(CityByGender$Male[i])) {
    CityByGender$Male[i] <- 0
  } 
  if (is.na(CityByGender$Female[i])) {
    CityByGender$Female[i] <- 0
  }
}



# Convert 'city' column to factor


# Plotting



ggplot(CityByGender, aes(x = city)) +
  geom_line(aes(y = Female, color = "Female"), size = 1.5, group = 1) +
  geom_line(aes(y = Male, color = "Male"), size = 1.5, group = 1) +
  labs(title = "Number of Females and Males in Each City",
       x = "City", y = "Count") +
  scale_color_manual(values = c("Female" = "red", "Male" = "blue"),
                     labels = c("Female", "Male"),
                     guide = guide_legend(title = "Gender")) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))



#=====================================================================================
#Analysis 1.11 
##what is the percntage of termination of in each  businessUnit
#purpose 
# understand the  termination gape  between each buiness unit  
#result  
# 
 unique(Data$businessUnit)

totalTerminationCount <- sum(Data$status == "TERMINATED")

termBusinessUnit <- Data %>%
  select(businessUnit, status) %>%
  filter(status == "TERMINATED") %>%
  group_by(businessUnit) %>%
  summarise(termination_count = n(),
            contribution_percentage = (termination_count / totalTerminationCount) * 100)

termBusinessUnit





ggplot(termBusinessUnit, aes(x = "", y = contribution_percentage, fill = businessUnit)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Termination Percentage by Business Unit", fill = "Business Unit") +
  guides(fill = guide_legend(title = "Business Unit")) +
  geom_text(aes(label = paste0(round(contribution_percentage ,2), "%")), 
                position = position_stack(vjust = 0.5)) 




#===============================================================================
#1.12 Analysis 
#question 
##what is the number of the  employees in each store  and what the  number of the  termination  in them 
#purpose  
#understand weather or  not  the  quantity  is on of the mean reason of the  termination
#result 
#the result  shows that the quantity of the  employees in each store  doesn't  have  relationship  with the  
#termination number of the  stores in  which the  store that have  number of the employee that is somehow close  to the  
#average  has the  heights  termination rate  

 storeEmp <-  Data %>% 
                 select(store,ID) %>% 
                group_by(store) %>%
                summarise(Emp = n())
                
 storeTerm <-  Data %>% 
                  select(store,status) %>% 
                  group_by(store) %>%
                  filter(status == "TERMINATED") %>%
                  summarise(status = n())

storeEmpTerm   <-   merge( storeEmp,   storeTerm, by = "store" )
any(is.na( storeEmpTerm))
View(storeEmpTerm)


ggplot(storeEmpTerm, aes(x = store)) +
  geom_line(aes(y = status, color = "Termination Count"), size = 1.5, group = 1) +
  geom_line(aes(y = Emp, color = "Employee Count"), size = 1.5, group = 1) +
  geom_point(aes(y = status, color = "Termination Count"), size = 3) +
  geom_point(aes(y = Emp, color = "Employee Count"), size = 3) +
  geom_text(aes(y = status, label = store), vjust = -1, color = "black", size = 3) +
  geom_text(aes(y = Emp, label = Emp), vjust = -1, color = "black", size = 3)+ 
  labs(title = "Termination Count and Employee Count by Store",
       x = "Store",
       y = "Count") 


#=============================================================================================
#1.13 Analysis 
# find the  percntage   of the  males and  the  females in each store? 
#purpose  figure  out  whether if the  gander is the main reason 
#of the termination in the  stores with the high tremination number 
#result  
#the  reasult shows in some case the  higher  number of th female  
#will increase number of the  termination  however it  is not  a major factor 

#get the  perc or the  num 



Num <- function(selectOne, selectTwo, colName, value) {
  df <- data.frame()

    df <- Data %>%
      select({{ selectOne }}, {{ selectTwo }}) %>%
      group_by({{ selectOne }}) %>%
      filter({{ selectTwo }} == value) %>%
      summarise({{ colName }} := n())

  return(df)
}



storeMale <- Num( store, gender, maleNum , "Male")

storeFem <- Num(store, gender, femaleNum, "Female")

storeTerm  <-  Num(store, status, status, "TERMINATED")

storeGenPerc <- merge(storeMale, storeFem, by = "store")

storeGenPerc  <-  merge( storeGenPerc ,storeTerm  , by = "store" )

storeGenPerc  %>% arrange(desc(femaleNum)) %>% top_n(3)

storeGenPerc  %>% arrange(desc(maleNum))  %>% top_n(3)

storeGenPerc  %>% arrange(desc(status))  %>% top_n(3)

#addtional feature     theme_minimal()



ggplot(storeGenPerc, aes(x = store)) +
  geom_line(aes(y = maleNum, color = "Male")) +
  geom_point(aes(y = maleNum, color = "Male")) +
  geom_line(aes(y = femaleNum, color = "Female")) +
  geom_point(aes(y = femaleNum, color = "Female")) +
  geom_line(aes(y = status, color = "Status")) +
  geom_point(aes(y = status, color = "Status")) +
  geom_text(aes(y = maleNum, label = maleNum), vjust = -1, color = "blue", size = 3) +
  geom_text(aes(y = femaleNum, label = femaleNum), vjust = 1, color = "pink", size = 3) +
  geom_text(aes(y = status, label = status), vjust = -1, color = "green", size = 3) +
  ylab("Count") +
  scale_color_manual(values = c("Male" = "blue", "Female" = "pink", "Status" = "green")) +
  labs(title = "Store Data",
       x = "Store",
       y = "Count") +
  theme_minimal()



# Adjust the graph appearance

#=====================================================

#1.14 analysis 
#number of the stores andthier  termination number  in each city ? 
#purpose
#find out  if the  number  of stores in cities  is major  reason 
#which make  some  of the  cities have  huge number of the  terminattion 
#in relation to others 
#result  
# there a strong  relationship with the  number  of the  stoes in each city and the  termination number  
#this  shows that the city that which include a greater number  of the  stores have  a more  number  
# in of the  store  have  more  termination number 


citySummary <- Data %>%
  select(city, status, store) %>%
  group_by(city) %>%
  summarise(numStores = n_distinct(store),
            terminationCount = sum(status == "TERMINATED")) %>% arrange(desc(numStores))

any(is.na(storeSummary))
View(storeSummary)

##addtional feature   bubble  chart ^^^^^^^^^^^^^^^^


# Create the bubble chart with automatic colors for each city

ggplot(citySummary, aes(x = numStores, y = terminationCount, label = city, color = city)) +
geom_point(size = 2) +
scale_size(range = c(3, 10)) +
scale_color_discrete() +
ylab("Termination Count") +
xlab("Number of Stores") +
labs(title = "Bubble Chart: Number of Stores vs Termination Count by City") 






#============================================================================================
#1.15 Analysis  
#number of the termination in each department  
#purpose  find out  if there is  solid relationship  between the department 
#and the termination number in  which  some  department have  heights number  of the  termination 
#in comparison  to the  other  department

#result 
#"Customer Service" department has the highest number of terminations, while the "Legal" and 
#"Investment" departments have the lowest termination numbers, indicating varying levels of 
#employee turnover across departments.

termDepert <-  Data  %>% 
               select(department , status)  %>% 
               group_by(department) %>%
               filter(status == "TERMINATED") %>% 
               summarise(TermNumber =  n() ) %>% arrange(desc(TermNumber))
unique(termDepert$department)
 
termDepert %>% filter(TermNumber ==  max(termDepert$TermNumber))

termDepert %>% filter(TermNumber ==  min(termDepert$TermNumber))

termDepert %>% filter(TermNumber ==  median(termDepert$TermNumber))


#addtional fearure rainbow(length(unique(termDepert$department)))  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^


# Create the bar chart
ggplot(termDepert, aes(x = TermNumber, y = department, fill = department)) +
  geom_col() +
  geom_text(aes(label = TermNumber), hjust = -0.2, color = "black", size = 2) +
  labs(title = "Number of Terminations by Department",
       x = "Number of Terminations",
       y = "Department") +
  scale_fill_manual(values = rainbow(length(unique(termDepert$department))), guide = guide_legend(title = "Department", ncol = 2 ))


#=========================================================================================
# 1.16 analysis 
#three  trend of the  number termination  for  each age  group for  the  past 5 years  ?
#purpose  
#this  will be  conducted to  observe specific age groups that consistently show higher
#or lower termination rates or  is  there a specific  year where most age  goup tend  
#to terminate  more 
#result  
# the  old  peolpe termiantion number have an increment  trends  
# specifically  people aged between 60 and 65  
#people age  between 19 t0 30 tends to have  more  stable trend hoveve  
#this group of age experience  a sharp increat in the  termination number during  2014




# Filter the data and select the relevant columns
# Filter the data and select the relevant columns

termAgeL5Year <- Data %>%
  select(status, age, statusYear) %>%
  filter(status == "TERMINATED") %>%
  group_by(statusYear, age) %>%
  filter(statusYear >= max(Data$statusYear) - 4) %>% 
  summarise(count = n())



View(termAgeL5Year )



ggplot(termAgeL5Year, aes(x = statusYear, y = count, fill = factor(age))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Number of Terminations", fill = "Age Group") 
#============================================================================================
#1.17 analysis 
#What is the number of resignations among individuals aged between 21 and 30 in the company over the last 5 years? 
#we specify the age range of interest (21 to 30) and focus on 
#the count of resignations during the specified time frame. 
#This question allows us to explore the resignation patterns among young employees and gain 
#insights into any potential trends or issues related to their termination. 

#result 
# the result  shows there  is  a huge  number  of  the   Resigned in the  last  two years 
# in 2014  the 54  Resigned made  and 53 out of them where  from people  ageed 
#21  years old  
#also in year  2015  there was  a 26 number  of the  Resigned all are made  from 
#people  aged 26 yeras old
#previous three years shows huge  Resigned number  made  by people aged 22, 23,and 30 

remove(resignations)
resignations <- Data %>%
   select(termReason,  age , statusYear) %>%
  filter(termReason == "Resignation", age >= 21, age <= 30, statusYear >= max(Data$statusYear) - 4) %>%
  group_by(statusYear , age ) %>%
  summarise(Resigned = n()) %>% arrange(desc(Resigned ))

View(resignations)

# display which   age  group have  the  height Resigned number an in which  year 
resignations 

#people  aged 30 have  the  highest  Resigned number  
#followed  by people  aged  21 
resignations %>% 
  select(age, Resigned) %>% 
  group_by(age) %>% 
  summarise(ResignedCount = sum(Resigned)) %>% 
   arrange(desc(ResignedCount ))


#display the lowest 
resignations %>%
  arrange((Resigned ))


ggplot(resignations, aes(x = factor(statusYear), y = Resigned, fill = factor(age))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Resigned), vjust = -0.5, size = 3, position = position_dodge(width = 0.9)) +
  labs(x = "Year", y = "Number of Resignations", fill = "Age Group")






#============================================================================================
#1.18 analysis 
# the  heighs  termination amoung  each age  group  
#purpose 
# investigate  reason why  pople  from different age group may leave  the  company 
#result
# 

#addtion feature   slice_max(n, with_ties = FALSE) ^^^^^^^^^^^^^^^^^^^^^^^^
highestTermination <- Data %>%
  select(status , age  , termReason) %>% 
  filter(status == "TERMINATED") %>%
  group_by(age) %>%
  count(termReason) %>%
  slice_max(n, with_ties = FALSE)

View(highestTermination)

# Create the bubble graph Addtional feature ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
ggplot(highestTermination, aes(x = termReason, y = age, size = n, color = factor(age))) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(5, 15)) +
  geom_text(aes(label = n), size = 2, vjust = 1, color = "black") +
  labs(x = "Termination Reason", y = "Age", size = "Count", color = "Age Range") 

# Create the bubble graph with gradient color scale



#=======================================================================================
#1.19 Analysis 
#what is  pecntage  of the  people aged between 19 to 30 in each job position. 
#pupose investigate  if the  job of the  employeee is the  reason behind thier termination 
#result  
#the  result  shows that 100% of the people  aged bewteen 19 to 30  are wotrking in the  lower level jobs  
#this answer the  question why the empoyye  age between 21  to 30 have a high huge  resigned  number  
#from this analysis  my conduct that the  ... required alot of time  
#or and maybe the  salaries  are not enough  so the  employee leave  the  work for  any other  job 
#operiontes 

jobYoung <- Data %>%
  select(age ,job) %>%
  filter(age >= 19, age <= 30) %>%
  group_by(job) %>%
  summarise(percentage = (sum(n()) / sum(Data$age >= 19 & Data$age <= 30)) * 100) %>%
  arrange(desc(percentage))


 ggplot(jobYoung, aes(x = "", y = percentage, fill = job)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  labs(fill = "Job", title = "Percentage of People aged 30 to 19 by Job") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5) ,size = 2)


#=================================================================================================================
#20 compare the number  of  the  people aged less 30 between 31 and 40 and above  50
#find out which age age  giup more  in the  business 
 #result  
 
 
 #aadtion feature  case_when ^^^^^^^^^^^^^^^^^^^^^^
 ageComparison <- Data %>%
   select(age) %>%
   filter(age < 30 | (age >= 31 & age <= 40) |(age >= 41 & age <= 50)  |age > 50) %>%
   group_by(ageGroup = case_when(
     age < 30 ~ "Less than 30",
     age >= 31 & age <= 40 ~ "31-40",
     age >= 41 & age<= 50 ~ "41-50",
     age > 50 ~ "Above 50"
   )) %>%
   summarise(count = n())
 
 View( ageComparison)
 
 ##remove(ageComparisonPerc)
 #ageComparisonPerc <- Data %>%
#   select(age) %>% 
#  filter(age < 30 | (age >= 31 & age <= 40) | age > 50) %>%
###  group_by(ageGroup = case_when(
   #  age < 30 ~ "Less than 30",
  #   age >= 31 & age <= 40 ~ "31-40",
  #   age > 50 ~ "Above 50"
  # )) %>%
  # summarise(count = n()) %>%
  # mutate(percentage = count / sum(count) * 100)
 


 ggplot(ageComparison, aes(x = ageGroup, y = count, fill = ageGroup)) +
   geom_bar(stat = "identity", color = "black", width = 0.5) +
   geom_text(aes(label = count), position = position_stack(vjust = 0.5), color = "white", size = 4) +
   labs(fill = "Age Group", x = "Age Group", y = "Count", title = "Comparison of Age Groups") 

 #===========================================================================================================
#conclusion 
 
 
 #============================================================================================================
 #============================================================================================================
 #Question 2 
 #which gender role leave the  company more and why? 
 
 #=========================================================================================
 #Analysis 2.1 
 #the total number  of the  male  and  the  female  in the  company ? 
 #understand which gender role  have  the compromise on the  higist percintage  
 #result  
 #the  number of percnatge  of the female  works is heighr  then the  males by 4.4% ~
 # Calculating total number of males and females
 genderPercentage <- Data %>%
   select(gender) %>%
   group_by(gender) %>%
   summarise(Count = n()) %>%
   mutate(Percentage = (Count / sum(Count)) * 100)
 
 
 
 malePercentage <- genderPercentage$Percentage[genderPercentage$gender == "Male"]
 femalePercentage <- genderPercentage$Percentage[genderPercentage$gender == "Female"]
 
 subtraction <-  femalePercentage - malePercentage 
 
 subtraction
 
 
 ggplot(genderPercentage, aes(x = "", y = Percentage, fill = gender)) +
   geom_bar(stat = "identity", width = 1) +
   coord_polar("y", start = 0) +
   labs(title = "Gender Distribution",
        fill = "Gender",
        y = "") +
   theme_minimal() +
   theme(legend.position = "right") +
   geom_text(aes(label = paste0(round(Percentage, 1), "%")),
             position = position_stack(vjust = 0.5),
             size = 4)
 
 
 #========================================================================================
 #analysis 2.2 
 #anaysis if the  the  only  reson of the  gape between the  termiantuo abetween  male  and the  f
 #tne  female  is the  gape  between the  male  and female  workers
 # Total number of male and female
 total_male <- 3006
 total_female <- 3278
 
 # Total terminations
 total_terminations <- 1485
 
 # Percentage of terminations for females and males
 female_termination_percentage <- 54.32
 male_termination_percentage <- 45.68
 
 # Calculate the number of terminations for females and males
 female_terminations <- (female_termination_percentage / 100) * total_terminations
 male_terminations <- (male_termination_percentage / 100) * total_terminations
 
 # Print the number of terminations for females and males
 female_terminations
 male_terminations
 
 
 #=======================================================================================
 #Analysis 2.3 
 
 # the  termination number for  each gender group ? 
 #purpose  
 #find out  which gender group have  the  heights termination number 
 #result 
 #out of 1485 termination  915 females  terminated  and  570 male  
 #this  indicate  that not  only  the number  difference  is the 
 #thing that make  the  female  trematinated more  
 
 genTerm <- Data %>% select(status,gender) %>% filter(status == "TERMINATED") %>% 
   group_by(gender) %>% 
   summarise(Count = n()) 
 sum(genTerm$Count)
 
###  345
 
 ggplot(genTerm, aes(x = gender, y = Count, fill = gender) ) +
   geom_bar(stat = "identity") +
   labs(title = "Termination Count by Gender",
        x = "Gender", y = "Termination Count") +
   scale_fill_manual(values = c( "salmon", "steelblue")) + 
   geom_text(aes(label = Count),  vjust = -0.5)
 
 

#====================================================================================
 #Analysis  2.4
 # the percatge  of the male  and the  female  from the tolal pecnatge of male  and female  in each termination reason? 
 #reasult 
 #in case of the  layoff and resigned the  man exceed  the  woman
 #besed on that and pervous analysis  we may conlcude  that the man  compromise more 
 #in age  group between 21 to 30 
 #regarding the  female  workers thhe female workes have  higher retirement  percentage 
 # which mean the  female compromise  more in age  group over  5o years old 
#however  the  retimermt is the  mean reason of the  tremination for both genders
 library(dplyr)
 
 # Calculate the total percentage of males and females
 
 
 #total number of male female
 totalGenderCounts <- Data %>%
   select(gender, status) %>%
   group_by(gender) %>%
   filter(status == "TERMINATED") %>%
   summarise(totalCount = n())

 termGenderCounts <- Data %>%
   select(termReason, gender, status ) %>%
   group_by(termReason, gender) %>%
   filter(status == "TERMINATED") %>%
   summarise(count = n())

 
 percentageGenderCounts <- termGenderCounts %>%
   left_join(totalGenderCounts, by = "gender") %>%
   mutate(percentage = count / totalCount * 100) %>% 
   select(-count, -totalCount)
 
 View(percentageGenderCounts)
 
 ggplot(percentageGenderCounts, aes(x = termReason, y = percentage, fill = gender)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(title = "Percentage of Male and Female in Each Termination Reason",
        x = "Termination Reason", y = "Percentage") +
   scale_fill_manual(values = c("Female" = "pink", "Male" = "blue"))
 #=========================================================================================
 #analysis 2.5  
 #what is the percnatge  of treminated male  and the  female  among  each  age group ? 
 #result it shows that one of the  reason that female  have  a higher  termination rate beacuse  
 #is that most of the  elderly  workers are female  and the  elderly  
 #people have high  number  of the  termination dude  to age  constraints 
 
 totalGenderCounts <- Data %>%
   select(gender,status) %>%
   group_by(gender) %>%
   filter(status == "TERMINATED") %>%
   summarise(totalCount = n())
 
 termGenderCounts <- Data %>%
   select(gender, age, status) %>%
   group_by(age, gender) %>%
   filter(status == "TERMINATED") %>%
   summarise(count = n())
 
 AgeGenderCounts <- termGenderCounts %>%
   left_join(totalGenderCounts, by = "gender") %>%
   mutate(percentage = count / totalCount * 100) %>% 
   select(-count, -totalCount) 
 
 
 AgeGroupCounts <- AgeGenderCounts %>%
   mutate(AgeGroup = case_when(
     age <= 30 ~ "Below or Equal to 30",
     age > 30 & age <= 40 ~ "Above 30 to 40",
     age > 40 & age <= 50 ~ "Above 40 to 50",
     age > 50 ~ "Above 50"
   )) %>%
   group_by(AgeGroup, gender) %>%
   summarise(Percentage = sum(percentage))
 
 ##Addtional fearure  this chart ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
 ggplot(AgeGroupCounts, aes(x = gender, y = Percentage, group = AgeGroup, color = AgeGroup)) +
   geom_line() +
   geom_point() +
   labs(title = "Percentage of Terminated Male and Female in Each Age Group",
        x = "Gender", y = "Percentage") +
   theme(legend.position = "top")
 #================================================================================
 #analysis  2.6  
 #what is the number  of the  male  and the female  in each  job  postion. 
 #purpose  find if the  job postion is the  main factor that casue  the  gape  
 #beteen the  number  of the  male  and female  termination  
 #result  
 #all the jobs with  heighest numbe r of the emaployee shows that number  of the  males is ovr  the  feamles 
 #also the  higest level  jobs  show a similar  conterpution for both male and the feamle  
 #this  may conclude  that the job postion is not  the  factore  that effect the  gap betwee both genders in the  number 
 #for  the  teramination
 
 
 jobPositionCounts <- Data %>%
   select(gender, job) %>%
   group_by(job, gender) %>%
   summarise(count = n()) %>% 
  arrange(desc(count))
 
 
 
 
View( jobPositionCounts)
library(ggplot2)

# Plot the number of males and females in each job position
library(ggplot2)

# Plot the number of males and females in each job position
library(ggplot2)

# Plot the number of males and females in each job position

ggplot(jobPositionCounts, aes(x = count, y = job, fill = gender)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), color = "white", size = 2) +
  labs(title = "Number of Males and Females in Each Job Position",
       x = "Count", y = "Job Position", fill = "Gender") +
  scale_fill_manual(values = c("blue", "red"))


#=======================================================================================
#analysis  2.7  
# what is the  number of male  and the termianted male  and female  in the department top 10 depratmentt with 
#heighes number  of the  termination? 


#department  with top heighst termaontion number
termDep <- Data %>%  
  select(department, status) %>%
  group_by(department) %>%
  filter(status == "TERMINATED") %>% 
  summarise(Depcount = n()) %>% 
  arrange(desc(Depcount)) %>% 
  top_n(10)


# each department  number the  male and the female 
depMaleFem <- Data %>% 
  select(department, gender, status) %>% 
  group_by(department, gender) %>% 
  filter(status == "TERMINATED") %>%
  summarise(count = n())

#top 10 departments  with highest  termination number  
#and the number  of the  male  and the  female  in each of these departments  

depMaleFemTopTermDep <- merge(termDep, depMaleFem, by = "department")

#remover the  depcount  column 
depMaleFemTopTermDep <-   depMaleFemTopTermDep %>%  
  select(-Depcount)


library(ggplot2)

# Bar chart
library(ggplot2)

#addtional feartur  make the  x axis slope  
# theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar chart
ggplot(depMaleFemTopTermDep, aes(x = department, y = count, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Termination Count by Gender in Top 10 Departments",
       x = "Department", y = "Termination Count", fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#==================================================================================
#analysis  2.8 
#what  is the  number  of the  male   and the  female  in the cities 
#with most treminated number. 
#purpose is to find of the  female  are more  locatedin the  cities with higher termination number
#the result  show  that the both number  of the  mae and the  feamles increas in the  countries with heighs tr,ination number  
#however  the  

termCity  =  Data %>% 
             select(city , status) %>% 
             group_by(city) %>%  
             filter(status == "TERMINATED") %>%
             summarise(termCount = n())
             

CityMaleFemale =   Data  %>% 
                      select(city , gender , status) %>%  
                      group_by(city, gender) %>%
                      filter(status == "TERMINATED")%>% 
                      summarise(count = n())
  

termCityMaleFemale <- merge(CityMaleFemale, termCity, by = "city") 

termCityMaleFemale %>% arrange(desc(termCount)) <-  termCityMaleFemale


ggplot(termCityMaleFemale, aes(x = city, y = count, group = gender, color = gender)) +
  geom_line() +
  geom_line(aes(x = city, y = termCount, color = "Total"), linetype = "dashed") +
  labs(title = "Termination Count by Gender in Each City",
       x = "City", y = "Termination Count", color = "Gender") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_manual(values = c("blue", "red", "black"), labels = c("Termaninated Female", "Terminated Male", "total termination")) 


#==========================================================================================
#analysis  2.9 
#what is the  precentage of the male  and female in each buiesness unit? 
#find out  if the  female most  the  females work in the stores which  have  a heigher  number
# of the  termination 
#result
#the number of the female in each buisness unit  is  more  then the numbewr of the  meals 
#with  lowe amount of  the  differin the  value  
#this make  conclusion  although te  female  work more in the head offece  more  
#mean better jobs  bu there  termination   prcnatge  is hegier  in general 
#which mean that the businessUnit not  have  a effect on the  higer 
#termonation number  for  the  female  workers 




businessUnitPercentage <- Data %>%
  select(businessUnit, gender) %>%
  group_by(businessUnit, gender) %>%
  summarise(count = n()) %>%
  group_by(businessUnit) %>%
  mutate(percentage = (count / sum(count)) * 100)


# Create the bar plot
ggplot(businessUnitPercentage, aes(x = businessUnit, y = percentage, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Gender Distribution by Business Unit",
       x = "Business Unit",
       y = "Percentage") +
  theme_minimal() +
  geom_text(aes(label =  paste0(round(percentage, 2), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", 
            size = 3)



#==================================================================================
#2.10 what is the  termination amount of the  fmale and the  male  over the years? 
#finding if the reason behinde the  gape of the  number  of the  termaintion between the feamle  
# and the male  occer in constant  pattern or  there is a years which casus this  gape  
#result  
#based on the  result  we can conclude  the  the terminated female  exceed the  number  of the  males 
#over the  paset  year the  gape of the  termination number  increased dramatically  starting 

#from year 2013 and ending on year 2013 



library(tidyverse)

termYearsMaleFemale <-  Data %>% 
                        select(statusYear, gender,  status) %>% 
                        group_by(statusYear , gender) %>% 
                        filter(status == "TERMINATED") %>% 
                        summarise(termCount = n() ) 

view(termYearsMaleFemale)
names(termYearsMaleFemale) <-  c("Years", "gender", "Amount") 

#additional  feature  pivot_wider function ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


termYearsMaleFemale <- termYearsMaleFemale %>%
  pivot_wider(names_from = gender, values_from = Amount, names_prefix = "count")

termYearsMaleFemale %>%
  ggplot(aes(x = Years)) +
  geom_line(aes(y = countMale, color = "Male")) +
  geom_line(aes(y = countFemale, color = "Female")) +
  geom_point(aes(y = countMale, color = "Male"), size = 3) +
  geom_point(aes(y = countFemale, color = "Female"), size = 3) +
  geom_text(aes(y = countMale, label = countMale), vjust = -1, color = "black") +
  geom_text(aes(y = countFemale, label = countFemale), vjust = 1, color = "black") +
  labs(title = "Termination Amount by Year and Gender",
       x = "Year",
       y = "Termination Amount",
       color = "Gender") +
  transition_reveal(Years)

#=================================================================================
#conclusion  


#==================================================================================
#Question 3  
#QWhat is the difference between the number of terminations for 2006 to 2015
#and the number of hires across all employees, and how it is differ amoug different aspect and how  this may 
# the  company in the  future. 


#===========================================================
#===========================================================
#===========================================================
#Analysis3.1  

#what is the gape beteen total number of the  termianation and  the hirring in each year starting from 2006 untill 2015?  ?
#under find out  if the  there is any risk in the  futur like  the company will suffer 
# from the  lake  of the  employee or increasing  number  of the employee
#result  the  hirring number aahve  gone through Constance  state  however 
#it suddenly  decrees in the  year 2014  to zero  and continue  in zero in year 2015 
#the  termination amount  shows a smaller amount then the  hirring amount int the past 
# years however  it  increase  sharply  in the  year 2014 and  the  2015 
# as result  we can notice that from the  Differenc column that the  tremination amount 
#was less than the  hirring amount  untill 2014 the  termination amount becom more  than the  hiring amount 
#by huge gap inducting huge  number  of the  termination in smane  time  
#if the  this gape  continue  the  company  my suffer  in the  further from the  lake  
#of the  employees 



#######
########## termaintion number in each year start from  2006 to 2015 
totalHireTermNumInYears <- Data %>%
  select(statusYear, status) %>%
  arrange(statusYear) %>%
  group_by(statusYear) %>%
  filter(status == "TERMINATED") %>%
  summarise(termCount = n())


##########  harring  number in each year start from  2006 to 2015 
countHiring <-  Data  %>% 
                select(hireDate) %>% 
              mutate(hireYear = year(as.Date(hireDate))) %>% 
                select(-hireDate)  %>%
                arrange(hireYear) %>% 
                group_by(hireYear) %>% 
                summarise(hireCount = n())


#margin between the  
names(countHiring) <-   c("Years", "hireCount")
names(totalHireTermNumInYears) <- c("Years", "termCount") 
totalHireTermNumInYears <- merge(totalHireTermNumInYears, countHiring, by = "Years", all =  TRUE ) %>% filter(Years  >= 2006)



##removing Na values

i = 0 
for (i in 1:length(totalHireTermNumInYears$termCount) ) {
  
  if (is.na(totalHireTermNumInYears$termCount[i])) {
    
    totalHireTermNumInYears$termCount[i] <- 0
  } 
  if (is.na(totalHireTermNumInYears$hireCount[i])) {
    
    totalHireTermNumInYears$hireCount[i] <- 0
  }
}

totalHireTermNumInYears$Difference <-totalHireTermNumInYears$hireCount - totalHireTermNumInYears$termCount  


totalHireTermNumInYears %>% 
  ggplot(aes(x = Years)) +
  geom_line(aes(y = termCount, color = "Termination")) +
  geom_line(aes(y = hireCount, color = "Hiring")) +
  geom_line(aes(y = Difference, color = "Difference")) +  # Add line for Difference
  geom_point(aes(y = termCount, color = "Termination"), size = 3) +
  geom_point(aes(y = hireCount, color = "Hiring"), size = 3) +
  geom_text(aes(y = termCount, label = termCount), vjust = -1, color = "black", hjust = -0.2) +
  geom_text(aes(y = hireCount, label = hireCount), vjust = 1, color = "black", hjust = -0.2) +
  geom_text(aes(y = Difference, label = Difference), vjust = -1, color = "black", hjust = -0.2) +  # Add text for Difference
  labs(title = "Termination and Hiring Count by Year",
       x = "Year",
       y = "Count",
       color = "Type") +
  scale_color_manual(values = c("Termination" = "red", "Hiring" = "blue", "Difference" = "green"), 
                     labels = c("Termination", "Hiring", "Difference"))  # Add legend labels and colors




#========================================================================================================================
#Analysis 3.2  
# what is the expected difference amount between the heiring the  the  termination  in the  years 
#from 2016 to 2020? 
#predict  the  hiring and the  tremination number  in  company  for  duration between 
#2016 to 2020. 
#result  
#basde on the  forcasting  analysis 
#the termiantion amount will continue in stable  amount  wiht silt not over  10 betweeb each years 
#the  hirring  amount  will continue decressin in the  in the  up comming   years 
# and expect to  to reach 0 with in between 2019 and 2018 
#the  deference  amount between hiring and tremination will increase 
#in linear  approach  as result of the  decrement  in the  hirring number 
#this may conclud that the company my suffer  in the next your from 
#decreaseing of the  amount of the  employee




#uing the  totalHireTermNumInYear dataste from the  anaysis  
#3.1 

###all here after this lline is new  features  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# Step 1: Split the data
# Divide the data into a training set and a test set
# For simplicity, we'll use the first 10 years as the training set and the last 5 years as the test set
training_set <- totalHireTermNumInYears[1:10, ]
test_set <- totalHireTermNumInYears[11:15, ]

# Step 2: Train the model
# Fit a linear regression model using the training set
modelHire <- lm(hireCount ~ Years, data = training_set)
modelTerm <- lm(termCount ~ Years, data = training_set)
modeldiff<- lm(Difference ~ Years, data = training_set)

# Step 3: Make predictions
# Predict the hiring numbers for the next five years using the trained model
next_years <- data.frame(Years = 2016:2020)  # Create a dataframe for the next five years
predHire <- predict(modelHire, newdata = next_years)
predTerm <- predict(modelTerm, newdata = next_years)
predDiff <- predict(modeldiff, newdata = next_years)

# Display the predicted hiring numbers

training_set <- totalHireTermNumInYears[1:10, ]
test_set <- totalHireTermNumInYears[11:15, ]

modelHire <- lm(hireCount ~ Years, data = training_set)
modelTerm <- lm(termCount ~ Years, data = training_set)
modeldiff<- lm(Difference ~ Years, data = training_set)


newHire <- data.frame(Year = next_years$Years, Hiring = predHire) 
newTerm <- data.frame(Year = next_years$Years, Termination = predTerm)
newDiff <- data.frame(Year = next_years$Years, Difference = predDiff)
print(newHire)
print(newTerm)
print(newDiff)

preHireTrem <- merge(newHire,newTerm , by ="Year" )

preHireTrem <- merge(newDiff , preHireTrem , by = "Year")

preHireTrem %>% 
  ggplot(aes(x = Year)) +
  geom_line(aes(y = Termination, color = "Termination")) +
  geom_line(aes(y = Hiring, color = "Hiring")) +
  geom_line(aes(y = Difference , color = "Difference")) +
  geom_point(aes(y = Termination, color = "Termination"), size = 3) +
  geom_point(aes(y = Hiring, color = "Hiring"), size = 3) +
  geom_point(aes(y = Difference , color = "Difference"), size = 3) +
  geom_text(aes(y = Termination, label = round(Termination, 2)), vjust = -1, color = "black", hjust = -0.2) +
  geom_text(aes(y = Hiring, label = round(Hiring, 2)), vjust = 1, color = "black", hjust = -0.2) +
  geom_text(aes(y = Difference , label = round(Difference , 2)), vjust = 1, color = "black", hjust = -0.2) +
  labs(title = "Termination and Hiring Count forecasting next 5 years",
       x = "Year",
       y = "Amount",)
+
  transition_reveal(Year)



#=====================================================================================
#Analysis 3.3 
##what is the  age between the hirring  and the  termination  
#among  each age  group? 
#result  
#understanding  which age  group the company  my gain more  and which will lose  
#more  and how that will effect  the  company 






### count tremaintion  under each  age  in each year 
ageTerm  = Data %>% 
              select(age , status, statusYear)  %>% 
              group_by(age, statusYear) %>% 
              filter(status == "TERMINATED") %>%
              summarise(termCount = n()) %>% 
             arrange(statusYear) %>% 
               group_by(statusYear) %>% 
              arrange(statusYear)

#counting the  hiring  in each age under each year 
ageHire  = Data  %>% 
           select (age ,hireDate) %>% 
           mutate (hireYear = year(hireDate)) %>%
           group_by(age ,hireYear) %>% 
            summarise(hireCount = n())%>% 
           group_by(hireYear)

#changing column names from the  merging  process
names(ageTerm) =  c("age",  "Year", "termCount")
names(ageHire) = c( "age",  "Year", "HireCount")


#combining  the  counts of the  hiring data and  termination  data in each year 
#by using the  year and  age  column
# Perform a left join between ageHire and ageTerm
ageHireTerm <- merge(ageHire, ageTerm, by = c("Year" , "age"), all = TRUE) %>% 
              group_by(Year) %>% 
             filter(Year >= 2006) %>% 
               arrange(Year)
               



##removing Na values

i = 0 
for (i in 1:length(ageHireTerm$HireCount) ) {

  if (is.na(ageHireTerm$HireCount[i])) {

    ageHireTerm$HireCount[i] <- 0
  } 
  if (is.na(ageHireTerm$termCount[i])) {

    ageHireTerm$termCount[i] <- 0
  }
}

## addtion feature  cut function




# Create categories  the  age  group  removind the  age column
ageHireTerm$ageGroup <- cut(ageHireTerm$age, breaks = c(-Inf, 31, 40, 50, Inf),
                            labels = c("Below 31", "31-40", "41-50", "Above 50"),
                            right = FALSE)



ageHireTerm <-  ageHireTerm %>%
                group_by(Year,ageGroup) %>%
                select(-age) %>%
                summarise(HireCount = sum(HireCount),
                          termCount = sum(termCount))



##validating  the  data 
sum(ageHire$HireCount[ageHire$Year >= 2006])
sum(ageHireTerm$HireCount)
view(ageHireTerm %>% tail(20))    

#addtiona feature  linetype = "solid"
ggplot() +
  geom_line(data = ageHireTerm, aes(x = Year, y = termCount, color = ageGroup), linetype = "solid", size = 1.2) +
  geom_point(data = ageHireTerm, aes(x = Year, y = termCount, color = ageGroup), size = 3) +
  geom_text(data = ageHireTerm, aes(x = Year, y = termCount, label = termCount), vjust = -1, color = "black", hjust = -0.2, size = 3) +
  geom_line(data = ageHireTerm, aes(x = Year, y = HireCount, color = ageGroup), linetype = "dashed", size = 1.2) +
  geom_point(data = ageHireTerm, aes(x = Year, y = HireCount, color = ageGroup), size = 3) +
  geom_text(data = ageHireTerm, aes(x = Year, y = HireCount, label = HireCount), vjust = -1, color = "black", hjust = -0.2, size = 3) +
  labs(title = "Termination and Hiring Count by Year",
       x = "Year",
       y = "Termination Number (solid), Hiring Number (dashed)") +
  guides(color = guide_legend(override.aes = list(shape = c(16, 16, 16, 16))), title = "Count Type")


#result  
#the  result of the analysis shows that the  people aged 
#31 and Below tend  to have the small number  of the  termination 
#in the  years from  2006 to 2010 which may  induct a better satisfaction
#better job  satisfaction  during that period of time however the  number of 
# treimantion increases  sharply  starting from 2010 untill it  reach it to 
#to the  pick in 2015  by relaing on the  analysis()  this happed dude 
# to the  higer  number of the  reigned  of the  employees aged  21 and  
  #30  this my  indeacte that the  reason my be less job satisfaction  amoung  the  group 
  #or  the  delay of the  promotion for  some of them  since all the  heights  postion 
  # in the  company role  by older group of peeople. however the is grop of peope  
  # recoded  heigher  hirring number  in all years but it  faill to 0 in the  2014 
  
  #people aged between 31 to 40 where  having a huge  hiring number  in the  2006 until 2008 
  # the  hiring suddenly  decress to zero in 2010 and continue  to increass silly  
  #in folowed years 
  #people aged  above  have  a 0 number  of the  termination which is a comman sance 
  #due to the age  constraint  
  # however this group of people continue  terminating  
  # by reffering to  analysis  number  ... we can tell that the  reson 
  #is the  age  constrant  in which may the  maxima allowed age  
  #is  65 acorrdign to analysis  (234) 
  
  #==========================================================================================
  #Analysis 3.4 
  
  #what is the  number  of the  termaintion and  hiring 
  #in each store. ?
  #purpse  find out which store will suffer from lack of the  employees 
  #which will will have  more  number  of  the  employees 
  #and how this will effect the stores in the  fture
storeTerm  = Data %>% 
          select(store , status, statusYear)  %>% 
          group_by(store, statusYear) %>% 
          filter(status == "TERMINATED") %>%
          summarise(termCount = n()) %>% 
          arrange(statusYear) %>% 
          group_by(statusYear) %>% 
          arrange(statusYear)
      
#counting the  hiring  in each age under each year 
storeHire  =  Data  %>% 
          select (store ,hireDate) %>% 
          mutate (hireYear = year(hireDate)) %>%
          group_by(store ,hireYear) %>% 
          summarise(hireCount = n())%>% 
          group_by(hireYear)


names(storeTerm) =  c("store",  "Year", "termCount")
names(storeHire) = c( "store",  "Year", "HireCount")


#combining  the  counts of the  hiring data and  termination  data in each year 
#by using the  year and  age  column
# Perform a left join between ageHire and ageTerm
storeHireTerm <- merge(storeHire, storeTerm, by = c("Year" , "store"), all = TRUE) %>% 
  group_by(Year) %>% 
  filter(Year >= 2006) %>% 
  arrange(Year)
view(storeHireTerm)%>% arrange(desc(termCount))



##removing Na values

i = 0 
for (i in 1:length(storeHireTerm$termCount) ) {
  
  if (is.na(storeHireTerm$HireCount[i])) {
    
    storeHireTerm$HireCount[i] <- 0
  } 
  if (is.na(storeHireTerm$termCount[i])) {
    
    storeHireTerm$termCount[i] <- 0
  }
}

### generate a  chart 




#extra feature  adding to plotes next to each other 
library(ggplot2)
library(patchwork)

# Bar plot for termination counts by store
termination_plot <- ggplot(storeHireTerm, aes(x = store, y = termCount, fill = factor(Year))) +
  geom_bar(stat = "identity") +
  labs(title = "Termination Counts by Store",
       x = "Store", y = "Termination Count") +
  scale_fill_manual(values = rainbow(length(unique(storeHireTerm$Year)))) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1, size = 5))

# Bar plot for hiring counts by store
hiring_plot <- ggplot(storeHireTerm, aes(x = store, y = HireCount, fill = factor(Year))) +
  geom_bar(stat = "identity") +
  labs(title = "Hiring Counts by Store",
       x = "Store", y = "Hiring Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1, size = 5))

# Arrange the two bar plots side by side using patchwork
combined_plot <- termination_plot + hiring_plot +
  plot_layout(ncol = 2)

# Display the combined plot
combined_plot


#this result  
#the  result  shows that  the hirring  amount  tend to  increas 
#significantly  in the tree stores which are  43 ,44,and 45 
#however  the  result  also revale that  those  stors in addtion to another 
# store  have  zore  numebr  of the  hiring  which  is  comman since 
#since  the  result  of  the  Analysis 3.1 which express that the years 2014 and the  2015 
#have  a 0 number  of  the hiring  
#althogh  45,44,43 have  number  of the  termination but  the  number of the  termiantion 
# is  so low in comparison  to the  number  of the  hiring amount  
#also  there is some  store  like  42,24, 4 that consist of 0 hiring  rate from the  
#year 2006 unttil year  2015 however thoose  store  are continually 
#lossing  a empoyee during the  mentioed years by refering to the  analysis  1.12 
# we can tell that those  store  not  have  nuw  hiring not  becuse  
#of the  satisfaction  in the  stff number however because ther  is no employees  to  
#avaliable to  work  this  will  conclude  thar  this stores and  the  semlier once  
# are  under the  danger of the  lack  of staffs 
#likewais the  mintioned store  the  stores 35 and 37  are  going through the  same 
#issues however those two stores have  the  highest temiantion rate  amoung  all the  stores
#which  0  hiring for  the  store  37 and only 3  for  the  35

#conclusion some the  stores will suffer  dude  the  lack of the  
#empoyees




################################################################
#Analysis 3.5 
#
departTerm  = Data %>% 
  select(department  , status, statusYear)  %>% 
  group_by(department, statusYear) %>% 
  filter(status == "TERMINATED") %>%
  summarise(termCount = n()) %>% 
  arrange(statusYear) %>% 
  group_by(statusYear) %>% 
  arrange(statusYear)

#counting the  hiring  in each age under each year 
departHire  =  Data  %>% 
  select (department ,hireDate) %>% 
  mutate (hireYear = year(hireDate)) %>%
  group_by(department ,hireYear) %>% 
  summarise(hireCount = n())%>% 
  group_by(hireYear)


names(departTerm) =  c("department",  "Year", "termCount")
names(departHire) = c( "department",  "Year", "HireCount")


#combining  the  counts of the  hiring data and  termination  data in each year 
#by using the  year and  age  column
# Perform a left join between ageHire and ageTerm
departHireTerm <- merge(departHire, departTerm, by = c("Year" , "department"), all = TRUE) %>% 
  group_by(Year) %>% 
  filter(Year >= 2006) %>% 
  arrange(Year)
view(departHireTerm)



##removing Na values

i = 0 
for (i in 1:length(departHireTerm$termCount) ) {
  
  if (is.na(departHireTerm$HireCount[i])) {
    
    departHireTerm$HireCount[i] <- 0
  } 
  if (is.na(departHireTerm$termCount[i])) {
    
    departHireTerm$termCount[i] <- 0
  }
}

### generate a  chart 


# Create a data frame with termination and hiring counts by department



hiringChart <- ggplot(departHireTerm, aes(x = department, y = HireCount, fill = factor(Year))) +
  geom_bar(stat = "identity", position = "identity", width = 0.5) +
  labs(title = "Hiring  Counts by Department",
       x = "Department", y = "Hiring Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

termChart <- ggplot(departHireTerm, aes(x = department, y = termCount, fill = factor(Year))) +
  geom_bar(stat = "identity", position = "identity", width = 0.5)+
  labs(title = "Termination Counts by Department",
       x = "Department", y = "Termination Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

unique(departHireTerm$department)


combined_plot <- hiringChart + termChart +
  plot_layout(ncol = 2)

#reasult 
#during the  duration from 2006 to  2015  the  only  department  that 
#accepted a new  emp;oyee are the "Customer Service" ,"Dairy" ,"Bakery" 
#"Meats,"Processed Foods" , "Produce" "Recruitment" ,"Store Management"  and 
#"Training" other  department  have  a zero hiring rate  
#the  department like  customer  sercive  , Dairy and the  bakery 
#have close or  almost  balanced amount of the  registration  and the  teramoantion 
#departments like  "Meats ,"Processed Foods"  and  "Produce"   although 
#they have amount  of the  hirring  however  the amount of the terminationis higer  
#the  reamin departemt  although the  didn't  have  a new  employee they continuer  
#lossing there employees  until the  stop loosing more  empyee or  hiring  employee this  
#may indicate  that tose  department not exist any more  in the  company
# year 2014 and  2015 contribute with more  then 50% of the  termination amount amoung 
# all the  departments 
#we conclude that the un existed of the  haring amount amoung the  durtion 
#it  can be a result  seatifcation of the  employee number  or maybe decressing the  slary 
#as usuall year 2015 and 2014 is pandemic in which huge  amount  the  employees leved the  company  
# this  my caues terminations  of the some deaprtments in the  company 
#fareht  investigation  requerd 

#conslcusion the  conapny  may close  some department  due  the  lake  of the  
#man power


# View the resulting data frame






###############################################################################
#Analysis  3.6
##### the  count of the  tremination and the  hiring accourding 
#to the different  city 
#this will help the company to identify which area are  more people  are  more  
#welinig to work  with company  which not  
#result  


cityTerm  = Data %>% 
  select(city  , status, statusYear)  %>% 
  group_by(city, statusYear) %>% 
  filter(status == "TERMINATED") %>%
  summarise(termCount = n()) %>% 
  arrange(statusYear) %>% 
  group_by(statusYear) %>% 
  arrange(statusYear)
library(lubridate)
#counting the  hiring  in each age under each year 
cityHire  =  Data  %>% 
  select (city ,hireDate) %>% 
  mutate (hireYear = year(hireDate)) %>%
  group_by(city ,hireYear) %>% 
  summarise(hireCount = n())%>% 
  group_by(hireYear)


names(cityTerm) =  c("City",  "Year", "termCount")
names(cityHire) <- c( "City",  "Year", "HireCount")


#combining  the  counts of the  hiring data and  termination  data in each year 
#by using the  year and  age  column
# Perform a left join between ageHire and ageTerm
cityHireTerm <- merge(cityHire, cityTerm, by = c("Year" , "City"), all = TRUE) %>% 
  group_by(Year) %>% 
  filter(Year >= 2006) %>% 
  arrange(Year)
View(cityHireTerm)



##removing Na values

i = 0 
for (i in 1:length(cityHireTerm$termCount) ) {
  
  if (is.na(cityHireTerm$HireCount[i])) {
    
    cityHireTerm$HireCount[i] <- 0
  } 
  if (is.na(cityHireTerm$termCount[i])) {
    
    cityHireTerm$termCount[i] <- 0
  }
}

### generate a  chart 

hiringChart <- ggplot(cityHireTerm, aes(x = City, y = HireCount, fill = factor(Year))) +
  geom_bar(stat = "identity", position = "identity", width = 0.5) +
  labs(title = "Hiring  Counts by city",
       x = "city", y = "Hiring Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))

termChart <- ggplot(cityHireTerm, aes(x =  City, y = termCount, fill = factor(Year))) +
  geom_bar(stat = "identity", position = "identity", width = 0.5)+
  labs(title = "Termination Counts by city",
       x = "ctity", y = "Termination Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))


combined_plot <- hiringChart + termChart +
  plot_layout(ncol = 2)



#result  
#the  result  show  that there are a cities wiht  higher  termaintion rate  but  it have
# huge  hriing  amount those  cisties  are line   Vancouver   and the  Victoria 
#aside  from that there is also some  cities with huge  amount of the termination however 
#they have  relatively  a smallest number  of the  hirring  amount  this this may lead the 
#conapny  to close  the   store  in those coites  
#also ther  are cities with hier  amount of the tramiantion but with out hiring  amount. 


#########################################################################################
#Anaysis  3.7 
# the  count of the  termination and hiring in each job in the company? 
# find which  job may treminted the  will be with out  emaployee in the futrer
#result  

jobTerm  = Data %>% 
  select(job  , status, statusYear)  %>% 
  group_by(job, statusYear) %>% 
  filter(status == "TERMINATED") %>%
  summarise(termCount = n()) %>% 
  arrange(statusYear) %>% 
  group_by(statusYear) %>% 
  arrange(statusYear)
library(lubridate)
#counting the  hiring  in each age under each year 
jobHire  =  Data  %>% 
  select (job ,hireDate) %>% 
  mutate (hireYear = year(hireDate)) %>%
  group_by(job ,hireYear) %>% 
  summarise(hireCount = n())%>% 
  group_by(hireYear)


names(jobTerm) =  c("job",  "Year", "termCount")
names(jobHire) = c( "job",  "Year", "HireCount")


#combining  the  counts of the  hiring data and  termination  data in each year 
#by using the  year and  age  column
# Perform a left join between ageHire and ageTerm
jobHireTerm <- merge(jobHire, jobTerm, by = c("Year" , "job"), all = TRUE) %>% 
  group_by(Year) %>% 
  filter(Year >= 2006) %>% 
  arrange(Year)
View(cityHireTerm)



##removing Na values

i = 0 
for (i in 1:length(jobHireTerm$termCount) ) {
  
  if (is.na(jobHireTerm$HireCount[i])) {
    
    jobHireTerm$HireCount[i] <- 0
  } 
  if (is.na(jobHireTerm$termCount[i])) {
    
    jobHireTerm$termCount[i] <- 0
  }
}

### generate a  chart 

hiringChart <- ggplot(jobHireTerm, aes(x = job, y = HireCount, fill = factor(Year))) +
  geom_bar(stat = "identity", position = "identity", width = 0.5) +
  labs(title = "Hiring  Counts by job",
       x = "job", y = "Hiring Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))

termChart <- ggplot(jobHireTerm, aes(x = job, y = termCount, fill = factor(Year))) +
  geom_bar(stat = "identity", position = "identity", width = 0.5)+
  labs(title = "Termination Counts by job",
       x = "job", y = "Termination Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))


combined_plot <- hiringChart + termChart +
  plot_layout(ncol = 2)

#result job like  cashier  accepted many 
#employee in 2013 however  in the  next  2 year  during  2014 and  2015 
# this  job lose gereter number  of the  employee then  revived in the  2013 
#this  due to the  resigned  emplyee how  age  30 and 21  during those 
#years this  gaind by the  analysis ... that  counucted 
#in addtion to  catch there is only  5 jobs  which  accepted 
#new  employeee from 2006  to  2015 simller  to the  catcher  
#those  jobs also const of he  termination rate  however 
#less in sight way.  the there remaining  jobs not  accepted any empoyees 
#anymore  but stil terminating  some  employee  
#while  otehr  jobs stop termination activity with no  hiring  activity  during 
#the  study periods  which make  use  conclued that those  jobs  not have  
#employees anymore  
#this  will  make it necessry  for  the  company to find  
#emplyees  to cover  the  important position  to urn the  business  




#======================================================================================
#Anaysis 3.8 
#what is the  hiring  and the  termination amount  amoung  each gender group? 

genTerm  = Data %>% 
  select(gender  , status, statusYear)  %>% 
  group_by(gender, statusYear) %>% 
  filter(status == "TERMINATED") %>%
  summarise(termCount = n()) %>% 
  arrange(statusYear) %>% 
  group_by(statusYear) %>% 
  arrange(statusYear)

#counting the  hiring  in each age under each year 
genHire  =  Data  %>% 
  select (gender ,hireDate) %>% 
  mutate (hireYear = year(hireDate)) %>%
  group_by(gender ,hireYear) %>% 
  summarise(hireCount = n())%>% 
  group_by(hireYear)


names(genTerm) =  c("gender",  "Year", "termCount")
names(genHire) = c( "gender",  "Year", "HireCount")



genHireTerm <- merge(genHire, genTerm, by = c("Year" , "gender"), all = TRUE) %>% 
  group_by(Year) %>% 
  filter(Year >= 2006) %>% 
  arrange(Year)
View(genHireTerm)



##removing Na values

i = 0 
for (i in 1:length(genHireTerm$termCount) ) {
  
  if (is.na(genHireTerm$HireCount[i])) {
    
    genHireTerm$HireCount[i] <- 0
  } 
  if (is.na(genHireTerm$termCount[i])) {
    
    genHireTerm$termCount[i] <- 0
  }
}



#addtional feature    facet_grid(. ~ gender, scales = "free_y") +


# Create the combined chart with line chart in front of the bar chart

combined_chart <- ggplot(genHireTerm) +
  geom_line(aes(x = Year, y = HireCount, color = "Hirring"), size = 1.5) + 
  geom_line(aes(x = Year, y = termCount, color  = "tremination"), size = 1.5) +
  geom_point(aes(x = Year, y = HireCount, color ="Hirring"), size = 3) +
  geom_point(aes(x = Year, y = termCount, color = "tremination"), size = 3) +
  facet_grid(. ~ gender, scales = "free_y") +
  labs(title = "Hiring and Termination Counts by Gender and Year",
       x = "Year", y = "Count") +
  theme_minimal() + 
  transition_reveal(Year)


#the result  shows the both gender experss a increamntin the  hirring  
#from 2007  to  2012 with samller  pattern  and the  there was a sharp
#decrment in the  hrirng  starting from the year 2012 untill it  reach zero  
#and continue becoming  zero until 2015 
#in the  other  habd the  termiantion number  shows the  opposite approach  
#in which it  start incressing  gradually  for feamle  and  the  male  it decreed 
#and arise  agin in the  2015 
#
#===================================================================================
#Analysis  3.9 
#Q what is the amount of the  hiring the  termination  in each buiness unint? 

busTerm  = Data %>% 
  select(businessUnit  , status, statusYear)  %>% 
  group_by(businessUnit ,  statusYear) %>% 
  filter(status == "TERMINATED") %>%
  summarise(termCount = n()) %>% 
  arrange(statusYear) %>% 
  group_by(statusYear) %>% 
  arrange(statusYear)


#counting the  hiring  in each age under each year 
busHire  =  Data  %>% 
  select (businessUnit ,hireDate) %>% 
  mutate (hireYear = year(hireDate)) %>%
  group_by(businessUnit ,hireYear) %>% 
  summarise(hireCount = n())%>% 
  group_by(hireYear)


names(busTerm) =  c("businessUnit",  "Year", "termCount")
names(busHire) = c( "businessUnit",  "Year", "HireCount")

busHireTerm <- merge(busHire, busTerm, by = c("Year" , "businessUnit"), all = TRUE) %>% 
  group_by(Year) %>% 
  filter(Year >= 2006) %>% 
  arrange(Year)




#combining  the  counts of the  hiring data and  termination  data in each year 
#by using the  year and  age  column
# Perform a left join between ageHire and ageTerm







##removing Na values

i = 0 
for (i in 1:length(busHireTerm$termCount) ) {
  
  if (is.na(busHireTerm$HireCount[i])) {
    
    busHireTerm$HireCount[i] <- 0
  } 
  if (is.na(busHireTerm$termCount[i])) {
    
    busHireTerm$termCount[i] <- 0
  }
}



# Create the combined chart with line chart in front of the bar chart
  ggplot(busHireTerm) +
  geom_line(aes(x = Year, y = HireCount, color = "Hirring"), size = 1.5) + 
  geom_line(aes(x = Year, y = termCount, color  = "tremination"), size = 1.5) +
  geom_point(aes(x = Year, y = HireCount, color ="Hirring"), size = 3) +
  geom_point(aes(x = Year, y = termCount, color = "tremination"), size = 3) +
  facet_grid(. ~businessUnit, scales = "free_y") +
  labs(title = "Hiring and Termination Counts by business unit and Year",
       x = "Year", y = "Count") +
  theme_minimal() + 
  transition_reveal(Year)

#the  analysis  conscluded that the  in the  headoffice  there  wasn't  any hirring  activies  
#however there was  small increamnt  and decrement in case  of the  amount of the  tremaintion 
  #during the  period  from 2006  to  2015 which my conclude  that  there  is on 
  #HeadOffice that  not  have  any empoyee or they mignt  not  exist anymore  
  #regardding the  stores and from 2006 until 2015  the stors have a almost  constants 
  #rate  of the  hirring  activites how  ever the  hirring  amount  decress suddenly  in the  2014  
  #the termination go between in cremtn and decreamnt  but  it increases  suddnly  in the  year 2014 and2015




#==================================================================================
#Analysis 3.10
#Q what is the hiring and the  termination rate  for each year 
#from 2006 to 2015? 
  rateTerm  = Data %>% 
    select(status, statusYear)  %>% 
    group_by(statusYear) %>% 
    filter(status == "TERMINATED") %>%
    summarise(termCount = n()) 
    
  
  #counting the  hiring  in each age under each year 
rateHire  =  Data  %>% 
     select(status, hireDate)  %>% 
    mutate (hireYear = year(hireDate)) %>%
    group_by(hireYear) %>% 
    summarise(hireCount = n())%>% 
    group_by(hireYear)



names(rateTerm) =  c( "Year", "termCount")
names(rateHire) = c( "Year", "HireCount")


rateData <- merge(rateHire, rateTerm, by = "Year", all = TRUE) %>% filter(Year >= 2006)

# Calculate the average number of employees for each year
employeeCount <- Data %>%
  select( ID,statusYear)  %>% 
  group_by(statusYear) %>%
  summarise(employeeCount = n_distinct(ID)) 
names(employeeCount ) = c( "Year", "employeeCount")

rateData <- merge(rateData, employeeCount, by = "Year", all = TRUE) 



i = 0 
for (i in 1:length(rateData$termCount) ) {
  
  if (is.na(rateData$HireCount[i])) {
    
    rateData$HireCount[i] <- 0
  } 
  if (is.na(rateData$termCount[i])) {
    
    rateData$termCount[i] <- 0
  }
}

  
rateData <- rateData %>%
  mutate(HiringRate = (HireCount / employeeCount) * 100,
         TerminationRate = (termCount / employeeCount) * 100)
remover()
  
rateData_long <- tidyr::pivot_longer(rateData, cols = c(HiringRate, TerminationRate), names_to = "Category", values_to = "Rate")
#addtional featurew 
#tidyr::pivot_longer
#geom_area()

# Reshape the rateData dataframe into long format


# Create the stacked area chart
 ggplot(rateData_long, aes(x = Year, y = Rate, fill = Category)) +
  geom_area() +
  labs(title = "Hiring and Termination Rates Over the Years",
       x = "Year", y = "Rate") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal()



 ggplot(rateData_long, aes(x = Year, y = Rate, fill = Category)) +
   geom_col(position = "dodge") +
   geom_text(aes(label = round(Rate, 2)), position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
   labs(title = "Hiring and Termination Rates Over the Years",
        x = "Year", y = "Rate") +
   scale_fill_manual(values = c("blue", "red")) +
   theme_minimal()
 
 
View(Data)

#----===================================================================================
#conclusion  

#=====================================================================================

#Main  Question. 
#Which department had a significant  terminations  rate  in the last year and  why? 



#=================================================================================================

#analysis 4.1  
#what is the  termination rate in each department in the 2014 and  2015 ? 
#result 
#In 2015, the departments of Accounting, Accounts Payable, Accounts Receivable, 
#Audit, Compensation, and Investment had the highest termination rates, with a 
#termination rate of 1. This means that all employees in these departments were
#terminated during that year. Additionally, other departments like Store Management, 
#Customer Service, Meats, Dairy, Processed Foods, and Produce also experienced 
#notable termination rates ranging from 0.01532033 to 0.66666667. It is crucial 
#o address the reasons behind these high termination rates in order to understand 
#and improve employee retention in these departments.



deprtTermRate <- Data %>%
  selec(statusYear,department, status) %>%
  filter(statusYear == 2015) %>%
  group_by(department, statusYear) %>%
  summarise(
    terminationCount = sum(status == "TERMINATED"),
    employeeCount = n(),
    terminationRate = terminationCount / employeeCount,
    .groups = "drop"
  ) %>% arrange(statusYear)


View(deprtTermRate)

ggplot(deprtTermRate, aes(x = terminationRate, y = department, fill = department)) +
  geom_bar(stat = "identity") +
  labs(x = "Termination Rate", y = "Department") +
  ggtitle("Termination Rate by Department in 2015") +
  geom_text(aes(label = round(terminationRate, 2)), 
            hjust = ifelse(deprtTermRate$terminationRate >= 0.5, -0.5, 1.5),
            vjust = 0.5, color = "black", size = 3) +
  scale_fill_manual(values = scales::hue_pal()(length(unique(deprtTermRate$department)))) +
  guides(fill = guide_legend(title = "Department"))


#================================================================================
#analysis 4.2  
#what is the  total number  of the  employee in each deartment? 
#result  
#In 2015, the departments with the highest termination rates were Bakery, Customer
#Service, Dairy, Meats, Processed Foods, and Produce. These departments had a 
#significant number of total employees, ranging from 795 to 966. It is crucial 
#to investigate the reasons behind the high termination rates in these departments 
#and take necessary actions to address any underlying issues. Additionally, departments 
#like Accounting, Accounts Payable, Accounts Receivable, Audit, Compensation, Investment,
#Executive, Recruitment, and Store Management had lower termination rates, indicating 
#relatively stable employee retention.

deprtTotalEmployees <- Data %>%
  filter(statusYear == 2015) %>%
  group_by(department, statusYear ) %>%
  summarise(totalEmployees = n())
  View(deprtTotalEmployees)


ggplot(deprtTotalEmployees, aes(x = department, y = totalEmployees, fill = department)) +
  geom_bar(stat = "identity") +
  labs(x = "Department", y = "Total Employees", fill = "Department") +
  ggtitle("Total Employees by Department in 2015") +
  geom_text(aes(label = totalEmployees), vjust = -0.5, color = "black") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = rainbow(length(unique(deprtTotalEmployees$department))))



  
#===========================================================================================
#analysis 4.3
#what is the  relationship between each department  the  termination reason? 

#In 2015, the departments with the highest termination counts were Customer 
#Service, Meats, Dairy, and Processed Foods. Termination reasons varied across
#departments and included retirement, layoff, and resignation. Customer Service 
#had a significant number of terminations, mainly due to layoffs (28) and retirement
#(7). Meats and Processed Foods experienced a combination of layoffs, resignations,
#and retirements. Dairy had a notable number of layoffs (20) and resignations (6). 
#These termination reasons may indicate factors such as workforce restructuring,
#retirement eligibility, and potential issues within specific departments. Analyzing 
#the underlying causes behind terminations can help identify areas for improvement and
#potentially reduce turnover rates.



deprtTermReason <- Data %>% 
  select(department, termReason, status, statusYear) %>%
  filter(statusYear == 2015 & status == "TERMINATED") %>%
  group_by(department, termReason) %>% 
  summarise(terminationCount = n())%>% arrange(terminationCount)
View(deprtTermReason) 


ggplot(deprtTermReason, aes(x = department, y = terminationCount, color = termReason, group = termReason)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = terminationCount), vjust = -1, size = 3) +
  labs(x = "Department", y = "Termination Count", color = "Termination Reason") +
  ggtitle("Termination Count by Department and Reason in 2015") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_discrete(name = "Termination Reason")


#=================================================================================
#analysis 4.4
#what is the avrage age in each department? 
#result  

#The average age of employees in 2015 varied across departments. Customer Service 
#had the lowest average age (31.87), indicating a younger workforce. Departments
#like Accounting, Accounts Payable, and Audit had an average age of 65, suggesting 
#an older employee base. Meats had an average age of 55.83, while Produce had an
#average age of 50.42. These age differences may be related to the termination rates,
#with older departments potentially experiencing more retirement-related terminations.
#Younger departments may see turnover due to career advancement or job changes. 
#Understanding age demographics helps identify potential drivers of termination rates
#in different departments.

#addtion feature  na.rm

deprtAvgAge <- Data %>%
  select(statusYear,department, age ) %>%
  filter(statusYear == 2015) %>% 
  group_by(department) %>%
  summarise(avgAge =  round(mean(age, na.rm = TRUE), 2))


ggplot(deprtAvgAge, aes(x = department, y = avgAge, label = round(avgAge, 1))) +
  geom_line(aes(group = 1), shape = 16, size = 1, color = "steelblue") +
  geom_point(shape = 16, size = 4, color = "steelblue") +
  geom_text(vjust = -1, color = "black") +
  labs(x = "Department", y = "Average Age") +
  ggtitle("Average Age by Department in 2015") +
  theme_bw() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))



#===================================================================================
#analysis 4.5
#what is the  avrage  length of service  in each department ?

#result  
#The average service length data reveals potential correlations with termination rates. 
#Customer Service has the shortest average service length (7.56), suggesting higher turnover.
#Processed Foods and Dairy show relatively low average service lengths (9.63 and 10.96), 
#indicating possible turnover challenges. Meats and Bakery have moderate average service lengths (20.22 and 14.54).
#Accounting, Accounts Payable, Accounts Receivable, Audit, Compensation, Investment, 
#and Store Management departments have stable employment, with average service lengths of 24 to 26. 
#This data implies that departments with shorter average service lengths may experience higher termination rates,
#emphasizing the need for employee retention strategies.


deprtAvgService <- Data %>%
  select(statusYear,department , serviceLength) %>%
  filter(statusYear == 2015) %>% 
  group_by(department) %>%
  summarise(avgService = round(mean(serviceLength, na.rm = TRUE), 2))


ggplot(deprtAvgService, aes(x = department, y = avgService, fill = department)) +
  geom_bar(stat = "identity") +
  labs(x = "Department", y = "Average Length of Service") +
  ggtitle("Average Length of Service by Department") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))


#===============================================================================
##analysis 4.6 
#number of the  active employees in each department in 2015? 
#The data provided reveals the number of active employees in various departments.
#The Customer Service department has the highest number of active employees (928),
#followed by Meats (875) and Dairy (842). These departments likely play crucial roles in serving customers 
#and maintaining operational efficiency.However, it is worth noting that the Executive 
#department has the lowest number of active employees (10),
#indicating a smaller and more specialized team. The employee count in each department reflects the distribution 
#of workforce and suggests varying levels of manpower allocation. Understanding the active employee
#count can provide insights into departmental capacities and resource allocation within the organization.


departActiveEmployees <- Data %>%
  select(statusYear ,department ,  status ) %>%
  filter(statusYear == 2015, status == "ACTIVE") %>%
  group_by(department ) %>%
  summarise(activeEmployees = n())

View(departActiveEmployees)


ggplot(departActiveEmployees, aes(x = department, y = activeEmployees, fill = department)) +
  geom_bar(stat = "identity") +
  labs(x = "Department", y = "Active Employees") +
  ggtitle("Number of Active Employees by Department in 2015") +
  scale_fill_hue() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#==========================================================================================

#Main Question 5 
#Which stors had a significant  terminations  rate  in the last year and  why? 


#=================================================================================
##analyisi  5.1 
#what is the  termination rate in each  store ?  
#result 

#Based on the data provided, the important information regarding the 
#termination rates in 2015 for different stores are as follows:

#1. Store 35: Termination Rate - 46.15%
#2. Store 27: Termination Rate - 100%
#3. Store 39: Termination Rate - 100%
#4. Store 37: Termination Rate - 100%
#5. Store 23: Termination Rate - 100%
#6. Store 32: Termination Rate - 4.03%
#7. Store 46: Termination Rate - 0.84%
#8. Store 8: Termination Rate - 2.62%

#These termination rates highlight significant turnover 
##issues in Store 35, Store 27, Store 39, Store 37, and Store 23, 
#where the termination rates are 100%, indicating a high number 
#of employee departures. Store 32 also shows a relatively higher termination rate of 4.03%. 
#On the other hand, Store 46 and Store 8 have relatively 
#lower termination rates at 0.84% and 2.62% respectively.

View(storeTermRate)


storeTermRate <- Data %>%
  select(store,status, statusYear) %>% 
  filter(statusYear == 2015) %>% 
  group_by(store) %>%
  summarise(
    terminationCount = sum(status == "TERMINATED"),
    employeeCount = n(),
    terminationRate = terminationCount / employeeCount,
    .groups = "drop"
  )




storeTermRate$store <- factor(storeTermRate$store)

ggplot(storeTermRate, aes(x = terminationRate, y = store, fill = store)) +
  geom_bar(stat = "identity") +
  labs(x = "Termination Rate", y = "Stores") +
  ggtitle("Termination Rate by Stores in 2015") +
  geom_text(aes(label = round(terminationRate, 2)), 
            hjust = ifelse(terminationRate >= 0.5, -0.5, 1.5),
            vjust = 0.5, color = "black", size = 3) +
  scale_fill_manual(values = scales::hue_pal()(length(unique(storeTermRate$store)))) +
  guides(fill = guide_legend(title = "Stores")) +
  theme(axis.text.y = element_text(angle = 0, hjust = 0.5))


#=================================================================================
##analysis 5.2  
#what is the  location of the store with  tremination record in the  2015? 
#result  
#The termination counts across different cities and stores provide insights into the employment 
#landscape. Notably, White Rock's Store 39 and Princeton's Store 27 had the highest termination 
#counts (24 and 17 respectively). Multiple stores in Vancouver, including Stores 35, 42, 44, 45, and 41, 
#experienced notable termination counts (ranging from 6 to 12). Victoria's Store 37 and New Westminster's Store 21 
#also had 6 terminations each. Analyzing these patterns can help organizations address potential 
#challenges related to employee turnover and implement strategies to improve retention and
#maintain a positive work environment. Proactively addressing termination rates contributes
#to the overall success and productivity of the organization.


storeCityTerm <- Data %>%
  select(statusYear,status, city,store, ) %>%
  filter(statusYear == 2015, status == "TERMINATED") %>%
  group_by(store, city) %>%
  summarise(terminationCount = n()) %>%
  arrange(desc(terminationCount)) %>%
  select(city, store, terminationCount)


terminationRecord




ggplot(storeCityTerm, aes(x = store, y = terminationCount, fill = city)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Store", y = "Termination Count", fill = "City") +
  ggtitle("Termination Count by Store and City in 2015") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))


#=============================================================================
##analysis 5.3 
#what is the  number of the  employees in each store ? 
#result  
#Among the provided store data, Store 35 had the highest termination rate of 46.15%. Store 27
#, Store 39, Store 37, and Store 23 all had termination rates of 100%, indicating 
#significant turnover. Store 32 had a termination rate of 4.03%, suggesting a moderate 
#level of employee departures. Store 46 and Store 8 had lower termination rates of 0.84% and 2.62%
#respectively. These termination rates highlight stores with potential retention challenges 
#and the need for further analysis to address employee turnover.

storeEmployeeCount <- Data %>%
  select(statusYear,store ) %>%
  filter(statusYear == 2015) %>%
  group_by(store) %>%
  summarise(employeeCount = n())

View(storeEmployeeCount)


ggplot(storeEmployeeCount, aes(x = store, y = employeeCount, fill = store )) +
  geom_bar(stat = "identity") +
  labs(x = "Store", y = "Number of Employees") +
  ggtitle("Number of Employees in Each Store (2015)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#========================================================================================
##analysis 5.4
#what is the  numbber of the  active  employees in each  store in 2015? 

#Based on the provided data, Store 46 has the highest number of active employees (473),
#indicating a potentially stable workforce. On the other hand, Store 3 has the lowest number of active employees (13),
#suggesting a smaller workforce. The number of active employees can influence the termination rate, 
#as stores with more employees may have a higher likelihood of terminations due to various factors such as performance issues or organizational 
#changes. It is important to consider the employee count alongside the termination rate to gain insights 
#into workforce dynamics and potential factors impacting employee retention

storeActiveEmployees <- Data %>% 
  select(statusYear, status, store ) %>%
  filter(statusYear == 2015, status == "ACTIVE") %>%
  group_by(store) %>%
  summarise(activeEmployees = n())

View(storeActiveEmployees)


ggplot(storeActiveEmployees, aes(x = store, y = activeEmployees, fill = store)) +
  geom_bar(stat = "identity") +
  labs(x = "Store", y = "Active Employees") +
  ggtitle("Number of Active Employees by Store in 2015") +


#=========================================================================================
#analysis   5.5 
#what is the  relationship between the  stores and the  termiantion resons ? 
#result 
#Among the stores, Store 39 has the highest number of terminations due to Layoff (24).
#Retirement is most common in Store 15 (6) and Store 18 (3),
#while Resignation is prevalent in Store 21 (3) and Store 44 (6). 
#The impact of these termination reasons on the overall termination rate of each store can vary. 
#Stores with a higher number of Retirement terminations might experience more voluntary departures,
#potentially indicating an aging workforce or individuals reaching retirement age. 
#Layoffs may reflect organizational restructuring or economic factors.
#Resignations can suggest employee dissatisfaction or career opportunities elsewhere. 
#Analyzing the termination rates alongside the reasons provides insights into 
#the workforce dynamics and potential areas for improvement in each store.

storeTermReason <- Data %>% 
  select(store, termReason, status, statusYear)  %>% 
  filter(statusYear == 2015) %>%
  group_by(store , termReason) %>% 
  filter(status == "TERMINATED") %>% 
  summarise(terminationCount = n())




ggplot(storeTermReason, aes(x = store, y = terminationCount, color = termReason, group = termReason)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = terminationCount), vjust = -1, size = 3) +
  labs(x = "stores", y = "Termination Count", color = "Termination Reason") +
  ggtitle("Termination Count by store and Reason in 2015") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_discrete(name = "Termination Reason")





#=======================================================================================
#analysis 5.6  
# what is the  verage  langth  of the  servic in ech stroe? 
#result  
#Store 44 has the shortest service length (4.99) while Store 35 has the highest (22.54).
#Shorter service lengths may contribute to higher turnover and resignations,
#while longer service lengths indicate more experienced and committed employees,
#potentially leading to lower termination rates and higher retirement numbers. 
#The relationship between service length and termination rates offers insights 
#into employee retention and store stability.

storeAvgService <- Data %>%
  select(statusYear, store , serviceLength )  %>% 
  filter(statusYear == 2015) %>%
  group_by(store) %>%
  summarise(avgService = mean(serviceLength, na.rm = TRUE))

View(storeAvgService)


ggplot(storeAvgService, aes(x = store, y = avgService)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Store", y = "Average Length of Service") +
  ggtitle("Average Length of Service by Store") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#==========================================================================================
#conclusion 

#====================================================================================

#==============================
#======================
#============= finally  TT TT TT ==================================