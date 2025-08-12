library(rio)
library(dplyr)
library(ggplot2)

info <- import("C:/Users/adeve/OneDrive/Documents/R/R Projects/Work from anywhere analysis using r/Work_From_Anywhere_Salary_Data.csv")

head(info)
str(info)
summary(info)

# Average Salary of the Employee
avg_salary <- mean(info$`Salary (Annual)`)
print(avg_salary)

# Average Experience of the Employee
avg_exp <- mean(info$`Years of Experience`)
paste(avg_exp,"Years",sep = " ")

# Average Job Satisfaction Score
avg_job_satisfaction <- mean(info$`Job Satisfaction Score (1-10)`)
print(avg_job_satisfaction)

# Average time since last Promotion
since_last_promotion <- mean(info$`Last Promotion (Years Ago)`)
paste(round(since_last_promotion,2),"Years",sep = " ")

# Maximum Salary of Employee
max_sal <- max(info$`Salary (Annual)`)
print(max_sal)

# Minimum Salary of Employee
min_sal <- min(info$`Salary (Annual)`)
print(min_sal)

# Line plot of the Perks given to Employee by its Count
perk_counts <- info %>%
  group_by(Perks) %>%
  summarise(Count = n())
print(perk_counts)

ggplot(perk_counts, aes(x = Perks, y = Count, group = 1)) +
  geom_line(color = "blue") +
  geom_point(size = 3, color = "red") +
  labs(title = "Perks Given to Employees",
       x = "Perk",
       y = "Count of Employees") +
  theme_minimal()


# Line plot of Employement Type by their Count
emp_count <- info %>%
  group_by(`Employment Type`) %>%
  summarise(count = n())
print(emp_count)

ggplot(emp_count, aes(x = `Employment Type`, y = count, group = 1)) +
  geom_line(color = "blue")+
  geom_point(size = 3, color = "red")+
  labs(title = "Employement Type",
       x = "Employement Type",
       y = "No of Employees")+
  theme_minimal()

# Line Plot of Company by No of Employee
no_of_emp_by_comp <- info %>%
  group_by(`Company`)%>%
  summarise(Count = n())
print(no_of_emp_by_comp)

ggplot(no_of_emp_by_comp, aes(x = Company, y = Count, group = 1)) +
  geom_line(color = "blue")+
  geom_point(size = 3, color = "red")+
  labs(title = "No of Employees by Company",
       x = "Company",
       y = "No of Employee")+
  theme_minimal()

# Bar Plot of Industry by its Count
ggplot(info, aes(x = Industry))+
  geom_bar(fill = "skyblue")+
  labs(
    title = "Industry Count",
    x = "Industry",
    y = "Count"
  ) +
  theme_minimal()

# Bar Plot of Tech Stack by their Count
ggplot(info, aes(x = `Tech Stack`))+
  geom_bar(fill = "green")+
  labs(
    title = "Tech Stack",
    x = "Tech Stack",
    y = "Count of Stack"
  )+
  theme_minimal()

# Pie Chart of Average Satisfaction Score by Remote Flexibility
avg_job_flex <- info %>%
  group_by(`Remote Flexibility`)%>%
  summarise(avg_satisfaction = mean(`Job Satisfaction Score (1-10)`))
print(avg_job_flex)

ggplot(avg_job_flex, aes(x = "", y = avg_satisfaction, fill = `Remote Flexibility`))+
  geom_bar(stat = "identity",width = 1)+
  coord_polar(theta = "y")+
  labs(
    tilte = "Avg Satisfaction Score by Remote Flexibility",
  )

# Line plot of Job Titles and their Avg Salaries
avg_sal_job_title <- info %>%
  group_by(`Job Title`) %>%
  summarise(avg_sal = mean(`Salary (Annual)`))
print(avg_sal_job_title)

ggplot(avg_sal_job_title, aes(x = `Job Title`,y = avg_sal, group = 1))+
  geom_line(color = "blue")+
  geom_point(size = 3, color = 'red')+
  labs(
    title = "Average Salary by Job Title",
    x = "Job Title",
    y = "Avg Sal"
  )+
  theme_minimal()

# Remote Flexibility Count
remote_flex <- info %>% 
  group_by(`Remote Flexibility`)%>%
  summarise(count = n())
print(remote_flex)

