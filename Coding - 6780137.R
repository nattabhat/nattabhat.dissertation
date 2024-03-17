# Read the data
Data <- read.csv("dataset_589.csv")
df <- Data
message("Data Upload Complete")

message("-------------Initiating Data Cleansing-------------")
atr <- attributes(Data)
message("Type of the variable data is : ", atr[2])
str(Data)

install.packages("reshape2")
install.packages("dplyr")
install.packages("ggplot2")
library(ggplot2)
library(dplyr)
library(reshape2)

# Feature Construction
age_ranges <- c(18, 25, 30, 40, 50, Inf)
age_labels <- c("18-24", "25-29", "30-39", "40-49", "50+")

# Create a new column "age_group" with age ranges
df <- df %>%
  mutate(age_group = cut(age, breaks = age_ranges, labels = age_labels, right = FALSE))
df <- df[, c(1, 23, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)]

table(df$age_group)


# Correlation Check
df_numeric <- df[,c(1, 4, 6, 7, 10, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21)]
cor_data <- round(cor(df_numeric), 2)
melted_cormat <- melt(cor_data)

# Correlation Matrix
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkblue", high = "darkred",
                       limit = c(-1,1), name="Correlation") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  geom_text(aes(Var2, Var1, label = value),size = 2) +
  labs(x = NULL, y = NULL)

sorted_cor <- sort(cor_data[,"Customer_Satisfaction"], decreasing = TRUE)
sorted_cor

#Normality Check
variables <- c(
  "age","Purchase_Frequency", 
  "Purchased_Recommendation_Frequency", "Browsing_Frequency", 
  "Customer_Reviews_Importance", "Add_to_Cart_Browsing", 
  "Cart_Completion_Frequency", 
  "Saveforlater_Frequency", "Review_Left", "Review_Reliability", 
  "Review_Helpfulness", "Personalized_Recommendation_Frequency", 
  "Recommendation_Helpfulness", "Rating_Accuracy", 
  "Customer_Satisfaction"
)

# Set up a grid for all the plots
par(mfrow = c(5, 4), mar = c(3, 3, 2, 1))

# Loop through the variables and create histograms with normal distribution curves
for (variable in variables) {
  # Create a new plot for each variable
  hist(df[[variable]], breaks = 15, 
       main = paste("Histogram of", variable), 
       xlab = "Value", prob = TRUE)
  
  # Calculate mean and standard deviation for the variable
  mean_var <- mean(df[[variable]])
  sd_var <- sd(df[[variable]])
  
  # Overlay a normal distribution curve
  curve(dnorm(x, mean = mean_var, sd = sd_var), add = TRUE, col = "blue", lwd = 2)
}

# Reset the plotting layout
par(mfrow = c(1, 1))
message("all numeric row seems to be in the acceptable normal distribution range")



# Convert numeric columns to a factor
# List of column names to convert to factors
columns_to_convert <- c(
  "Purchase_Frequency", "Purchased_Recommendation_Frequency", "Browsing_Frequency",
  "Customer_Reviews_Importance", "Add_to_Cart_Browsing", "Cart_Completion_Frequency",
  "Saveforlater_Frequency", "Review_Left", "Review_Reliability", "Review_Helpfulness",
  "Personalized_Recommendation_Frequency", "Recommendation_Helpfulness",
  "Rating_Accuracy", "Customer_Satisfaction"
)

# Convert the specified columns to factors using lapply
df[columns_to_convert] <- lapply(df[columns_to_convert], factor)

# Check the structure of the data frame
str(df)


cat("Checking for Duplicate Recods\n")
cat("-----------------------------\n")
DistinctData <- Data %>% distinct()
cat("There are no duplicates\n")


#Missing Records
cat("Checking for Missing Recods\n")
cat("---------------------------\n")
naIndex <- is.na(Data)
naWhere <- which(naIndex)
cat("There are no missing records\n")

#Summary of Data
cat("Summary of all the attributes\n")
cat("-----------------------------\n")
summary(Data)


#Unique Value for each categorical column
cat("Distribution of Mean Gender\n")
cat("------------------------------\n")
table(Data$Gender)

cat("Distribution of Mean Purchase_Categories\n")
cat("------------------------------\n")
table(Data$Purchase_Categories)

cat("Distribution of Mean Product_Search_Method\n")
cat("------------------------------\n")
table(Data$Product_Search_Method)

cat("Distribution of Mean Search_Result_Exploration\n")
cat("------------------------------\n")
table(Data$Search_Result_Exploration)

cat("Distribution of Mean Cart_Abandonment_Factors\n")
cat("------------------------------\n")
table(Data$Cart_Abandonment_Factors)

cat("Distribution of Mean Service_Appreciation\n")
cat("------------------------------\n")
table(Data$Service_Appreciation)

cat("Distribution of Mean Improvement_Areas\n")
cat("------------------------------\n")
table(Data$Improvement_Areas)

message("-------------End of Data Cleansing-------------")
############################################################################################################################


message("-------------Initiating Data Visualisation-------------")
#Pie Charts for Categorical Columns

# age_group pie chart
AG_data <- data.frame(
  Service_Appreciation = c(
    "18-24",
    "25-29",
    "30-39",
    "40-49",
    "50+"
  ),
  Count = c( 226, 98, 144, 87, 34)
)
AG_data$Percentage <- (AG_data$Count / sum(AG_data$Count)) * 100
ggplot(AG_data, aes(x = "", y = Percentage, fill = Service_Appreciation)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = "Age Group") +
  scale_fill_manual(values = c("18-24" = "lightskyblue", "25-29" = "plum", 
                               "30-39" = "lightgreen", "40-49" = "burlywood1","50+" = "lightcoral")) +
  coord_polar(theta = "y") +
  theme_void() +
  ggtitle("Age Group Distribution")

#Gender Pie chart
gender_counts <- c(Female = 347, Male = 138, Others = 19, "Prefer not to say" = 85)
gender_percentages <- (gender_counts / sum(gender_counts)) * 100
pie_data <- data.frame(
  Gender = names(gender_counts),
  Percentage = gender_percentages
)
ggplot(pie_data, aes(x = "", y = Percentage, fill = Gender)) +
  geom_bar(stat = "identity", width = 1,  color = "white") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = "Gender") +
  scale_fill_manual(values = c("Female" = "lightskyblue", "Male" = "plum",
                               "Prefer not to say" = "lightgreen", "Others" = "lightcoral")) +
  coord_polar(theta = "y") +
  theme_void() +
  ggtitle("Gender Distribution") +
  theme(plot.title = element_text(hjust = 0.5))


# Purchase Category Pie chart
PC_data <- data.frame(
  Purchase_Categories = c(
    "Beauty and Personal Care",
    "Beauty and Personal Care;Clothing and Fashion",
    "Beauty and Personal Care;Clothing and Fashion;Home and Kitchen",
    "Beauty and Personal Care;Clothing and Fashion;others",
    "Beauty and Personal Care;Home and Kitchen",
    "Clothing and Fashion",
    "Clothing and Fashion;Home and Kitchen",
    "Clothing and Fashion;Home and Kitchen;others",
    "Clothing and Fashion;others",
    "Groceries and Gourmet Food",
    "Groceries and Gourmet Food;Beauty and Personal Care;Clothing and Fashion;Home and Kitchen",
    "Groceries and Gourmet Food;Beauty and Personal Care;Clothing and Fashion;Home and Kitchen;others",
    "Home and Kitchen",
    "Others"
  ),
  Count = c(
    105, 46, 41, 12, 21, 104, 26, 15, 10, 14, 14, 31, 23, 127
  )
)
PC_data $Percentage <- (PC_data $Count / sum(PC_data $Count)) * 100
ggplot(PC_data , aes(x = "", y = Percentage, fill = Purchase_Categories)) +
  geom_bar(stat = "identity", width = 1,  color = "white") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = "Purchase Categories") +
  scale_fill_manual(values = c("Beauty and Personal Care" = "lightskyblue",
                               "Beauty and Personal Care;Clothing and Fashion" = "plum", 
                               "Beauty and Personal Care;Clothing and Fashion;Home and Kitchen" = "lightgreen", 
                               "Beauty and Personal Care;Clothing and Fashion;others" = "aquamarine3",
                               "Beauty and Personal Care;Home and Kitchen" = "burlywood1", "Clothing and Fashion" = "darkolivegreen1",
                               "Clothing and Fashion;Home and Kitchen" = "cornsilk1", 
                               "Clothing and Fashion;Home and Kitchen;others" = "darkgoldenrod1",
                               "Groceries and Gourmet Food" = "aquamarine1", 
                               "Groceries and Gourmet Food;Beauty and Personal Care;Clothing and Fashion;Home and Kitchen" = "darkorchid1",
                               "Groceries and Gourmet Food;Beauty and Personal Care;Clothing and Fashion;Home and Kitchen;others" = "brown1",
                               "Home and Kitchen" = "darkkhaki", "Others" = "lightcoral")) +
  coord_polar(theta = "y") +
  theme_void() +
  ggtitle("Purchase Categories Distribution")


# Product_Search_Method pie chart
PSM_data <- data.frame(
  Product_Search_Method = c("Categories", "Filter", "Keyword", "Others"),
  Count = c(218, 124, 209, 36)
)
PSM_data$Percentage <- (PSM_data$Count / sum(PSM_data$Count)) * 100
ggplot(PSM_data, aes(x = "", y = Percentage, fill = Product_Search_Method)) +
  geom_bar(stat = "identity", width = 1,  color = "white") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = "Product Search Method") +
  scale_fill_manual(values = c("Categories" = "lightskyblue", 
                               "Filter" = "plum", "Keyword" = "lightgreen", "Others" = "lightcoral"))  +
  coord_polar(theta = "y") +
  theme_void() +
  ggtitle("Product Search Method Distribution")



# Search_Result_Exploration pie chart
SRE_data <- data.frame(
  Search_Result_Exploration = c("First page", "Multiple pages"),
  Count = c(154, 435)
)
SRE_data$Percentage <- (SRE_data$Count / sum(SRE_data$Count)) * 100
ggplot(SRE_data, aes(x = "", y = Percentage, fill = Search_Result_Exploration)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = "Search Result Exploration") +
  scale_fill_manual(values = c("First page" = "lightskyblue", "Multiple pages" = "plum")) +
  coord_polar(theta = "y") +
  theme_void() +
  ggtitle("Search Result Exploration Distribution")


# Cart_Abandonment_Factors pie chart
CAF_data <- data.frame(
  Cart_Abandonment_Factors = c(
    "Changed my mind or no longer need the item",
    "Found a better price elsewhere",
    "High shipping costs",
    "Others"
  ),
  Count = c(234, 252, 69, 34)
)
CAF_data$Percentage <- (CAF_data$Count / sum(CAF_data$Count)) * 100
ggplot(CAF_data, aes(x = "", y = Percentage, fill = Cart_Abandonment_Factors)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = "Cart Abandonment Factors") +
  scale_fill_manual(values = c("Changed my mind or no longer need the item" = "lightskyblue",
                               "Found a better price elsewhere" = "plum", 
                               "High shipping costs" = "lightgreen", "Others" = "lightcoral")) +
  coord_polar(theta = "y") +
  theme_void() +
  ggtitle("Cart Abandonment Factors Distribution")


# Service_Appreciation pie chart
SA_data <- data.frame(
  Service_Appreciation = c(
    "Competitive prices",
    "Product recommendations",
    "User-friendly website/app interface",
    "Wide product selection",
    "Others"
  ),
  Count = c( 180, 185, 77, 142, 5)
)
SA_data$Percentage <- (SA_data$Count / sum(SA_data$Count)) * 100
ggplot(SA_data, aes(x = "", y = Percentage, fill = Service_Appreciation)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = "Service Appreciation") +
  scale_fill_manual(values =  c("Competitive prices" = "lightskyblue", "Product recommendations" 
                                = "plum", "User-friendly website/app interface" = "lightgreen",
                                "Wide product selection" = "burlywood1", "Others" = "lightcoral")) +
  coord_polar(theta = "y") +
  theme_void() +
  ggtitle("Service Appreciation Distribution")


# Improvement_Areas pie chart
IA_data <- data.frame(
  Improvement_Areas = c(
    "Customer service responsiveness",
    "Product quality and accuracy",
    "Reducing packaging waste",
    "UI",
    "Others"
  ),
  Count = c( 211, 156, 133, 75, 13)
)
IA_data$Percentage <- (IA_data$Count / sum(IA_data$Count)) * 100
ggplot(IA_data, aes(x = "", y = Percentage, fill = Improvement_Areas)) +
  geom_bar(stat = "identity", width = 1,  color = "white") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = "Improvement Areas") +
  scale_fill_manual(values = c("Customer service responsiveness" = "lightskyblue", "Product quality and accuracy" = "plum", 
                               "Reducing packaging waste" = "lightgreen","UI" = "burlywood1", "Others" = "lightcoral")) +
  coord_polar(theta = "y") +
  theme_void() +
  ggtitle("Improvement Areas Distribution")


# age_group and Purchase_Frequency
ggplot(df, aes(x = age_group, fill = Purchase_Frequency)) +
  geom_bar(position = "dodge") +
  labs(x = "age group", y = "count", fill = "Purchase Frequency") +
  geom_text(stat = 'count', aes(label = paste0(sprintf("%.1f", after_stat(count / sum(count) * 100)), "%")),
            position = position_dodge(width = 0.85), vjust = -0.2) +
  scale_fill_manual(values = c("darkolivegreen1", "lightgoldenrod1", "orange1", "lightcoral", "deeppink4")) +
  theme(legend.position = c(0.98, 0.98), legend.justification = c(1, 1), plot.title = element_text(hjust = 0.5)) +
  ggtitle("Purchase Frequency by age group")

#age_group and Browsing_Frequency
ggplot(df, aes(x = age_group, fill = Browsing_Frequency)) +
  geom_bar(position = "dodge") +
  labs(x = "age group", y = "count", fill = "Browsing_Frequency") +
  geom_text(stat = 'count', aes(label = paste0(sprintf("%.1f", after_stat(count / sum(count) * 100)), "%")), 
            position = position_dodge(width = 0.85), vjust = -0.2) +
  scale_fill_manual(values = c("darkolivegreen1", "lightgoldenrod1", "orange1", "lightcoral", "deeppink4")) +
  theme(legend.position = c(0.98, 0.98), legend.justification = c(1, 1), plot.title = element_text(hjust = 0.5)) +
  ggtitle("Browsing_Frequency by age group")

#age_group and Purchase_Categories
ggplot(df, aes(x = Purchase_Categories, fill = age_group)) +
  geom_bar(position = "dodge") +
  labs(x = "Purchase_Categories", y = "count", fill = "age_group") +
  geom_text(stat = 'count', aes(label = paste0(sprintf("%.1f", after_stat(count / sum(count) * 100)), "%")),
            position = position_dodge(width = 0.85), vjust = -0.2, size = 2.5) +
  scale_fill_manual(values = c("darkolivegreen1", "lightgoldenrod1", "orange1", "lightcoral", "deeppink4")) +
  theme(legend.position = c(0.98, 0.98), legend.justification = c(0.5, 0.5), plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
  ggtitle("Purchase Categories by age group") 

#age_group and Customer_Satisfaction
ggplot(df, aes(x = age_group, fill = Customer_Satisfaction)) +
  geom_bar(position = "dodge") +
  labs(x = "age group", y = "count", fill = "Customer_Satisfaction") +
  geom_text(stat = 'count', aes(label = paste0(sprintf("%.1f", after_stat(count / sum(count) * 100)), "%")), 
            position = position_dodge(width = 0.85), vjust = -0.2) +
  scale_fill_manual(values = c("darkolivegreen1", "lightgoldenrod1", "orange1", "lightcoral", "deeppink4")) +
  theme(legend.position = c(0.98, 0.98), legend.justification = c(1, 1), plot.title = element_text(hjust = 0.5)) +
  ggtitle("Customer_Satisfaction by age group")

#age_group and Improvement_Areas
ggplot(df, aes(x = Improvement_Areas, fill = age_group)) +
  geom_bar(position = "dodge") +
  labs(x = "Improvement_Areas", y = "count", fill = "age_group") +
  geom_text(stat = 'count', aes(label = paste0(sprintf("%.1f", after_stat(count / sum(count) * 100)), "%")), 
            position = position_dodge(width = 0.85), vjust = -0.2) +
  scale_fill_manual(values = c("darkolivegreen1", "lightgoldenrod1", "orange1", "lightcoral", "deeppink4")) +
  theme(legend.position = c(0.98, 0.98), legend.justification = c(1, 1), plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
  ggtitle("Improvement_Areas by age group")

#age_group and Service_Appreciation
ggplot(df, aes(x = Service_Appreciation, fill = age_group)) +
  geom_bar(position = "dodge") +
  labs(x = "Service_Appreciation", y = "count", fill = "age_group") +
  geom_text(stat = 'count', aes(label = paste0(sprintf("%.1f", after_stat(count / sum(count) * 100)), "%")),
            position = position_dodge(width = 0.85), vjust = -0.2) +
  scale_fill_manual(values = c("darkolivegreen1", "lightgoldenrod1", "orange1", "lightcoral", "deeppink4")) +
  theme(legend.position = c(0.98, 0.98), legend.justification = c(1, 1), plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
  ggtitle("Service_Appreciation by age group")


message("-------------End of Data Visualisation-------------")
############################################################################################################################

