library(data.table)
library(dplyr)
library(tidyr)

#Load Data
aisles <- fread("Downloads/instacart/aisles.csv")
departments <- fread("Downloads/instacart/departments.csv")
order_prior <- fread("Downloads/instacart/order_products__prior.csv")
order_train <- fread("Downloads/instacart/order_products__train.csv")
orders <- fread("Downloads/instacart/orders.csv")
products <- fread("Downloads/instacart/products.csv")

#Factoring
aisles$aisle <- as.factor(aisles$aisle)
departments$department <- as.factor(departments$department)
orders$eval_set <- as.factor(orders$eval_set)
products$product_name <- as.factor(products$product_name)

#Inner_Join Products and Departments
products <- products %>% 
  inner_join(aisles) %>% inner_join(departments) %>% 
  select(-aisle_id, -department_id)
rm(aisles, departments)

#Adding user_id in train dataset w.r.t. matching order_ids in train and order dataset
order_train$user_id <- orders$user_id[match(order_train$order_id, orders$order_id)]

#Inner join prior and recent orders by order_id
orders_products <- orders %>% inner_join(order_prior, by = "order_id")

# Products
#Sort (i) user_id (ii) order_number and then (iii) product_id in ascending
#Create groups on basis of user_id and product_id
#Create a new column with sequential numbering w.r.t. grouping of user_id and product_id
prd <- orders_products %>%
  arrange(user_id, order_number, product_id) %>%
  group_by(user_id, product_id) %>%
  mutate(product_time = row_number())%>%
  #mutate(mean_add_to_cart = mean(add_to_cart_order))%>%
  ungroup() %>%
  group_by(product_id) %>%
  summarise(
    prod_orders = n(),
    prod_reorders = sum(reordered),
    #proportion_of_total_order = prod_orders / prod_reorders,
    prod_first_orders = sum(product_time == 1),
    prod_second_orders = sum(product_time == 2),
    prod_third_orders = sum(product_time == 3),
    #Summing according to the day of the week the reorder number
    prod_dow_0 = sum(reordered & order_dow == 0),
    prod_dow_1 = sum(reordered & order_dow == 1),
    prod_dow_2 = sum(reordered & order_dow == 2),
    prod_dow_3 = sum(reordered & order_dow == 3),
    prod_dow_4 = sum(reordered & order_dow == 4),
    prod_dow_5 = sum(reordered & order_dow == 5),
    prod_dow_6 = sum(reordered & order_dow == 6),
    #Reorder number weekly, biweekly and monthly
    prod_reweekly = sum(reordered & days_since_prior_order <= 6, NA, na.rm = TRUE),
    prod_rebiweekly = sum(reordered & (days_since_prior_order >= 7 & days_since_prior_order <= 14), NA, na.rm = TRUE),
    prod_remonthly = sum(reordered & (days_since_prior_order >= 15 & days_since_prior_order <= 31), NA, na.rm = TRUE)  
  )

#Calculating Product Reorder Probability
prd$prod_reorder_probability_21 <- prd$prod_second_orders / prd$prod_first_orders
prd$prod_reorder_probability_32 <- prd$prod_third_orders / prd$prod_second_orders
prd$prod_reorder_probability <- (prd$prod_reorder_probability_21 + 
                                   prd$prod_reorder_probability_32) / 2

#How many times the product was reordered wrt the first one
#prd$prod_reorder_times <- prd$prod_reorders / prd$prod_first_orders
#Probabilty of the reodering of the product overall
prd$prod_reorder_ratio <- prd$prod_reorders / prd$prod_orders

#Filter Columns
prd <- prd %>% select(-prod_reorders, -prod_first_orders, -prod_second_orders)
#-prd$prod_reorder_probability_21, -prd$prod_reorder_probability_32)
prd <- prd %>% select(-prod_third_orders)
#prd <- prd %>% select(-prd$prod_reorder_probability_21)
prd <- prd[, -13]
prd <- prd[, -13]


#Getting mean of add_to_cart_order
#prd_add_to_cart_order <- orders_products %>%
# arrange(user_id, order_number, product_id) %>%
#group_by(user_id, product_id) %>%
#mutate(product_time = row_number())%>%
#summarise(
#usr_mean_add_to_cart = mean(add_to_cart_order),
# usr_mode_add_to_cart = getmode(add_to_cart_order)
#)

prd_usr <- orders_products %>%
  arrange(user_id, order_number, product_id) %>%
  group_by(user_id, product_id) %>%
  mutate(usr_prod_prev_order = order_number - lag(order_number, default=first(order_number)))

prd_usr <- prd_usr[, -c(3, 4, 5, 6, 7, 8, 9, 10)]


##Mode Function
getmode <- function(v) {
  v_t <- subset(v, v != "NA")
  uniqv <- unique(v_t)
  uniqv[which.max(tabulate(match(v_t, uniqv)))]
}


##USERS ORDER_____________
#From the orders table takes just rows with eval_set == prior and then groups by user_id
#and the find out the total number of days prior of the order and their mean
users <- orders %>%
  filter(eval_set == "prior") %>%
  group_by(user_id) %>%
  summarise(
    user_orders = max(order_number),
    #user_period = sum(days_since_prior_order, na.rm = T),
    user_mean_days_since_prior = mean(days_since_prior_order, na.rm = T),
    user_mode_days_since_prior = getmode(days_since_prior_order),
    user_mode_dow = getmode(order_dow)
  )


#Feature - DOW HOD
orders_products$order_dow_hod <- orders_products$order_dow * 24 + orders_products$order_hour_of_day
#This takes the orders_product table, groups_by the user_id problem and creates columns
#counting total user products ordered, then the probability of the user reordering and total
#distince products ordered by the user
us <- orders_products %>%
  group_by(user_id) %>%
  summarise(
    user_total_products = n(),
    #sum(reordered == 1) / user_total_products
    Proportion_Reorder = sum(reordered == 1) / user_total_products,
    #user_reorder_ratio = sum(reordered == 1) / sum(order_number > 1),
    user_distinct_products = n_distinct(product_id),
    user_mean_dow_hod = mean(order_dow_hod)
  )


users <- users %>% inner_join(us)

us <- orders %>%
  filter(eval_set != "prior") %>%
  select(user_id, order_id, eval_set,
         time_since_last_order = days_since_prior_order)

users <- users %>% inner_join(us)
# Database ----------------------------------------------------------------
data <- orders_products %>%
  group_by(user_id, product_id) %>% 
  summarise(
    tot_usr_prd_orders = n(),
    #usr_prd_first_order = min(order_number),
    #usr_prd_last_order = max(order_number),
    usr_prd_mean_add_to_cart_order = mean(add_to_cart_order),
    usr_prd_mode_add_to_cart_order = getmode(add_to_cart_order),
    usr_prd_reorder = sum(reordered)
  )

data <- data %>% 
  inner_join(prd, by = "product_id") %>%
  inner_join(users, by = "user_id")

data$usr_prd_reorder_rate <- data$tot_usr_prd_orders / data$user_orders  
data$usr_prd_reorder_rate <- data$usr_prd_reorder / data$user_orders
#data$usr_prd_orders_since_last_order <- data$user_orders - data$usr_prd_last_order
#data$usr_prd_order_rate_since_first_order <- data$tot_usr_prd_orders / 
#                                      (data$user_orders - data$usr_prd_first_order + 1)

data <- data %>% 
  left_join(order_train %>% select(user_id, product_id, reordered), 
            by = c("user_id", "product_id"))

write.csv(data, file = "/Users/akshaytambe/Downloads/instacart_raj.csv", row.names = F)
data <- fread("/Users/akshaytambe/Downloads/instacart_raj.csv")
str(data)

# Train / Test datasets ---------------------------------------------------
train <- as.data.frame(data[data$eval_set == "train",])
train$eval_set <- NULL
train$user_id <- NULL
train$product_id <- NULL
train$order_id <- NULL
train$reordered[is.na(train$reordered)] <- 0

test <- as.data.frame(data[data$eval_set == "test",])
test$eval_set <- NULL
test$user_id <- NULL
test$reordered <- NULL

library(xgboost)

params <- list(
  "objective"           = "reg:logistic",
  "eval_metric"         = "logloss",
  "eta"                 = 0.2,
  "max_depth"           = 6,
  "min_child_weight"    = 10,
  "gamma"               = 0.70,
  "subsample"           = 0.76,
  "colsample_bytree"    = 0.95,
  "alpha"               = 2e-05,
  "lambda"              = 10
)
help(sample_frac)
subtrain <- train %>% sample_frac(0.3)
X <- xgb.DMatrix(as.matrix(subtrain %>% select(-reordered)), label = subtrain$reordered)
model <- xgboost(data = X, params = params, nrounds = 30)

importance <- xgb.importance(colnames(X), model = model)
xgb.ggplot.importance(importance, top_n = 15)

rm(X, importance, subtrain)
gc()

# Apply model -------------------------------------------------------------
X <- xgb.DMatrix(as.matrix(test %>% select(-order_id, -product_id)))

#Prediction on Test Data for items which can be reordered or not
test$reordered <- predict(model, X)

#Filter on Probabibility
test$reordered <- (test$reordered > 0.21) * 1

#Filter only reordered items, Groups Products by Order ID
submission <- test %>%
  filter(reordered == 1) %>%
  group_by(order_id)

#Read Products File For Getting Product Names (Left Join Our Final File with ProductID)
products <- fread("Downloads/instacart/products.csv")
newsubmission <- submission %>% 
  group_by(order_id) %>% 
  left_join(products,by="product_id")

#Subset only OrderID and ProductName
newsubmission1 <- subset(newsubmission, select=c(order_id, product_name))
write.csv(newsubmission1, file = "Downloads/instacart/Manjiri_ICPN_output.csv", row.names = F)

#Install arules and use for MBA
require(arules)
require("arulesViz")
manjiridata <- fread("Downloads/instacart/Manjiri_ICPN_output.csv")

#Remove OrderID for MBA
manjiridata <- manjiridata %>% select(-order_id)
write.csv(manjiridata, file = "Downloads/instacart/Manjiri_ICPN1COL_output.csv", row.names = F)

#Transactions Reads the data in the form of items and frequency
transactions<-read.transactions("Downloads/instacart/Manjiri_ICPN_output.csv", format = "single", sep = ",",cols = c(1,2))

summary(transactions)

#Finalizing Strong Association Rules

#For Bananas-------------------------------------------
#Runs apriori algorithm for rules
groceryrules <- apriori(transactions, parameter = list(support = 0.001, confidence = 0.520, minlen=2, maxlen=3),appearance = list (lhs= c("Broccoli Crown", "Bartlett Pears", "Organic Fuji Apple", "Strawberries", "Honeycrisp Apple", "Organic Avocado", "Cucumber Kirby"),rhs="Banana"), control = list (verbose=F))

#Gives Number of Total Rules
summary(groceryrules)
#Sort By Confidence
inspect(sort(groceryrules, by="confidence")[1:20])

#Removing Redundant Rules
#get subset rules in vector
subsetRules <- which(colSums(is.subset(groceryrules, groceryrules)) > 1) 
length(subsetRules)
# remove subset rules. 
finalgroceryrules <- groceryrules[-subsetRules] 
inspect(sort(finalgroceryrules, by="support")[1:9])
plot(finalgroceryrules, method = "graph", interactive = T)

#For Organic Strawberries-------------------------------------------
#Runs apriori algorithm for rules
groceryrules <- apriori(transactions, parameter = list(support = 0.001, confidence = 0.580, minlen=2, maxlen=4),appearance = list (lhs = c("Bag of Organic Bananas", "Organic Raspberries", "Organic Kiwi", "Organic Hass Avocado", "Apple Honeycrisp Organic", "Organic Baby Spinach", "Organic Cucumber"),rhs="Organic Strawberries"), control = list (verbose=F))

#Gives Number of Total Rules
summary(groceryrules)
#Sort By Confidence
inspect(sort(groceryrules, by="confidence")[1:10])

#Removing Redundant Rules
#get subset rules in vector
subsetRules <- which(colSums(is.subset(groceryrules, groceryrules)) > 1) # get subset rules in vector
length(subsetRules)
# remove subset rules. 
finalgroceryrules <- groceryrules[-subsetRules]
inspect(sort(finalgroceryrules, by="confidence")[1:7])
plot(finalgroceryrules, method = "graph", interactive = T)

#For Organic Bag of Bananas-------------------------------------------
#Runs apriori algorithm for rules
groceryrules <- apriori(transactions, parameter = list(support = 0.001, confidence = 0.62, minlen=1, maxlen=3),appearance = list (lhs = c("Organic Navel Orange","Organic Raspberries","Organic Hass Avocado","Organic Kiwi","Organic Lemon","Organic Large Extra Fancy Fuji Apple"),rhs="Bag of Organic Bananas"), control = list (verbose=F))

#Gives Number of Total Rules
summary(groceryrules)
#Sort By Confidence
inspect(sort(groceryrules, by="confidence")[1:10])

#Removing Redundant Rules
#get subset rules in vector
subsetRules <- which(colSums(is.subset(groceryrules, groceryrules)) > 1)
length(subsetRules)
# remove subset rules. 
finalgroceryrules <- groceryrules[-subsetRules]
inspect(sort(finalgroceryrules, by="confidence")[1:5])
plot(finalgroceryrules, method = "graph", control = list(type="items"))
plot(finalgroceryrules, method = "graph", interactive = T)

#Sparkling Water Grapefruit-------------------------------------------
groceryrules <- apriori(transactions, parameter = list(support = 0.001, confidence = 0.20, minlen=2, maxlen=3),appearance = list (default="lhs",rhs="Sparkling Water Grapefruit"), control = list (verbose=F))

summary(groceryrules)
inspect(sort(groceryrules, by="confidence")[1:10])

subsetRules <- which(colSums(is.subset(groceryrules, groceryrules)) > 1) # get subset rules in vector
length(subsetRules)
finalgroceryrules <- groceryrules[-subsetRules] # remove subset rules. 
inspect(sort(finalgroceryrules, by="confidence")[1:5])
