#
# Clear & Set workspace/directory
#
rm(list=ls())
setwd("/Users/ibrahimradwan/Development/InstaCart")

#
# Include libs
#
library(ggplot2)
library(dplyr)
library(treemap)
library(arules)

#
# Load data files
#
aisles <- read.table("dataset/aisles.csv", header=TRUE, sep=",", quote="\"", comment.char = "")
departments <- read.table("dataset/departments.csv", header=TRUE, sep=",", quote="\"", comment.char = "")
orders <- read.table("dataset/orders.csv", header=TRUE, sep=",", quote="\"", comment.char = "")
products <- read.table("dataset/products.csv", header=TRUE, sep=",", quote="\"", comment.char = "")
order_products__prior <- read.table("dataset/order_products__prior.csv", header=TRUE, sep=",", quote="\"", comment.char = "")
order_products__train <- read.table("dataset/order_products__train.csv", header=TRUE, sep=",", quote="\"", comment.char = "")

summary(aisles)
summary(departments)
summary(orders)
summary(products)
summary(order_products__prior)
summary(order_products__train)

#
# Configs
#
options(scipen=10000)

#
# When do customers order
#
ggplot(orders, aes(x=order_hour_of_day)) + 
geom_histogram(stat="count",fill="skyblue")

#
# When do they purchase again
#
ggplot(orders, aes(x=days_since_prior_order)) + 
geom_histogram(stat="count",fill="skyblue")

#
# How many orders do people order
#
x <- count(filter(orders, eval_set=="prior"), order_number)
ggplot(x, aes(order_number, n)) + 
geom_line(color="skyblue", size=1) + 
geom_point(size=2, color="blue")

#
# How may items do people buy
#
x <- count(order_products__prior, order_id)
ggplot(x, aes(x=n)) + 
geom_histogram(stat="count",fill="skyblue")

#
# Bestsellers
#
x <- order_products__prior %>% 
       group_by(product_id) %>% 
       summarize(count = n()) %>% 
       top_n(12, wt = count) %>%
       left_join(select(products, product_id, product_name), by="product_id") %>%
       arrange(desc(count)) 

x

#
# What percentage of items bought are reordered
#
x <- order_products__prior %>% 
  group_by(reordered) %>% 
  summarize(count = n()) %>% 
  mutate(reordered = as.factor(reordered)) %>%
  mutate(proportion = count/sum(count))

x

#
# Treemaps
#
x <- products %>% group_by(department_id, aisle_id) %>% summarize(n=n())
x <- x %>% left_join(departments, by="department_id")
x <- x %>% left_join(aisles, by="aisle_id")

x2 <- order_products__prior %>% 
      group_by(product_id) %>% 
      summarize(count=n()) %>% 
      left_join(products, by="product_id") %>% 
      ungroup() %>% 
      group_by(department_id, aisle_id) %>% 
      summarize(sumcount = sum(count)) %>% 
      left_join(tmp, by = c("department_id", "aisle_id")) %>% 
      mutate(onesize = 1)

treemap(x, index=c("department","aisle"), vSize="n", title="", palette="Set3", border.col="#FFFFFF")
treemap(x2, index=c("department","aisle"), vSize="sumcount", title="", palette="Set3", border.col="#FFFFFF")

#
# Arules
#
# x <- order_products__prior %>% 
#      group_by(product_id) %>% 
#      left_join(products, by="product_id")
# 
# summary(x)
# 
# write.csv(x, file="dataset/transactions.csv")
# 
# transactions <- read.transactions("dataset/transactions.csv", format="single", sep=",", cols=c(2,6))
# 
# summary(transactions)
# 
# inspect(transactions[1:3])
# 
# groceryrules <- apriori(transactions, parameter = list(support =
#                                                          0.001, confidence = 0.25))
# inspect(groceryrules[1:60])