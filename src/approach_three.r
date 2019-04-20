#
# Clear
#
rm(list=ls())
setwd("/Users/ibrahimradwan/Development/InstaCartMarketBasketAnalysis/src/")

#
# Include libs
#
library(ggplot2)
library(dplyr)
library(tidyr)
library(treemap)
library(arules)
library(NbClust)
library(HSAUR)
library(cluster)
library(fpc)
library(clue)

#
# Load data files
#
aisles <- read.table("../dataset/aisles.csv", header=TRUE, sep=",", quote="\"", comment.char = "")
departments <- read.table("../dataset/departments.csv", header=TRUE, sep=",", quote="\"", comment.char = "")
orders <- read.table("../dataset/orders.csv", header=TRUE, sep=",", quote="\"", comment.char = "")
products <- read.table("../dataset/products.csv", header=TRUE, sep=",", quote="\"", comment.char = "")
order_products__prior <- read.table("../dataset/order_products__prior.csv", header=TRUE, sep=",", quote="\"", comment.char = "")
order_products__train <- read.table("../dataset/order_products__train.csv", header=TRUE, sep=",", quote="\"", comment.char = "")

test_orders <- filter(orders, eval_set=="test")
prior_orders <- filter(orders, eval_set=="prior")

reordered_products <- filter(order_products__prior, reordered==1) %>% 
                      group_by(product_id) %>% 
                      summarize(reorder_count=n()) %>% 
                      left_join(products, by="product_id") %>%
                      arrange(desc(reorder_count)) 

#
# Clustering
# 
prior_orders_clustring <- prior_orders %>% 
                          select(order_dow, order_hour_of_day, days_since_prior_order)
                         
levels(prior_orders_clustring$days_since_prior_order) <- c(levels(prior_orders_clustring$days_since_prior_order), -1)

prior_orders_clustring <- prior_orders_clustring %>%
                          mutate(days_since_prior_order=replace(days_since_prior_order, is.na(days_since_prior_order), -1))

clusters <- kmeans(prior_orders_clustring, 4)

prior_orders["cluster"] <- as.factor(clusters$cluster)

# predict test clusters
test_orders_clustering <- test_orders %>% 
                          select(order_dow, order_hour_of_day, days_since_prior_order)
test_orders_clustering_classes <- cl_predict(clusters, test_orders_clustering)
test_orders["cluster"] <- as.factor(test_orders_clustering_classes)

prior_orders_clustring <- filter(orders, eval_set=="prior") %>% 
                          select(order_dow, order_hour_of_day, days_since_prior_order) %>%
                          mutate(days_since_prior_order=replace(days_since_prior_order, is.na(days_since_prior_order), -1))

# Get top products per cluster
clusters_products <- prior_orders %>% 
                     left_join(order_products__prior, by="order_id") %>%
                     group_by(cluster)

clusters_products2 <- clusters_products %>%
                      select(cluster, product_id) %>%
                      group_by(cluster, product_id) %>%
                      summarize(cluster_products_count=n())

clusters_products3 <- clusters_products %>%
                      summarize(cluster_total_products_count=n())

clusters_products <- clusters_products2 %>% 
                     right_join(clusters_products3)

# User average order products count
user_avg_products_count <- filter(orders, eval_set=="prior") %>% 
                           left_join(order_products__prior, by="order_id") %>%
                           group_by(user_id, order_id) %>%
                           summarize(order_products_count=n()) %>%
                           group_by(user_id) %>%
                           summarize(products_count=as.integer(mean(order_products_count, na.rm=TRUE))) %>%
                           select(user_id, products_count)

# User reorder rate for his previously ordered products
user_products_reorder_count <- filter(orders, eval_set=="prior")  %>% 
                               left_join(order_products__prior, by="order_id") %>%
                               group_by(user_id, product_id) %>%
                               summarize(product_reorder_count=n(), product_add_to_cart_order_avg=as.integer(mean(add_to_cart_order, na.rm=TRUE)))

users_prior_products <- left_join(test_orders, prior_orders, by="user_id") %>%
                        left_join(user_avg_products_count, by="user_id") %>%
                        left_join(order_products__prior, by=c("order_id.y" = "order_id")) %>%
                        inner_join(user_products_reorder_count) %>%
                        left_join(clusters_products, by=c("product_id")) %>%
                        inner_join(reordered_products, by="product_id") %>%
                        mutate(days_since_prior_order.x=replace(days_since_prior_order.x,is.na(days_since_prior_order.x), 0)) %>%
                        mutate(days_since_prior_order.y=replace(days_since_prior_order.y,is.na(days_since_prior_order.y), 0)) %>%
                        mutate(reorder_count=ifelse(abs(days_since_prior_order.x - days_since_prior_order.y) <= 5, 2, 1)) %>%
                        mutate(reorder_count=as.integer(reorder_count * product_reorder_count)) %>%
                        mutate(reorder_count=reorder_count) %>%
                        mutate(reorder_count=reorder_count + 100 * cluster_products_count / cluster_total_products_count) %>%
                        select(order_id.x, product_id, reorder_count, products_count) %>%
                        rename(order_id=order_id.x) %>%
                        group_by(order_id) %>%
                        arrange(order_id, desc(reorder_count)) %>%
                        distinct(product_id, .keep_all = TRUE) %>%
                        slice(1:products_count[1]) %>%
                        select(order_id, product_id) %>%
                        mutate(product_id = paste(product_id, collapse = " ")) %>%
                        distinct(order_id, .keep_all = TRUE) %>%
                        rename(products=product_id)

users_prior_products <- users_prior_products[c("order_id", "products")]

write.csv(users_prior_products, file="../dataset/submission.csv", row.names=FALSE, quote=FALSE)                     
