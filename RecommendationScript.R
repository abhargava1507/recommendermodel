#loading the libraries

require(dplyr)
library(recommenderlab)
library(reshape2)
library(ggplot2)
library(readxl)
library(writexl)
require(kableExtra)

#loading all the files

aisles_data <- read.csv("./Downloads/instacart-market-basket-analysis/aisles.csv")
departments_data <- read.csv("./Downloads/instacart-market-basket-analysis/departments.csv")
order_products_prior_data <- read.csv("./Downloads/instacart-market-basket-analysis/order_products__prior.csv")
order_products_train_data <- read.csv("./Downloads/instacart-market-basket-analysis/order_products__train.csv")
orders_data <- read.csv("./Downloads/instacart-market-basket-analysis/orders.csv")
products_data <- read.csv("./Downloads/instacart-market-basket-analysis/products.csv")

#products low sold 
non_ordered_products_prior <- order_products_prior_data %>%
  select(1:4) %>%
  filter(reordered == 0)
non_ordered_products_train <- order_products_train_data %>%
  select(1:4) %>%
  filter(reordered == 0)

non_ordered_products <- rbind(non_ordered_products_prior, non_ordered_products_train)

low_volume <- non_ordered_products %>%
  select(1:4) %>%
  group_by(product_id) %>%
  summarise(count  = n()) %>%
  filter(count <=1)

low_volume_products <- merge(low_volume, products)[3]

#considering only the products that have been reordered
reordered_products_prior <- order_products_prior_data %>%
  select(1:4) %>%
  filter(reordered == 1)
reordered_products_train <- order_products_train_data %>%
  select(1:4) %>%
  filter(reordered == 1)

reordered_products <- rbind(reordered_products_prior, reordered_products_train)

#merging the datasets together

merged_dataset <- merge(reordered_products, products, by = "product_id")

#keeping only the columns necessary for further analysis
dataset_cleaned <- merged_dataset[1:4]

#calculating the high volume products ; products that have been purchased five times or more
high_volume <- dataset_cleaned %>%
  select(1:4) %>%
  group_by(product_id) %>%
  summarize(Total_Count = n()) %>%
  filter(Total_Count >4)

high_volume_products <- high_volume[1]

#merging the high volume products with rest of the dataset
updated_dataset_cleaned <- merge(dataset_cleaned,high_volume_products)

#merging the orders dataset  
complete_dataset <- merge(updated_dataset_cleaned,orders)

#keeping only the user_iD and product_ID and add_to_cart_order columns for further anlaysis
analysis_data <- complete_dataset[c("user_id","product_id", "add_to_cart_order")]

#calcualting the number of times a product is purchased by a particular user
recommender_database <- analysis_data %>%
  group_by(user_id, product_id, add_to_cart_order) %>%
  summarise(count = n())
mean(recommender_database$count)


#keeping the records where userspurchased a product 5 times or more
updated_database_recommender <- database_recommender %>%
  filter(count>4)

updated_database_recommender$score <- ifelse(updated_database_recommender$add_to_cart_order == 1, updated_database_recommender$count*1,
                                             ifelse(updated_database_recommender$add_to_cart_order == 2, updated_database_recommender$count*0.95,
                                                    ifelse(updated_database_recommender$add_to_cart_order == 3, updated_database_recommender$count*0.9,
                                                           ifelse(updated_database_recommender$add_to_cart_order == 4, updated_database_recommender$count*0.85,
                                                                  ifelse(updated_database_recommender$add_to_cart_order == 5, updated_database_recommender$count*0.8,
                                                                         ifelse(updated_database_recommender$add_to_cart_order == 6, updated_database_recommender$count*0.75,
                                                                                ifelse(updated_database_recommender$add_to_cart_order == 7, updated_database_recommender$count*0.7,
                                                                                       ifelse(updated_database_recommender$add_to_cart_order == 8, updated_database_recommender$count*0.65,
                                                                                              ifelse(updated_database_recommender$add_to_cart_order == 9, updated_database_recommender$count*0.6,
                                                                                                     ifelse(updated_database_recommender$add_to_cart_order == 10, updated_database_recommender$count*0.55,
                                                                                                            ifelse(updated_database_recommender$add_to_cart_order == 11, updated_database_recommender$count*0.5,
                                                                                                                   ifelse(updated_database_recommender$add_to_cart_order == 12, updated_database_recommender$count*0.45,
                                                                                                                          ifelse(updated_database_recommender$add_to_cart_order == 13, updated_database_recommender$count*0.4,
                                                                                                                                 ifelse(updated_database_recommender$add_to_cart_order == 14, updated_database_recommender$count*0.35,
                                                                                                                                        ifelse(updated_database_recommender$add_to_cart_order == 15, updated_database_recommender$count*0.3,
                                                                                                                                               ifelse(updated_database_recommender$add_to_cart_order == 16, updated_database_recommender$count*0.25,
                                                                                                                                                      ifelse(updated_database_recommender$add_to_cart_order == 17, updated_database_recommender$count*0.2,
                                                                                                                                                             ifelse(updated_database_recommender$add_to_cart_order == 18, updated_database_recommender$count*.15,
                                                                                                                                                                    ifelse(updated_database_recommender$add_to_cart_order == 19, updated_database_recommender$count*0.1, updated_database_recommender$count*0.05)))))))))))))))))))
                                                                                                                                                                        

mean(updated_database_recommender$score)
#rearranging the columns
scored_database <- updated_database_recommender[c(1,2,5)] %>%
  filter(score>4.99)

scored_dataframe <- as.data.frame(scored_database)

scored_dataframe <- transform(scored_dataframe, score = (scored_dataframe$score - min(scored_dataframe$score))/(max(scored_dataframe$score) - min(scored_dataframe$score)))

scored_dataframe_norm_updated <- scored_dataframe %>%
  filter(score>0.05)
mean(scored_dataframe_norm_updated$score)
range(scored_dataframe_norm_updated$score)
class(scored_dataframe_norm_updated)

write_xlsx(scored_dataframe_norm_updated, "./Downloads/scored_dataframe.xlsx")

scored_dataframe_norm_updated <- read_xlsx("./Downloads/scored_dataframe.xlsx")
scored_dataframe_norm_updated <- as.data.frame(scored_dataframe_norm_updated)

#converting thr matrix into realRatingMatrix for recommenderlab to work
scored_dataframe_norm_updated$user_id <- as.factor(scored_dataframe_norm_updated$user_id)
scored_dataframe_norm_updated$product_id <- as.factor(scored_dataframe_norm_updated$product_id)
scored_dataframe_norm_updated$score <- as.numeric(scored_dataframe_norm_updated$score) 

real_rating_matrix <- as(scored_dataframe_norm_updated, "realRatingMatrix")
getRatingMatrix(real_rating_matrix[c(1:5),c(1:4)])

sets_train_test <- evaluationScheme(real_rating_matrix, method = "split", train = 0.7, given = 1)
evaluation_sets <- evaluationScheme(real_rating_matrix, method = "cross", k = 5, given = 1, goodRating = 0.2)
?evaluationScheme

popular <- Recommender(getData(sets_train_test, "train"),method="POPULAR")

predict_popular <- predict(popular,getData(sets_train_test, "known"), n = 5, type = "topNList")

predict_popular_items <- as(predict_popular@items, "list")
predict_popular_ratings <- as(predict_popular@ratings, "list")

predict_popular_items["4166"]

predict_ibcf_center_cos_items["4166"]
predict_ibcf_center_pear_items["4166"]
predict_ibcf_z_cos_items["4166"]
predict_ibcf_z_pear_items["4166"]

predict_ubcf_center_cos_items["4166"]
predict_ubcf_center_pear_items["4166"]
predict_ubcf_z_cos_items["4166"]
predict_ubcf_z_pear_items["4166"]


actual_products <-scored_database %>%
  select(product_id) %>%
  group_by(product_id) %>%
  filter(user_id == "4166")


#target all the customers

unique(predict_popular_items)
predict_popular_items[1654:16]
predict_popular_ratings[1:15]

#checking product names
prod_names <- products %>%
  select(product_name) %>%
  filter (products$product_id == "3474" | products$product_id == "4385" | products$product_id == "2297" | products$product_id == "866" |products$product_id ==  "2321")
prod_names

#IBCF

IBCF_Center_Cos <- Recommender(getData(sets_train_test, "train"), "IBCF", param = list(normalize = "center", method = "Cosine"))
IBCF_Z_Cos <- Recommender(getData(sets_train_test, "train"), "IBCF", param = list(normalize = "Z-score", method = "Cosine"))
# IBCF_C_E <- Recommender(getData(sets_train_test, "train"), "IBCF", param = list(normalize = "center", method = "Euclidean"))
# IBCF_Z_E <- Recommender(getData(sets_train_test, "train"), "IBCF", param = list(normalize = "Z-score", method = "Euclidean"))
IBCF_Center_Pear <- Recommender(getData(sets_train_test, "train"), "IBCF", param = list(normalize = "center", method = "pearson"))
IBCF_Z_Pear <- Recommender(getData(sets_train_test, "train"), "IBCF", param = list(normalize = "Z-score", method = "pearson"))
model_details <- getModel(IBCF_Center_Cos)
dim(model_details$sim)

predict_ibcf_center_cos <-predict(object = IBCF_Center_Cos, newdata = getData(sets_train_test, "known"), n = 5, type = "topNList")
predict_ibcf_z_cos <-predict(object = IBCF_Z_Cos, newdata = getData(sets_train_test, "known"), n = 5, type = "topNList")
# predict_ibcf_c_e <-predict(object = IBCF_C_E, newdata = getData(sets_train_test, "known"), n = 5, type = "topNList")
# predict_ibcf_z_e <-predict(object = IBCF_Z_E, newdata = getData(sets_train_test, "known"), n = 5, type = "topNList")
predict_ibcf_center_pear <-predict(object = IBCF_Center_Pear, newdata = getData(sets_train_test, "known"), n = 5, type = "topNList")
predict_ibcf_z_pear <-predict(object = IBCF_Z_Pear, newdata = getData(sets_train_test, "known"), n = 5, type = "topNList")

predict_rating_ibcf_center_cos <-predict(object = IBCF_Center_Cos, newdata = getData(sets_train_test, "known"), n = 5, type = "ratings")
predict_rating_ibcf_z_cos <-predict(object = IBCF_Z_Cos, newdata = getData(sets_train_test, "known"), n = 5, type = "ratings")
# predict_rating_ibcf_c_e <-predict(object = IBCF_C_E, newdata = getData(sets_train_test, "known"), n = 5, type = "ratings")
# predict_rating_ibcf_z_e <-predict(object = IBCF_Z_E, newdata = getData(sets_train_test, "known"), n = 5, type = "ratings")
predict_rating_ibcf_center_pear <-predict(object = IBCF_Center_Pear, newdata = getData(sets_train_test, "known"), n = 5, type = "ratings")
predict_rating_ibcf_z_pear <-predict(object = IBCF_Z_Pear, newdata = getData(sets_train_test, "known"), n = 5, type = "ratings")

predict_ibcf_center_cos_items <- as(predict_ibcf_center_cos@items, "list")
predict_ibcf_z_cos_items <- as(predict_ibcf_z_cos@items, "list")
predict_ibcf_center_pear_items <- as(predict_ibcf_center_pear@items, "list")
predict_ibcf_z_pear_items <- as(predict_ibcf_z_pear@items, "list")


prod_names <- products %>%
  select(product_name) %>%
  filter (products$product_id == "24852")
prod_names

#UBCF

UBCF_Center_Cos <- Recommender(getData(sets_train_test, "train"), "UBCF", param=list(normalize = "center",method="Cosine"))
UBCF_Center_Pear <- Recommender(getData(sets_train_test, "train"), "UBCF", param=list(normalize = "center",method="pearson"))
UBCF_Z_Cos <- Recommender(getData(sets_train_test, "train"), "UBCF", param=list(normalize = "Z-score",method="Cosine"))
UBCF_Z_Pear <- Recommender(getData(sets_train_test, "train"), "UBCF", param=list(normalize = "Z-score",method="pearson"))
UBCF_Details <- getModel(UBCF_Center_Cos)
# UBCF_Details$data
# names(UBCF_Details)

predict_ubcf_center_pear <- predict(object = UBCF_Center_Pear, newdata = getData(sets_train_test, "known"), n = 5, type = "topNList")
predict_ubcf_center_cos <- predict(object = UBCF_Center_Cos, newdata = getData(sets_train_test, "known"), n = 5, type = "topNList")
predict_ubcf_z_cos <- predict(object = UBCF_Z_Cos, newdata = getData(sets_train_test, "known"), n = 5, type = "topNList")
predict_ubcf_z_pear <- predict(object = UBCF_Z_Pear, newdata = getData(sets_train_test, "known"), n = 5, type = "topNList")

predict_rating_ubcf_center_pear <- predict(object = UBCF_Center_Pear, newdata = getData(sets_train_test, "known"), n = 5, type = "ratings")
predict_rating_ubcf_center_cos <- predict(object = UBCF_Center_Cos, newdata = getData(sets_train_test, "known"), n = 5, type = "ratings")
predict_rating_ubcf_z_cos <- predict(object = UBCF_Z_Cos, newdata = getData(sets_train_test, "known"), n = 5, type = "ratings")
predict_rating_ubcf_z_pear <- predict(object = UBCF_Z_Pear, newdata = getData(sets_train_test, "known"), n = 5, type = "ratings")

predict_ubcf_center_cos_items <- as(predict_ubcf_center_cos@items, "list")
predict_ubcf_z_cos_items <- as(predict_ubcf_z_cos@items, "list")
predict_ubcf_center_pear_items <- as(predict_ubcf_center_pear@items, "list")
predict_ubcf_z_pear_items <- as(predict_ubcf_z_pear@items, "list")


IBCF_Center_Cos_ConfMat <- calcPredictionAccuracy(predict_ibcf_center_cos, data = getData(sets_train_test, "unknown"), byUser = FALSE, goodRating = 0.2, given = 5)
IBCF_Z_Cos_ConfMat <- calcPredictionAccuracy(predict_ibcf_z_cos, data = getData(sets_train_test, "unknown"), byUser = FALSE, goodRating= 0.2, given = 5)
IBCF_Center_Pear_ConfMat <- calcPredictionAccuracy(predict_ibcf_center_pear, data = getData(sets_train_test, "unknown"), byUser = FALSE, goodRating= 0.2, given = 5)
IBCF_Z_Pear_ConfMat <- calcPredictionAccuracy(predict_ibcf_z_pear, data = getData(sets_train_test, "unknown"), byUser = FALSE, goodRating= 0.2, given = 5)
UBCF_Center_Cos_ConfMat <- calcPredictionAccuracy(predict_ubcf_center_cos, data = getData(sets_train_test, "unknown"), byUser = FALSE, goodRating= 0.2, given = 5)
UBCF_Center_Pear_ConfMat <- calcPredictionAccuracy(predict_ubcf_center_pear, data = getData(sets_train_test, "unknown"), byUser = FALSE, goodRating= 0.2, given = 5)
UBCF_Z_Cos_ConfMat <- calcPredictionAccuracy(predict_ubcf_z_cos, data = getData(sets_train_test, "unknown"), byUser = FALSE, goodRating= 0.2, given = 5)
UBCF_Z_Pear_ConfMat <- calcPredictionAccuracy(predict_ubcf_z_pear, data = getData(sets_train_test, "unknown"), byUser = FALSE, goodRating= 0.2, given = 5)

IBCF_Center_Cos_Error <- calcPredictionAccuracy(predict_rating_ibcf_center_cos, data = getData(sets_train_test, "unknown"), byUser = FALSE, goodRating = 0.2, given = 5)
IBCF_Z_Cos_Error <- calcPredictionAccuracy(predict_rating_ibcf_z_cos, data = getData(sets_train_test, "unknown"), byUser = FALSE, goodRating= 0.2, given = 5)
IBCF_Center_Pear_Error <- calcPredictionAccuracy(predict_rating_ibcf_center_pear, data = getData(sets_train_test, "unknown"), byUser = FALSE, goodRating= 0.2, given = 5)
IBCF_Z_Pear_Error <- calcPredictionAccuracy(predict_rating_ibcf_z_pear, data = getData(sets_train_test, "unknown"), byUser = FALSE, goodRating= 0.2, given = 5)
UBCF_Center_Cos_Error <- calcPredictionAccuracy(predict_rating_ubcf_center_cos, data = getData(sets_train_test, "unknown"), byUser = FALSE, goodRating= 0.2, given = 5)
UBCF_Center_Pear_Error <- calcPredictionAccuracy(predict_rating_ubcf_center_pear, data = getData(sets_train_test, "unknown"), byUser = FALSE, goodRating= 0.2, given = 5)
UBCF_Z_Cos_Error <- calcPredictionAccuracy(predict_rating_ubcf_z_cos, data = getData(sets_train_test, "unknown"), byUser = FALSE, goodRating= 0.2, given = 5)
UBCF_Z_Pear_Error <- calcPredictionAccuracy(predict_rating_ubcf_z_pear, data = getData(sets_train_test, "unknown"), byUser = FALSE, goodRating= 0.2, given = 5)

# listed_confusion_matrices <- list(IBCF_Center_Cos_ConfMat,
#                          IBCF_Center_Pear_ConfMat,
#                          IBCF_Z_Cos_ConfMat,
#                          IBCF_Z_Pear_ConfMat,
#                          UBCF_Center_Cos_ConfMat,
#                          UBCF_Center_Pear_ConfMat,
#                          UBCF_Z_Cos_ConfMat,
#                          UBCF_Z_Pear_ConfMat)



confusion_matrix <- data.frame(IBCF_Center_Cos_ConfMat,
                        IBCF_Center_Pear_ConfMat,
                        IBCF_Z_Cos_ConfMat,
                        IBCF_Z_Pear_ConfMat,
                        UBCF_Center_Cos_ConfMat,
                        UBCF_Center_Pear_ConfMat,
                        UBCF_Z_Cos_ConfMat,
                        UBCF_Z_Pear_ConfMat)
confusion_matrix_updated <- t.data.frame(confusion_matrix)
dataframe_conf_matrix <- as.data.frame(confusion_matrix_updated)


listed_errors <- list(IBCF_Center_Cos_Error,
                                  IBCF_Center_Pear_Error,
                                  IBCF_Z_Cos_Error,
                                  IBCF_Z_Pear_Error,
                                  UBCF_Center_Cos_Error,
                                  UBCF_Center_Pear_Error,
                                  UBCF_Z_Cos_Error,
                                  UBCF_Z_Pear_Error)


errors<- data.frame(IBCF_Center_Cos_Error,
                    IBCF_Center_Pear_Error,
                    IBCF_Z_Cos_Error,
                    IBCF_Z_Pear_Error,
                    UBCF_Center_Cos_Error,
                    UBCF_Center_Pear_Error,
                    UBCF_Z_Cos_Error,
                    UBCF_Z_Pear_Error)

errors_updated <- t.data.frame(errors)
dataframe_errors <- as.data.frame(errors_updated)
ggplot(dataframe_errors, aes(x = rownames(dataframe_errors), y = RMSE )) +
  geom_bar(stat = "identity", fill = c("darkolivegreen")) +
  labs(x = "Model", y = "RMSE Value", title  = "Bar Plot for RMSE") +
  coord_flip()


models_to_evaluate <- list(
  IBCF_cosc = list(name = "IBCF", param = list(method = "cosine", normalize = "center")),
  IBCF_cosz = list(name = "IBCF", param = list(method = "cosine", normalize = "Z-score")),
  IBCF_peac = list(name = "IBCF", param = list(method = "pearson", normalize = "center")),
  IBCF_peaz = list(name = "IBCF", param = list(method = "pearson", normalize = "Z-score")),
  
  UBCF_peac = list(name = "UBCF", param = list(method = "pearson", normalize = "center")),
  UBCF_cosz = list(name = "UBCF", param = list(method = "cosine", normalize = "Z-score")),
  UBCF_cosc = list(name = "UBCF", param = list(method = "cosine", normalize = "center")),
  UBCF_peaz = list(name = "UBCF", param = list(method = "pearson", normalize = "Z-score")),
  
  Popular_Model = list(name = "POPULAR", param = NULL))

models_to_evaluate[4:8]

results <- recommenderlab::evaluate(x = evaluation_sets, method = models_to_evaluate, n= c(1,5,seq(10,100,10)), type = "topNList")

plot(results[1:8])

plot(results[1:8], col = rainbow(8), lty = c(1:8), legend = FALSE)
title(main = "ROC curves")

legend("bottomright", text.font = 10, cex = 0.6, col = rainbow(8), bty = "l", lty = c(1:8), legend = c("IBCF_cosc", "IBCF_cosz", "IBCF_peac", "IBCF_peaz", "UBCF_peac", "UBCF_cosz", "UBCF_cosc","UBCF_peaz"))

plot(results[1:8], "prec/rec" ,col = rainbow(8), lty = c(1:8), legend = FALSE, xlim = c(0,0.06))
title(main = "Precision-Recall Plot")
legend("topright", cex = 0.6, col = rainbow(8), bty = "l", lty = c(1:8), legend = c("IBCF_cosc", "IBCF_cosz", "IBCF_peac", "IBCF_peaz", "UBCF_peac", "UBCF_cosz", "UBCF_cosc","UBCF_peaz"))

 prod_names <- products %>%
  select(product_name) %>%
  filter (products$product_id == "10" | products$product_id == "137"| products$product_id == "193" | products$product_id == "252" | products$product_id == "287")

prod_names

final_recommender_list <- data.frame("Popular" = products %>%
                                       select(product_name) %>%
                                       filter (products$product_id == "3185" |
                                                 products$product_id == "4975"|
                                                 products$product_id == "3568" |
                                                 products$product_id == "2534" |
                                                 products$product_id == "746"),
                                     "IBCF_Center_Cos" = products %>%
                                       select(product_name) %>%
                                       filter (products$product_id == "100" |
                                                 products$product_id == "206"|
                                                 products$product_id == "241" |
                                                 products$product_id == "260" |
                                                 products$product_id == "268"),
                                     "IBCF_Center_Pear" = products %>%
                                       select(product_name) %>%
                                       filter (products$product_id == "2563" |
                                                 products$product_id == "3709"|
                                                 products$product_id == "779" |
                                                 products$product_id == "2443" |
                                                 products$product_id == "2930"),
                                     "IBCF_Z_Cos" = products %>%
                                       select(product_name) %>%
                                       filter (products$product_id == "100" |
                                                 products$product_id == "206"|
                                                 products$product_id == "241" |
                                                 products$product_id == "260" |
                                                 products$product_id == "268"),
                                     "IBCF_Z_Pear" = products %>%
                                       select(product_name) %>%
                                       filter (products$product_id == "2563" |
                                                 products$product_id == "3709"|
                                                 products$product_id == "3074" |
                                                 products$product_id == "2930" |
                                                 products$product_id == "2443"),
                                     "UBCF_Center_Cos" = products %>%
                                       select(product_name) %>%
                                       filter (products$product_id == "2704" |
                                                 products$product_id == "3328"|
                                                 products$product_id == "2527" |
                                                 products$product_id == "754" |
                                                 products$product_id == "655"),
                                     "UBCF_Center_Pear" = products %>%
                                       select(product_name) %>%
                                       filter (products$product_id == "2772" |
                                                 products$product_id == "504"|
                                                 products$product_id == "2070" |
                                                 products$product_id == "2930" |
                                                 products$product_id == "2593"),
                                     "UBCF_Z_Cos" = products %>%
                                       select(product_name) %>%
                                       filter (products$product_id == "2704" |
                                                 products$product_id == "3328"|
                                                 products$product_id == "2527" |
                                                 products$product_id == "754" |
                                                 products$product_id == "796"),
                                     "UBCF_Z_Pear" = products %>%
                                       select(product_name) %>%
                                       filter (products$product_id == "2070" |
                                                 products$product_id == "2772"|
                                                 products$product_id == "504" |
                                                 products$product_id == "2563" |
                                                 products$product_id == "2593"),
                                     "Actual Products" = products %>%
                                       select(product_name) %>%
                                       filter (products$product_id == "2210" |
                                                 products$product_id == "13176"|
                                                 products$product_id == "19019" |
                                                 products$product_id == "21709" |
                                                 products$product_id == "21903" )
                                       
                                     
)

class(final_recommender_list)
names(final_recommender_list) <- c("Popular","IBCF_Center_Cos","IBCF_Center_Pear","IBCF_Z_Cos","IBCF_Z_Pear","UBCF_Center_Cos","UBCF_Center_Pear","UBCF_Z_Cos","UBCF_Z_Pear", "Actual Products")

recommendations <- write_xlsx(final_recommender_list, "./Downloads/list.xlsx")



