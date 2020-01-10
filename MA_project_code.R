#importing cereals data, calculating GP, filling NA with 0 for AdSize
cereals = read.csv("/Users/beingpratiksahoo/Desktop/cereals.csv", header = TRUE)
cereals$GP = cereals$UPC_SCAN_QTY * (cereals$NET_RETAIL_AMT - cereals$NET_BASECST_AMT)
cereals$AD_ITM_PRTY_CODE[is.na(cereals$AD_ITM_PRTY_CODE)] <- 0

#segmenting promoted and nonpromoted cereals
cereals_nonpromoted <- subset(cereals,AD_ITM_PRTY_CODE == 0 | AD_ITM_PRTY_CODE == 4)
cereals_promoted <- subset(cereals,AD_ITM_PRTY_CODE == 3 | AD_ITM_PRTY_CODE == 5 | AD_ITM_PRTY_CODE == 6 | AD_ITM_PRTY_CODE == 7 | AD_ITM_PRTY_CODE == 8 | AD_ITM_PRTY_CODE == 9)

#3 individual items considered to study promotions
item1_cereal <- subset(cereals,ITEM_NUM == 10101903)
item2_cereal <- subset(cereals,ITEM_NUM == 10105038)
item3_cereal <- subset(cereals,ITEM_NUM == 10103476)

item1_cereal_promo <- subset(item1_cereal,AD_ITM_PRTY_CODE == 8)
item1_cereal_nonpromo <- subset(item1_cereal,AD_ITM_PRTY_CODE == 0)

item2_cereal_promo <- subset(item2_cereal,AD_ITM_PRTY_CODE == 5 | AD_ITM_PRTY_CODE == 8 | AD_ITM_PRTY_CODE == 9)
item2_cereal_nonpromo <- subset(item2_cereal,AD_ITM_PRTY_CODE == 0 | AD_ITM_PRTY_CODE == 4 )
item2_cereal_5promo <- subset(item2_cereal_promo,AD_ITM_PRTY_CODE == 5)
item2_cereal_8promo <- subset(item2_cereal_promo,AD_ITM_PRTY_CODE == 8)
item2_cereal_9promo <- subset(item2_cereal_promo,AD_ITM_PRTY_CODE == 9)

item3_cereal_promo <- subset(item3_cereal,AD_ITM_PRTY_CODE == 5 | AD_ITM_PRTY_CODE == 8 | AD_ITM_PRTY_CODE == 9)
item3_cereal_nonpromo <- subset(item3_cereal,AD_ITM_PRTY_CODE == 0 | AD_ITM_PRTY_CODE == 4 )
item3_cereal_5promo <- subset(item3_cereal_promo,AD_ITM_PRTY_CODE == 5)
item3_cereal_8promo <- subset(item3_cereal_promo,AD_ITM_PRTY_CODE == 8)
item3_cereal_9promo <- subset(item3_cereal_promo,AD_ITM_PRTY_CODE == 9)

#calculating promo score for each promo type week wise
cereals_promoted_9 <- subset(cereals,AD_ITM_PRTY_CODE == 9)

april_1_cereals_promoted_9 <- subset(cereals_promoted_9,WK_STRT_DATE == "2018-04-01")
april_2_cereals_promoted_9 <- subset(cereals_promoted_9,WK_STRT_DATE == "2018-04-08")
april_3_cereals_promoted_9 <- subset(cereals_promoted_9,WK_STRT_DATE == "2018-04-15")
april_4_cereals_promoted_9 <- subset(cereals_promoted_9,WK_STRT_DATE == "2018-04-22")
april_5_cereals_promoted_9 <- subset(cereals_promoted_9,WK_STRT_DATE == "2018-04-29")
may_1_cereals_promoted_9 <- subset(cereals_promoted_9,WK_STRT_DATE == "2018-05-06")
may_2_cereals_promoted_9 <- subset(cereals_promoted_9,WK_STRT_DATE == "2018-05-13")
may_3_cereals_promoted_9 <- subset(cereals_promoted_9,WK_STRT_DATE == "2018-05-20")
may_4cereals_promoted_9 <- subset(cereals_promoted_9,WK_STRT_DATE == "2018-05-27")
june_1_cereals_promoted_9 <- subset(cereals_promoted_9,WK_STRT_DATE == "2018-06-03")
june_2_cereals_promoted_9 <- subset(cereals_promoted_9,WK_STRT_DATE == "2018-06-10")
june_3_cereals_promoted_9 <- subset(cereals_promoted_9,WK_STRT_DATE == "2018-06-17")
june_4_cereals_promoted_9 <- subset(cereals_promoted_9,WK_STRT_DATE == "2018-06-24")
july_1_cereals_promoted_9 <- subset(cereals_promoted_9,WK_STRT_DATE == "2018-07-01")
july_2_cereals_promoted_9 <- subset(cereals_promoted_9,WK_STRT_DATE == "2018-07-08")
july_3_cereals_promoted_9 <- subset(cereals_promoted_9,WK_STRT_DATE == "2018-07-15")
july_4_cereals_promoted_9 <- subset(cereals_promoted_9,WK_STRT_DATE == "2018-07-22")
july_5_cereals_promoted_9 <- subset(cereals_promoted_9,WK_STRT_DATE == "2018-07-29")
aug_1_cereals_promoted_9 <- subset(cereals_promoted_9,WK_STRT_DATE == "2018-08-05")
aug_2_cereals_promoted_9 <- subset(cereals_promoted_9,WK_STRT_DATE == "2018-08-12")
aug_3_cereals_promoted_9 <- subset(cereals_promoted_9,WK_STRT_DATE == "2018-08-19")
aug_4_cereals_promoted_9 <- subset(cereals_promoted_9,WK_STRT_DATE == "2018-08-26")
sep_1_cereals_promoted_9 <- subset(cereals_promoted_9,WK_STRT_DATE == "2018-09-02")
sep_2_cereals_promoted_9 <- subset(cereals_promoted_9,WK_STRT_DATE == "2018-09-09")
sep_3_cereals_promoted_9 <- subset(cereals_promoted_9,WK_STRT_DATE == "2018-09-16")
sep_4_cereals_promoted_9 <- subset(cereals_promoted_9,WK_STRT_DATE == "2018-09-23")
sep_5_cereals_promoted_9 <- subset(cereals_promoted_9,WK_STRT_DATE == "2018-09-30")

#understanding factors respomsible for positive and negative promo score
cereals_9promo <- subset(cereals,AD_ITM_PRTY_CODE == 9)
cereals_9promo_dis = subset(cereals_promoted_9,DSCNT_AMT != 0 & DIGTL_DSCNT_AMT == 0 & MIXNMTCH_DSCNT_AMT == 0)
cereals_9promo_digital = subset(cereals_promoted_9,DIGTL_DSCNT_AMT != 0 & DSCNT_AMT == 0 & MIXNMTCH_DSCNT_AMT == 0)
cereals_9promo_mixmatch = subset(cereals_promoted_9,MIXNMTCH_DSCNT_AMT != 0 & DIGTL_DSCNT_AMT == 0 & DSCNT_AMT == 0)
cereals_9promo_dis_digital = subset(cereals_promoted_9,MIXNMTCH_DSCNT_AMT == 0 & DIGTL_DSCNT_AMT != 0 & DSCNT_AMT != 0)
cereals_9promo_dis_mixmtch = subset(cereals_promoted_9,MIXNMTCH_DSCNT_AMT != 0 & DIGTL_DSCNT_AMT == 0 & DSCNT_AMT != 0)
cereals_9promo_digital_mixmtch = subset(cereals_promoted_9,MIXNMTCH_DSCNT_AMT != 0 & DIGTL_DSCNT_AMT != 0 & DSCNT_AMT == 0)
cereals_9promo_dis_digital_mixmtch = subset(cereals_promoted_9,MIXNMTCH_DSCNT_AMT != 0 & DIGTL_DSCNT_AMT != 0 & DSCNT_AMT != 0)
cereals_9promo_non_dis_digital_mixmtch = subset(cereals_promoted_9,MIXNMTCH_DSCNT_AMT == 0 & DIGTL_DSCNT_AMT == 0 & DSCNT_AMT == 0)

cereals_8promo <- subset(cereals,AD_ITM_PRTY_CODE == 8)
cereals_8promo_dis = subset(cereals_promoted_8,DSCNT_AMT != 0 & DIGTL_DSCNT_AMT == 0 & MIXNMTCH_DSCNT_AMT == 0)
cereals_8promo_digital = subset(cereals_promoted_8,DIGTL_DSCNT_AMT != 0 & DSCNT_AMT == 0 & MIXNMTCH_DSCNT_AMT == 0)
cereals_8promo_mixmatch = subset(cereals_promoted_8,MIXNMTCH_DSCNT_AMT != 0 & DIGTL_DSCNT_AMT == 0 & DSCNT_AMT == 0)
cereals_8promo_dis_digital = subset(cereals_promoted_8,MIXNMTCH_DSCNT_AMT == 0 & DIGTL_DSCNT_AMT != 0 & DSCNT_AMT != 0)
cereals_8promo_dis_mixmtch = subset(cereals_promoted_8,MIXNMTCH_DSCNT_AMT != 0 & DIGTL_DSCNT_AMT == 0 & DSCNT_AMT != 0)
cereals_8promo_digital_mixmtch = subset(cereals_promoted_8,MIXNMTCH_DSCNT_AMT != 0 & DIGTL_DSCNT_AMT != 0 & DSCNT_AMT == 0)
cereals_8promo_dis_digital_mixmtch = subset(cereals_promoted_8,MIXNMTCH_DSCNT_AMT != 0 & DIGTL_DSCNT_AMT != 0 & DSCNT_AMT != 0)
cereals_8promo_non_dis_digital_mixmtch = subset(cereals_promoted_8,MIXNMTCH_DSCNT_AMT == 0 & DIGTL_DSCNT_AMT == 0 & DSCNT_AMT == 0)

#storewise
cereals_store_709 <- subset(cereals,CUST_NUM == 709)
cereals_store_440 <- subset(cereals,CUST_NUM == 440)
cereals_store_343 <- subset(cereals,CUST_NUM == 343)

cereals_store_709_nonpromo <- subset(cereals_store_709,AD_ITM_PRTY_CODE == 0 | AD_ITM_PRTY_CODE == 4)
cereals_store_709_3_5promo <- subset(cereals_store_709,AD_ITM_PRTY_CODE == 3 | AD_ITM_PRTY_CODE == 5)
cereals_store_709_7promo <- subset(cereals_store_709,AD_ITM_PRTY_CODE == 7)
cereals_store_709_8promo <- subset(cereals_store_709,AD_ITM_PRTY_CODE == 8)
cereals_store_709_9promo <- subset(cereals_store_709,AD_ITM_PRTY_CODE == 9)

cereals_store_440_nonpromo <- subset(cereals_store_440,AD_ITM_PRTY_CODE == 0 | AD_ITM_PRTY_CODE == 4)
cereals_store_440_3_5promo <- subset(cereals_store_440,AD_ITM_PRTY_CODE == 3 | AD_ITM_PRTY_CODE == 5)
cereals_store_440_7promo <- subset(cereals_store_440,AD_ITM_PRTY_CODE == 7)
cereals_store_440_8promo <- subset(cereals_store_440,AD_ITM_PRTY_CODE == 8)
cereals_store_440_9promo <- subset(cereals_store_440,AD_ITM_PRTY_CODE == 9)

cereals_store_343_nonpromo <- subset(cereals_store_343,AD_ITM_PRTY_CODE == 0 | AD_ITM_PRTY_CODE == 4)
cereals_store_343_3_5promo <- subset(cereals_store_343,AD_ITM_PRTY_CODE == 3 | AD_ITM_PRTY_CODE == 5)
cereals_store_343_7promo <- subset(cereals_store_343,AD_ITM_PRTY_CODE == 7)
cereals_store_343_8promo <- subset(cereals_store_343,AD_ITM_PRTY_CODE == 8)
cereals_store_343_9promo <- subset(cereals_store_343,AD_ITM_PRTY_CODE == 9)

#regression
cereals_9promo_train = cereals_9promo[1:50000,]
cereals_9promo_train$SP = (cereals_9promo_train$NET_RETAIL_AMT + cereals_9promo_train$DSCNT_AMT + cereals_9promo_train$DIGTL_DSCNT_AMT + cereals_9promo_train$MIXNMTCH_DSCNT_AMT)
cereals_9promo_train$GP = cereals_9promo_train$UPC_SCAN_QTY * (cereals_9promo_train$SP - (cereals_9promo_train$NET_BASECST_AMT + cereals_9promo_train$DSCNT_AMT + cereals_9promo_train$DIGTL_DSCNT_AMT + cereals_9promo_train$MIXNMTCH_DSCNT_AMT))
cereals_9promo_test = cereals_9promo[50001:50500,]
cereals_9promo_test$SP = (cereals_9promo_test$NET_RETAIL_AMT + cereals_9promo_test$DSCNT_AMT + cereals_9promo_test$DIGTL_DSCNT_AMT + cereals_9promo_test$MIXNMTCH_DSCNT_AMT)
cereals_9promo_test$GP = cereals_9promo_test$UPC_SCAN_QTY * (cereals_9promo_test$SP - (cereals_9promo_test$NET_BASECST_AMT + cereals_9promo_test$DSCNT_AMT + cereals_9promo_test$DIGTL_DSCNT_AMT + cereals_9promo_test$MIXNMTCH_DSCNT_AMT))

cereals_9promo_regression <- lm(GP ~ NET_BASECST_AMT + SP + DSCNT_AMT + DIGTL_DSCNT_AMT + MIXNMTCH_DSCNT_AMT + UPC_SCAN_QTY ,data = cereals_9promo_train)
write.csv(cereals_9promo_test, "/Users/beingpratiksahoo/Desktop/regression_test.csv")

#clustering
cereals_promoted_gp_adtype <- subset(cereals_promoted, select = c("AD_ITM_PRTY_CODE","GP"))

set.seed(20)
promo_gp_cluster <- kmeans(cereals_promoted_gp_adtype[1:108000, 1:2], 6, nstart = 20)
promo_gp_cluster
cluster_number = data.frame(promo_gp_cluster$cluster)

promo_gp_cluster$cluster <- as.factor(promo_gp_cluster$cluster)
ggplot2::ggplot(cereals_promoted_gp_adtype[1:108000, 1:2], ggplot2::aes(AD_ITM_PRTY_CODE, GP, color = promo_gp_cluster$cluster)) + ggplot2::geom_point()


cereals_9promo %>%
  filter(CUST_NUM != 327)