data_file <- "data/hillstrom.csv"

set.seed(12345)
d <- read.csv(data_file, stringsAsFactors = FALSE)

# train:test = 4:1

train_idx <- unlist(
  lapply(split(seq_len(nrow(d)), d$segment), function(idx){
    sample(idx, size = floor(0.8 * length(idx)))
  })
)


dtrain <- d[train_idx, ]
dtest  <- d[-train_idx, ]

d$trainIdx <- seq_len(nrow(d)) %in% train_idx

table(d$segment, d$conversion, d$visit)

segs = unique(d$segment)
res <- data.frame(
  segment = segs,
  n = sapply(segs, function(s) nrow(d[d$segment==s, ])),
  purchases = sapply(segs, function(s) sum(d$conversion[d$segment==s])),
  conversion_rate = sapply(segs, function(s) mean(d$conversion[d$segment==s])),
  visit_rate = sapply(segs, function(s) mean(d$visit[d$segment==s]))
)

print(res)

# df |>
#   group_by(segment) |>
#   summarise(
#     n = n(),
#     purchases = sum(conversion),
#     conversion_rate = mean(conversion),
#     visit_rate = mean(visit)
#   )

library(dplyr)
d %>% 
  group_by(trainIdx) %>% 
  summarise(count = n())

num_cols <- c("recency", "history", "mens", "womens", "newbie")
cat_cols <- c("history_segment", "zip_code", "channel")
features <- c(num_cols, cat_cols)


balance <- as.matrix(d[num_cols])
for (v in cat_cols) for (level in sort(unique(d[[v]])))
  balance <- cbind(balance, setNames(data.frame(as.numeric(d[[v]] == level)), paste(v, level, sep="=")))
balance <- as.matrix(balance)

smd <- data.frame(feature=colnames(balance))



unique(d$segment)

smd <- sapply(c("Womens E-Mail", "Mens E-Mail"), function(a){
  
  x1 <- balance[d$segment == a, , drop=FALSE]
  x0 <- balance[d$segment == "No E-Mail", , drop=FALSE]
  
  denom <- sqrt((apply(x1, 2, var)+apply(x0, 2, var))/2)
  denom[denom == 0] <- NA_real_
  
  (colMeans(x1)-colMeans(x0))/denom
}) 

smd <- abs(smd)
 

library(tibble)
library(tidyr)
library(ggplot2)

smd_long <- smd |>
  as.data.frame() |>
  rownames_to_column("feature") |>
  pivot_longer(
    cols = -feature,
    names_to = "arm",
    values_to = "SMD"
  )


p <- ggplot(smd_long, aes(x = SMD, y = feature, color = arm, shape = arm)) +
  geom_point(size = 2) +
  geom_vline(xintercept = 0, linetype = "solid", color = "grey50", linewidth = 0.6) +
  geom_vline(xintercept = 0.1, linetype = "dashed", color = "grey50", linewidth = 0.6) +
  scale_color_manual(values = c("#3B66E3", "#FF9900")) +
  scale_shape_manual(values = c(16, 17)) +
  labs(x = "Standardized Mean Difference", y = NULL, color = NULL, shape = NULL) +
  theme_classic() +
  theme(legend.position = "top")



pdf("outputs/smd.pdf", width = 5.5, height = 4.5)
print(p)
dev.off()


# numeric variables
Xtrain <- as.matrix(dtrain[num_cols])
Xtest  <- as.matrix(dtest[num_cols])

for (v in cat_cols) {
  levels_v <- sort(unique(dtrain[[v]]))
  for (level in levels_v) {
    name <- paste(v, level, sep="=")
    Xtrain <- cbind(Xtrain, setNames(data.frame(as.numeric(dtrain[[v]] == level)), name))
    Xtest <- cbind(Xtest, setNames(data.frame(as.numeric(dtest[[v]] == level)), name))
  }
}

Xtrain <- as.matrix(Xtrain)
Xtest  <- as.matrix(Xtest)

storage.mode(Xtrain) <- "double"
storage.mode(Xtest)  <- "double"

binary_train <- dtrain$segment %in% c(
  "No E-Mail",
  "Womens E-Mail"
)

binary_test <- dtest$segment %in% c(
  "No E-Mail",
  "Womens E-Mail"
)

Xb_train <- Xtrain[binary_train, , drop=FALSE]
Xb_test  <- Xtest[binary_test, , drop=FALSE]

Ab_train <- as.numeric(
  dtrain$segment[binary_train] == "Womens E-Mail"
)

Ab_test <- as.numeric(
  dtest$segment[binary_test] == "Womens E-Mail"
)

Yb_train <- dtrain$conversion[binary_train]
Yb_test  <- dtest$conversion[binary_test]

table(Ab_train)
table(Ab_test)

mean(Yb_train[Ab_train == 0])
mean(Yb_train[Ab_train == 1])

