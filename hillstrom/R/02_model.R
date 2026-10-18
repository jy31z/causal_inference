ate <- sapply(c("Womens E-Mail", "Mens E-Mail"), function(a){
  
  y1 <- d$conversion[d$segment == a]
  y0 <- d$conversion[d$segment == "No E-Mail"]
  
  effect <- mean(y1) - mean(y0)
  se <- sqrt(var(y1) / length(y1) + var(y0) / length(y0))
  
  c(
    ATE_pp = 100 * effect,
    SE_pp = 100 * se,
    CI95_low_pp = 100 * (effect - 1.96 * se),
    CI95_high_pp = 100 * (effect + 1.96 * se),
    simultaneous_low_pp = 100 * (effect - 2.2414 * se),
    simultaneous_high_pp = 100 * (effect + 2.2414 * se)
  )
})

ate





######

library(grf)

set.seed(12345)

S_model <- regression_forest(
  cbind(Xb_train, A=Ab_train),
  Yb_train
)

X1 <- cbind(Xb_test, A=1)
X0 <- cbind(Xb_test, A=0)

S_test <- predict(S_model, X1)$predictions -
  predict(S_model, X0)$predictions

set.seed(12345)

T_model1 <- regression_forest(
  Xb_train[Ab_train == 1, , drop=FALSE],
  Yb_train[Ab_train == 1]
)

T_model0 <- regression_forest(
  Xb_train[Ab_train == 0, , drop=FALSE],
  Yb_train[Ab_train == 0]
)

T_test <- predict(T_model1, Xb_test)$predictions -
  predict(T_model0, Xb_test)$predictions

# X-learner
mu1_train <- predict(T_model1, Xb_train)$predictions
mu0_train <- predict(T_model0, Xb_train)$predictions

D1 <- Yb_train[Ab_train == 1] - mu0_train[Ab_train == 1]
D0 <- mu1_train[Ab_train == 0] - Yb_train[Ab_train == 0]

set.seed(12345)
X_model1 <- regression_forest(
  Xb_train[Ab_train == 1, , drop=FALSE],
  D1
)

X_model0 <- regression_forest(
  Xb_train[Ab_train == 0, , drop=FALSE],
  D0
)


tau1 <- predict(X_model1, Xb_test)$predictions
tau0 <- predict(X_model0, Xb_test)$predictions
X_test <- 0.5 * tau0 + 0.5 * tau1


mu1 <- predict(T_model1, Xb_train)$predictions
mu0 <- predict(T_model0, Xb_train)$predictions

e <- rep(0.5, length(Ab_train))
pseudo_DR <- mu1 - mu0 + Ab_train * (Yb_train - mu1) / e - (1 - Ab_train) * (Yb_train - mu0) / (1 - e)

set.seed(12345)

DR_model <- regression_forest(
  Xb_train,
  pseudo_DR
)

DR_test <- predict(
  DR_model,
  Xb_test
)$predictions

scores_test <- data.frame(
  S  = S_test,
  T  = T_test,
  X  = X_test,
  DR = DR_test
)

head(scores_test)
res <- summary(scores_test)


######


library(ggplot2)

grid <- seq(0, 1, length.out=101)

a <- Ab_test
y <- Yb_test

# IPW pseudo-outcome
z <- a*y/0.5 - (1-a)*y/0.5

curve_list <- list()
curve_table <- data.frame()

for (name in colnames(scores_test)) {
  
  score <- scores_test[[name]]
  
  # Sort predicted CATE from highest to lowest
  order_idx <- order(-score, method="radix")
  
  # cumulative IPW uplift
  cum <- c(0, cumsum(z[order_idx])) / length(z)
  
  # Python uses zero-based indexing, so add 1 for R
  k <- floor(grid * length(z))
  gain <- cum[k + 1]
  
  # centered Qini
  qini <- gain - grid * cum[length(cum)]
  
  # trapezoidal integration
  area <- sum(
    diff(grid) * (head(qini, -1) + tail(qini, -1)) / 2
  )
  
  curve_list[[name]] <- data.frame(
    fraction = grid,
    gain = 100 * gain,
    qini = 100 * qini,
    learner = name
  )
  
  curve_table <- rbind(
    curve_table,
    data.frame(
      learner = name,
      Qini_area = area
    )
  )
}

curves <- do.call(rbind, curve_list)

p1 <- ggplot(
  curves,
  aes(x=fraction, y=gain, color=learner)
) +
  geom_line(linewidth=1) +
  
  geom_abline(
    intercept=0,
    slope=100*mean(z),
    linetype="dashed",
    color="black"
  ) +
  
  labs(
    x="Fraction targeted",
    y="Increment per customer (pp)",
    title="IPW cumulative uplift",
    color=NULL
  ) +
  
  theme_classic(base_size=15) +
  theme(
    legend.position="top"
  )

pdf('outputs/qini.pdf')
print(p1)
dev.off()


p2 <- ggplot(
  curves,
  aes(x=fraction, y=qini, color=learner)
) +
  geom_line(linewidth=1) +
  
  geom_hline(
    yintercept=0,
    linetype="dashed",
    color="grey50"
  ) +
  
  labs(
    x="Fraction targeted",
    y="Increment per customer (pp)",
    title="Centered Qini",
    color=NULL
  ) +
  
  theme_classic(base_size=15) +
  theme(
    legend.position="top"
  )

pdf('outputs/cen_qini.pdf')
print(p2)
dev.off()












