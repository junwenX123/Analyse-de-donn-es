# ============================================================
# CREDIT CARD FRAUD - PROJET ADD
# ============================================================

df = read.csv("C:/Users/achou/Downloads/credit_card_fraud_10k.csv", sep = ",", 
              stringsAsFactors = FALSE)
head(df)
str(df)

# _______________________________________________________________________________________________
# 1) Nettoyage / conversions
# _______________________________________________________________________________________________

# 1.1) Compter le nombre total de valeurs manquantes dans tout le data frame
total_na = sum(is.na(df))
total_na

# 1.2) Doublons sur l'identifiant (il devrait être unique)
nb_doublons_id = sum(duplicated(df$transaction_id))
nb_doublons_id

# 1.3) Conversions en facteurs (catégorielles)

# Variable cible (0/1 -> labels lisibles)
df$is_fraud = factor(df$is_fraud,levels = c(0, 1), labels = c("Legit", "Fraud"))

# Catégorie marchand (chr -> factor)
df$merchant_category = factor(df$merchant_category)

# Variables binaires (int 0/1 -> factor No/Yes)
df$foreign_transaction = factor(df$foreign_transaction, levels = c(0, 1),labels = c("No", "Yes"))

df$location_mismatch = factor(df$location_mismatch, levels = c(0, 1), labels = c("No", "Yes"))

# Tranches horaires (regroupement de transaction_hour en classes)
df$hour_group = cut(df$transaction_hour,
                     breaks = c(-1, 3, 7, 11, 15, 19, 23),
                     labels = c("00-03", "04-07", "08-11",
                                "12-15", "16-19", "20-23"))

df$hour_group = factor(df$hour_group)

head(df)
str(df)


# _______________________________________________________________________________________________
# 2) Analyse univariée
# _______________________________________________________________________________________________

vars_num = c("amount", "device_trust_score", "velocity_last_24h", "cardholder_age")
vars_cat = c("transaction_hour", "hour_group", "merchant_category",
              "foreign_transaction", "location_mismatch", "is_fraud")
# ---- Palette sobre ----
col_num = "#4C78A8"   # bleu
col_box = "#72B7B2"   # vert/bleu doux
col_bar = "#F58518"   # orange
col_pct = "#54A24B"   # vert
col_line = "#2F2F2F"   # gris foncÃ©


# 2.1) Variables numériques
resume_numerique = function(x) {
  n = length(x)
  q = quantile(x, probs = c(0.25, 0.5, 0.75))
  data.frame(
    n      = n,
    mean   = mean(x),
    sd     = sd(x),
    min    = min(x),
    Q1     = as.numeric(q[1]),
    median = as.numeric(q[2]),
    Q3     = as.numeric(q[3]),
    max    = max(x)
  )
}

plot_num_2en1 = function(x, var_name) {
  op = par(no.readonly = TRUE)
  par(mfrow = c(1, 2), mar = c(4, 4, 4, 1) + 0.1)
  
  hist(x, breaks = 30,
       col = col_num, border = "white",
       main = paste("Histogramme-", var_name),
  xlab = var_name)
  rug(x, col = col_line)
  
  boxplot(x,
          col = col_box, border = col_line,
          main = paste("Boxplot-", var_name),
          ylab = var_name)
  
  par(op)
}

for (v in vars_num) {
  cat("\n===================================================\n")
  cat("Variable numérique :", v, "\n")
  cat("====================================================\n")
  
  print(resume_numerique(df[[v]]))
  
  # Figure 2-en-1
  plot_num_2en1(df[[v]], v)
}

# 2.2) Variables catégorielles et binaires

table_categorielle = function(x) {
  tab = table(x)
  prop = prop.table(tab)
  data.frame(
    modalite   = names(tab),
    effectif   = as.integer(tab),
    pourcentage = round(100 * as.numeric(prop), 2)
  )[order(-as.integer(tab)), ]
}

plot_cat = function(x, var_name) {
  tab = table(x, useNA = "ifany")
  pct = round(100 * prop.table(tab), 1)
  
  op = par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)
  
  bp1 = barplot(
    tab,
    col = col_bar, border = "white",
    las = 1, cex.names = 0.9,
  main = paste("Effectifs -", var_name),
    ylab = "Effectif"
  )
  
  grid(nx = NA, ny = NULL)
  
}


for (v in vars_cat) {
  cat("\n====================================================\n")
  cat("Variable catégorielle :", v, "\n")
  cat("===================================================\n")
  
  print(table_categorielle(df[[v]]))
  
  # Figure 2-en-1
  plot_cat(df[[v]], v)
}

# _______________________________________________________________________________________________
# 3) Analyse bivariée vs is_fraud
# _______________________________________________________________________________________________


# 3.1) Outils

# Résumé numérique par groupe
resume_num_par_groupe = function(x, g) {
  out = by(x, g, function(v) {
    c(
      n      = length(v),
  mean   = mean(v),
      sd     = sd(v),
      median = median(v),
      IQR    = IQR(v),
      min    = min(v),
      max    = max(v)
    )
  })
  res = do.call(rbind, out)
  data.frame(groupe = rownames(res), res, row.names = NULL, check.names = FALSE)
}


# Shapiro
shapiro_test = function(x, alpha = 0.05, max_n = 5000) {
  n = length(x)
  
  # Conditions d'application du test de Shapiro-Wilk :
  # - R l'accepte pour n entre 3 et 5000
  # - au-delà, le test est refusé et de toute facon trop sensible
  if (n < 3) {
    stop("Shapiro-Wilk : n < 3 (test impossible).")
  }
  if (n > max_n) {
    set.seed(1)
    x = sample(x, max_n)
    n = length(x)
    # On fait le test sur un sous-échantillon (propre et reproductible)
  }
  
  p = shapiro.test(x)$p.value
  list(ok = (p > alpha), p = p, n = n)
}

# Bartlett (uniquement si normalité plausible dans les 2 groupes)
bartlett_test = function(x, g) {
  bt = bartlett.test(x ~ g)
  list(p = bt$p.value, method = "Bartlett (normalité requise)")
}

# Variances : Brown-Forsythe (Levene médiane) robuste
brown_forsythe = function(x, g) {
  z = abs(x - ave(x, g, FUN = median))
  fit = oneway.test(z ~ g)
  list(p = fit$p.value, method = "Brown-Forsythe (Levene médiane)")
}

cramers_v = function(tab) {
  chi = suppressWarnings(chisq.test(tab, correct = FALSE))
  n = sum(tab)
  k = min(nrow(tab) - 1, ncol(tab) - 1)
  V = sqrt(as.numeric(chi$statistic) / (n * k))
  list(V = V)
}



# 3.2) NUMERIQUES vs is_fraud (conditions + tests)
  analyse_num_vs_fraud = function(df, var_name,
                                alpha = 0.05,
                                tcl_n = 30,
                                show_plots = TRUE) {
  
  x = df[[var_name]]
  g = df$is_fraud
  
  cat("\n===================================================\n")
  cat("Bivariée NUM :", var_name, "vs is_fraud\n")
  cat("====================================================\n")
  
  # Descriptif
  print(resume_num_par_groupe(x, g))
  
  # Groupes
  x_legit = x[g == "Legit"]
  x_fraud = x[g == "Fraud"]
  n_legit = length(x_legit)
  n_fraud = length(x_fraud)
  
  
  # Plots
  if (show_plots) {
    op = par(no.readonly = TRUE)
    par(mfrow = c(1, 3), mar = c(4,4,4,1) + 0.1)
    
    boxplot(x ~ g, main = paste("Boxplot :", var_name),
            xlab = "Statut", ylab = var_name)
    grid(nx = NA, ny = NULL)
    
    for (lev in levels(g)) {
      v = x[g == lev]
      qqnorm(v, main = paste("QQ-plot", var_name, "-", lev))
      qqline(v, col = "#2F2F2F")
    }
  par(op)
  }
  
  # -------- Vérification conditions : normalité --------
  cat("\n--- Normalité ---\n")
  
  
  sh_legit = shapiro_test(x_legit, alpha)
  sh_fraud = shapiro_test(x_fraud, alpha)
  
  cat("Shapiro Legit : p =", signif(sh_legit$p, 4), "| normalité ~", sh_legit$ok, "\n")
  cat("Shapiro Fraud : p =", signif(sh_fraud$p, 4), "| normalité ~", sh_fraud$ok, "\n")
  
  # -------- TCL / CLT (pour l'inférence sur la moyenne) --------
  # Si n >= 30 dans chaque groupe -> approximation normale des moyennes raisonnable,
  # donc Welch t-test acceptable meme si Shapiro rejette la normalité.
  tcl_ok = (n_legit >= tcl_n && n_fraud >= tcl_n)
  cat("\n--- Condition TCL (n>= ", tcl_n, " dans chaque groupe) ---\n", sep = "")
  cat("TCL applicable =", tcl_ok, "\n")
  cat("Interpretation : TCL => on peut tester une difference de moyennes (Welch) meme si non-normal.\n")
  
  # -------- Variances --------
  cat("\n--- Homogénéité des variances ---\n")
  bf = brown_forsythe(x, g)
  cat(bf$method, ": p =", signif(bf$p, 4), "\n")
  
  # Bartlett uniquement si normalité plausible (les deux groupes)
  if (sh_legit$ok && sh_fraud$ok) {
    bt = bartlett_test(x, g)
    cat(bt$method, ": p =", signif(bt$p, 4), "\n")
  } else {
    cat("Bartlett : NON appliqué (normalité non validée -> test non fiable)\n")
  }
  
  # -------- Tests statistiques --------
  cat("\n--- Tests statistiques ---\n")
  
  # 1) Test paramétrique (différence de moyennes) :
  # - Si TCL OK -> Welch t-test autorisé (robuste variances inégales)
  # - Si TCL NON OK -> on ne fait Welch que si normalité OK des 2 groupes (sinon à éviter)
  do_welch = FALSE
  if (tcl_ok) {
    do_welch = TRUE
    cat("Décision Welch : OUI (TCL OK)\n")
  } else if (sh_legit$ok && sh_fraud$ok) {
    do_welch = TRUE
    cat("Décision Welch : OUI (normalité validée, malgré TCL non garanti)\n")
  } else {
    cat("Décision Welch : NON (TCL insuffisant + normalité non validée)\n")
  }
  
  if (do_welch) {
    tt = t.test(x ~ g, var.equal = FALSE)   # Welch
    cat("\n[Welch t-test] (différence de moyennes)\n")
    cat("p-value =", signif(tt$p.value, 4), "\n")
    cat("IC 95% (moyenne Legit - Fraud) : [",
        round(tt$conf.int[1], 3), "; ", round(tt$conf.int[2], 3), "]\n", sep = "")
  }
  
  # 2) Test non paramétrique (différence de distributions / rangs)
  # Ton exigence : si Shapiro rejette => on FAIT AUSSI Wilcoxon.
  # Ici on le fait dès qu'au moins un groupe est non-normal.
  do_wilcox = (!sh_legit$ok || !sh_fraud$ok)
  
  if (do_wilcox) {
    w = wilcox.test(x ~ g, exact = FALSE, conf.int = TRUE)
    cat("\n[Wilcoxon / Mann-Whitney] (robuste, basé sur rangs)\n")
    cat("p-value =", signif(w$p.value, 4), "\n")
    if (!is.null(w$conf.int)) {
      cat("IC 95% (shift Hodges-Lehmann) : [",
          round(w$conf.int[1], 3), "; ", round(w$conf.int[2], 3), "]\n", sep = "")
    }
  } else {
    cat("\nWilcoxon : optionnel (normalité validée dans les 2 groupes).\n")
  }
  
  cat("\nConclusion :\n")
  cat("- Welch cible la différence de MOYENNES (pertinent si TCL OK).\n")
  cat("- Wilcoxon cible une différence de DISTRIBUTIONS / positions (robuste à non-normalité/outliers).\n")
}


# 2.3) CATEGORIELLES vs is_fraud (conditions + tests)

analyse_cat_vs_fraud = function(df, var_name,
                                alpha = 0.05,
                                B_mc = 20000,
                                show_plots = TRUE) {
  
  x = df[[var_name]]
  g = df$is_fraud
  
  cat("\n====================================================\n")
  cat("Bivariée CAT :", var_name, "vs is_fraud\n")
  cat("===================================================\n")
  
  tab = table(x, g)
  print(tab)
  
  # Taux de fraude par modalité
  fraud_rate = prop.table(tab, 1)[, "Fraud"] * 100
  res_rate = data.frame(
    modalite = names(fraud_rate),
    taux_fraude_pct = round(as.numeric(fraud_rate), 2),
    row.names = NULL
  )
  cat("\nTaux de fraude (%) par modalité :\n")
  print(res_rate[order(-res_rate$taux_fraude_pct), ])
  
  if (show_plots) {
    op = par(no.readonly = TRUE)
    par(mar = c(8,4,4,1) + 0.1)
    barplot(fraud_rate, las = 2, cex.names = 0.85,
            main = paste("Taux de fraude (%) selon", var_name),
            ylab = "Taux de fraude (%)")
    grid(nx = NA, ny = NULL)
    par(op)
  }
  
  # Conditions Chi² : attendus
  chi_try = suppressWarnings(chisq.test(tab, correct = FALSE))
  expected = chi_try$expected
  min_exp = min(expected)
  n_low = sum(expected < 5)
  
  cat("\n--- Conditions Chi² ---\n")
  cat("Attendu minimal =", round(min_exp, 3), "\n")
  cat("Nombre de cases avec attendu < 5 =", n_low, "\n")
  cat("Régle : si attendus faibles -> Fisher (2x2) ou Chi² Monte-Carlo (rxc).\n")
  
  cat("\n--- Test statistique ---\n")
  is_2x2 = (nrow(tab) == 2 && ncol(tab) == 2)
  
  if (n_low > 0) {
    if (is_2x2) {
      cat("Choix : Fisher exact (2x2, attendus faibles)\n")
      f = fisher.test(tab)
      cat("p-value =", signif(f$p.value, 4), "\n")
      cat("Odds Ratio =", round(as.numeric(f$estimate), 3),
          "| IC 95% [", round(f$conf.int[1], 3), "; ", round(f$conf.int[2], 3), "]\n", sep = "")
    } else {
      cat("Choix : Chi² Monte-Carlo (rxc, attendus faibles)\n")
      chi_mc = chisq.test(tab, correct = FALSE, simulate.p.value = TRUE, B = B_mc)
      cat("p-value (MC) =", signif(chi_mc$p.value, 4), "\n")
      v = cramers_v(tab)
      cat("Cramer V =", round(v$V, 3), "\n")
    }
  } else {
    cat("Choix : Chi² (conditions OK)\n")
    chi = chisq.test(tab, correct = FALSE)
    cat("p-value =", signif(chi$p.value, 4), "\n")
    v = cramers_v(tab)
    cat("Cramer V =", round(v$V, 3), "\n")
    
  if (is_2x2) {
      # OR + IC via Fisher (plus informatif en 2x2)
      f = fisher.test(tab)
      cat("Odds Ratio =", round(as.numeric(f$estimate), 3),
          "| IC 95% [", round(f$conf.int[1], 3), "; ", round(f$conf.int[2], 3), "]\n", sep = "")
    }
}
}


# 2.4) Lancement

vars_num = c("amount", "device_trust_score", "velocity_last_24h", "cardholder_age")
vars_cat = c("hour_group", "merchant_category", "foreign_transaction", "location_mismatch")

for (v in vars_num) analyse_num_vs_fraud(df, v, alpha = 0.05, tcl_n = 30, show_plots = TRUE)
for (v in vars_cat) analyse_cat_vs_fraud(df, v, alpha = 0.05, B_mc = 20000, show_plots = TRUE)


# ___________________________________________________________________
# 4) LOGISTIC REGRESSION
# _______________________________________________________________________________________________

set.seed(123)


# 4.1) STYLE GLOBAL (couleurs & mise en page)


cols = c("#4C78A8", "#F58518", "#54A24B", "#E45756", "#72B7B2", "#B279A2")
model_colors = setNames(cols, c("MAIN_unw","MAIN_w","INTER_unw","INTER_w","STEP_unw","STEP_w"))

col_legit = grDevices::adjustcolor("#4C78A8", alpha.f = 0.35)
col_fraud = grDevices::adjustcolor("#E45756", alpha.f = 0.35)

par(bg = "white")
par(mar = c(4.2, 4.2, 3.2, 1.2))

fmt = function(x, k = 4) formatC(x, format = "f", digits = k)


# 4.2) OUTILS GENERAUX


to_binary = function(y, positive = "Fraud") {
  as.integer(y == positive)
}

clip_prob = function(p, eps = 1e-15) {
  p = pmin(pmax(p, eps), 1 - eps)
  p
}

# Points ROC (FPR, TPR)
roc_points = function(y_true, proba, positive = "Fraud") {
  y = to_binary(y_true, positive)
  o = order(proba, decreasing = TRUE)
  y = y[o]
  
  tp = cumsum(y)
  fp = cumsum(1 - y)
  
  tpr = tp / sum(y)
  fpr = fp / sum(1 - y)
  
  data.frame(fpr = c(0, fpr, 1), tpr = c(0, tpr, 1))
}

# Points PR (Recall, Precision)
pr_points = function(y_true, proba, positive = "Fraud") {
  y = to_binary(y_true, positive)
  o = order(proba, decreasing = TRUE)
  y = y[o]
  
  tp = cumsum(y)
  fp = cumsum(1 - y)
  
  precision = tp / (tp + fp)
  recall = tp / sum(y)
  data.frame(recall = c(0, recall), precision = c(1, precision))
}

# ROC AUC (Mann-Whitney via rangs)
roc_auc_rank = function(y_true, proba, positive = "Fraud") {
  y = (y_true == positive)
  n_pos = sum(y)
  n_neg = sum(!y)
  
  r = rank(proba, ties.method = "average")
  auc = (sum(r[y]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
  as.numeric(auc)
}

# PR AUC (trapèzes)
pr_auc_trapz = function(y_true, proba, positive = "Fraud") {
  pts = pr_points(y_true, proba, positive)
  recall = pts$recall
  precision = pts$precision
  
  sum((recall[-1] - recall[-length(recall)]) *
        (precision[-1] + precision[-length(precision)]) / 2)
}

# Brier score (calibration globale)
brier_score = function(y_true, proba, positive = "Fraud") {
  y = to_binary(y_true, positive)
  mean((y - proba)^2)
}

# Matrice de confusion + métriques (à un seuil thr)
conf_metrics = function(y_true, proba, thr = 0.5, positive = "Fraud") {
  y_true = factor(y_true, levels = c("Legit", "Fraud"))
  pred = ifelse(proba >= thr, positive, "Legit")
  pred = factor(pred, levels = c("Legit", "Fraud"))
  
  TP = sum(pred == positive & y_true == positive)
  FP = sum(pred == positive & y_true != positive)
  TN = sum(pred != positive & y_true != positive)
  FN = sum(pred != positive & y_true == positive)
  
  precision = if ((TP + FP) == 0) 0 else TP / (TP + FP)
  recall = if ((TP + FN) == 0) 0 else TP / (TP + FN)
  f1 = if ((precision + recall) == 0) 0 else 2 * precision * recall / (precision + recall)
  
  specificity = if ((TN + FP) == 0) 0 else TN /(TN + FP)
  bal_acc = (recall + specificity) / 2
  
  list(TP = TP, FP = FP, TN = TN, FN = FN,
       precision = precision, recall = recall, f1 = f1,
       specificity = specificity, balanced_accuracy = bal_acc)
}

# Seuil optimal (max F1)
best_threshold_f1 = function(y_true, proba) {
  grid = seq(0.01, 0.99, by = 0.01)
  f1s = sapply(grid, function(t) conf_metrics(y_true, proba, thr = t)$f1)
  t_best = grid[which.max(f1s)]
  list(threshold = t_best, best_f1 = max(f1s))
}

# Tableau coefficients (OR + IC + p-value)
coef_table = function(model) {
  b = coef(model)
  se = sqrt(diag(vcov(model)))
  z = b / se
  p = 2 * pnorm(-abs(z))
  
  OR = exp(b)
  CI_low = exp(b - 1.96 * se)
  CI_high = exp(b + 1.96 * se)
  
  out = data.frame(
    term = names(b),
    coef = as.numeric(b),
    SE = as.numeric(se),
    OR = as.numeric(OR),
    CI_low = as.numeric(CI_low),
    CI_high = as.numeric(CI_high),
    p_value = as.numeric(p)
  )
  
  out[order(out$p_value), ]
}


# 4.4) CALIBRATION 


calibration_bins = function(y_true, proba, n_bins = 10, positive = "Fraud") {
  y = to_binary(y_true, positive)
  proba = clip_prob(proba)
  
  cuts = seq(0, 1, length.out = n_bins + 1)
  bin = cut(proba, breaks = cuts, include.lowest = TRUE)
  
  mean_pred = tapply(proba, bin, mean)
  obs_rate = tapply(y, bin, mean)
  n = tapply(y, bin, length)
  
  out = data.frame(
    bin = levels(bin),
    n = as.numeric(n),
    mean_pred = as.numeric(mean_pred),
    obs_rate = as.numeric(obs_rate)
  )
  
  out = out[is.finite(out$mean_pred) & is.finite(out$obs_rate), ]
  out
}

ece_score = function(calib_df) {
  w = calib_df$n / sum(calib_df$n)
  sum(w * abs(calib_df$obs_rate - calib_df$mean_pred))
}

calibration_intercept_slope = function(y_true, proba, positive = "Fraud") {
  y = to_binary(y_true, positive)
  p = clip_prob(proba)
  logit_p = log(p / (1 - p))
  
  # a ~ 0 et b ~ 1 => bonne calibration
  fit = glm(y ~ logit_p, family = binomial())
  list(intercept = as.numeric(coef(fit)[1]), slope = as.numeric(coef(fit)[2]))
}

plot_calibration = function(y_true, proba, n_bins = 10, main = "Calibration", positive = "Fraud") {
  calib = calibration_bins(y_true, proba, n_bins, positive)
  ece = ece_score(calib)
  
  plot(c(0, 1), c(0, 1),
       type = "n", xlab = "Probabilité prédite (moyenne par bin)",
         ylab = "Taux observé (moyenne par bin)",
       main = paste0(main, "  |  ECE=", fmt(ece, 3)),
       xlim = c(0, 1), ylim = c(0, 1))
  grid(col = "#E6E6E6")
  abline(0, 1, lty = 2, col = "#666666", lwd = 2)
  
  # Taille point ~ nombre d'observations
  cex_pts = 0.7 + 1.6 * (calib$n / max(calib$n))
  points(calib$mean_pred, calib$obs_rate, pch = 19,
         cex = cex_pts, col = "#1f77b4")
  lines(calib$mean_pred, calib$obs_rate, lwd = 2, col = "#1f77b4")
  
  # Lissage (tendance)
  sm = lowess(calib$mean_pred, calib$obs_rate, f = 2/3)
  lines(sm, lwd = 2, lty = 3, col = "#E45756")
  
  invisible(calib)
}


# 4.5) PREPARATION DATA


# Feature robuste: log(1 + amount)
df$log_amount = log1p(df$amount)

# Nettoyage variables inutiles/redondantes
df$transaction_id = NULL
df$transaction_hour = NULL
df$amount = NULL


# 4.6) SPLIT STRATIFIE (fraude rare => indispensable)


stratified_split = function(df, p_train = 0.80) {
  idx_legit = which(df$is_fraud == "Legit")
  idx_fraud = which(df$is_fraud == "Fraud")
  
   n_legit_train = floor(p_train * length(idx_legit))
  n_fraud_train = floor(p_train * length(idx_fraud))
  
  idx_train = c(sample(idx_legit, n_legit_train), sample(idx_fraud, n_fraud_train))
  list(train = df[idx_train, ], test = df[-idx_train, ])
}

sp = stratified_split(df, p_train = 0.80)
train = sp$train
test = sp$test

# Meme niveaux de facteurs train/test (sinon predict casse)
for (v in names(train)) {
  if (is.factor(train[[v]])) test[[v]] = factor(test[[v]], levels = levels(train[[v]]))
}

cat("\nFraud rate train =", round(100 * mean(train$is_fraud == "Fraud"), 2), "%\n")
cat("Fraud rate test  =", round(100 * mean(test$is_fraud == "Fraud"), 2), "%\n")


# 4.7) POIDS (weighted) : équilibrer la classe rare


w_ratio = round(sum(train$is_fraud == "Legit") / sum(train$is_fraud == "Fraud"))
train$w = 1
train$w[train$is_fraud == "Fraud"] = w_ratio


# 4.8) FORMULES (MAIN / INTER / STEP)


f_main = is_fraud ~ log_amount + device_trust_score + velocity_last_24h + cardholder_age +
  merchant_category + foreign_transaction + location_mismatch + hour_group

# Interactions "logiques" + petites non-linéarités (quadratiques)
f_inter = is_fraud ~ log_amount + I(log_amount^2) +
  device_trust_score + velocity_last_24h + I(velocity_last_24h^2) +
  cardholder_age +
  merchant_category + foreign_transaction + location_mismatch + hour_group +
  foreign_transaction:location_mismatch +
  foreign_transaction:hour_group +
  merchant_category:foreign_transaction +
  device_trust_score:velocity_last_24h

# Scope large (toutes interactions 2-way) - peut etre lent
scope_up = as.formula("~ (log_amount + device_trust_score + velocity_last_24h + cardholder_age +
                      merchant_category + foreign_transaction + location_mismatch + hour_group)^2")


# 4.9) ENTRAINEMENT DES 6 MODELES


# MAIN
m_main_unw = glm(f_main, data = train, family = binomial(), control = glm.control(maxit = 100))
m_main_w   = glm(f_main, data = train, family = binomial(), weights = w, control = glm.control(maxit = 100))

# INTER
m_inter_unw = glm(f_inter, data = train, family = binomial(), control = glm.control(maxit = 100))
m_inter_w   = glm(f_inter, data = train, family = binomial(), weights = w, control = glm.control(maxit = 100))

# STEP AIC (unweighted)
m0_unw = glm(f_main, data = train, family = binomial(), control = glm.control(maxit = 100))
m_step_unw = step(m0_unw, scope = list(lower = f_main, upper = scope_up),
                  direction = "both", trace = 0)

# STEP AIC (weighted)
m0_w = glm(f_main, data = train, family = binomial(), weights = w, control = glm.control(maxit = 100))
m_step_w = step(m0_w, scope = list(lower = f_main, upper = scope_up),
                direction = "both", trace = 0)

models = list(
  MAIN_unw = m_main_unw,
  MAIN_w   = m_main_w,
  INTER_unw = m_inter_unw,
  INTER_w   = m_inter_w,
  STEP_unw = m_step_unw,
  STEP_w   = m_step_w
)


# 4.10) EVALUATION


eval_one = function(name, model, test_df) {
  p = clip_prob(predict(model, newdata = test_df, type = "response"))
  
  roc = roc_auc_rank(test_df$is_fraud, p)
  pr  = pr_auc_trapz(test_df$is_fraud, p)
  bs  = brier_score(test_df$is_fraud, p)
  
  bt = best_threshold_f1(test_df$is_fraud, p)
  cm = conf_metrics(test_df$is_fraud, p, thr = bt$threshold)
  
  cal = calibration_intercept_slope(test_df$is_fraud, p)
  ece = ece_score(calibration_bins(test_df$is_fraud, p, n_bins = 10))
  
  cat("\n====================================================\n")
  cat("MODEL:", name, "\n")
  cat("ROC AUC:", fmt(roc), " | PR AUC:", fmt(pr),
      " | Brier:", fmt(bs), "\n")
  cat("Calibration: intercept=", fmt(cal$intercept, 3),
      " slope=", fmt(cal$slope, 3),
      " | ECE=", fmt(ece, 3), "\n")
  cat("Best F1 threshold:", fmt(bt$threshold, 2), " | F1:", fmt(bt$best_f1, 4), "\n")
  cat("TP=", cm$TP, " FP=", cm$FP, " TN=", cm$TN, " FN=", cm$FN, "\n")
  cat("Precision=", fmt(cm$precision), " Recall=", fmt(cm$recall),
      " F1=", fmt(cm$f1), " BalancedAcc=", fmt(cm$balanced_accuracy), "\n")
  cat("====================================================\n")
  
  list(
    name = name, proba = p,
    roc_auc = roc, pr_auc = pr, brier = bs,
    calib_intercept = cal$intercept, calib_slope = cal$slope, ece = ece,
    best_thr = bt$threshold
  )
}

evals = lapply(names(models), function(nm) eval_one(nm, models[[nm]], test))
names(evals) = names(models)

# Tableau comparatif (arrondi + tri PR AUC)
results = data.frame(
  model = names(evals),
  roc_auc = round(sapply(evals, function(z) z$roc_auc), 4),
  pr_auc  = round(sapply(evals, function(z) z$pr_auc), 4),
  brier   = round(sapply(evals, function(z) z$brier), 4),
  calib_intercept = round(sapply(evals, function(z) z$calib_intercept), 3),
  calib_slope     = round(sapply(evals, function(z) z$calib_slope), 3),
  ece             = round(sapply(evals, function(z) z$ece), 3),
  best_thr        = round(sapply(evals, function(z) z$best_thr), 2)
)

cat("\n==================== TABLEAU COMPARATIF (tri PR AUC) ====================\n")
print(results[order(-results$pr_auc), ], row.names = FALSE)

best_name = results$model[which.max(results$pr_auc)]
best_model = models[[best_name]]
best_proba = evals[[best_name]]$proba

# 4.11) GRAPHES


# ROC (6 modèles)
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
     xlab = "False Positive Rate (FPR)",
     ylab = "True Positive Rate (TPR)",
     main = "ROC Curves (6 modÃ¨les)")
grid(col = "#E6E6E6")
abline(0, 1, lty = 2, col = "#666666", lwd = 2)

for (nm in names(evals)) {
  pts = roc_points(test$is_fraud, evals[[nm]]$proba)
  lines(pts$fpr, pts$tpr, col = model_colors[nm], lwd = 2)
}

leg_roc = sapply(names(evals), function(nm) {
  paste0(nm, " (AUC=", fmt(evals[[nm]]$roc_auc, 3), ")")
})
legend("bottomright", legend = leg_roc, col = model_colors[names(evals)],
       lwd = 2, cex = 0.8, bty = "n")

# PR (6 modÃ¨les)
plot(0, 1, type = "n", xlim = c(0, 1), ylim = c(0, 1),
     xlab = "Recall",
     ylab = "Precision",
     main = "Precision-Recall Curves (6 modèles)")
grid(col = "#E6E6E6")

for (nm in names(evals)) {
  pts = pr_points(test$is_fraud, evals[[nm]]$proba)
  lines(pts$recall, pts$precision, col = model_colors[nm], lwd = 2)
}

leg_pr = sapply(names(evals), function(nm) {
  paste0(nm, " (AUC=", fmt(evals[[nm]]$pr_auc, 3), ")")
})
legend("topright", legend = leg_pr, col = model_colors[names(evals)],
       lwd = 2, cex = 0.8, bty = "n")

# Histogramme des probabilités (best model)
p_legit = best_proba[test$is_fraud == "Legit"]
p_fraud = best_proba[test$is_fraud == "Fraud"]

hist(p_legit, breaks = 30, freq = FALSE,
     main = paste("Distribution P(Fraud) -", best_name),
     xlab = "Probabilité prédite", xlim = c(0, 1),
     col = col_legit, border = "white")
hist(p_fraud, breaks = 30, freq = FALSE, add = TRUE,
     col = col_fraud, border = "white")
grid(col = "#E6E6E6")
legend("topright", legend = c("Legit", "Fraud"),
       fill = c(col_legit, col_fraud), bty = "n")

# Calibration (best model)
plot_calibration(test$is_fraud, best_proba, n_bins = 10,
                 main = paste("Calibration (10 bins) -", best_name))


# 4.12) INTERPRETATION : TOP OR (best model)


cat("\n==================== TOP 15 VARIABLES (p-value) ===================\n")
top15 = head(coef_table(best_model), 15)
top15$OR = round(top15$OR, 3)
top15$CI_low = round(top15$CI_low, 3)
top15$CI_high = round(top15$CI_high, 3)
top15$p_value = signif(top15$p_value, 3)
print(top15, row.names = FALSE)