
# Estimating heritability from repeated measures with regress package ----
fit_h2_regress <- function(
    df,
    y_var,
    id_var,
    family_var,
    zygosity_var,
    mz_label = "MZ",
    dz_label = "DZ"
) {
  
  ## ---------------------------
  ## Basic checks
  ## ---------------------------
  stopifnot(
    y_var %in% names(df),
    id_var %in% names(df),
    family_var %in% names(df),
    zygosity_var %in% names(df)
  )
  
  ## ---------------------------
  ## Extract columns
  ## ---------------------------
  df <- df[!is.na(df[[y_var]]), ]
  
  df$id       <- factor(df[[id_var]])
  df$family   <- df[[family_var]]
  df$zygosity <- df[[zygosity_var]]
  
  ids <- levels(df$id)
  
  ## ---------------------------
  ## Build subject-level df
  ## ---------------------------
  subject_df <- unique(
    df[, c("id", "family", "zygosity")]
  )
  subject_df <- subject_df[match(ids, subject_df$id), ]
  
  ## ---------------------------
  ## Fixed effects (intercept only)
  ## ---------------------------
  X <- model.matrix(~ 1, data = df)
  
  ## ---------------------------
  ## Build T matrix
  ## ---------------------------
  n <- nrow(df)
  m <- length(ids)
  
  Tmat <- Matrix::sparseMatrix(
    i = seq_len(n),
    j = as.integer(df$id),
    x = 1,
    dims = c(n, m)
  )
  
  ## ---------------------------
  ## Build K (additive genetics)
  ## ---------------------------
  phi <- matrix(0, m, m)
  
  for (i in seq_len(m)) {
    for (j in seq_len(m)) {
      
      if (subject_df$family[i] == subject_df$family[j]) {
        
        if (i == j) {
          phi[i, j] <- 0.5
        } else if (
          subject_df$zygosity[i] == mz_label &&
          subject_df$zygosity[j] == mz_label
        ) {
          phi[i, j] <- 0.5
        } else if (
          subject_df$zygosity[i] == dz_label &&
          subject_df$zygosity[j] == dz_label
        ) {
          phi[i, j] <- 0.25
        }
      }
    }
  }
  
  K <- 2 * phi
  
  ## ---------------------------
  ## Shared environment (Lambda)
  ## ---------------------------
  Lambda <- outer(
    subject_df$family,
    subject_df$family,
    FUN = function(a, b) as.integer(a == b)
  )
  diag(Lambda) <- 0
  
  ## ---------------------------
  ## Covariance bases
  ## ---------------------------
  V_A <- as.matrix(Tmat %*% K %*% t(Tmat))
  V_C <- as.matrix(Tmat %*% Lambda %*% t(Tmat))
  V_E <- as.matrix(Tmat %*% Matrix::Diagonal(m) %*% t(Tmat))
  
  ## ---------------------------
  ## Fit model
  ## ---------------------------
  fit <- regress::regress(
    df[[y_var]] ~ X,
    ~ V_A + V_C + V_E,
    pos = c(TRUE, TRUE, TRUE)
  )
  
  vc <- fit$sigma
  names(vc) <- c("V_A", "V_C", "V_E", "V_M")
  
  h2 <- vc["V_A"] / (vc["V_A"] + vc["V_C"] + vc["V_E"])
  
  ## ---------------------------
  ## Return clean object
  ## ---------------------------
  list(
    outcome = y_var,
    vc = vc,
    h2 = as.numeric(h2),
    fit = fit
  )
}




# Jackknife bootstrapping for ACE variance components confidence intervals
fit_h2_regress_boot <- function(
    df,
    y_var,
    id_var,
    family_var,
    zygosity_var,
    conf_level = 0.95,
    mz_label = "MZ",
    dz_label = "DZ"
) {
  
  ## ============================
  ## Internal helper: single fit
  ## ============================
  .fit_once <- function(df_sub) {
    df_sub <- df_sub[!is.na(df_sub[[y_var]]), ]
    df_sub$id       <- factor(df_sub[[id_var]])
    df_sub$family   <- df_sub[[family_var]]
    df_sub$zygosity <- df_sub[[zygosity_var]]
    
    ids <- levels(df_sub$id)
    subject_df <- unique(df_sub[, c("id", "family", "zygosity")])
    subject_df <- subject_df[match(ids, subject_df$id), ]
    
    X <- model.matrix(~ 1, data = df_sub)
    n <- nrow(df_sub)
    m <- length(ids)
    
    Tmat <- Matrix::sparseMatrix(
      i = seq_len(n), j = as.integer(df_sub$id), x = 1, dims = c(n, m)
    )
    
    phi <- matrix(0, m, m)
    for (i in seq_len(m)) {
      for (j in seq_len(m)) {
        if (subject_df$family[i] == subject_df$family[j]) {
          if (i == j) { phi[i, j] <- 0.5 } 
          else if (subject_df$zygosity[i] == mz_label && subject_df$zygosity[j] == mz_label) { phi[i, j] <- 0.5 } 
          else if (subject_df$zygosity[i] == dz_label && subject_df$zygosity[j] == dz_label) { phi[i, j] <- 0.25 }
        }
      }
    }
    
    K <- 2 * phi
    Lambda <- outer(subject_df$family, subject_df$family, FUN = function(a, b) as.integer(a == b))
    
    V_A <- as.matrix(Tmat %*% K %*% t(Tmat))
    V_C <- as.matrix(Tmat %*% Lambda %*% t(Tmat))
    V_E <- as.matrix(Tmat %*% Matrix::Diagonal(m) %*% t(Tmat))
    
    fit <- regress::regress(df_sub[[y_var]] ~ X, ~ V_A + V_C + V_E, pos = c(TRUE, TRUE, TRUE))
    
    # Extract only the three primary ACE components
    vc <- fit$sigma[1:3] # omit the residual error V_M, which is the 4th term
    names(vc) <- c("V_A", "V_C", "V_E")
    return(vc)
  }
  
  ## ============================
  ## Full-sample fit
  ## ============================
  vc_full <- .fit_once(df)
  
  ## ============================
  ## Jackknife over families
  ## ============================
  families <- unique(df[[family_var]])
  K <- length(families)
  
  # Matrix to store V_A, V_C, V_E for each jackknife sample
  vc_jack_mat <- matrix(NA, nrow = K, ncol = 3)
  colnames(vc_jack_mat) <- c("V_A", "V_C", "V_E")
  
  for (k in seq_along(families)) {
    fam <- families[k]
    df_k <- df[df[[family_var]] != fam, ]
    
    vc_jack_mat[k, ] <- tryCatch(
      .fit_once(df_k),
      error = function(e) rep(NA_real_, 3)
    )
  }
  
  # Remove failed iterations
  vc_jack_mat <- vc_jack_mat[complete.cases(vc_jack_mat), ]
  K_eff       <- nrow(vc_jack_mat)
  
  ## ============================
  ## Jackknife Stats Calculation
  ## ============================
  # Mean of jackknife estimates
  vc_bar <- colMeans(vc_jack_mat)
  
  # Jackknife Variance: ((K-1)/K) * sum((theta_k - theta_bar)^2)
  # We apply this across columns (V_A, V_C, V_E)
  var_vc <- ((K_eff - 1) / K_eff) * colSums(sweep(vc_jack_mat, 2, vc_bar, "-")^2)
  se_vc  <- sqrt(var_vc)
  
  # CI Calculation
  alpha    <- 1 - conf_level
  zval     <- qnorm(1 - alpha / 2)
  ci_lower <- vc_full - (zval * se_vc)
  ci_upper <- vc_full + (zval * se_vc)
  
  ## ============================
  ## Format Results Table
  ## ============================
  results <- data.frame(
    Estimate = vc_full,
    Std.Error = se_vc,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper
  )
  
  return(list(
    outcome     = y_var,
    results     = results,
    conf_level  = conf_level,
    iterations  = K_eff,
    raw_jack    = vc_jack_mat
  ))
}




# Old Jacknife function that only estimated h2 ---------------------------------
# # Jackknife bootstrapping for heritability confidence intervals ----
# fit_h2_regress_boot <- function(
#     df,
#     y_var,
#     id_var,
#     family_var,
#     zygosity_var,
#     conf_level = 0.95,
#     mz_label = "MZ",
#     dz_label = "DZ"
# ) {
#   
#   ## ============================
#   ## Internal helper: single fit
#   ## ============================
#   .fit_once <- function(df_sub) {
#     
#     df_sub <- df_sub[!is.na(df_sub[[y_var]]), ]
#     
#     df_sub$id       <- factor(df_sub[[id_var]])
#     df_sub$family   <- df_sub[[family_var]]
#     df_sub$zygosity <- df_sub[[zygosity_var]]
#     
#     ids <- levels(df_sub$id)
#     
#     ## Subject-level info
#     subject_df <- unique(df_sub[, c("id", "family", "zygosity")])
#     subject_df <- subject_df[match(ids, subject_df$id), ]
#     
#     ## Fixed effects (intercept only)
#     X <- model.matrix(~ 1, data = df_sub)
#     
#     ## T matrix
#     n <- nrow(df_sub)
#     m <- length(ids)
#     
#     Tmat <- Matrix::sparseMatrix(
#       i = seq_len(n),
#       j = as.integer(df_sub$id),
#       x = 1,
#       dims = c(n, m)
#     )
#     
#     ## Kinship (phi) → K
#     phi <- matrix(0, m, m)
#     
#     for (i in seq_len(m)) {
#       for (j in seq_len(m)) {
#         if (subject_df$family[i] == subject_df$family[j]) {
#           if (i == j) {
#             phi[i, j] <- 0.5
#           } else if (
#             subject_df$zygosity[i] == mz_label &&
#             subject_df$zygosity[j] == mz_label
#           ) {
#             phi[i, j] <- 0.5
#           } else if (
#             subject_df$zygosity[i] == dz_label &&
#             subject_df$zygosity[j] == dz_label
#           ) {
#             phi[i, j] <- 0.25
#           }
#         }
#       }
#     }
#     
#     K <- 2 * phi
#     
#     ## Shared environment (Lambda)
#     Lambda <- outer(
#       subject_df$family,
#       subject_df$family,
#       FUN = function(a, b) as.integer(a == b)
#     )
#     
#     ## Covariance bases
#     V_A <- as.matrix(Tmat %*% K %*% t(Tmat))
#     V_C <- as.matrix(Tmat %*% Lambda %*% t(Tmat))
#     V_E <- as.matrix(Tmat %*% Matrix::Diagonal(m) %*% t(Tmat))
#     
#     ## Fit model
#     fit <- regress::regress(
#       df_sub[[y_var]] ~ X,
#       ~ V_A + V_C + V_E,
#       pos = c(TRUE, TRUE, TRUE)
#     )
#     
#     vc <- fit$sigma
#     names(vc) <- c("V_A", "V_C", "V_E", "V_M")
#     
#     h2 <- vc["V_A"] / (vc["V_A"] + vc["V_C"] + vc["V_E"])
#     
#     list(vc = vc, h2 = as.numeric(h2))
#   }
#   
#   ## ============================
#   ## Full-sample fit
#   ## ============================
#   full_fit <- .fit_once(df)
#   h2_full  <- full_fit$h2
#   
#   ## ============================
#   ## Jackknife over families
#   ## ============================
#   families <- unique(df[[family_var]])
#   K <- length(families)
#   
#   h2_jack <- numeric(K)
#   
#   for (k in seq_along(families)) {
#     fam <- families[k]
#     df_k <- df[df[[family_var]] != fam, ]
#     
#     h2_jack[k] <- tryCatch(
#       .fit_once(df_k)$h2,
#       error = function(e) NA_real_
#     )
#   }
#   
#   h2_jack <- h2_jack[!is.na(h2_jack)]
#   K_eff   <- length(h2_jack)
#   
#   ## ============================
#   ## Jackknife SE & CI
#   ## ============================
#   h2_bar <- mean(h2_jack)
#   
#   var_h2 <- (K_eff - 1) / K_eff *
#     sum((h2_jack - h2_bar)^2)
#   
#   se_h2 <- sqrt(var_h2)
#   
#   alpha <- 1 - conf_level
#   zval  <- qnorm(1 - alpha / 2)
#   
#   ci_lower <- h2_full - zval * se_h2
#   ci_upper <- h2_full + zval * se_h2
#   
#   ## ============================
#   ## Return object
#   ## ============================
#   list(
#     outcome     = y_var,
#     vc          = full_fit$vc,
#     h2          = h2_full,
#     se          = se_h2,
#     conf_level  = conf_level,
#     ci          = c(lower = ci_lower, upper = ci_upper),
#     h2_jack     = h2_jack
#   )
# }


