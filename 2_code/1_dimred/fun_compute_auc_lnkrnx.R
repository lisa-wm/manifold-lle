# ------------------------------------------------------------------------------
# COMPUTE AUC_LNK_RNX
# ------------------------------------------------------------------------------

# Purpose: compute the area under the rnx curve

# TOP-LEVEL FUNCTION: COMPUTE AUC MEASURE --------------------------------------

compute_auc_lnk_rnx <- function(data, embeddings) {
  
  r_nx <- compute_rnx(data, embeddings)
  sum(r_nx / seq_along(r_nx)) / sum(1 / seq_along(r_nx))
  
}

# BOTTOM-LEVEL FUNCTION: COMPUTE RNX CURVE -------------------------------------

compute_rnx <- function(data, embeddings) {

  q_matrix <- suppressWarnings(
    as.matrix(coRanking::coranking(data, embeddings)))
  
  nrow_q <- nrow(q_matrix)
  n <- nrow_q + 1L
  
  q_nx <- diag(apply(apply(q_matrix, 2L, cumsum), 1L, cumsum)) /
    seq_len(nrow_q) / n
  
  r_nx <- ((n - 1L) * q_nx - seq_len(nrow_q)) / (n - 1L - seq_len(nrow_q))
  
  r_nx[-nrow_q]
  
}


