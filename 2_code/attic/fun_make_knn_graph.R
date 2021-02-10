# ------------------------------------------------------------------------------
# CREATING NEIGHBORHOOD GRAPHS
# ------------------------------------------------------------------------------

# Purpose: create neighborhood graph for geodesic distance computation

# FIXME MAKE OWN VERSION!!

make_knn_graph <- function(data, neighborhood_size) {
  
  edges_df <- as.data.frame(copy(data)[, edges := paste0("x_", .I)])
  
  vertices_df <- expand.grid(edges_df$edges, edges_df$edges)
  names(vertices_df) <- c("from", "to")
  
  knn_graph <- graph_from_data_frame(
    vertices_df, 
    directed = TRUE)
  
}


makeKNNgraph <- function (x, k, eps = 0, diag = FALSE){
  ## requireNamespace("RANN")
  ## requireNamespace("igraph")
  
  ## consts
  INF_VAL <- 1.340781e+15
  NA_IDX  <- 0
  BDKD_LIM <- 1000000                 #todo: figure out a good value here
  
  ## select parameters
  M <- nrow(x)
  treetype <- "kd"                # if (M < BDKD_LIM) "kd" else "bd"
  # see:
  # https://github.com/jefferis/RANN/issues/19
  searchtype <- if (eps == 0) "standard" else "priority"
  
  ## RANN::nn2 returns the points in data with respect to query
  ## e.g. the rows in the output are the points in query and the
  ## columns the points in data.
  nn2res <- RANN::nn2(data = x, query = x, k = k + 1, treetype = treetype,
                      searchtype = searchtype, eps = eps)
  
  ## create graph: the first ny nodes will be y, the last nx nodes
  ## will be x, if x != y
  ## it is not really pretty to create a
  ## directed graph first and then make it undirected.
  g <- igraph::make_empty_graph(M, directed = TRUE)
  g[from = if (diag) rep(seq_len(M), times = k + 1)
    else      rep(seq_len(M), times = k),
    to   = if (diag) as.vector(nn2res$nn.idx)
    else      as.vector(nn2res$nn.idx[, -1]),
    attr = "weight"] <-
    if (diag)  as.vector(nn2res$nn.dists)
  else as.vector(nn2res$nn.dists[, -1])
  
  return(igraph::as.undirected(g, mode = "collapse", edge.attr.comb = "first"))
}
