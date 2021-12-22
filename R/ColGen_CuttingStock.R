#' @title A Column Generate method in R.
#' @description The method is to get continuous solution of cutting stock problem.
#' @param demand_vec A vector of 2 columns, denotes the needed length and its corresponding number.
#' @param total_len the length of unit stock.
#' @return a list containing optimal cost, cutting plan of a stock and the corresponding number needed.
#' @importFrom linprog solveLP
#' @importFrom Rsymphony Rsymphony_solve_LP
#' @examples
#' \dontrun{
#' demand_vec <- matrix(c(3, 6, 7, 25, 20, 18), nrow=3)
#' total_len = 16
#' res <- ColGen_CuttingStock(demand_vec, total_len)
#' }
#' @export
ColGen_CuttingStock <- function(demand_vec, total_len){
  # Return the number of stocks and cutting plan of each stock.
  # :demand_vec: an n*2 data frame recording customers'
  # demand of different type of stocks.
  # :total_len: int type representing total length of unit stock.
  
  # --------------------------------------------
  # We first calculate a feasible solution.
  n = nrow(demand_vec)
  m = n
  A = diag(ceiling(total_len/demand_vec[,1]-1))
  c = rep(1, m)
  b = demand_vec[,2]
  dir = rep(">=", n)
  res = solveLP(c, b, A, const.dir = dir)
  # --------------------------------------------
  # start the dual part: integer programming.
  dual_c = res$con$dual
  dual_res = Rsymphony_solve_LP(-dual_c, demand_vec[,1], "<=", total_len, types = rep("I", n), max = F)
  reduced_price = 1+dual_res$objval
  # --------------------------------------------
  # When reduced price >= 0, we can terminate the process to get the optimal;
  # or we may add one column to the master problem.
  while(reduced_price < 0){
    new_col = dual_res$solution
    m = m + 1
    A = matrix(cbind(A, new_col), nrow=n)
    c = rep(1, m)
    res = solveLP(c, b, A, const.dir = dir)
    # --------------------------------------------
    # start the dual part: integer programming.
    dual_c = res$con$dual
    dual_res = Rsymphony_solve_LP(-dual_c, demand_vec[,1], "<=", total_len, types = rep("I", n), max = F)
    reduced_price = 1+dual_res$objval
  }
  result = list(value = t(floor(res$solution))%*%t(A)%*%rep(1, n), cutting_plan = A, num_demand = floor(res$solution))
  return(result)
}