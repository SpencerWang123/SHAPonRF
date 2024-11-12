#' cv_rf_parallel
#' functional to determine the optimal number of variables used in each tree of the RF model using parallel computing
#' @param rf_dt_temp data.frame with first column as response variable.
#' @param K  K-fold cross validation
#' @param mtry_range range of number of variables used in each trees. example:c(1:10)
#' @param num_cores how many cores are used in parallel computing. default as 10
#' @param sample_ratio in case the data was too big, part of the data is used in the cross validation to enhance the efficiency of the algorithm; default as 0.5
#'
#' @return data.frame with 2 columns indicating the cross validation error of each mtry

cv_rf_parallel=function(rf_dt_temp,K=10,mtry_range,num_cores=4,sample_ratio=0.5,seeds=123)
{
  set.seed(seeds)
  colnames(rf_dt_temp)[1]="y"
  library(randomForest)
  cvlist=function(data,k)
  {
    N=nrow(data)
    index=rep(1:k,ceiling(N/k))[1:N]
    selected_index=sample(N,N)
    index_df=data.frame(data_index=selected_index,data_arrangement=index)
    index_df=index_df[order(index_df$data_index),]
    return(index_df)
  }


  cv_index=cvlist(rf_dt_temp,K)
  temp_mse_all=NULL

  #交叉验证并行计算函数
  cv_mse_calculation=function(mtry)
  {
    #生成交叉验证数据
    cv_data=NULL
    for(i in 1:K)
    {
      temp_train=rf_dt_temp[cv_index$data_arrangement!=i,]
      temp_val=rf_dt_temp[cv_index$data_arrangement==i,]
      temp_cv_data=list(temp_train,temp_val)
      cv_data=c(cv_data,temp_cv_data)
    }

    #定义交叉验证函数-可并行
    k_val_function=function(K)
    {
      temp_N=nrow(cv_data[2*K-1][[1]])
      sample_index=sample(temp_N,temp_N*sample_ratio)
      temp_rf=randomForest(y~.,data=cv_data[2*K-1][[1]][sample_index,],mtry=mtry,importance=TRUE)
      mse=mean(cv_data[2*K][[1]][,1]-predict(temp_rf,cv_data[2*K][[1]]))^2
      return(mse)
    }

    #并行计算交叉验证均值
    library(parallel)
    cl <- makeCluster(num_cores)
    clusterExport(cl,  envir = environment(),varlist =c("rf_dt_temp", "K", "cvlist","randomForest","cv_data","mtry","sample_ratio"))
    temp_mse<- parLapply(cl, 1:K, k_val_function)
    temp_mtry_cv_mse=mean(unlist(temp_mse))
    stopCluster(cl)

    return(temp_mtry_cv_mse)
  }


  #循环计算不同变量个数的cv mse
  mtry_cv_mse=NULL
  for (mtry in mtry_range)
  {
    temp_mtry_cv_mse=cv_mse_calculation(mtry)
    mtry_cv_mse=c(mtry_cv_mse,temp_mtry_cv_mse)
  }

  cv_result=data.frame(mtry=mtry_range,cv_mse=mtry_cv_mse)
  return(cv_result)
}
