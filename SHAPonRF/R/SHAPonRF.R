#' SHAPonRF
#' full process of SHAP analysis on RandomForest model
#' @param rf_dt data.frame with first column indicating the response variable
#' @param mtry_range range of number of variables used in each trees. example:c(1:10)
#' @param inter_list variable list for interaction plotting. A character vector. odd elements and even elements are effect1 and effect2 respectively. If null, plot won't be produced.
#' @param K_fold_CV K-fold cross validation
#' @param seed random seed;default 123
#' @param num_cores how many cores are used in parallel computing. default as 10
#' @param sample_ratio in case the data was too big, part of the data is used in the cross validation to enhance the efficiency of the algorithm; default as 0.5
#' @param need_scale_shap SHAP value scale for visualization
#' @param need_odd_remove delete odd value SHAP value scale for visualization
#' @param variable_list variable list for importance plotting. A character vector. if null all features are plotted
#' @param id_list  observation list for plotting. A character vector. if null, all observations are plotted.
#'
#' @return list(cv_mse:cross validation error ; best_rf: best randomforest model,shap_res: results from SHAP; SHAP_plots:visualization of SHAP analysis

SHAPonRF=function(rf_dt,mtry_range=2:ncol(),K_fold_CV=10,num_cores=4,sample_ratio=1,need_scale_shap=T,need_odd_remove=T,variable_list=NULL,id_list=NULL,inter_list=NULL,seed=123)
{
  #data processing
  colnames(rf_dt)[1]="y"
  rf_dt_x=rf_dt[,-1]
  #CV to determine mtry
  cv_mse=cv_rf_parallel(rf_dt,seeds=seed,K=K_fold_CV,mtry_range=mtry_range,num_cores=num_cores,sample_ratio=sample_ratio)
  #best model
  set.seed(seed);
  best_rf=randomForest(y~.,data=rf_dt,mtry=mtry_range[which.min(cv_mse$cv_mse)],importance=TRUE);
  #shap results
  shap_res=shap_analysis(best_rf,rf_dt_x)
  #shap plot
  quantile_cut_fun=function(x,quantile_index=0.1)
  {x[x>quantile(x,1-quantile_index)]=quantile(x,1-quantile_index);
  x[x<quantile(x,quantile_index)]=quantile(x,quantile_index);return(x)}

  if(need_odd_remove)
  {
    dt_x=data.frame(apply(rf_dt_x,2,quantile_cut_fun,quantile_index=0.1))
    colnames(dt_x)=colnames(dt_x)
    rf_dt_x=dt_x
  }
  plot_dt=shap_res$plot_data

  if(need_scale_shap)
  {
    plot_dt_modified=shap_res$plot_data
    plot_dt_modified$feature_value[plot_dt_modified$feature_value>2]=2
    plot_dt_modified$feature_value[plot_dt_modified$feature_value<(-2)]=-2
    plot_dt=plot_dt_modified
  }

  SHAP_plots=plot_shap(plot_data = plot_dt,new_data=rf_dt_x,shap_df =shap_res$SHAP,id_list=id_list,variable_list =variable_list,interaction_variable_list = inter_list)
  #return
  return(list(cv_mse=cv_mse,best_rf=best_rf,shap_res=shap_res,SHAP_plots=SHAP_plots))
}
