#' shap_analysis
#' function to carry out SHAP analysis using fastshap
#' @param model best model produced from CV
#' @param new_data data need SHAP analysis. Response variables shouldn't be included.
#' @param seed random seed. default as 123
#' @param simulation_times simulation times
#'
#' @return SHAP res from fastshap

shap_analysis=function(model,new_data,seed=123,simulation_times=10)
{
  library(fastshap)
  library(ggbeeswarm)
  library(reshape2)
  # Fit
  fit <- model
  # 封装预测函数
  pfun <- function(object, newdata) {  # 输入样本要求返回数值向量
    predict(object, newdata = newdata)
  }
  # 通过模拟计算SHAp
  set.seed(seed)  # 随机种子
  shap_dt=new_data
  shap <- explain(fit, X = new_data, nsim = simulation_times,
                  pred_wrapper = pfun)

  shap_df=as.data.frame(shap)#shap df-作为输出
  shap_importance = apply(shap_df,2,function(x)mean(abs(x)))#计算变量重要性-作为输出
  #画图函数
  feature_importance=colnames(shap_df)[order(shap_importance,decreasing = T)]
  feature=colnames(shap_df)
  ###矩阵转三列表
  shap_df_plot=reshape2::melt(as.matrix(shap_df));colnames(shap_df_plot)=c("id","feature","shap_value")
  ###添加重要性列
  importance_df=aggregate(shap_value~feature,data = shap_df_plot,function(x)mean(abs(x)))
  colnames(importance_df)=c("feature","importance")
  shap_df_plot=merge(shap_df_plot,importance_df)

  #现在将变量的取值作为颜色可视化到上面来，这里需要原本三线表之上再加上一列函数取值，并且添加函数取值之前保证数据的量纲是一致的，做标准化
  plot_rf_dt=as.data.frame(apply(shap_dt,2,scale));rownames(plot_rf_dt)=rownames(shap_dt)
  shap_feature_value=reshape2::melt(as.matrix(plot_rf_dt));colnames(shap_feature_value)=c("id","feature","feature_value")
  shap_df_plot=merge(shap_df_plot,shap_feature_value,by=c("id","feature"))
  #######返回结果#######
  shap_res=list(SHAP=shap_df,importance=shap_importance,plot_data=shap_df_plot,feature_importance=feature_importance)
  return(shap_res)
}
