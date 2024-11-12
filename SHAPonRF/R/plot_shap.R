#' plot_shap
#' variable importance plot and variable interaction plot
#' @param plot_data plot data from SHAP_analysis
#' @param new_data data need SHAP analysis. Response variables shouldn't be included.
#' @param shap_df shap values from SHAP_analysis. A character vector
#' @param variable_list variable list for importance plotting. A character vector. if null all features are plotted
#' @param id_list observation list for plotting. A character vector. if null, all observations are plotted.
#' @param interaction_variable_list variable list for interaction plotting. A character vector. odd elements and even elements are effect1 and effect2 respectively. If null, plot won't be produced.
#'
#' @return ggplot objects

plot_shap=function(plot_data,new_data,shap_df,variable_list=NULL,id_list=NULL,interaction_variable_list=NULL)
{
  library(patchwork)
  library(ggplot2)
  if(!is.null(variable_list)){plot_data=plot_data[plot_data$feature%in%variable_list,]}
  if(!is.null(id_list)){plot_data=plot_data[plot_data$id%in%id_list,]}

  #单变量+重要性图
  p1 <- ggplot(plot_data, aes(x = shap_value, y = reorder(feature,importance))) +
    geom_quasirandom(groupOnX = FALSE, varwidth = TRUE, size =1, alpha = 0.8, aes(color = feature_value)) +
    scale_color_gradient(low =  "#ffcd30", high =  "#6600cd") +
    labs(x="SHAP value",y="")+
    theme_bw()+
    theme(axis.text = element_text(color = "black"),panel.border = element_rect(linewidth = 1))+
    geom_vline(xintercept = 0,linetype="dashed",color="grey",linewidth=1)

  p2=NULL
  plots=NULL
  if(!is.null(interaction_variable_list))
  {
    plots=list()
    for(i in 1:(length(interaction_variable_list)/2))
    {

      #从这里先生成数据-原始数据需要new_data+shap_df
      #数据是四列-id+  shap(target_feature)+  feature_value+  interaction_feature_value
      target_feature=interaction_variable_list[2*(i-1)+1]
      interaction_feature=interaction_variable_list[2*(i-1)+2]
      interaction_plot_dt_temp=data.frame(id=rownames(new_data),shap_value=shap_df[,target_feature],feature_value=new_data[,target_feature],interaction_feature_value=new_data[,interaction_feature])
      # 绘图
      plot_index=i
      plots[[plot_index]]=ggplot(interaction_plot_dt_temp, aes(feature_value,shap_value, color=value))+
        geom_quasirandom(groupOnX = FALSE, varwidth = TRUE, size =1, alpha = 0.8, aes(color = interaction_feature_value)) +
        scale_color_gradient(low =  "#ffcd30", high =  "#6600cd") +
        labs(x=target_feature,y=paste("SHAP (",target_feature,")",sep = ""))+
        #ggtitle(sprintf("%s ~ %s", target_feature, interaction_feature))+theme(plot.title = element_text(size = 10))+
        labs(colour=interaction_feature)
    }
    p2=wrap_plots(plots, ncol = ceiling((length(interaction_variable_list)/2)^0.5))
  }
  plot_list=list(shap_plot=p1,interaction_plot=p2,unwrap_inter_plot=plots)
  return(plot_list)
}
