##' magpie2ggplot2
#' 
#' Function for plotting MAgPIE objects with ggplot2
#' 
#' 
#' @usage
#' magpie2ggplot2(data,scenario="default",ylab="Value",title=NULL,
#' xaxis="Year",yaxis="Value",facet_x="Region", facet_y=NULL,
#' geom="line",stack=F,color="Scenario",fill="Region",
#' shape=NULL,linetype=NULL,alpha=NULL,labs=NULL,stat=NULL,
#' text_size=11,hline=NULL,legend_position="right",
#' scales="fixed",ncol=5,xlim=NULL,ylim=NULL,file=NULL,
#' scale=NULL, breaks_x=waiver(),breaks_y=waiver(),
#' xaxis_angle=90,xlab="Year",linewidth=1,pointwidth=2,
#' color_pal=NULL,bar_width=NULL,show_grid=FALSE,
#' group="Region",axis_text_col="black",zoom=FALSE,
#' space="fixed",legend_nrow=NULL,legend_ncol=NULL,
#' facet_style="default",na.rm=FALSE,
#' stack_share=FALSE,point_position="identity",
#' normalize=NULL,...)
#' @param data MAgPIE object or list of MAgPIE objects or dataframe of MAgPIE
#' object(s); the name of the list entries is used as scenario name in the
#' legend
#' @param scenario If the scenario name is stored in the third dimension:
#' position of the entry in the third dimension that contains the scenario
#' name. Default: "default"
#' @param ylab y-axis text
#' @param title title appering at the top of the plot
#' @param xaxis x-axis of the plot, default: "Year"
#' @param yaxis y-axis of the plot, default: "Value"
#' @param facet_x x-axis facet, default: "Region"
#' @param facet_y y-axis facet, default: NULL
#' @param geom "line","point","bar", or "area". If "none" only the ggplot base object is returned.
#' @param stack stacked bar plot (TRUE) or not (FALSE)
#' @param color Dimension to be colored, default: "Scenario"
#' @param fill Dimension to be filled in stacked plots, default: NULL
#' @param shape Dimension to be shaped, default: NULL
#' @param alpha Dimension to be transparent in stacked plots, default: NULL
#' @param linetype Dimension to differ in linetypes, default: NULL
#' @param labs vector with legend titles for color,fill,shape,alpha,linetype
#' @param stat adds statistics (e.g. "sum" or "mean"), default: NULL
#' @param text_size text size of axis, legend and title
#' @param hline NULL or MAgPIE objects. Adds a horizontal line.
#' @param legend_position right (default), left, top, bottom or none
#' @param scales fixed (default), free, free_y or free_x; ?facetgrid for
#' details
#' @param ncol Number of columns used in facet_wrap function
#' @param xlim x axis limits; NULL or vector with limits
#' @param ylim y axis limits; NULL or vector with limits
#' @param file File name the output should be written to using ggsave
#' @param scale scaling of ggplot2 object before saving to file
#' @param breaks_x Vector of x-axis breaks, by default ggplot2 takes the
#' decision
#' @param breaks_y Vector of y-axis breaks, by default ggplot2 takes the
#' decision
#' @param xaxis_angle Angle of xaxis text in degree, default=90
#' @param xlab x-axis text
#' @param linewidth linewidth, default=1
#' @param pointwidth pointwidth, default=3
#' @param color_pal vector of colors defining the color palette, if NULL colors
#' are chosen automaticly
#' @param bar_width width of bars in geom bar, default value: NULL
#' @param show_grid show minor and major grid lines; FALSE (default) or TRUE
#' @param group dimension used for grouping; default value: "Region"
#' @param axis_text_col color of axis text and ticks; default value: "black"
#' @param zoom TRUE zooms the plot according to xlim and ylim, FALSE omits
#' values not the range of xlim and ylim
#' @param space fixed (default), free, free_y or free_x; ?facetgrid for details
#' @param legend_nrow Number of rows used in legend
#' @param legend_ncol Number of columns used in legend
#' @param facet_style style of facets, default or paper
#' @param na.rm Boolean deciding whether NA values should be filtered out of
#' the data or not.
#' @param stack_share stacked bar plot showing shares (TRUE) or absolut values
#' (FALSE)
#' @param point_position position of points; "identity" or position_dodge(width
#' = 1)
#' @param normalize NULL (default) or year which should be used to normalize
#' the data
#' @param ... Further options passed to as.ggplot
#' @return ggplot2 object representing one or more MAgPIE objects.
#' @author Florian Humpenoeder, David Klein
#' @seealso \code{\link{scratch_plot},\link{histoplot},}
#' @examples
#' 
#' \dontrun{
#' crop_area_reg <- list()
#' crop_area_reg[["scenario_name"]] <- croparea("fulldata.gdx")
#' magpie2ggplot2(crop_area_reg,ylab="10^6 ha",title="Croparea",stack=T,facet_x="Scenario",color=NULL)}
#' \dontrun{
#' prices <- list()
#' prices[["Scenario1"]] <- prices("Scenario1.gdx",crops=c("tece","maiz"))
#' prices[["Scenario2"]] <- prices("Scenario2.gdx",crops=c("tece","maiz"))
#' magpie2ggplot2(prices,ylab="US$/ton DM",title="Agricultural prices")}
#' @export
#' @importFrom magclass is.magpie setYears
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @importFrom ggplot2 ggplot scale_y_discrete aes_string facet_grid facet_wrap geom_bar scale_fill_manual geom_area geom_line rel
#' geom_point scale_color_manual geom_hline aes_ stat_summary ggtitle theme element_rect element_line element_text scale_alpha_discrete 
#' element_blank coord_cartesian scale_x_date scale_x_continuous scale_x_discrete scale_y_date scale_y_continuous guides guide_legend ggsave 
#' @importFrom mip plotstyle

magpie2ggplot2 <- function(data,scenario="default",ylab="Value",title=NULL,xaxis="Year",yaxis="Value",facet_x="Region",facet_y=NULL,
                           geom="line",stack=F,color="Scenario",fill="Region",shape=NULL,linetype=NULL,alpha=NULL,labs=NULL,stat=NULL,
                           text_size=11,hline=NULL,legend_position="right",scales="fixed",ncol=5,xlim=NULL,ylim=NULL,file=NULL,scale=NULL,
                           breaks_x=waiver(),breaks_y=waiver(),xaxis_angle=90,xlab="Year",linewidth=1,pointwidth=2,color_pal=NULL,bar_width=NULL,
                           show_grid=FALSE,group="Region",axis_text_col="black",zoom=FALSE,space="fixed",legend_nrow=NULL,legend_ncol=NULL,
                           facet_style="default",na.rm=FALSE,stack_share=FALSE,point_position="identity",normalize=NULL,...){
  .interact <- function(x) {
    if(length(x)>1) {
      return(paste0("interaction(",paste(x,collapse=", "),")"))
    } else {
      return(x)
    }
  }
  
  group   <- .interact(group)
  facet_x <- .interact(facet_x)
  facet_y <- .interact(facet_y)
  color   <- .interact(color)
  fill    <- .interact(fill)
  shape   <- .interact(shape)
  
  .e <- environment()
  #require("ggplot2", quietly = TRUE)
  #require("RColorBrewer", quietly = TRUE)
  #  assign("set1",set1,envir=.e)
  number_ticks <- function(n) {function(limits) pretty(limits, n)}
  if(!is.null(normalize)) {
    if(is.magpie(data)) {
      data <- data - setYears(data[,normalize,],NULL)
    } else stop("Normalization works only for MAgPIE objects. Sorry...")
  }
  if (!is.data.frame(data)) data <- as.ggplot(data,scenario=scenario,...)
  if(na.rm) data <- na.omit(data)
  
  #set colors
  if(is.null(color_pal)) {
    set1 <- colorRampPalette(brewer.pal(9,"Set1"))  
    if(!is.null(fill)) {
      scale_fill <- as.character(plotstyle(as.character(unique(data[[fill]])),out="color"))
      if(fill != "Region") levels(data[[fill]]) <- plotstyle(as.character(unique(data[[fill]])),out="legend")
    }
    if(!is.null(color)) {
      scale_color <- as.character(plotstyle(as.character(unique(data[[color]])),out="color"))
      if(color != "Region") levels(data[[color]]) <- plotstyle(as.character(unique(data[[color]])),out="legend")
    }
#    if(!is.null(fill)) scale_fill <- plotstyle(as.character(unique(data[[fill]]))) #set1(length(unique(data[[fill]]))) #
#    if(!is.null(color)) scale_color <- plotstyle(as.character(unique(data[[color]]))) #set1(length(unique(data[[color]]))) #
  } else {
    scale_fill <- color_pal
    scale_color <- color_pal
  }
  
  #positive and negative values
  data_pos <- data
  data_neg <- data
  data_pos$Value[data_pos$Value<0] <- 0
  data_neg$Value[data_neg$Value>=0] <- 0
  
  theme_my <- function(base_size = 11, base_family = "")
  {
    txt <- element_text(size = base_size, colour = "black", face = "plain")
    bold_txt <- element_text(size = base_size, colour = "black", face = "bold")
    
    theme_bw(base_size = base_size, base_family = base_family) +
      theme(
        legend.key = element_blank(), 
        strip.background = element_rect(color="black",fill="grey95"),
        
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        
        text = txt, 
        plot.title = txt, 
        
        axis.title = txt, 
        axis.text = txt, 
        
        legend.title = bold_txt, 
        legend.text = txt ) 
  }
  
  #plotting
  p <- ggplot(data, aes_string(x=xaxis, y=yaxis), environment=.e)+theme_my(base_size = text_size)
  if (!is.null(facet_y)) {
    if(is.null(facet_x)) facet_x <- "."
    p <- p + facet_grid(as.formula(paste(facet_y, "~", facet_x)), scales=scales, space=space)
  } else if (!is.null(facet_x)) p <- p + facet_wrap(as.formula(paste("~", facet_x)), scales=scales, ncol=ncol)
  if (geom == "bar") {
    if (stack) {
      if (stack_share) {
        if (any(data_pos$Value >= 0,na.rm=TRUE)) p <- p + geom_bar(data=data_pos,position='fill',stat='identity',aes_string(fill=fill,alpha=alpha,width=bar_width))
        if (any(data_neg$Value < 0,na.rm=TRUE)) p <- p + geom_bar(data=data_neg,position='fill',stat='identity',aes_string(fill=fill,alpha=alpha,width=bar_width))
      } else {
        if (any(data_pos$Value >= 0,na.rm=TRUE)) p <- p + geom_bar(data=data_pos,position='stack',stat='identity',aes_string(fill=fill,group=group,alpha=alpha,width=bar_width))
        if (any(data_neg$Value < 0,na.rm=TRUE)) p <- p + geom_bar(data=data_neg,position='stack',stat='identity',aes_string(fill=fill,group=group,alpha=alpha,width=bar_width))        
      }
    } else p <- p + geom_bar(data=data,position='dodge',stat='identity',aes_string(fill=fill,group=group,alpha=alpha,width=bar_width))
    p <- p + scale_fill_manual(values=scale_fill)
    # if (!is.null(fill)) {
    #   if ((fill == "Region") & (is.null(color_pal))) {
    #     if (length(unique(data$Region)) == 1) {
    #       p <- p + scale_fill_manual(values=nice_colors(style="contrast_area",saturation=1)[11])
    #     } else if (length(unique(data$Region)) == 10) {
    #       p <- p + scale_fill_manual(values=nice_colors(style="contrast_area",saturation=1)[1:10])
    #     } else if (length(unique(data$Region)) == 11) {
    #       p <- p + scale_fill_manual(values=nice_colors(style="contrast_area",saturation=1)[1:11])
    #     } else p <- p + scale_fill_manual(values=scale_fill)
    #   } else if (identical(levels(data$Data1),c("crop", "past", "forestry", "forest", "urban", "other"))) {
    #     p <- p + scale_fill_manual(values=c("chocolate4", "#E6AB02", "darkgreen", "#66A61E", "black", "brown3", "honeydew4"))
    #   } else p <- p + scale_fill_manual(values=scale_fill)
    # }
  } else if (geom == "area") {
    if (stack) {
      if (stack_share) {
        if (any(data_pos$Value >= 0,na.rm=TRUE)) p <- p + geom_area(data=data_pos,position='fill',stat='identity',aes_string(fill=fill, alpha=alpha))
        if (any(data_neg$Value < 0,na.rm=TRUE)) p <- p + geom_area(data=data_neg,position='fill',stat='identity',aes_string(fill=fill, alpha=alpha))
      } else {
        if (any(data_pos$Value >= 0,na.rm=TRUE)) p <- p + geom_area(data=data_pos,position='stack',stat='identity',aes_string(fill=fill, alpha=alpha))
        if (any(data_neg$Value < 0,na.rm=TRUE)) p <- p + geom_area(data=data_neg,position='stack',stat='identity',aes_string(fill=fill, alpha=alpha))
      }
      
    } else p <- p + geom_area(data=data,position='dodge',stat='identity',aes_string(fill=fill, alpha=alpha))
    p <- p + scale_fill_manual(values=scale_fill)
    # if (!is.null(fill)) {
    #   if ((fill == "Region") & (is.null(color_pal))) {
    #     if (length(unique(data$Region)) == 1) {
    #       p <- p + scale_fill_manual(values=nice_colors(style="contrast_area",saturation=1)[11])
    #     } else if (length(unique(data$Region)) == 10) {
    #       p <- p + scale_fill_manual(values=nice_colors(style="contrast_area",saturation=1)[1:10])
    #     } else if (length(unique(data$Region)) == 11) {
    #       p <- p + scale_fill_manual(values=nice_colors(style="contrast_area",saturation=1)[1:11])
    #     } else p <- p + scale_fill_manual(values=scale_fill)
    #   } else if (identical(levels(data$Data1),c("crop", "past", "forestry", "forest", "urban", "other"))) {
    #     p <- p + scale_fill_manual(values=c("chocolate4", "#E6AB02", "darkgreen", "#66A61E", "black", "brown3", "honeydew4"))
    #   } else p <- p + scale_fill_manual(values=scale_fill)
    # }
  } else if (geom == "line") {
    p <- p + geom_line(size=linewidth,aes_string(group=group,color=color,shape=shape,linetype=linetype)) + geom_point(size=pointwidth,aes_string(group=group,color=color,shape=shape))
    p <- p + scale_color_manual(values=scale_color)
    # if (!is.null(color)) {
    #   if ((color=="Region") & (is.null(color_pal))) {
    #     if ((is.null(color_pal)) & (length(unique(data$Region)) == 1) ) p <- p + scale_color_manual(values=nice_colors(style="contrast_area",saturation=1)[11])
    #     else if (length(unique(data$Region)) == 5)  p <- p + scale_color_manual(values=nice_colors(style="contrast_area",saturation=1)[c(9,5,1,8,2)])
    #     else if (length(unique(data$Region)) == 6)  p <- p + scale_color_manual(values=nice_colors(style="contrast_area",saturation=1)[c(9,5,1,8,2,11)])
    #     else if (length(unique(data$Region)) == 10) p <- p + scale_color_manual(values=nice_colors(style="contrast_area",saturation=1)[1:10])
    #     else if (length(unique(data$Region)) == 11) p <- p + scale_color_manual(values=nice_colors(style="contrast_area",saturation=1)[1:11])      
    #   } else p <- p + scale_color_manual(values=scale_color)
    # }
  } else if (geom == "point") {
    p <- p + geom_point(size=pointwidth,aes_string(group=group,color=color,shape=shape,alpha=alpha),position=point_position)
    p <- p + scale_color_manual(values=scale_color)
    # if (!is.null(color)) {
    #   if ((color=="Region") & (is.null(color_pal))) {
    #     if (length(unique(data$Region)) == 1)        p <- p + scale_color_manual(values=nice_colors(style="contrast_area",saturation=1)[11])
    #     else if (length(unique(data$Region)) == 10)  p <- p + scale_color_manual(values=nice_colors(style="contrast_area",saturation=1)[1:10])
    #     else if (length(unique(data$Region)) == 11)  p <- p + scale_color_manual(values=nice_colors(style="contrast_area",saturation=1)[1:11])      
    #   } else p <- p + scale_color_manual(values=scale_color)
    # }
  } else if (geom == "none") {
    p <- p
  }
  if (!is.null(hline)) {
    hline <- as.ggplot(hline,...)
    p <- p + geom_hline(data=hline,aes_(yintercept=~Value))
  }
  if (!is.null(stat)) p <- p + stat_summary(fun.y=stat, color="black", geom="point", size = 2) + stat_summary(fun.y=stat, color="black", geom="line", size = 1)
  if (!is.null(labs)) p  <- p + labs(color=labs[1],fill=labs[2],shape=labs[3],alpha=labs[4],linetype=labs[5])
  p  <- p + labs(y=ylab,x=xlab) + ggtitle(title) + theme(legend.position=legend_position) #panel.background = element_rect(fill="white", colour="black"),panel.grid.minor=element_line(colour="white"))# + scale_alpha_discrete(range=c(0.5, 1))
  if (xaxis_angle == 90) p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  if (facet_style == "default") p <- p
  else if (facet_style == "paper") p <- p + theme(strip.background = element_blank())
  if(show_grid) p <- p + theme(panel.grid.major.x=element_line(size = rel(1)),panel.grid.minor.x=element_line(size = rel(0.5)))
  labels <- waiver()
  
  if (!zoom & stack & !is.null(ylim) & yaxis=="Value") stop("Limiting y axis in stacked plots works only with zoom = TRUE")
  if (!zoom & stack & !is.null(xlim) & xaxis=="Value") stop("Limiting x axis in stacked plots works only with zoom = TRUE")
  
  if (zoom) {
    if (xaxis=="Value") {
      xlim2 <- xlim
      if (!is.null(xlim) & xaxis == "Year" & !is.factor(data[,xaxis])) xlim2 <- as.Date(paste(xlim2,"-01-01",sep=""))
      if (is.factor(data[,xaxis])) xlim2 <- NULL else xlim <- NULL
      p <- p + coord_cartesian(xlim=xlim2)      
    } else if (yaxis=="Value") {
      ylim2 <- ylim
      if (!is.null(ylim) & yaxis == "Year" & !is.factor(data[,yaxis])) ylim2 <- as.Date(paste(ylim2,"-01-01",sep=""))
      if (is.factor(data[,yaxis])) ylim2 <- NULL else ylim <- NULL
      p <- p + coord_cartesian(ylim=ylim2)
    }
  }
  
  #xaxis
  if (xaxis == "Year" & !is.factor(data[,xaxis]) & !is.numeric(data[,xaxis])) {
    if (!is.null(xlim)) xlim <- as.Date(paste(xlim,"-01-01",sep=""))
    if (!is.list(breaks_x)) {
      labels <- format(as.Date(paste(breaks_x,"-01-01",sep="")), format = "%Y")    
      breaks_x <- as.Date(paste(breaks_x,"-01-01",sep=""))
    }
    p <- p + scale_x_date(limits=xlim,breaks=breaks_x,labels=labels)  
  } else if (is.numeric(data[,xaxis])) {
    p <- p + scale_x_continuous(limits=xlim,breaks=breaks_x)
  } else p <- p + scale_x_discrete(limits=xlim,breaks=breaks_x)
  
  #yaxis
  if (yaxis == "Year" & !is.factor(data[,yaxis]) & !is.numeric(data[,yaxis])) {
    if (!is.null(ylim)) ylim <- as.Date(paste(ylim,"-01-01",sep=""))
    if (!is.list(breaks_y)) {
      labels <- format(as.Date(paste(breaks_y,"-01-01",sep="")), format = "%Y")    
      breaks_y <- as.Date(paste(breaks_y,"-01-01",sep=""))
    }
    p <- p + scale_y_date(limits=ylim,breaks=breaks_y,labels=labels)  
  } else if (is.numeric(data[,yaxis])) {
    p <- p + scale_y_continuous(limits=ylim,breaks=breaks_y)
  } else p <- p + scale_y_discrete(limits=ylim,breaks=breaks_y)
  
  if (!is.null(axis_text_col)) p <- p + theme(axis.text = element_text(colour=axis_text_col),axis.ticks = element_line(colour=axis_text_col))
  # reverse_switch has to be predefined so everything can be set in one step below, because consecutive use of guide_legend would overwrite previous setting
 if (geom=="bar" & stack & all(data_neg$Value == 0,na.rm=TRUE)) reverse_switch=TRUE else reverse_switch=FALSE 
  p <- p + guides(color = guide_legend(nrow=legend_nrow,ncol=legend_ncol,title.position = "top", byrow = TRUE)) +
           guides(fill  = guide_legend(nrow=legend_nrow,ncol=legend_ncol,title.position = "top", byrow = TRUE,reverse=reverse_switch)) +
           guides(shape = guide_legend(nrow=legend_nrow,ncol=legend_ncol,title.position = "top", byrow = TRUE)) +
           guides(alpha = guide_legend(nrow=legend_nrow,ncol=legend_ncol,title.position = "top", byrow = TRUE)) +
           guides(linetype = guide_legend(nrow=legend_nrow,ncol=legend_ncol,title.position = "top", byrow = TRUE)) +
           theme(legend.box.just = "left")
  if(!is.null(file)) {
    ggsave(file,p,scale=scale)
  } else {
    return(p)  
  }
}
