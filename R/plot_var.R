
#' @title Plot a variable
#' @description Plot the distribution of a variable and (optionally) facet the plot by a
#'   grouping variable.
#'
#'   The \code{x} and \code{group} arguments can be unquoted column names in \code{data},
#'   or expressions representing functions of one or more column names in \code{data}.
#'   These expressions are passed to \link[dplyr]{transmute} which is used to generate the
#'   data for plotting.
#'
#' @details \link[dplyr]{type_sum} is used to determine the class of the \code{x}
#'   variable:
#'   \itemize{
#'     \item if \code{x} is one of \code{"chr"}, \code{"fct"}, \code{"lgl"} or
#'     \code{"ord"} then it is treated as categorical
#'     \item if \code{x} is one of \code{"dbl"} or \code{"int"} then it is treated as
#'     non-categorical
#'     \item no other variable types are supported for \code{x}
#'   }
#'
#'   \link[ggplot2]{geom_histogram} is used to plot non-categorical x variables and
#'   \link[ggplot2]{geom_bar} is used to plot categorical x variables.
#' @param data a data frame
#' @param x variable for which the distribution will be plotted
# @param y optional y variable
#' @param group optional grouping variable
#' @param min_distinct_xnum integer, if \code{x} is non-categorical and the number of
#'   distinct values in \code{x} is less than \code{min_distinct_xnum} a error will be
#'   thrown (see Details)
#' @param max_distinct_xcat integer, if \code{x} is categorical and the number of distinct
#'   values in \code{x} exceeds \code{max_distinct_xcat} a error will be thrown (see
#'   Details)
#' @param max_distinct_group integer, if the number of distinct values in \code{group}
#'   exceeds \code{max_distinct_group} a error will be thrown
#' @param bins passed to \link[ggplot2]{geom_histogram} (ignored if \code{x} is
#'   categorical)
#' @examples
#' \dontrun{
#'
#' mtcars %>% plot_var(cyl)
#' mtcars %>% plot_var(cyl / disp)
#' mtcars %>% plot_var(cyl / disp, vs + am)
#' }
#' @export

plot_var = function(data, x,
                    # y,
                    group,
                    min_distinct_xnum = 10,
                    max_distinct_xcat = 50,
                    max_distinct_group = 3,
                    bins = 30
){

    # col_x = "#317eac"

    # use_y = !missing(y)
    use_group = !missing(group)

    # data trans -----
    quos = list(x = enquo(x))
    # if(use_y) quos$y = enquo(y)
    if(use_group) quos$group = enquo(group)
    data_trans = data %>%
        as_tibble %>%
        transmute(!!!quos)

    # var names -----
    var_names = list(x = quo_name(quos$x))
    # if(use_y) var_names$y = quo_name(quos$y)
    if(use_group) var_names$group = quo_name(quos$group)

    # check x -----
    x_type = data_trans$x %>% type_sum
    if(!x_type %in% c("chr", "dbl", "fct", "int", "lgl", "ord")){
    # if(!x_type %in% c("chr", "date", "dbl", "fct", "int", "lgl", "ord")){
        stop(paste0("cant plot x variable type '", x_type, "'"))
    }
    x_is_number = x_type %in% c("dbl", "int")
    # x_is_number = x_type %in% c("dbl", "int", "date")
    # x_is_date = x_type %in% "date"

    # check group -----
    if(use_group){
        if(n_distinct(data_trans$group) > max_distinct_group){
            stop(paste("group has more than", max_distinct_group, "distinct values"))
        }
        data_trans$group %<>% as.factor
        levels(data_trans$group) %<>% paste(var_names$group, "=", .)
    }else{
        data_trans$group = 1
    }
    group_fct = levels(as.factor(data_trans$group)) %>% factor(., .)

    # data x -----
    if(x_is_number){
        data_x = data_trans %>% {
            ggplot(.) + geom_histogram(aes(.data$x, group = .data$group), bins = bins)
        } %>%
            ggplot_build %>%
            pluck("data") %>%
            pluck(1) %>%
            as_tibble %>%
            mutate(group = group_fct[.data$group]) %>%
            mutate(width = .data$xmax - .data$xmin) %>%
            group_by(.data$group) %>%
            mutate(prop = .data$count / sum(.data$count, na.rm = TRUE)) %>%
            ungroup %>%
            select(one_of("x", "prop", "group", "width"))
        # if(x_is_date){
        #     data_x$x %<>% as.Date(origin = "1970-01-01 UTC")
        # }
    }else{
        data_x = data_trans %>%
            group_by(.data$x, .data$group) %>%
            summarise(n = n()) %>%
            group_by(.data$group) %>%
            mutate(prop = .data$n / sum(.data$n), width = 0.9) %>%
            ungroup
    }

    # plot x -----
    suppressWarnings({
        plot = data_x %>%
            ggplot() +
            geom_bar(
                aes(.data$x, .data$prop, group = .data$group, width = .data$width),
                stat = "identity",
                # fill = col_x,
                alpha = 0.8
            ) +
            labs(title = var_names$x, y = "Density", x = var_names$x) +
            theme(legend.position = "none")
    })
    if(use_group){
        plot = plot + facet_wrap(~group, ncol = 1)
    }

    # check y -----
    # if(use_y){
    #     y_type = df$y %>% type_sum
    #     if(!y_type %in% c("dbl", "int")){
    #         warn(str_c("cant plot y variable type '", y_type, "'"))
    #         use_y = FALSE
    #     }
    # }
    # if(use_y){
    #     if(is.null(y_method)) y_method = if(is_num) "gam" else "glm"
    #     if(is_num){
    #         if(!y_method %in% c("gam", "glm", "loess", "none")){
    #             warn("expecting y_method to be one of: 'gam', 'glm', 'loess', 'none'")
    #             y_method = "none"
    #         }
    #     }else{
    #         if(!y_method %in% c("glm", "boxplot", "none")){
    #             warn("expecting y_method to be one of: 'glm', 'boxplot', 'none'")
    #             y_method = "none"
    #         }
    #     }
    # }
    # if(use_y){
    #     if(is.null(y_points)) y_points = FALSE
    #     y_points = isTRUE(y_points)
    #     if(!y_points && y_method == "none"){
    #         use_y = FALSE
    #     }
    # }
    # if(use_y){
    #     y_smooth = y_method != "none"
    #     if(y_smooth){
    #         if(is.null(y_family)){
    #             y_family = if(y_method == "loess") "gaussian" else guess_family(df$y)
    #         }
    #         if(y_family == "binomial" && y_method == "boxplot"){
    #             warn(str_c("boxplot method unsuitable for y variable type ", y_family))
    #         }
    #     }
    # }

    # # plot y -----
    # if(use_y){
    #
    # }

    plot

}

