
#' @rdname desc_df
#' @name desc_df
#' @title Describe a data frame
#' @description Returns a \link[dplyr]{tibble} with rows corresponding to the variables in
#'   \code{x} and columns giving values for a range of summary statistics. If
#'   \code{verbose = TRUE} comments are printed to the console to highlight possible
#'   issues with data that might warrent further inspection prior to analysis.
#' @param x a data frame
#' @param max_p_na ???
#' @param min_distinct_num integer - a warning will be printed if any \code{dbl} or \code{int}
#'   variables have fewer than this number of distinct (non-negative) values
#' @param max_distinct_cat integer - a warning will be printed if any \code{chr}, \code{fct}
#'   or \code{ord} variables have fewer than this number of distinct (non-negative) values
#' @param min_distinct_mode integer - mode statistics will not be calculate for variables
#'   with more than this number of distinct (non-negative) values
#' @param max_p_mode proportion - a warning will be printed if the proportion of the model
#'   value for any variables exceeds this value
#' @param verbose if \code{TRUE} messages will be printed to the console
#' @export
#' @examples
#' \dontrun{
#' desc_df(iris)
#' desc_df(mtcars)
#' desc_df(esoph)
#' desc_df(attenu)
#' desc_df(nycflights13::airlines)
#' desc_df(nycflights13::airports)
#' desc_df(nycflights13::flights)
#' desc_df(nycflights13::planes)
#' desc_df(nycflights13::weather)
#' }

desc_df = function(x,
                    max_p_na = 0.99,
                    min_distinct_num = 10,
                    max_distinct_cat = 30,
                    min_distinct_mode = 100,
                    max_p_mode = 0.99,
                    verbose = TRUE){

    heading(deparse(substitute(x)))

    if(!inherits(x, "data.frame")){
        stop("expecting x to be a data.frame")
    }
    if(is.null(verbose)){
        verbose = isTRUE(getOption("verbose"))
    }

    # general -----

    if(verbose){
        item("size: ", utils::object.size(x) %>% format(units = "auto"))
        item("rows: ", nrow(x) %>% format(big.mark = ","))
        item("cols: ", ncol(x) %>% format(big.mark = ","))
    }

    # vars -----

    out = c("name", "type", "n_distinct") %>%
        purrr::set_names(., .) %>%
        purrr::map_df(function(i) rep(NA_character_, ncol(x)))

    # names -----

    out$name = x %>% colnames

    # types -----

    if(verbose) bullet("col types:")
    valid_types = c("chr", "fct", "ord", "lgl", "int", "dbl")
    out$type = x %>% map_chr(type_sum)
    if(verbose){
        temp = out$type %>% table
        fnames = format(names(temp))
        for(i in 1:length(temp)){
            invalid = !names(temp)[i] %in% valid_types
            txt = paste0(fnames[i], ": ", as.integer(temp[i]))
            if(invalid) txt %<>% paste("(not supported)")
            fun = if(invalid) warn else item
            fun(txt)
        }
    }

    # n_na -----

    if(verbose) bullet("checking for NA values...")
    out$n_na = x %>%
        map_int(function(x){
            if(!type_sum(x) %in% valid_types) return(NA_integer_)
            sum(is.na(x))
        })
    out$p_na = out$n_na / nrow(x)
    if(verbose){
        any_na = (out$n_na > 0) %in% TRUE
        if(any(any_na)){
            warn(sum(any_na), " vars with NA values (max prop NA ~",
            sprintf("%.2f", max(out$p_na, na.rm = TRUE)), ")")
        }
    }

    # n_distinct -----

    if(verbose) bullet("checking n distinct values...")
    out$n_distinct = x %>%
        map_int(function(x){
            if(!type_sum(x) %in% valid_types) return(NA_integer_)
            n_distinct(x, na.rm = TRUE)
        })
    if(verbose){
        # any only 0 distinct
        temp = out %>%
            filter(.data$n_distinct %in% 0)
        if(nrow(temp) > 0){
            warn(nrow(temp), " vars with 0 distinct values")
            for(i in 1:nrow(temp)){
                warn("", temp$name[i], .bullet = NA)
            }
        }
        # any only 1 distinct
        temp = out %>%
            filter(.data$n_distinct %in% 1)
        if(nrow(temp) > 0){
            warn(nrow(temp), " vars with only 1 distinct value")
        }
        # dbl/int with less than 10 distinct
        temp = out %>%
            filter(.data$type %in% c("dbl", "int")) %>%
            filter(.data$n_distinct < min_distinct_num)
        if(nrow(temp) > 0){
            warn(nrow(temp), " dbl/int vars with n_distinct < ", min_distinct_num,
                 " (min n_distinct = ", min(temp$n_distinct), ")")
        }
        # chr/fct/ord with more than 30 distinct
        temp = out %>%
            filter(.data$type %in% c("chr", "fct", "ord")) %>%
            filter(.data$n_distinct > max_distinct_cat)
        if(nrow(temp) > 0){
            warn(nrow(temp), " chr/fct/ord vars with n_distinct > ", max_distinct_cat,
                 " (max n_distinct = ", max(temp$n_distinct), ")")
        }
    }

    # non-finite -----

    if(verbose) bullet("checking for non-finite values...")

    # > any_nan -----

    out$any_nan = x %>%
        map_lgl(function(x){
            if(!type_sum(x) %in% c("dbl", "int")) return(NA)
            any(is.nan(x), na.rm = TRUE)
        })
    if(verbose){
        sum_any_nan = sum(out$any_nan, na.rm = TRUE)
        if(sum_any_nan > 0){
            warn(paste(sum_any_nan, "dbl/int vars have NaN values"))
        }
    }

    # > any_inf -----

    out$any_inf = x %>%
        map_lgl(function(x){
            if(!type_sum(x) %in% c("dbl", "int")) return(NA)
            any(is.infinite(x), na.rm = TRUE)
        })
    if(verbose){
        sum_any_inf = sum(out$any_inf, na.rm = TRUE)
        if(sum_any_inf > 0){
            warn(paste(sum_any_inf, "dbl/int vars have infinite values"))
        }
    }

    # range -----

    if(verbose) bullet("checking range...")

    # > any_pos -----

    out$any_pos = x %>%
        map_lgl(function(x){
            if(!type_sum(x) %in% c("dbl", "int")) return(NA)
            any(x > 0, na.rm = TRUE) %in% TRUE
        })

    # > any_zero -----

    out$any_zero = x %>%
        map_lgl(function(x){
            if(!type_sum(x) %in% c("dbl", "int")) return(NA)
            any(x == 0, na.rm = TRUE) %in% TRUE
        })

    # > any_neg -----

    out$any_neg = x %>%
        map_lgl(function(x){
            if(!type_sum(x) %in% c("dbl", "int")) return(NA)
            any(x < 0, na.rm = TRUE) %in% TRUE
        })

    if(verbose){
        # strictly positive
        temp = out %>%
            filter(.data$any_pos & !.data$any_zero & !.data$any_neg)
        if(nrow(temp) > 0){
            item(nrow(temp), " dbl/int vars are strictly positive (no zeros)")
        }
        # non-negative (positive or zero)
        temp = out %>%
            filter(.data$any_pos & .data$any_zero & !.data$any_neg)
        if(nrow(temp) > 0){
            item(nrow(temp), " dbl/int vars are non-negative (postive or zero)")
        }
        # only zero
        temp = out %>%
            filter(!.data$any_pos & .data$any_zero & !.data$any_neg)
        if(nrow(temp) > 0){
            warn(nrow(temp), " dbl/int vars are all zero")
        }
        # non-positive (i.e. negative or zero)
        temp = out %>%
            filter(!.data$any_pos & .data$any_zero & .data$any_neg)
        if(nrow(temp) > 0){
            item(nrow(temp), " dbl/int vars are non-positive (negative or zero)")
        }
        # strictly negative
        temp = out %>%
            filter(!.data$any_pos & !.data$any_zero & .data$any_neg)
        if(nrow(temp) > 0){
            item(nrow(temp), " dbl/int vars are strictly negative (no zeros)")
        }
    }

    # > max -----

    out$max = x %>%
        purrr::map_dbl(function(x){
            if(!type_sum(x) %in% c("dbl", "int")) return(NA_real_)
            result = try(max(x, na.rm = TRUE))
            if(inherits(result, "try-error")) return(NA_real_)
            result %>% as.numeric
        })

    # > min -----

    out$min = x %>%
        purrr::map_dbl(function(x){
            if(!type_sum(x) %in% c("dbl", "int")) return(NA_real_)
            result = try(min(x, na.rm = TRUE))
            if(inherits(result, "try-error")) return(NA_real_)
            result %>% as.numeric
        })

    # > binary -----

    out %<>%
        mutate(
            binary = if_else(
                .data$type %in% valid_types,
                .data$min %in% 0 &
                    .data$max %in% 1 &
                    .data$n_distinct %in% 2,
                NA
            )
        )
    if(verbose){
        temp = out %>%
            filter(.data$binary)
        if(nrow(temp) > 1){
            item(paste(nrow(temp), "dbl/int vars are binary"))
        }
    }

    # mode -----

    out %<>%
        mutate(
            min_distinct_mode = (.data$n_distinct <= !!min_distinct_mode) %in% TRUE,
            table = vector("list", ncol(x))
        )
    i = out$min_distinct_mode
    out$table[i] = x[,i] %>% purrr::map(table)
    out %<>%
        mutate(
            mode = .data$table %>%
                map_chr(function(x){
                    if(is.null(x)) return(NA_character_)
                    names(x)[which.max(x)]
                }),
            n_mode = table %>%
                map_int(function(x){
                    if(is.null(x)) return(NA_integer_)
                    max(x)
                }),
            p_mode = .data$n_mode / nrow(x)
        ) %>%
        select(-.data$table,
                      -.data$min_distinct_mode)
    if(verbose){
        # mode == 0
        temp = out %>%
            filter(.data$type %in% c("dbl","int")) %>%
            filter((.data$p_mode > max_p_mode) %in% TRUE)
        if(nrow(temp) > 0){
            warn(nrow(temp), " dbl/int vars have mode = 0")
        }
        # p_mode > max_p_mode
        temp = out %>%
            filter((.data$p_mode > max_p_mode) %in% TRUE)
        if(nrow(temp) > 0){
            warn(nrow(temp), " vars have p_mode > ", max_p_mode)
        }
    }

    out
}

heading = function(..., .col = "cyan"){
    if(is.null(.col)) .col = "cyan"
    cli::cat_rule(..., col = .col)
}

bullet = function(..., .col = NULL){
    # if(is.null(.col)) .col = "white"
    if(is.null(.col)) .col = "grey50"
    cli::cat_bullet(..., col = .col, bullet_col = .col, bullet = "bullet")
}

item = function(..., .col = NULL){
    if(is.null(.col)) .col = "white"
    # if(is.null(.col)) .col = "grey50"
    # cli::cat_bullet(..., col = .col, bullet_col = .col, bullet = "line")
    cli::cat_line(..., col = .col)
}

warn = function(..., .bullet = NULL){
    if(is.null(.bullet)){
        cli::cat_line(..., col = "orange")
    }else{
        cli::cat_bullet(..., col = "orange", bullet_col = "orange", bullet = .bullet)
    }
}

bullets = function(...){
    purrr::walk(as.list(c(...)), bullet)
}

items = function(...){
    purrr::walk(as.list(c(...)), item)
}
