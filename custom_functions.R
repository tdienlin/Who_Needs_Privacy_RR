# readability function
readability <- function(data){
  reading_estimates <- c("Flesch.Kincaid", "FOG", "Coleman.Liau.short", "Dale.Chall.PSK")
  textstat_readability(data, measure = reading_estimates) %>% 
    as.data.frame %>% 
    mutate(average = rowMeans(select(., -document)))
}

# plot distributions

plot_hist <- function(name){
  
  dat <- 
    select(
      d, paste0(rep(paste0(name, "_0"), 4), 1:4)
    ) %>% 
    pivot_longer(
      cols = everything(),
      names_to = "item"
    )
  
  ggplot(dat, aes(value)) +
    geom_bar() +
    scale_x_binned() +
    facet_wrap(facets = "item")
}

# extracting results.
get_res <- function(predictor_var, privacy_var, object = "tab_cor"){
  
    if(object == "tab_reg") {
      dat <- 
        tab_reg %>% 
        filter(`Need for privacy` == privacy_var & Predictor == predictor_var) %>%
        select(Estimate, CI_low, CI_high)
    } else {
      dat <- 
        tab_cor %>% 
        filter(`Need for privacy` == privacy_var & Predictor == predictor_var) %>%
        select(Estimate, CI_low, CI_high)
    }
  
  paste0(
    if(object == "tab_reg") {
      "_$\\beta$_ = "
    } else {
      "_r_ = "  
    },
    my_round(dat$Estimate, "std"),
    ", 90% CI ",
    my_round(dat$CI_low, "std"),
    ", ",
    my_round(dat$CI_high, "std"),
    ""
  )
}
