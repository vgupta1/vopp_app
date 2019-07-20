### Does the actual work fore creating bounds
library(tidyverse)
library(ggplot2)
library(purrr)
library(lamW)

##Assume vmin = c for now?
make_unitless <- function(c, mu, vmax){
  return(tibble(S=vmax/mu, M= 1-c/mu))
}

delta_L <- function(S, M){
  ratio = (S + M - 1)/M
  - M * log(ratio) / lambertWm1(-1 /exp(1) / ratio )
}

delta_M <- function(S, M){
  ratio = (S + M -1)/M
  M * log(ratio) / (1 + log(ratio))
}

delta_H <- function(S, M){
  ratio = (S + M -1)/M
  (S - 1)/ratio
}

#Do this the lazy way for now and do not worry about vectorization
## Assumes that validation ensures D is not negative and <= DeltaH
wc_bound <-function(S, M, D){
  #D must be positive and less than DeltaH
  if(D < delta_L(S, M)){
    ratio = D/M - 1
    return( lambertWm1(ratio/exp(1)) / ratio)
  }  else if( D < delta_M(S, M) ){
    ratio = (S + M - 1)/M
    return ( M * log(ratio)/ D)
  } else {
    ratio = (S + M - 1)/M
    return (-lambertWm1( -1 / exp(1) / ratio / (1- D/M) ))
  }
  return(0)
}


#Actually computes the plot for different B
wc_plot <- function(S, M){
  D_grid = seq(from=0, to= delta_H(S, M), length.out=100)
  out <- map_dbl(D_grid, ~wc_bound(S, M, .))  
  
  #Fix the last one for consistency
  out[100] <- 1.0
  dat <- tibble(Ds=D_grid, vals=out - 1)
  
  ggplot(dat, aes(D_grid, vals)) + 
    geom_point() + 
    geom_line(color="blue") +
    ylab("(%) Improvement") +
    xlab("Coefficient of Deviation") + 
    scale_y_continuous(labels=scales::percent) + 
    theme_bw(base_size=18)   +
    geom_vline(xintercept = delta_L(S, M),
               linetype="dashed") +
    geom_vline(xintercept = delta_M(S, M),
               linetype="dashed")
}


#####
#Tight CCDF Constructions
# For now constructed on unitless scale...
#####
ccdf_med <- function(S, M, D){
  alpha_m <- 1/wc_bound(S, M, D)
  tstar = exp((alpha_m + D - 1)/alpha_m)
  supp = seq(0, S, length.out=1000)
  Fbar <- function(x){
    if(x <= 0){
      return(1)
    }else if(x <= tstar){
      return(alpha_m/tstar)
    }else if(x < S){
      return (alpha_m / x)
    }
    return (0)
    
  }  #ends FBAR
  
  tibble(supp=supp, 
         vals = map_dbl(supp, Fbar)) %>%
    ggplot(aes(supp, vals)) + 
    geom_line() +
    theme_bw(base_size=18) + 
    xlab("x") + ylab("Fbar(x)") + 
    ylim(0, 1)
}

ccdf_high<- function(S, M, D){
  alpha_H <- 1/wc_bound(S, M, D)
  supp = seq(0, S, length.out=1000)
  
  Fbar <- function(x){
    if( x <= 0 ){
      return (1.)
    } else if (x <= alpha_H/(1-D)){
      return(1-D)
    } else if (x < S){
      return( alpha_H /x ) 
    } else{
      return (0.)
    }
  } #ends Fbar
  tibble(supp=supp, 
         vals = map_dbl(supp, Fbar)) %>%
    ggplot(aes(supp, vals)) + 
    geom_line(color="red") +
    theme_bw(base_size=18) + 
    xlab("x") + ylab("Fbar(x)") + 
    ylim(0, 1)
}

ccdf_low <- function(S, M, D){
  alpha_L = 1/wc_bound(S, M, D)
  supp = seq(0, S, length.out=1000)
  Fbar<- function(x){
    if (x < alpha_L){
      return (1)
    }else if(x <= exp(D/alpha_L)){
      return (alpha_L/x)
    }
    return(0.)
  }# end fbar
  tibble(supp=supp, 
         vals = map_dbl(supp, Fbar)) %>%
    ggplot(aes(supp, vals)) + 
    geom_line(color="blue") +
    theme_bw(base_size=18) + 
    xlab("x") + ylab("Fbar(x)") + 
    ylim(0, 1)
}

ccdf_plot <- function(S, M, D){
  #D must be positive and less than DeltaH
  if(D < delta_L(S, M)){
    return( ccdf_low(S, M, D) )
  }  else if( D < delta_M(S, M) ){
    return ( ccdf_med(S, M, D)  )
  } else {
    return ( ccdf_high(S, M, D))
  }
  return(NULL)
}