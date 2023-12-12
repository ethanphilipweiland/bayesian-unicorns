# Preliminaries

rm(list=ls()) # Clearing environment
ipak <- function(pkg){ # Function for installing and loading packages
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("tidyverse",
              "bayess",
              "gridExtra",
              "stargazer",
              "coda")
ipak(packages)
unicorns <- read_csv("unicorn_companies.csv", show_col_types = FALSE) # reading in data

# Cleaning data

  # Valuation
  names(unicorns)[2] <- "Valuation" 
  unicorns <- unicorns %>%
    mutate(Valuation = gsub("\\$", "", Valuation)) %>%
    mutate(Valuation = as.numeric(Valuation))
  
# Analysis 
  
  # Clustering with a Bayesian Normal Mixture Model
  
    # Summarizing and plotting densities
    summary(unicorns$Valuation)
    IQR(unicorns$Valuation)
    
    figure1 <- ggplot(unicorns, aes(x=Valuation)) +
      geom_density() +
      labs(title="Figure 1: Distribution of Unicorn Valuation (Untransformed)",
           x="Valuation ($B)",
           y="Density")
    unicorns <- mutate(unicorns, log_Valuation = log(Valuation))
    figure2 <- ggplot(unicorns, aes(x=log_Valuation)) +
      geom_density() +
      labs(title="Figure 2: Distribution of Unicorn Valuation (Logged)",
           x="Logged Valuation ($B)",
           y="Density") 
    grid.arrange(figure1, figure2, ncol=2)
    
    # Implementing gibbsnorm()
    log_valuation <- as.matrix(unicorns$log_Valuation)
    set.seed(83)
    
    mix=list(k=1, mu=mean(log_valuation), sig=var(log_valuation))
    one_component <- gibbsnorm(10000, log_valuation, mix) #k=1
    
    mix=list(k=2, mu=mean(log_valuation), sig=var(log_valuation))
    two_component <- gibbsnorm(10000, log_valuation, mix) #k=2
    
    mix=list(k=3, mu=mean(log_valuation), sig=var(log_valuation))
    three_component <- gibbsnorm(10000, log_valuation, mix) #k=3
    
    mix=list(k=4, mu=mean(log_valuation), sig=var(log_valuation))
    four_component <- gibbsnorm(10000, log_valuation, mix) #k=4
    
    
    # Comparing Models
    model_comparison_table <- data.frame(Components = c(1:4),
                                         logLik = c(mean(one_component$lopost),
                                                    mean(two_component$lopost),
                                                    mean(three_component$lopost),
                                                    mean(four_component$lopost)))
    stargazer(model_comparison_table, 
              type="html",
              summary=FALSE,
              title="Table 1: Model Fit by Number of Component Distributions",
              rownames=F)
    
    mixture_plot <- function(number_mixtures, original_density=TRUE) {
      data <- 0
      
      if (number_mixtures==1) {
        data <- data.frame(Values = rnorm(1000, mean=mean(one_component$mu), sd=sqrt(one_component$sig)))
      }
      
      if (number_mixtures==2) {
        prob <- apply(two_component$p, MARGIN=2, FUN=mean)
        component <- sample(c(1,2), size=1000, replace=TRUE, prob=prob)
        for (i in 1:length(component)) {
          if (component[i]==1) {
            data[i] <- rnorm(1, mean=mean(two_component$mu[,1]), sd=sqrt(mean(two_component$sig[,2])))
          } else if (component[i]==2) {
            data[i] <- rnorm(1, mean=mean(two_component$mu[,2]), sd=sqrt(mean(two_component$sig[,2])))
          }
        }
      }
      
      if (number_mixtures==3) {
        prob <- apply(three_component$p, MARGIN=2, FUN=mean)
        component <- sample(c(1:3), size=1000, replace=TRUE, prob=prob)
        for (i in 1:length(component)) {
          if (component[i]==1) {
            data[i] <- rnorm(1, mean(three_component$mu[,1]), sd=sqrt(mean(three_component$sig[,1])))
          } else if (component[i]==2) {
            data[i] <- rnorm(1, mean(three_component$mu[,2]), sd=sqrt(mean(three_component$sig[,2])))
          } else if (component[i]==3) {
            data[i] <- rnorm(1, mean(three_component$mu[,3]), sd=sqrt(mean(three_component$sig[,3])))
          }
        }
      }
      
      if (number_mixtures==4) {
        prob <- apply(four_component$p, MARGIN=2, FUN=mean)
        component <- sample(c(1:4), size=1000, replace=TRUE, prob=prob)
        for (i in 1:length(component)) {
          if (component[i]==1) {
            data[i] <- rnorm(1, mean(four_component$mu[,1], sd=sqrt(mean(four_component$sig[,1]))))
          } else if (component[i]==2) {
            data[i] <- rnorm(1, mean(four_component$mu[,2], sd=sqrt(mean(four_component$sig[,2]))))
          } else if (component[i]==3) {
            data[i] <- rnorm(1, mean(four_component$mu[,3], sd=sqrt(mean(four_component$sig[,3]))))
          } else if (component[i]==4) {
            data[i] <- rnorm(1, mean(four_component$mu[,4], sd=sqrt(mean(four_component$sig[,4]))))
          }
        }
      }
      
      plot <- ggplot(data.frame(Values=data), aes(x=Values)) +
        geom_density(col='red') +
        labs(y="Density",
             x="Logged Valuations") +
        xlim(-1, 5)
      
      if (original_density==TRUE) {
        data <- data.frame(Values = data, Source=rep("Model", 1000))
        original <- data.frame(Values = unicorns$log_Valuation, Source=rep("Data", length(unicorns$log_Valuation)))
        data <- rbind(data, original)
        data$Source <- factor(data$Source)
        plot <- ggplot(data, aes(x=Values, group=Source, col=Source)) +
          geom_density() +
          labs(y="Density",
               x="Logged Valuations") +
          xlim(-1, 5)
      }
      
      return(plot)
    }
    
    set.seed(67)
    figure3 <- mixture_plot(1) + ggtitle("Figure 3: One Component Bayesian Mixture Model")
    figure4 <- mixture_plot(2) + ggtitle("Figure 4: Two Component Bayesian Mixture Model")
    figure5 <- mixture_plot(3) + ggtitle("Figure 5: Three Component Bayesian Mixture Model")
    figure6 <- mixture_plot(4) + ggtitle("Figure 6: Four Component Bayesian Mixture Model")
    grid.arrange(figure3, figure4, figure5, figure6, ncol=2, nrow=2)
  
    # Examining three component model
    figure5 
    table2 <- data.frame(Mean = apply(three_component$mu, MARGIN=2, FUN=mean),
                         SD = apply(three_component$sig, MARGIN=2, FUN=mean),
                         Probability = apply(three_component$p, MARGIN=2, FUN=mean))
    table2$SD <- sqrt(table2$SD)
    rownames(table2) <- c("Component 1", "Component 2", "Component 3")
    stargazer(table2,
              type="html",
              summary=FALSE,
              title="Table 2: Estimated Parameter Expectations of Three Components")
    table3 <- data.frame(Mean = apply(three_component$mu, MARGIN=2, FUN=sd),
                         SD = apply(three_component$sig, MARGIN=2, FUN=sd),
                         Probability = apply(three_component$p, MARGIN=2, FUN=sd))
    rownames(table3) <- c("Component 1", "Component 2", "Component 3")
    stargazer(table3,
              type="html",
              summary=FALSE,
              title="Table 3: Estimated Parameter Standard Deviations of Three Components")
    
    # Effective sample size
    table4 <- data.frame(Mean = apply(three_component$mu, MARGIN=2, FUN=effectiveSize),
                         Variance = apply(three_component$sig, MARGIN=2, FUN=effectiveSize),
                         p = apply(three_component$p, MARGIN=2, FUN=effectiveSize))
    rownames(table4) <- c("Component 1", "Component 2", "Component 3")
    stargazer(table4, 
              type='html',
              summary=FALSE,
              title="Table 4. Effective Sample Sizes Across Parameters and Components")
    
    # Prior sensitivity analysis
    set.seed(811)
    mix=list(k=3, mu=2*mean(log_valuation), sig=var(log_valuation))
    double_mean <- gibbsnorm(10000, log_valuation, mix) # k=3, double mean prior
    
    mix=list(k=3, mu=mean(log_valuation), sig=2*var(log_valuation))
    double_variance <- gibbsnorm(10000, log_valuation, mix) # k=3, double variance prior
    
    sensitivity_data <- data.frame(Values = unicorns$log_Valuation, Source=rep("Data", length(unicorns$log_Valuation))) #original data
    
    data <- 0
    prob <- apply(three_component$p, MARGIN=2, FUN=mean) # three component model original prior
    component <- sample(c(1:3), size=1000, replace=TRUE, prob=prob)
    for (i in 1:length(component)) {
      if (component[i]==1) {
        data[i] <- rnorm(1, mean(three_component$mu[,1]), sd=sqrt(mean(three_component$sig[,1])))
      } else if (component[i]==2) {
        data[i] <- rnorm(1, mean(three_component$mu[,2]), sd=sqrt(mean(three_component$sig[,2])))
      } else if (component[i]==3) {
        data[i] <- rnorm(1, mean(three_component$mu[,3]), sd=sqrt(mean(three_component$sig[,3])))
      }
    }
    sensitivity_data <- rbind(sensitivity_data, data.frame(Values = data, Source = rep("Normal Priors", length(data))))
    
    data <- 0
    prob <- apply(double_mean$p, MARGIN=2, FUN=mean) # three component model double mean prior
    component <- sample(c(1:3), size=1000, replace=TRUE, prob=prob)
    for (i in 1:length(component)) {
      if (component[i]==1) {
        data[i] <- rnorm(1, mean(double_mean$mu[,1]), sd=sqrt(mean(double_mean$sig[,1])))
      } else if (component[i]==2) {
        data[i] <- rnorm(1, mean(double_mean$mu[,2]), sd=sqrt(mean(double_mean$sig[,2])))
      } else if (component[i]==3) {
        data[i] <- rnorm(1, mean(double_mean$mu[,3]), sd=sqrt(mean(double_mean$sig[,3])))
      }
    }
    sensitivity_data <- rbind(sensitivity_data, data.frame(Values = data, Source = rep("Double Mean Prior", length(data))))
    
    data <- 0
    prob <- apply(double_variance$p, MARGIN=2, FUN=mean) # three component model double variance prior
    component <- sample(c(1:3), size=1000, replace=TRUE, prob=prob)
    for (i in 1:length(component)) {
      if (component[i]==1) {
        data[i] <- rnorm(1, mean(double_variance$mu[,1]), sd=sqrt(mean(double_variance$sig[,1])))
      } else if (component[i]==2) {
        data[i] <- rnorm(1, mean(double_variance$mu[,2]), sd=sqrt(mean(double_variance$sig[,2])))
      } else if (component[i]==3) {
        data[i] <- rnorm(1, mean(double_variance$mu[,3]), sd=sqrt(mean(double_variance$sig[,3])))
      }
    }
    sensitivity_data <- rbind(sensitivity_data, data.frame(Values = data, Source = rep("Double Variance Prior", length(data))))
    
    figure7 <- ggplot(sensitivity_data, aes(x=Values, group=Source, col=Source)) +
      geom_density() +
      labs(title="Figure 8. Unicorn Valuation vs. Predicted Valuation Across\nDifferent Prior Configurations",
           y="Density",
           x="Logged Valuation")
      
    figure8 <- ggplot(sensitivity_data, aes(x=Values, group=Source)) +
      geom_density() +
      facet_wrap(~Source, ncol=2) +
      labs(title="Figure 7: Unicorn Valuation vs. Predicted Valuation Across\nDifferent Prior Configurations (Faceted)",
           y="Density",
           x="Logged Valuation")
    
    grid.arrange(figure8, figure7, ncol=2)
    
    table5 <- data.frame(Prior = c("Unadjusted Prior",
                                   "Double Mean Prior",
                                   "Double Variance Prior"),
                         logLik = c(mean(three_component$lopost),
                                    mean(double_mean$lopost),
                                    mean(double_variance$lopost)))
    stargazer(table5,
              type="html",
              summary=FALSE,
              rownames=FALSE,
              title="Table 5: Log Likelihoods Across Different Prior Specifications")
    
# Appendix
  
  # Prior plots
  
    # p
    dirichlet_pmf_plot <- function(x) {
      y <- rdirichlet(10000, rep(1/2, x)) %>%
        apply(MARGIN=2, FUN=mean) %>%
        data.frame() %>%
        cbind(c(1:x))
      names(y) <- c("Mass", "Value")
      y$Mass <- round(y$Mass, 2)
      title <- paste(paste0("Figure A", x, ":"), "Prior PMF for x =", x)
      ggplot(y, aes(x=Value, y=Mass)) +
        geom_point(size=5) +
        geom_segment(aes(x=Value, y=Mass, xend=Value, yend=0)) +
        xlim(1,x) +
        ylim(0,1) +
        ggtitle(title)
    }
    dir1 <- dirichlet_pmf_plot(1)
    dir2 <- dirichlet_pmf_plot(2)
    dir3 <- dirichlet_pmf_plot(3)
    dir4 <- dirichlet_pmf_plot(4)
    dir5 <- dirichlet_pmf_plot(5)
    grid.arrange(dir1, dir2, dir3, dir4,
                 ncol=2, 
                 nrow=2)
    
    # sigma2
    set.seed(32)
    var_priors <- data.frame(Values = rgamma(10000, 10, var(unicorns$log_Valuation)))
    ggplot(var_priors, aes(x=Values)) +
      geom_density() +
      labs(title = expression("Figure A5: Prior Distribution of 1 /" ~ sigma^2),
           y="Density")
    
    # mu
    mean_prior_plot <- function(number_plots) {
      set.seed(1)
      
      for(i in 1:number_plots) {
        precision <- rgamma(1, 10, var(unicorns$log_Valuation))
        values <- rnorm(1000, mean=mean(unicorns$log_Valuation), sd=sqrt(1/precision))
        
        if (i==1) {
          data <- data.frame(Values=values, Precision=rep(precision, length(values))) 
        }
        
        new_data <- data.frame(Values=values, Precision=rep(precision, length(values)))
        data <- rbind(data, new_data)
      }
      
      data$Precision <- round(data$Precision, 2)
      data$Precision <- factor(data$Precision)
      
      output <- ggplot(data, aes(x=Values, group=Precision, col=Precision)) +
        geom_density() +
        labs(title="Figure A6: Prior Plot for Mean Conditional on Various Precisions",
             y="Density")
      return(output)
    }
    mean_prior_plot(10)
    
    # Trace Plots
    
      # mu
      par(mfrow=c(3,1))
      plot(three_component$mu[,1], type="l", main=expression("Figure A7: Trace Plot of First Component" ~ mu), ylab=expression(mu[1]))
      plot(three_component$mu[,2], type="l", main=expression("Figure A8: Trace Plot of Second Component" ~ mu), ylab=expression(mu[2]))
      plot(three_component$mu[,3], type="l", main=expression("Figure A9: Trace Plot of Third Component" ~ mu), ylab=expression(mu[3]))
      
      #sigma
      plot(three_component$sig[,1], type="l", main=expression("Figure A10: Trace Plot of First Component" ~ sigma^2), ylab=expression(sigma[1]^2))
      plot(three_component$sig[,2], type="l", main=expression("Figure A11: Trace Plot of Second Component" ~ sigma^2), ylab=expression(sigma[2]^2))
      plot(three_component$sig[,3], type="l", main=expression("Figure A12: Trace Plot of Third Component" ~ sigma^2[3]), ylab=expression(sigma[2]^2))
      
      # p
      plot(three_component$p[,1], type="l", main=expression("Figure A13: Trace Plot of First Component p"), ylab=expression(p[1]))
      plot(three_component$p[,2], type="l", main=expression("Figure A14: Trace Plot of Second Component p"), ylab=expression(p[2]))
      plot(three_component$p[,3], type="l", main=expression("Figure A15: Trace Plot of Third Component p"), ylab=expression(p[3]))
      
    # ACF Plots
      
      # mu
      acf(three_component$mu[,1], main=expression("Figure A16: ACF Plot of First Component" ~ mu), ylab=expression(mu[1]))
      acf(three_component$mu[,2], main=expression("Figure A17: ACF Plot of Second Component" ~ mu), ylab=expression(mu[2]))
      acf(three_component$mu[,3], main=expression("Figure A18: ACF Plot of Third Component" ~ mu), ylab=expression(mu[3]))

      # sigma
      acf(three_component$sig[,1], main=expression("Figure A19: ACF Plot of First Component" ~ sigma^2), ylab=expression(sigma[1]^2))
      acf(three_component$sig[,2], main=expression("Figure A20: ACF Plot of Second Component" ~ sigma^2), ylab=expression(sigma[2]^2))
      acf(three_component$sig[,3], main=expression("Figure A21: ACF Plot of Third Component" ~ sigma^2), ylab=expression(sigma[3]^2))
      
      # p
      acf(three_component$p[,1], main=expression("Figure A22: ACF Plot of First Component p"), ylab=expression(p[1]))
      acf(three_component$p[,2], main=expression("Figure A23: ACF Plot of Second Component p"), ylab=expression(p[2]))
      acf(three_component$p[,3], main=expression("Figure A24: ACF Plot of Third Component p"), ylab=expression(p[3]))
