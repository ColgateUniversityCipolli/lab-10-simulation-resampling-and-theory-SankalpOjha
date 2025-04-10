library(tidyverse)
library(patchwork)
library(e1071)

#Part 1
sample.size <- 1004
num.polls <- 10000

polls.basic.sim <- rbinom(n = num.polls, size = sample.size, prob = 0.39)/sample.size

basic.plot <- ggplot() +
  geom_histogram(aes(x = polls.basic.sim, y = after_stat(density))) +
  geom_density(aes(x=polls.basic.sim), color = "red") +
  theme_bw() +
  xlab("p")+
  geom_vline(xintercept=0.39, color = "green")+
  geom_hline(yintercept = 0) +
  ggtitle("Sampling Distribution For p, Sample Size = 1004")

(basic.plot)

range.mid.95 <- quantile(x=polls.basic.sim, 0.975) - quantile(x=polls.basic.sim, 0.025)
margin.of.error <- 0.5*range.mid.95

sample.size2 <- sample.size * 2

polls.basic.sim2 <- rbinom(n = num.polls, size = sample.size2, prob = 0.39)/sample.size2

basic.plot2 <- ggplot() +
  geom_histogram(aes(x = polls.basic.sim2, y = after_stat(density))) +
  geom_density(aes(x=polls.basic.sim2), color = "red") +
  theme_bw() +
  xlab("p")+
  geom_vline(xintercept=0.39, color = "green")+
  geom_hline(yintercept = 0) +
  ggtitle("Sampling Distribution For p, Sample Size = 2008")

(basic.plot2)

range2.mid.95 <- quantile(x=polls.basic.sim2, 0.975) - quantile(x=polls.basic.sim2, 0.025)
margin.of.error2 <- 0.5*range2.mid.95

basic.plot + basic.plot2

#Part 2
resamples <- 1000

data.gallup <- tibble(data = c(satisfied = rep(1, times = round(0.39 * sample.size)),
                               unsatisfied = rep(0, times = round(0.59 * sample.size)),
                               no.opin = rep(0, times = round(0.02 * sample.size)))
)

p.hat <- tibble(p = numeric(resamples))

for(i in 1:resamples){
  resample <- sample(x = data.gallup$data,
                     size = nrow(data.gallup),
                     replace = T)
  
  p.hat$p[i] <- mean(resample, na.rm = T)
}

resample.plot <- ggplot(data = p.hat)+
  geom_histogram(aes(x = p, y = after_stat(density)))+
  geom_density(aes(x = p), color = "red")+
  theme_bw()+
  geom_hline(yintercept = 0) +
  ggtitle("Resampling Sampling Distribution, Resample Size = 1000")

resamp.mid.95 <- quantile(x=p.hat$p, 0.975) - quantile(x=p.hat$p, 0.025)
resamp.moe <- 0.5*resamp.mid.95

(resample.plot)

#Simulation over n and p
n <- seq(100, 3000, by = 10)
p <- seq(0.01, 0.99, by = 0.01)

np.data <- tibble(n.val = numeric(),
                  p.val = numeric(),
                  estim.moe = numeric()
                  )

for (curr.n in n){
  for(curr.p in p){
    curr.poll <- rbinom(n=num.polls, size = curr.n, prob = curr.p)/curr.n
    curr.mid.95 <- quantile(x=curr.poll, 0.975) - quantile(x=curr.poll, 0.025)
    curr.moe <- 0.5*curr.mid.95
    np.data <- bind_rows(np.data, tibble(n=curr.n, p=curr.p, estim.moe = curr.moe))
  }
}

np.plot <- ggplot(data = np.data) +
  geom_raster(aes(x = p, y = n, fill = estim.moe)) +
  scale_fill_distiller("Estimated Margin of Error", palette = "Set2") +
  theme_bw()+
  geom_vline(xintercept = 0.39)+
  geom_hline(yintercept = 1004, color = "red") +
  geom_hline(yintercept = 2008, color = "skyblue")
  
(np.plot)


#Actual Margin of Error Calculation
n.w <- seq(100, 2000, by = 10)
p.w <- seq(0.01, 0.99, by = 0.01)

z.sample <- rnorm(n=10000)
z.pin <- qnorm(0.975) #quantile(x=z.sample, 0.975) - quantile(x=z.sample, 0.025)

wilson.data <- tibble(wilson.n.val = numeric(),
                      wilson.p.val = numeric(),
                      wilson.estim.moe = numeric()
)

for (curr.n in n.w){
  for(curr.p in p.w){
    wilson.moe <- z.pin * ((sqrt(curr.n*curr.p*(1-curr.p) + (z.pin^2)/4)) / (curr.n+z.pin^2))
    
    wilson.data <- bind_rows(wilson.data, 
                             tibble(wilson.n.val=curr.n, 
                                    wilson.p.val=curr.p, 
                                    wilson.estim.moe = wilson.moe))
  }
}

wilson.plot <- ggplot(data = wilson.data)+
  geom_raster(aes(x = wilson.p.val, y = wilson.n.val, fill = wilson.estim.moe))+
  scale_fill_distiller("Wilson Margin of Error", palette = "Set2")+
  theme_bw()+
  ylab("n")+
  geom_vline(xintercept = 0.39)+
  geom_hline(yintercept = 1004, color = "darkred")

np.plot + wilson.plot
