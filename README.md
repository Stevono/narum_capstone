---
title: "Capstone"
author: "narum_steven"
date: "4/26/2020"
output: html_document

---

```{r}
library(readr)
library(tidyverse)
library(knitr)
library(ez)
library(viridis)
```


1) Provide a brief background and significance about a specific research problem that interests you. It could be project you’re involved with now, or a rotation project, or something you’d like to work on. The reader will need to understand enough background to make sense of the experiment you propose below. Keep it brief. In one short paragraph.

***The ability of nanocarriers to reach the cytoplasm to enact a therapeutic response has been a controversial and heavily researched topic. It has been discovered that many nanoparticle formulations are uptaken primarily through an clathrin-mediated endocytic pathway, but are degraded within lysosomes through an endosomal acidification process before it can reach its therapeutic destination. However, recently there has been new debate regarding the size of nanoparticles and its influence on uptake pathways, suggesting that smaller particles can bypass this acidification process and immediately enter the cytoplasm through caveolae-mediated uptake, yet literature results have not been consistent with this hypothesis, with some papers showing the exact opposite result.***

2) Briefly state something that is unknown about this system that can be discovered through, and leads to, an experiment.  For example, "It is not known whether....."

***Although much of these uptake processes are cell-type dependent, it is not generally known if there is a size cut-off threshold that suggests one uptake pathway over another. Experimentation with known endocytic-inhibitors could help finally put rest to this debate.***

3) Make an “if” “then” prediction that is related to item #2. It should be of the general form, “if X is true, then Y should happen”.

***It is predicted that if the nanoparticle is <100 nm in diameter and clathrin-mediated endocytosis is inhibited, then the uptake will change if the diameter is >100 nm due to the influence of caveolae-mediated endocytosis..***

4) What dependent variable will be observed to test this prediction in item #3? What predictor variable will be used to manipulate the system experimentally? Define the inherent properties of these variables (eg, are they sorted, ordered or measured).

***The dependent variable will be the fluorescent intensity measured of fluorescent nanoparticle beads, which will be normalized to a 100% scale. The nanoparticle diameter will be changed to determine how uptake changes based on size of the particle/bead. The dependent variable is measured, and the predictor variable is sorted.***

5) Write a statistical hypothesis.  There should be a null and alternate. These should be explicitly consistent with the prediction in item #3 and the response variable in #4. In other words, make sure the statistical hypotheses that you write here serves as a test of the prediction made in item #3.

***The null hypothesis states that the uptake of the <100 nm nanoparticles will be equal to the uptake of >100 nm nanoparticles in the presence of clathrin-mediated endocytosis inhibitors. The alternative hypothesis states that the uptake of the <100 nm nanoparticles will be different from the uptake of >100 nm nanoparticles in the presence of clathrin-mediated endocytosis inhibitors.***

6) What is the statistical test you would use to test the hypothesis in item #5? Briefly defend what makes this appropriate for the hypothesis and the experimental variables. If there are alternatives, why is this approach chosen instead? Points will not be awarded if the justification involves something like "because everybody does it this way".

***A one-way ANOVA test will be conducted to test the hypothesis since there will be more than two sizes measured, and it can be used to test if there are differences between groups without increased error as a result of multiple comparisons. Additional post-hoc tests will be conducted if significance is found during this analysis. A student's t-test or regression will not be conducted since there would be increased type 1 family-wise error with multiple t-tests. The regression analysis may not be as suitable for this experiment since nanobead size may be involved in many uptake pathways, so an inference ***

7) List the procedures and decision rules you have for executing and interpreting the experiment. These procedures range from selection of experimental units, to randomization to primary endpoint to threshold decisions. Define (and defend) what you believe will be the independent replicate.

***The predictor variable will involve 25 nm, 50 nm, 100 nm, 200 nm, and 500 nm since this involves a wide range of sizes that may make relevant any unexpected trends with larger or smaller sizes. Clathrin-mediated inhibitors such as filipin, genistein, and chlorpromazine can be used to limit the uptake pathways so that the results are primarily due to caveolae-mediated endocytosis, which is regarded as the ideal uptake method since it avoids the degradation pathways. Nanobeads will be assessed for size and ensured that there is a monodisperse population before testing. Non-phagocytic HUVEC cells would be the ideal cell line as it expresses both clathrin and caveolae mediated uptake dependence. Over a period of 2 months, each weekly cell passaging will involve removing a portion of cells into a well-plate and randomly assigning and injecting a nanobead size and measuring fluorescence after 24-hour incubation. Since many of the samples will not share the same passage number, this should satisfy the independent replicate condition.***

8) Produce a graph of a simulation for the expected results. Create a dataMaker-like function in R to create and plot the data. Label and scale any axis. The graph should illustrate the magnitude of the expected response, or the level of response that you expect to see and would be minimally scientifically relevant. Be sure to illustrate any variation that is expected.

```{r}
simData <- data.frame(nb1_25nm = rnorm(n=5,mean=95,sd=5),
                      nb2_50nm = rnorm(n=5,mean=90,sd=10),
                      nb3_100nm = rnorm(n=5,mean=80,sd=10),
                      nb4_200nm = rnorm(n=5,mean=50,sd=15),
                      nb5_500nm =rnorm(n=5,mean=20,sd=20)) %>%
  pivot_longer(cols = nb1_25nm:nb5_500nm,
               names_to="Predictor",
               values_to="Response")



ggplot(simData,aes(Predictor,Response,color=Predictor)) +
  stat_summary(fun.data = "mean_sdl", 
               fun.args = list(mult=1), 
               geom="crossbar", 
               width=0.3, 
               color="red") +
  geom_jitter(width=0.15, size=2) +
  scale_x_discrete(labels = c('25 nm','50 nm','100 nm','200 nm','500 nm')) +
  scale_color_manual(labels=c('25 nm','50 nm','100 nm','200 nm','500 nm'), values = c("blue", "red","purple","green","black")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  labs(x="Nanobead size",y="Normalized Uptake (%)")
```


9) Write and perform a Monte Carlo analysis to calculate a sample size necessary to test the hypothesis. This Monte Carlo must test the primary endpoint.

```{r}



b = 95
a = 0.947
c = 0.842
d = 0.526
e = 0.211
sd = 15 #expected standard deviation of Outcome variable
n = 40 # number of independent replicates per group
sims = 100 #number of Monte Carlo simulations to run. 

CRdataMaker <- function(n, b, a, f, sd) { 
  
  
  a1 <- rnorm(n, b, sd) #25 nm
  a2 <- rnorm(n, (b*a), sd) #50 nm
  a3 <- rnorm(n, (b*c), sd) #100 nm
  a4 <- rnorm(n, (b*d), sd) #200 nm
  a5 <- rnorm(n, (b*e), sd) #500 nm
  
    
    Outcome <- c(a1, a2, a3, a4, a5)
    Predictor <- c(rep(c("a1", "a2", "a3","a4","a5"), each = n))
    ID <- as.factor(c(1:length(Predictor)))
    df <-data.frame(ID, Predictor, Outcome)
    }

pval <- replicate(
  sims, {

    sample.df <- CRdataMaker(n, b, a, f, sd)

    sim.ezaov <- ezANOVA(
            data = sample.df,
            wid = ID,
            dv = Outcome,
            between = Predictor,
            type = 2
            )
  pval <- sim.ezaov$ANOVA[1,5]

    }
  )

pwr.pct <- sum(pval<0.05)/sims*100
paste(pwr.pct, sep="", "% power. Change 'n' in your initializer for higher or lower power.")




pval <- replicate(
  sims, {
 
    sample.df <- CRdataMaker(n, b, a, f, sd)
    pval <- pairwise.t.test(sample.df$Outcome, sample.df$Predictor)$p.value[2,2]
    
    }
  )

pwr.pct <- sum(pval<0.05)/sims*100
paste(pwr.pct, sep="", "% power. Change 'n' in your initializer for higher or lower power.")
```

***From the power analysis, any sample size showed 100% power, which is due to the large differences between sample groups. However, in post-hoc analysis, it was found that individual comparisons between groups that were more similar in size required much larger sample sizes to achieve 80% power. As an example, it required n=40 to achieve 80% power between the 50 nm and 100 nm nanobead groups, suggesting that a large enough sample size can achieve significant difference. ***

10) Write up it all in RMarkdown. Code chunks to illustrate specific points are welcome other than for the Monte Carlo code. Knit and submit and upload the html document by the due data. If it is readable to your best friend, it is readable to us.


