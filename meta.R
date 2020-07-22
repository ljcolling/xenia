odel1.meta <- readr::read_csv("~/GitHub/fischerRRR-manuscript/data/meta_data/model1.meta.csv") %>% rename(Estimate = y, SE = v) %>% select(-ConditionDescription)
model1 = new.env()
model1.estimates <- load("~/GitHub/fischerRRR-manuscript/data/processed_rdata/results1.Rdata",envir = model1)
model1$fit.stats$AIC %>% tibble::as_tibble() %>% mutate(specification = rownames(model1$fit.stats$AIC)) %>% filter(EAMMCS == min(EAMMCS )) %>% pull(specification) -> specification

model1$estimates$EAMMCS[[specification]]$fe.estimates %>% as_tibble() %>% mutate(condition = rownames(model1$estimates$EAMMCS[[specification]]$fe.estimates)) -> meta.estimates

meta.estimates %>% mutate(DependentVariable = recode(condition,
                                                     "d250 * Not.Applicable" = "d250",
                                                     "d500 * Not.Applicable" = "d500",
                                                     "d750 * Not.Applicable" = "d750",
                                                     "d1000 * Not.Applicable" = "d1000")) %>%
  mutate(Freq = model1.meta %>% filter(DependentVariable == "d250") %>% pull(Freq) %>% sum()) %>%
  mutate(LabID = "meta") %>% select(LabID,Freq,DependentVariable,Estimate,SE)-> meta.estimates
rbind(model1.meta,meta.estimates) -> model1.meta.data

model1.meta.data %>% mutate(Type = ifelse(LabID == "meta","meta","lab")) -> model1.meta.data
```
----

  ```{r include=FALSE}
model1.meta.data %>% filter(DependentVariable == "d250") %>% add_case(.before = 18) %>% ggplot(aes(x = Estimate, y = LabID, xmin = Estimate - qnorm(.95) * SE, xmax = Estimate + qnorm(.95) * SE, size = Freq, shape = Type))+ geom_vline(xintercept = 0, linetype = 2, colour = "grey") + geom_point() + geom_errorbarh(size = .5, height = .5) + scale_shape_manual(values = c("lab" = 15, "meta" = 18))  + theme_minimal() + scale_y_discrete(limits = LabLabels$Nodes, labels = LabLabels$LabNames, name = NULL) + geom_hline(yintercept = 2, linetype = 1, size = 1) + theme(panel.grid.major = element_blank()) + scale_x_continuous(limits = c(-12,12))  + theme(legend.position = "none") -> meta.plot1
```
```{r echo=FALSE, fig.cap="Meta-analysis", out.height="150%"}
widgetframe::frameWidget(plotly::ggplotly(meta.plot1), width = "100%", height = "150%")
```
