
install.packages("flexmix")
library("flexmix")

attach(project)
str(project)
summary(project)


if (require("mlogit")) {
data("Catsup", package = "mlogit")
## To reduce the time needed for the example only a subset is used
Catsup <- subset(Catsup, id %in% 1:100)
Catsup$experiment <- seq_len(nrow(Catsup))
vnames <- c("display", "feature", "price")
Catsup_long <-
reshape(Catsup,
idvar = c("id", "experiment"),
times = c(paste("heinz", c(41, 32, 28), sep = ""),
"hunts32"),
timevar = "brand",
varying = matrix(colnames(Catsup)[2:13], nrow = 3, byrow = TRUE),
v.names = vnames,
direction = "long")
Catsup_long$selected <- with(Catsup_long, choice == brand)
Catsup_long <- Catsup_long[, c("id", "selected", "experiment", vnames, "brand")]
Catsup_long$brand <- relevel(factor(Catsup_long$brand), "hunts32")
set.seed(0808)
flx1 <- flexmix(selected ~ display + feature + price + brand | id,
model = FLXMRcondlogit(strata = ~ experiment),
data = Catsup_long, k = 1)
}


project$experiment <- seq_len(nrow(project))
flx1 <- flexmix(Choice ~ Philips1+Capacity5_1+Capacity10_1 + Price59_1+Price79_1+Filter_1+Grinder_1|Respondent,
                model = FLXMRcondlogit(strata = ~ experiment),
                data = project, k = 2)


out1 <- Flexmix(Choice ~ Philips1+Capacity5_1+Capacity10_1+Price59_1+Price79_1+Filter_1+Grinder_1|Respondent,
                          data= project,control=list(verbose=0),k=1:2,nrep=5)
out1
out1.1<-getModel(out1,"BIC")
summary(out1.1)
pr1.1<-posterior(out1.1)
pr1.1
out2 <- stepFlexmix(Choice ~ Philips2+Capacity5_2+Capacity10_2+Price59_2+Price79_2+Filter_2+Grinder_2|Respondent,data= project,control=list(verbose=0),k=1:2,nrep=5)
out2
out2.1<-getModel(out2,"BIC")
summary(out2.1)
pr2.1<-posterior(out1.1)
pr2.1
out3 <- stepFlexmix(Choice ~ Philips3+Capacity5_3+Capacity10_3+Price59_3+Price79_3+Filter_3+Grinder_3|Respondent,data= project,control=list(verbose=0),k=1:2,nrep=5)
out3
out3.1<-getModel(out3,"BIC")
summary(out3.1)
pr3.1<-posterior(out1.1)
pr3.1

#FLXMRCondlogit
