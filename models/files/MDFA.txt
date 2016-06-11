modelInfo <- list(label = "MDFA",
                  library = NULL,
                  loop = NULL,
                  type = "Regression",
                  parameters = data.frame(parameter = "parameter",
                                          class = "character",
                                          label = "parameter"),
                  grid = function(x, y, len = NULL, search = "grid") data.frame(parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- if(is.data.frame(x)) x else as.data.frame(x)

source("https://www.dropbox.com/s/j4cshlyclih2car/wrap.r?dl=1")
source("https://www.dropbox.com/s/lkv8ww25mcu0uqg/imdfa.R?dl=1")
##length of vector
len<-length(x[,1])
# Bandpass specification
ub<-3
lb<-550
cutoff<-pi/ub
# Target lead/lag
Lag<--1
# lin_expweight<-F replicates the traditional customization with expweight as the parameter - recommended -.
# lin_expweight<-T replicates the stuff in section 6 of the trilemma paper with Tucker McElroy (paper can be downloaded from sefblog)
lin_expweight<-F
weight_constraint<-rep(1/(length(x[1,])-1),length(x[1,])-1)
d<-0
#--------------------------------------------------------
# Compute DFT's (spectral matrix) and specify target bandpass filter
spec_obj<-spec_comp(len,x,d)
weight_func<-spec_obj$weight_func
K<-length(weight_func[,1])-1
# target symmetric bandpass filter
#Gamma<-((0:K)<K/ub)&((0:K)>K/lb)
# target symmetric trend filter: use this one when going through my examples!
Gamma<-((0:K)<K/ub)
L<-100
lambda_smooth<-0.000
lambda_decay<-0
lambda<-0
expweight<-4
lambda_cross<-0
i1<-F
i2<-F
plots=T









       
                    out <- i_mdfa_obj<-IMDFA_comp(Lag,K,L,lambda,weight_func,Gamma,expweight,cutoff,i1,i2,weight_constraint,lambda_cross,lambda_decay,lambda_smooth,x,plots,lin_expweight)
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    filter(modelFit, newdata)
                    },
                  prob = NULL,
                  predictors = NULL,
                  tags = c("Linear Regression", "Accepts Case Weights"),
                  varImp = function(object, ...) {
                    values <- summary(object)$coef
                    varImps <-  abs(values[-1, grep("value$", colnames(values))])
                    out <- data.frame(varImps)
                    colnames(out) <- "Overall"
                    if(!is.null(names(varImps))) rownames(out) <- names(varImps)
                    out   
                  },
                  sort = function(x) x)