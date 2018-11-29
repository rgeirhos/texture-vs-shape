###################################################################
#   Data analysis script for texture-shape experiments   
#   -------------------------------------------------------
#   Author:   Robert Geirhos
#   Based on: R version 3.2.3
###################################################################

source("data-analysis-helper.R")
source("autoanalyse-helper.R")

FIGUREPATH = "../paper-figures/"
DATAPATH = "../raw-data/"

###################################################################
#               loading & preprocessing experimental data
###################################################################

## pre-trained CNNs
origdat = get.expt.data("original-experiment")
greydat = get.expt.data("grayscale-experiment")
texturedat = get.expt.data("texture-experiment")
sildat = get.expt.data("silhouette-filled-experiment")
edgedat = get.expt.data("edge-experiment")
transfer512nomaskdat = get.expt.data("style-transfer-512-nomask-experiment", is.style.transfer = TRUE)
transfer.content.guess = get.expt.data("style-transfer-512-nomask-content-guessing-experiment", is.style.transfer = TRUE)
transfer.texture.guess = get.expt.data("style-transfer-512-nomask-texture-guessing-experiment", is.style.transfer = TRUE)
tex.filled.rotated = get.expt.data("texture-filled-rotated-experiment", is.style.transfer = TRUE)

# trained CNNs
origdat.t = get.expt.data("original-experiment-trained-CNNs", append.human.data = T)
greydat.t = get.expt.data("grayscale-experiment-trained-CNNs", append.human.data = T)
texturedat.t = get.expt.data("texture-experiment-trained-CNNs", append.human.data = T)
sildat.t = get.expt.data("silhouette-filled-experiment-trained-CNNs", append.human.data = T)
edgedat.t = get.expt.data("edge-experiment-trained-CNNs", append.human.data = T)
transfer512nomaskdat.t = get.expt.data("style-transfer-512-nomask-experiment-trained-CNNs", is.style.transfer = TRUE, append.human.data = T)
transfer.content.guess.t = get.expt.data("style-transfer-512-nomask-content-guessing-experiment-trained-CNNs", is.style.transfer = TRUE, append.human.data = TRUE)
transfer.texture.guess.t = get.expt.data("style-transfer-512-nomask-texture-guessing-experiment-trained-CNNs", is.style.transfer = TRUE, append.human.data = TRUE)
tex.filled.rotated.t = get.expt.data("texture-filled-rotated-experiment-trained-CNNs", is.style.transfer = TRUE, append.human.data = T)

# add ResNet-50 data for comparison
origdat = rbind(origdat, origdat.t[origdat.t$subj=="resnet50", ])
greydat = rbind(greydat, greydat.t[greydat.t$subj=="resnet50", ])
texturedat = rbind(texturedat, texturedat.t[texturedat.t$subj=="resnet50", ])
sildat = rbind(sildat, sildat.t[sildat.t$subj=="resnet50", ])
edgedat = rbind(edgedat, edgedat.t[edgedat.t$subj=="resnet50", ])

###################################################################
#               BARPLOTS
###################################################################

img.dir = "../stimuli/visualisation-stimuli/"
img1.path = paste(img.dir, "cat7_original.png", sep="")
img2.path = paste(img.dir, "cat7_greyscale.png", sep="")
img3.path = paste(img.dir, "cat7_silhouette_filled.png", sep="")
img4.path = paste(img.dir, "cat7_edge.png", sep="")
img5.path = paste(img.dir, "elephant1.png", sep="")
img6.path = paste(img.dir, "cat7-elephant1_nomask.png", sep="")
img.names = c(img1.path, img2.path, img3.path, img4.path, img5.path, img6.path, img6.path)

plot.width = 8.0
plot.height = 8.0

# before training
for(angle in c(0, 90)) {
  pdf(file=paste(FIGUREPATH, "results/barplots/barplots_all_", as.character(angle), ".pdf", sep=""), 
      width=plot.width, 
      height=plot.height)
  par(mfrow=c(1,1), mar = rep(0, 4))
  dat.here = list(origdat, greydat, sildat, edgedat, texturedat)
  plot.all.barplots(img.list=img.names[1:5],
                    barplot.values = get.accuracies.for.experiments.helper(dat.here),
                    angle = angle,
                    exp.names=c("original", "greyscale", "silhouette", "edges", "texture"))
  
  par(mar=c(5.1, 4.1, 4.1, 2.1))
  dev.off()
}

## after training
for(angle in c(0, 90)) {
  pdf(file=paste(FIGUREPATH, "results/barplots/trained_barplots_all_", as.character(angle), ".pdf", sep=""), 
      width=plot.width, 
      height=plot.height)
  par(mfrow=c(1,1), mar = rep(0, 4))
  dat.here = list(origdat.t, greydat.t, sildat.t, edgedat.t, texturedat.t)
  plot.all.barplots(img.list=img.names[1:5],
                    barplot.values = get.accuracies.for.experiments.helper(dat.here),
                    angle = angle,
                    exp.names=c("original", "greyscale", "silhouette", "edges", "texture"))
  par(mar=c(5.1, 4.1, 4.1, 2.1))
  dev.off()
}

###################################################################
#   CONFLICT PLOTS
###################################################################
NETWORK.DATA.t = list(resnet.IN, resnet.SIN)

# add ResNet-50 to data; name .c for "combined"
transfer512nomaskdat.c = rbind(transfer512nomaskdat, transfer512nomaskdat.t[transfer512nomaskdat.t$subj=="resnet50", ])
tex.filled.rotated.c = rbind(tex.filled.rotated, tex.filled.rotated.t[tex.filled.rotated.t$subj=="resnet50", ])
transfer.content.guess.c = rbind(transfer.content.guess, transfer.content.guess.t[transfer.content.guess.t$subj=="resnet50", ])
transfer.texture.guess.c = rbind(transfer.texture.guess, transfer.texture.guess.t[transfer.texture.guess.t$subj=="resnet50", ])


# style-transfer-512-nomask-experiment
## before training
fig.path = paste(FIGUREPATH, "results/style-transfer-512-nomask-experiment/", sep="/")
print.conflict.results(transfer512nomaskdat.c, fig.path, x.is.content=TRUE, categories=get.category.order(transfer512nomaskdat, x.is.content = TRUE))
print.conflict.results(transfer512nomaskdat.c, fig.path, x.is.content=FALSE, categories=get.category.order(transfer512nomaskdat, x.is.content = FALSE))
## after training
fig.path = paste(FIGUREPATH, "/results/style-transfer-512-nomask-experiment/trained_", sep="")
print.conflict.results(transfer512nomaskdat.t, fig.path, x.is.content=TRUE, categories=get.category.order(transfer512nomaskdat.t, x.is.content = TRUE))
print.conflict.results(transfer512nomaskdat.t, fig.path, x.is.content=FALSE, categories=get.category.order(transfer512nomaskdat.t, x.is.content = FALSE))

# texture-filled-rotated-experiment
## before training
fig.path = paste(FIGUREPATH, "/results/texture-filled-rotated-experiment/", sep="")
print.conflict.results(tex.filled.rotated.c, fig.path, x.is.content=TRUE, categories=get.category.order(tex.filled.rotated, x.is.content = TRUE))
print.conflict.results(tex.filled.rotated.c, fig.path, x.is.content=FALSE, categories=get.category.order(tex.filled.rotated, x.is.content = FALSE))
## after training
fig.path = paste(FIGUREPATH, "/results/texture-filled-rotated-experiment/trained_", sep="")
print.conflict.results(tex.filled.rotated.t, fig.path, x.is.content=TRUE, categories=get.category.order(tex.filled.rotated.t, x.is.content = TRUE))
print.conflict.results(tex.filled.rotated.t, fig.path, x.is.content=FALSE, categories=get.category.order(tex.filled.rotated.t, x.is.content = FALSE))

# style-transfer-512-nomask-content-guessing-experiment
## before training
fig.path = paste(FIGUREPATH, "results/style-transfer-512-nomask-content-guessing-experiment/", sep="/")
print.conflict.results(transfer.content.guess.c, fig.path, x.is.content=TRUE, categories=get.category.order(transfer.content.guess, x.is.content = TRUE))
print.conflict.results(transfer.content.guess.c, fig.path, x.is.content=FALSE, categories=get.category.order(transfer.content.guess, x.is.content = FALSE))

# style-transfer-512-nomask-texture-guessing-experiment
## before training
fig.path = paste(FIGUREPATH, "results/style-transfer-512-nomask-texture-guessing-experiment/", sep="/")
print.conflict.results(transfer.texture.guess.c, fig.path, x.is.content=TRUE, categories=get.category.order(transfer.texture.guess, x.is.content = TRUE))
print.conflict.results(transfer.texture.guess.c, fig.path, x.is.content=FALSE, categories=get.category.order(transfer.texture.guess, x.is.content = FALSE))

# Open Images
NETWORK.DATA.t = list(resnet.oidv2, resnet.IN.101)
fig.path = paste(FIGUREPATH, "results/style-transfer-512-nomask-experiment/resnet101_oidv2_", sep="/")
print.conflict.results(transfer512nomaskdat.t, fig.path, x.is.content=TRUE, categories=get.category.order(transfer512nomaskdat.t, x.is.content = TRUE))

# Other networks (ResNet-152, SqueezeNet1_1, DenseNet-121)
NETWORK.DATA.t = list(resnet.IN.152, squeezenet.IN, densenet.IN.121)
fig.path = paste(FIGUREPATH, "results/style-transfer-512-nomask-experiment/other_networks_", sep="/")
print.conflict.results(transfer512nomaskdat.t, fig.path, x.is.content=TRUE, categories=get.category.order(transfer512nomaskdat.t, x.is.content = TRUE))

# AlexNet before and after training
NETWORK.DATA.t = list(alexnet, alexnet.SIN)
fig.path = paste(FIGUREPATH, "results/style-transfer-512-nomask-experiment/alexnet_trained_", sep="/")
print.conflict.results(transfer512nomaskdat.t, fig.path, x.is.content=TRUE, categories=get.category.order(transfer512nomaskdat.t, x.is.content = TRUE))

# VGG16 before and after training
NETWORK.DATA.t = list(vgg.IN, vgg.SIN)
fig.path = paste(FIGUREPATH, "results/style-transfer-512-nomask-experiment/vgg16_trained_", sep="/")
print.conflict.results(transfer512nomaskdat.t, fig.path, x.is.content=TRUE, categories=get.category.order(transfer512nomaskdat.t, x.is.content = TRUE))

# DenseNet121 before and after training
NETWORK.DATA.t = list(densenet.IN.121, densenet.SIN.121)
fig.path = paste(FIGUREPATH, "results/style-transfer-512-nomask-experiment/densenet121_trained_", sep="/")
print.conflict.results(transfer512nomaskdat.t, fig.path, x.is.content=TRUE, categories=get.category.order(transfer512nomaskdat.t, x.is.content = TRUE))

# SqueezeNet1_1 before and after training
NETWORK.DATA.t = list(squeezenet.IN, squeezenet.SIN)
fig.path = paste(FIGUREPATH, "results/style-transfer-512-nomask-experiment/squeezenet1_1_trained_", sep="/")
print.conflict.results(transfer512nomaskdat.t, fig.path, x.is.content=TRUE, categories=get.category.order(transfer512nomaskdat.t, x.is.content = TRUE))

# VGG16 and AlexNet after training
NETWORK.DATA.t = list(alexnet.SIN, vgg.SIN)
fig.path = paste(FIGUREPATH, "results/style-transfer-512-nomask-experiment/alexnet_vgg_trained_", sep="/")
print.conflict.results(transfer512nomaskdat.t, fig.path, x.is.content=TRUE, categories=get.category.order(transfer512nomaskdat.t, x.is.content = TRUE))

###################################################################
#       Generalisation analysis (distortions)
###################################################################

GENERALISATION.DATAPATH = "../raw-data/raw-data-from-generalisation-paper/"

noisedat.t = get.expt.data("noise-experiment-trained-CNNs", append.human.data = TRUE, generalisation.project = TRUE)

contrastpngdat.t = get.expt.data("contrast-png-experiment-trained-CNNs", append.human.data = TRUE, generalisation.project = TRUE)
contrastpngdat.t$condition = as.character(contrastpngdat.t$condition)
contrastpngdat.t$condition = lapply(contrastpngdat.t$condition, function(y){substring(y, 2)})
contrastpngdat.t$condition = as.character(as.numeric(contrastpngdat.t$condition)) # '05' -> '5' etc

phasescrambling.t = get.expt.data("phase-scrambling-experiment-trained-CNNs", append.human.data = TRUE, generalisation.project = TRUE)
highpassdat.t = get.expt.data("highpass-experiment-trained-CNNs", append.human.data = TRUE, generalisation.project = TRUE)
lowpassdat.t = get.expt.data("lowpass-experiment-trained-CNNs", append.human.data = TRUE, generalisation.project = TRUE)
rotationdat.t = get.expt.data("rotation-experiment-trained-CNNs", append.human.data = TRUE, generalisation.project = TRUE)

eidolondat.t = get.expt.data("eidolon-experiment-trained-CNNs", append.human.data = TRUE, generalisation.project = TRUE)
e0dat.t = get.eidolon.dat.preprocessed(eidolondat.t, 0)
e3dat.t = get.eidolon.dat.preprocessed(eidolondat.t, 3)
e10dat.t = get.eidolon.dat.preprocessed(eidolondat.t, 10)

plot.observer.range = T
width=6.0
height=5.0

# custom par settings for all of these plots
custom.mar = c(5.1, 5.1, 4.1, 2.1)

# condition 'Inf' is excluded here
pdf(file=paste(FIGUREPATH, "results/generalisation/highpass_accuracy.pdf", sep=""), 
    width=width, 
    height=height)
par(mfrow=c(1,1), mar=custom.mar)
plot.performance(highpassdat.t[highpassdat.t$condition!="Inf", ],
                 plot.observer.range = plot.observer.range,
                 plot.legend = F,
                 logarithmic.scale = T,
                 log.base = 2,
                 main="",
                 xlab=expression("Log"[2]*" of filter standard deviation"),
                 ylab="")
dev.off()

# condition '0' is excluded here
pdf(file=paste(FIGUREPATH, "results/generalisation/lowpass_accuracy.pdf", sep=""), 
    width=width, 
    height=height)
par(mfrow=c(1,1), mar=custom.mar)
plot.performance(lowpassdat.t[lowpassdat.t$condition!="0", ],
                 plot.legend = F,
                 logarithmic.scale = T,
                 log.base=2,
                 plot.observer.range = plot.observer.range,
                 main="",
                 xlab=expression("Log"[2]*" of filter standard deviation"),
                 ylab="")
dev.off()

## uniform noise
pdf(file=paste(FIGUREPATH, "results/generalisation/uniform_noise_accuracy.pdf", sep=""), 
    width=width, 
    height=height)
par(mfrow=c(1,1), mar=custom.mar)
plot.performance(noisedat.t,
                 legend.position = "topright",
                 cex.legend=1.7,
                 plot.observer.range = plot.observer.range,
                 main="",
                 xlab="Uniform noise width",
                 x.range=c(0,0.9),
                 ticks=c(0.0, 0.2, 0.4, 0.6, 0.9))
dev.off()

## contrast
pdf(file=paste(FIGUREPATH, "results/generalisation/contrast_accuracy.pdf", sep=""), 
    width=width, 
    height=height)
par(mfrow=c(1,1), mar=custom.mar)
plot.performance(contrastpngdat.t,
                 plot.observer.range = plot.observer.range,
                 logarithmic.scale = T,
                 plot.legend = F,
                 log.base = 10,
                 main="",
                 xlab=expression("Log"[10]*" of contrast in percent"),
                 ylab="")
dev.off()

## eidolon I (coh = 1.0)
pdf(file=paste(FIGUREPATH, "results/generalisation/eidolon_coh_10_accuracy.pdf", sep=""), 
    width=width, 
    height=height)
par(mfrow=c(1,1), mar=custom.mar)
plot.performance(e10dat.t, plot.legend = F, plot.observer.range = plot.observer.range,
                 logarithmic.scale = T,
                 log.base = 2, main="", xlab=expression("Log"[2]*" of 'reach' parameter"))
dev.off()

## eidolon II (coh = 0.3)
pdf(file=paste(FIGUREPATH, "results/generalisation/eidolon_coh_3_accuracy.pdf", sep=""), 
    width=width, 
    height=height)
par(mfrow=c(1,1), mar=custom.mar)
plot.performance(e3dat.t, plot.legend = F, plot.observer.range = plot.observer.range,
                 logarithmic.scale = T,
                 log.base = 2, main="", xlab=expression("Log"[2]*" of 'reach' parameter"),
                 ylab="")
dev.off()

## eidolon III (coh = 0.0)
pdf(file=paste(FIGUREPATH, "results/generalisation/eidolon_coh_0_accuracy.pdf", sep=""), 
    width=width, 
    height=height)
par(mfrow=c(1,1), mar=custom.mar)
plot.performance(e0dat.t, plot.legend = F, plot.observer.range = plot.observer.range,
                 logarithmic.scale = T,
                 log.base = 2, main="", xlab=expression("Log"[2]*" of 'reach' parameter"),
                 ylab="")
dev.off()


# phase noise
pdf(file=paste(FIGUREPATH, "results/generalisation/phase_noise_accuracy.pdf", sep=""), 
    width=width, 
    height=height)
par(mfrow=c(1,1), mar=custom.mar)
plot.performance(phasescrambling.t,
                 plot.observer.range = plot.observer.range,
                 plot.legend = F,
                 main="",
                 x.range=c(0, 180),
                 xlab="Phase noise width [°]",
                 ticks=c(0, 30, 60, 90, 120, 150, 180),
                 ylab="")
dev.off()

###################################################################
#   PAPER NUMBERS
###################################################################

# these are the numbers for shape / texture bias as reported in paper.

# fractions of texture/shape responses per subject
d = get.fractions.of.responses(transfer512nomaskdat.c)
# note that this is the average over categories, not trials
# (should be more or less equal)
for(s in unique(d$subj)) {
  frac.content = mean(d[d$subj==s, ]$frac.content.decisions)
  frac.texture = mean(d[d$subj==s, ]$frac.texture.decisions)
  print(paste("proportion of content responses for subj ", s, 
              " (within responses that have correct shape XOR correct texture): ",
              as.character(round(frac.content / (frac.content+frac.texture), digits = 3)), sep=""))
}

# Response time analysis
print.response.statistics = function(dat) {
  print(unique(dat$experiment.name))
  m = median(dat[dat$is.human==TRUE & dat$object_response!="na", ]$rt)
  print(round(1000*m))
}
print.response.statistics(origdat)
print.response.statistics(greydat)
print.response.statistics(sildat)
print.response.statistics(edgedat)
print.response.statistics(texturedat)
print.response.statistics(transfer512nomaskdat)
print.response.statistics(transfer.content.guess)
print.response.statistics(transfer.texture.guess)
print.response.statistics(tex.filled.rotated)

# overall median RT
d = rbind(origdat, greydat, sildat, edgedat, texturedat)
for(e in list(transfer512nomaskdat, transfer.content.guess,
              transfer.texture.guess, tex.filled.rotated)) {
  f = e
  f$texture = NULL
  d = rbind(d, f)
}
if(! length(unique(d$experiment.name) == 9)){
  stop("not all experiments included here! Check again...")
}
print.response.statistics(d)

# correlation between shape bias & edge accuracy
d = get.fractions.of.responses(transfer512nomaskdat.c)
d$shape.bias = d$frac.content.decisions / (d$frac.content.decisions + d$frac.texture.decisions)
e = get.accuracy.per.category(edgedat)
for(subj in unique(e$subj)) {
  d1 = d[d$subj==subj, ]$shape.bias
  e1 = e[e$subj==subj, ]$accuracy
  c = cor(d1, e1, method="spearman")
  print(paste(subj, ": ", as.character(round(c, 3)), sep=""))
}

###################################################################
#  comparison with NIPS 2018 results (only private, not for paper)
###################################################################
DATAPATH = "../../../vision-model/object-recognition-combined/raw-data/fine-tuning/"

cex.hack = 2.5
lwd.hack = 2.5

# condition 'Inf' is excluded here
d = get.expt.data("highpass-experiment")
pdf(file=paste(FIGUREPATH, "results/baseline/highpass_accuracy.pdf", sep=""), 
    width=width, 
    height=height)
par(mfrow=c(1,1), mar=custom.mar)
plot.performance(highpassdat.t[highpassdat.t$condition!="Inf", ],
                 plot.observer.range = plot.observer.range,
                 plot.legend = F,
                 logarithmic.scale = T,
                 log.base = 2,
                 main="",
                 xlab=expression("Log"[2]*" of filter standard deviation"),
                 ylab="")
a = get.accuracy(d[d$subj=="sixteen11--high-pass-multiple", ])
b = get.accuracy(d[d$subj=="sixteen34--high-pass-multiple--uniform-noise-multiple--200epochs", ])
a = a[1:7, ] # exclude 'Inf'
b = b[1:7, ] # exclude 'Inf'
lines(log(a$x, base=2), a$y/100.0, col="blue", type = "b", cex=cex.hack, lwd=lwd.hack)
lines(log(b$x, base=2), b$y/100.0, col="green", type = "b", cex=cex.hack, lwd=lwd.hack)
dev.off()

# condition '0' is excluded here
d = get.expt.data("lowpass-experiment")
pdf(file=paste(FIGUREPATH, "results/baseline/lowpass_accuracy.pdf", sep=""), 
    width=width, 
    height=height)
par(mfrow=c(1,1), mar=custom.mar)
plot.performance(lowpassdat.t[lowpassdat.t$condition!="0", ],
                 plot.legend = F,
                 logarithmic.scale = T,
                 log.base=2,
                 plot.observer.range = plot.observer.range,
                 main="",
                 xlab=expression("Log"[2]*" of filter standard deviation"),
                 ylab="")
a = get.accuracy(d[d$subj=="sixteen11--high-pass-multiple", ])
b = get.accuracy(d[d$subj=="sixteen34--high-pass-multiple--uniform-noise-multiple--200epochs", ])
a = a[2:8, ] # exclude '0'
b = b[2:8, ] # exclude '0'
lines(log(a$x, base=2), a$y/100.0, col="blue", type = "b", cex=cex.hack, lwd=lwd.hack)
lines(log(b$x, base=2), b$y/100.0, col="green", type = "b", cex=cex.hack, lwd=lwd.hack)
dev.off()

# PLOTTED EXPERIMENTS, NO CHANGES NECESSARY:
## uniform noise
d = get.expt.data("noise-png-experiment")
pdf(file=paste(FIGUREPATH, "results/baseline/uniform_noise_accuracy.pdf", sep=""), 
    width=width, 
    height=height)
par(mfrow=c(1,1), mar=custom.mar)
plot.performance(noisedat.t,
                 cex.legend=1.7,
                 plot.legend = FALSE,
                 plot.observer.range = plot.observer.range,
                 main="",
                 xlab="Uniform noise width",
                 x.range=c(0,0.9),
                 ticks=c(0.0, 0.2, 0.4, 0.6, 0.9))
a = get.accuracy(d[d$subj=="sixteen11--high-pass-multiple", ])
b = get.accuracy(d[d$subj=="sixteen34--high-pass-multiple--uniform-noise-multiple--200epochs", ])
lines(a$x, a$y/100.0, col="blue", type = "b", cex=cex.hack, lwd=lwd.hack)
lines(b$x, b$y/100.0, col="green", type = "b", cex=cex.hack, lwd=lwd.hack)
dev.off()

## contrast
d = get.expt.data("contrast-png-experiment")
pdf(file=paste(FIGUREPATH, "results/baseline/contrast_accuracy.pdf", sep=""), 
    width=width, 
    height=height)
par(mfrow=c(1,1), mar=custom.mar)
plot.performance(contrastpngdat.t,
                 plot.observer.range = plot.observer.range,
                 logarithmic.scale = T,
                 plot.legend = F,
                 log.base = 10,
                 main="",
                 xlab=expression("Log"[10]*" of contrast in percent"),
                 ylab="")
a = get.accuracy(d[d$subj=="sixteen11--high-pass-multiple", ])
a$x = as.character(a$x)
a$x = lapply(a$x, function(y){substring(y, 2)})
a$x = as.numeric(a$x)
a = a[order(a$x),]
b = get.accuracy(d[d$subj=="sixteen34--high-pass-multiple--uniform-noise-multiple--200epochs", ])
b$x = as.character(b$x)
b$x = lapply(b$x, function(y){substring(y, 2)})
b$x = as.numeric(b$x)
b = b[order(b$x),]
lines(log(a$x, base=10), a$y/100.0, col="blue", type = "b", cex=cex.hack, lwd=lwd.hack)
lines(log(b$x, base=10), b$y/100.0, col="green", type = "b", cex=cex.hack, lwd=lwd.hack)
dev.off()

## eidolon I (coh = 1.0)
de = get.expt.data("eidolon-experiment")
d = get.eidolon.dat.preprocessed(de, 10)
pdf(file=paste(FIGUREPATH, "results/baseline/eidolon_coh_10_accuracy.pdf", sep=""), 
    width=width, 
    height=height)
par(mfrow=c(1,1), mar=custom.mar)
plot.performance(e10dat.t, plot.legend = F, plot.observer.range = plot.observer.range,
                 logarithmic.scale = T,
                 log.base = 2, main="", xlab=expression("Log"[2]*" of 'reach' parameter"))
a = get.accuracy(d[d$subj=="sixteen11--high-pass-multiple", ])
b = get.accuracy(d[d$subj=="sixteen34--high-pass-multiple--uniform-noise-multiple--200epochs", ])
lines(log(a$x, base=2), a$y/100.0, col="blue", type = "b", cex=cex.hack, lwd=lwd.hack)
lines(log(b$x, base=2), b$y/100.0, col="green", type = "b", cex=cex.hack, lwd=lwd.hack)
dev.off()

## eidolon II (coh = 0.3)
d = get.eidolon.dat.preprocessed(de, 3)
pdf(file=paste(FIGUREPATH, "results/baseline/eidolon_coh_3_accuracy.pdf", sep=""), 
    width=width, 
    height=height)
par(mfrow=c(1,1), mar=custom.mar)
plot.performance(e3dat.t, plot.legend = F, plot.observer.range = plot.observer.range,
                 logarithmic.scale = T,
                 log.base = 2, main="", xlab=expression("Log"[2]*" of 'reach' parameter"),
                 ylab="")
a = get.accuracy(d[d$subj=="sixteen11--high-pass-multiple", ])
b = get.accuracy(d[d$subj=="sixteen34--high-pass-multiple--uniform-noise-multiple--200epochs", ])
lines(log(a$x, base=2), a$y/100.0, col="blue", type = "b", cex=cex.hack, lwd=lwd.hack)
lines(log(b$x, base=2), b$y/100.0, col="green", type = "b", cex=cex.hack, lwd=lwd.hack)
dev.off()

## eidolon III (coh = 0.0)
d = get.eidolon.dat.preprocessed(de, 0)
pdf(file=paste(FIGUREPATH, "results/baseline/eidolon_coh_0_accuracy.pdf", sep=""), 
    width=width, 
    height=height)
par(mfrow=c(1,1), mar=custom.mar)
plot.performance(e0dat.t, plot.legend = F, plot.observer.range = plot.observer.range,
                 logarithmic.scale = T,
                 log.base = 2, main="", xlab=expression("Log"[2]*" of 'reach' parameter"),
                 ylab="")
a = get.accuracy(d[d$subj=="sixteen11--high-pass-multiple", ])
b = get.accuracy(d[d$subj=="sixteen34--high-pass-multiple--uniform-noise-multiple--200epochs", ])
lines(log(a$x, base=2), a$y/100.0, col="blue", type = "b", cex=cex.hack, lwd=lwd.hack)
lines(log(b$x, base=2), b$y/100.0, col="green", type = "b", cex=cex.hack, lwd=lwd.hack)
dev.off()


# phase noise
d = get.expt.data("phase-scrambling-experiment")
pdf(file=paste(FIGUREPATH, "results/baseline/phase_noise_accuracy.pdf", sep=""), 
    width=width, 
    height=height)
par(mfrow=c(1,1), mar=custom.mar)
plot.performance(phasescrambling.t,
                 plot.observer.range = plot.observer.range,
                 plot.legend = F,
                 main="",
                 x.range=c(0, 180),
                 xlab="Phase noise width [°]",
                 ticks=c(0, 30, 60, 90, 120, 150, 180),
                 ylab="")
a = get.accuracy(d[d$subj=="sixteen11--high-pass-multiple", ])
b = get.accuracy(d[d$subj=="sixteen34--high-pass-multiple--uniform-noise-multiple--200epochs", ])
lines(a$x, a$y/100.0, col="blue", type = "b", cex=cex.hack, lwd=lwd.hack)
lines(b$x, b$y/100.0, col="green", type = "b", cex=cex.hack, lwd=lwd.hack)
dev.off()

###################################################################
#    CROP PDFs
###################################################################
crop.pdfs("./../paper-figures/results/")

