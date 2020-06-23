###################################################################
#   Data analysis HELPER script for texture-shape experiments
#   All important functions for data analysis should be collected
#   here (to be used for plotting, presentations, and in the
#   analysis.R script)
#   -------------------------------------------------------
#   Author:   Robert Geirhos
###################################################################

library(ggplot2)
library(lattice)
library(jpeg)
library(R.matlab)
library(graphics)
library(pROC)
library(psych)
library(grid)
library(gridExtra)
library(stats)
library(png)
library(scales)
library(pBrackets)
library(PET)
library(TeachingDemos)

###################################################################
#               some general settings
###################################################################

NETWORKS = sort(c("alexnet", "googlenet", "vgg", "resnet50"))
CATEGORIES = sort(c("airplane", "bear", "bicycle", "bird", "boat", "bottle", "car",
                    "cat", "chair", "clock", "dog", "elephant", "keyboard", "knife",
                    "oven", "truck"))   
ANIMAL.CATEGORIES = sort(c("bear", "bird", "cat", "dog", "elephant"))   
NON.ANIMAL.CATEGORIES = sort(c("airplane", "bicycle", "boat", "bottle", "car",
                               "chair", "clock", "keyboard", "knife",
                               "oven", "truck"))   

NUM.OVERALL.PARTICIPANTS = 42 # arbitrary but large enough

# define colour scheme for plots. Numbers indicate saturation.
alexnet.100 = rgb(125, 165, 75, maxColorValue =  255)
alexnet.80 = rgb(151, 183, 111, maxColorValue =  255)
alexnet.60 = rgb(177, 201,  147, maxColorValue =  255)

googlenet.100 = rgb(130, 185, 160, maxColorValue =  255)
googlenet.80 = rgb(160, 199, 179, maxColorValue =  255)
googlenet.60 = rgb(186, 213, 198, maxColorValue =  255)

vgg.100 = rgb(50, 110, 30, maxColorValue =  255)
vgg.80 = rgb(97, 132, 71, maxColorValue = 255)
vgg.70 = rgb(117, 144, 89, maxColorValue =  255)
vgg.60 = rgb(144, 159, 110, maxColorValue = 255)
vgg.40 = rgb(177, 188, 156, maxColorValue =  255)

human.100 = rgb(165, 30, 55, maxColorValue = 255)
human.80 = rgb(180, 77, 80, maxColorValue = 255)
human.70 = rgb(188, 98, 97, maxColorValue = 255)
human.60 = rgb(197, 121, 116, maxColorValue = 255)
human.40 = rgb(216, 166, 159, maxColorValue = 255)
human.20 = rgb(235, 210, 205, maxColorValue = 255)
densenet.100 = rgb(175, 110, 150, maxColorValue = 255)
resnet.oidv2.100 = rgb(130, 185, 160, maxColorValue = 255)
squeezenet.100 = rgb(145, 105, 70, maxColorValue = 255)

### subjects containing ResNet (for SIN experiments etc)
resnet.IN.100 = rgb(68, 78, 87, maxColorValue = 255)
resnet.SIN.100 = rgb(210, 150, 0, maxColorValue = 255)
resnet.IN = list(name="ResNet-50 (IN)",
                 color=resnet.IN.100,
                 pch=22,
                 data.name="resnet50")
resnet.SIN = list(name="ResNet-50 (SIN)",
                  color=resnet.SIN.100,
                  pch=22,
                  data.name="resnet50-train-60-epochs")
resnet.oidv2 = list(name="ResNet-101 (oidv2)",
                    color=resnet.oidv2.100,
                    pch=22,
                    data.name="oidv2-resnet101")
resnet.IN.101 = list(name="ResNet-101",
                     color=resnet.IN.100,
                     pch=22,
                     data.name="resnet101")
resnet.IN.152 = list(name="ResNet-152",
                     color=resnet.IN.100,
                     pch=22,
                     data.name="resnet152")
squeezenet.IN = list(name="Squeezenet1_1 (IN)",
                     color=squeezenet.100,
                     pch=23,
                     data.name="squeezenet1-1")
squeezenet.SIN = list(name="Squeezenet1_1 (SIN)",
                      color="orange",
                      pch=23,
                      data.name="squeezenet1-1-train-60-epochs-lr0.001")
densenet.IN.121 = list(name="DenseNet-121",
                       color=densenet.100,
                       pch=24,
                       data.name="densenet121")
densenet.SIN.121 = list(name="DenseNet-121",
                        color="orange",
                        pch=24,
                        data.name="densenet121-train-60-epochs")
alexnet.SIN = list(name="AlexNet (SIN)",
                   color=alexnet.100,
                   pch=23,
                   data.name="alexnet-train-60-epochs-lr0.001")
vgg.IN = list(name="VGG-16 (IN)",
              color=vgg.100,
              pch=24,
              data.name="vgg16")
vgg.SIN = list(name="VGG-16 (SIN)",
               color=vgg.100,
               pch=24,
               data.name="vgg16-train-60-epochs-lr0.01")

use.blue.color.scheme = TRUE
if(use.blue.color.scheme) {
  vgg.100 = rgb(0, 105, 170, maxColorValue = 255)
  alexnet.100 = rgb(65, 90, 140, maxColorValue = 255)
  googlenet.100 = rgb(80, 170, 200, maxColorValue = 255)
}

human.cols = c("1" = human.60, "2" = human.80, "3" = human.100)
alexnet.cols   = c("-1" = alexnet.60, "-2" = alexnet.80, "-3" = alexnet.100)
googlenet.cols   = c("-1" = googlenet.60, "-2" = googlenet.80, "-3" = googlenet.100)
vgg.cols   = c("-1" = vgg.60, "-2" = vgg.80, "-3" = vgg.100)

get.equally.spaced.colors = function(r, g, b, n=7) {
  # return n equally spaced colors, with the middle one
  # being grey (127, 127, 127)
  
  cols = list()
  
  rs = seq(from=r, to=255-r, length.out=n)
  gs = seq(from=g, to=255-g, length.out=n)
  bs = seq(from=b, to=255-b, length.out=n)
  counter = 1
  for(i in 1:n) {
    cols[counter] = rgb(rs[counter], gs[counter], bs[counter], maxColorValue = 255)
    counter = counter + 1 
  }
  
  return(cols)
}

confdiff.cols.all = get.equally.spaced.colors(0, 0, 125)
confdiff.human.cols   = c("1" = confdiff.cols.all[3], "2" = confdiff.cols.all[2], "3" = confdiff.cols.all[1])
confdiff.net.cols   = c("-1" = confdiff.cols.all[5], "-2" = confdiff.cols.all[6], "-3" = confdiff.cols.all[7])


cols = list()
counter = 1
for(i in c(-3:3)) {
  val = counter * 255 / 7
  cols[counter] = rgb(val, val, val, maxColorValue = 255)
  counter = counter +1
}


HUMAN.COLS = c(human.100, human.80, human.60, human.40, human.20)
DNN.RANGE.LWD = 2
LINES.LWD = 2.5
POINTS.CEX.VAL = 2.5

alexnet = list(name="AlexNet",
               color=alexnet.100,
               pch=23,
               data.name="alexnet")
googlenet = list(name="GoogLeNet",
                 color=googlenet.100,
                 pch=19,
                 data.name="googlenet")
vgg = list(name="VGG-16",
           color=vgg.100,
           pch=24,
           data.name="vgg")
NETWORK.DATA = list()
NETWORK.DATA[[1]] = alexnet
NETWORK.DATA[[2]] = googlenet
NETWORK.DATA[[3]] = vgg
# add ResNet-50 to NETWORK.DATA
NETWORK.DATA[[4]] = resnet.IN
NETWORK.DATA[[4]]$name = "ResNet-50" # prevent from printing ResNet-50 (IN)


PARTICIPANTS = list()
for(i in 1:NUM.OVERALL.PARTICIPANTS) {
  n = paste("subject-", ifelse(i<10, "0", ""), i, sep="")
  PARTICIPANTS[[i]] = list(name=n,
                           color=human.100,
                           pch=1,
                           data.name=n)
}
HUMAN.DATA.NAME = "participants (avg.)"
human.avg = list(name="participants (avg.)",
                 color=human.100,
                 pch=1, # 21Note: this is a filled circle, in contrast to the vision-model-DNN experiments.
                 data.name=HUMAN.DATA.NAME)

SIN.subjects = list()
SIN.subjects[[1]] = resnet.IN
SIN.subjects[[2]] = resnet.SIN
SIN.subjects[[3]] = human.avg


NETWORKS.t = sort(c("resnet50", "resnet50-train-60-epochs"))
NETWORK.DATA.t = list(resnet.IN, resnet.SIN)
ALL.NETWORKS = c(NETWORKS, "resnet50-train-60-epochs")

image.categories = list()
for(cat in CATEGORIES) {
  image.categories[[cat]] = readPNG(paste("category-images/",
                                          cat, ".png", sep=""))
}


get.matching.name = function(name) {
  if(length(name) > 1) {
    for(net in NETWORK.DATA) {
      if(net$data.name==name) {
        stop("not defined")
      }
    }
    return("human")
  } else {
    for(net in NETWORK.DATA) {
      if(net$data.name==name) {
        return(net$name)
      }
    }
  }
  return(name)
}

get.all.subjects = function(dat, avg.human.data) {
  # Return all subjects, including networks.
  networks = NULL
  network.data = NULL
  subjects = NULL
  for(s in unique(dat$subj)) {
    if(s %in% NETWORKS) {
      subjects = NETWORK.DATA
      networks = NETWORKS
      network.data = NETWORK.DATA
    } else if (s %in% NETWORKS.t) {
      subjects = NETWORK.DATA.t
      networks = NETWORKS.t
      network.data = NETWORK.DATA.t
    }
  }
  
  i = length(network.data) + 1
  
  if(avg.human.data & any(! unique(dat$subj) %in% networks)) {
    subjects[[i]] = human.avg
  } else {
    counter = 1
    for(p in PARTICIPANTS) {
      if(p$data.name %in% unique(dat$subj)) {
        subjects[[i]] = p
        subjects[[i]]$color = HUMAN.COLS[i - length(network.data)]
        i = i+1
        counter = counter+1
      }
    }
  }
  return(subjects) 
}


###################################################################
#               correlation analysis functions
###################################################################

get.correlations = function(human.dat, network.dat,
                            verbose=FALSE, round.digits=3) {
  # Return data frame with average correlations between human
  # and network data.
  # If verbose=TRUE, print every correlation.
  
  results = data.frame(network = NETWORKS, avg.corr = numeric(length(NETWORKS)),
                       experiment.name = unique(human.dat$experiment.name))
  for(net in NETWORKS) {
    if(verbose) {
      print("")
      print(net)
    }
    corr.list = numeric()
    has.found.data = FALSE
    for(sess in unique(network.dat$session)) {
      subj.name = paste("subject-0", as.character(sess), sep="")
      subj.dat = human.dat[human.dat$subj==subj.name, ]
      net.dat  = network.dat[network.dat$subj==net & network.dat$session==sess, ]
      
      if(nrow(subj.dat) == nrow(net.dat)) {
        if(any(!(subj.dat$imagename == net.dat$imagename))) {
          stop("mismatch in get.correlations(): problem with imagenames")
        }
        
        has.found.data = TRUE
        correlation = cor(subj.dat$is.correct, net.dat$is.correct)
        corr.list = append(corr.list, correlation)
        if(verbose) {
          print(paste("Corr of", subj.name, "and", net, ":",
                      as.character(correlation)))
        }
      } else {
        print("length mismatch.")
      }
    }
    if(has.found.data) {
      if(verbose) {
        #print(paste("Mean correlation for", net, ":", mean(corr.list)))
        print(summary(corr.list))
      }
      results[results$network==net, ]$avg.corr = round(mean(corr.list), round.digits)
    }
  }
  return(results)
}


###################################################################
#               plotting functions
###################################################################


plot.illustation.confusion = function(dat1, dat2.network, 
                                      divide.alpha.by, main=NULL) {
  confusion.human = get.confusion(dat1)
  confusion.human = confusion.human[as.character(confusion.human$category) == 
                                      as.character(confusion.human$object_response), ]
  confusion.network = get.confusion(dat2.network)
  confusion.network = confusion.network[as.character(confusion.network$category) == 
                                          as.character(confusion.network$object_response), ]
  
  n.categories = length(unique(confusion.human$category))
  plot(1:n.categories, 1:n.categories, ylim=c(0, 1), xlim=c(1, n.categories), type="n",
       ylab="Accuracy", main=main, xaxt="n", xlab="Categories")
  # plot categories as x axis labels
  axis(1, labels = FALSE, at=1:n.categories)
  labels = sort(as.character(unique(colordat$category)))
  text(1:n.categories, -0.1, srt = 45, adj = 1,
       labels = labels, xpd = TRUE)
  
  net = NETWORK.DATA[[3]] #vgg
  
  bar.width = 0.4
  for(x in 1:n.categories) {
    human.accuracy = confusion.human$Percent[x]/100.0
    human.cat.freq = confusion.human[x, ]$CategoryFreq
    net.accuracy = confusion.network$Percent[x]/100.0
    net.cat.freq = confusion.network[x, ]$CategoryFreq
    
    alpha.counter = 1
    for(alpha in ((c(0.05, 0.01, 0.001))/divide.alpha.by)) { # alpha level: critical!
      
      human.lower.bound = 0
      while(! (abs(is.in.CI(confusion.network[x, ]$Freq, net.cat.freq,
                            human.lower.bound, human.cat.freq,
                            conf.level = 1-alpha)) < 1e-5)) {
        human.lower.bound = human.lower.bound + 1
      }
      human.upper.bound = human.cat.freq
      while(! (abs(is.in.CI(confusion.network[x, ]$Freq, net.cat.freq,
                            human.upper.bound, human.cat.freq,
                            conf.level = 1-alpha)) < 1e-5)) {
        human.upper.bound = human.upper.bound - 1
      }
      if(human.lower.bound > 0) {
        rect(x-0.5*bar.width, 0.0,
             x+0.5*bar.width, human.lower.bound / human.cat.freq, col=confdiff.cols.all[[4+alpha.counter]])
      }
      if(human.upper.bound < human.cat.freq) {
        rect(x-0.5*bar.width, human.upper.bound / human.cat.freq,
             x+0.5*bar.width, 1.0, col=confdiff.cols.all[[4-alpha.counter]])
      }
      
      alpha.counter = alpha.counter + 1
    }
    
    # human & network accuracy
    points(x, net.accuracy, pch=19, col=net$color)
    points(x, human.accuracy, pch=19, col=human.100)
  }
}


difference.matrix = function(dat1, dat2.network,
                             plot.accuracies = TRUE,
                             plot.x.y.labels = TRUE,
                             plot.scale = TRUE,
                             main=NULL,
                             binomial=FALSE,
                             divide.alpha.by=16.0*17.0*9.0) {
  # Plot the difference of two confusion matrices
  
  if(length(unique(dat2.network$subj)) != 1) {
    warning("more than one network found in dat2.network:")
    print(unique(dat2.network$subj))
    network.name = unique(dat2.network$subj)
  } else {
    network.name = "GROUP"
  }
  
  
  if(is.null(main)) {
    main=paste("Confusion matrix ", unique(dat1$experiment.name),
               ": \u0394 (", get.matching.name(unique(dat1$subject)),
               ", ",
               get.matching.name(network.name), ")",
               sep="")
  }
  
  confusion1 = get.confusion(dat1)
  confusion2 = get.confusion(dat2.network)
  confusion.difference = confusion1
  confusion.difference$Percent = confusion1$Percent - confusion2$Percent
  
  diag.sd = sd(confusion1[as.character(confusion1$category) ==
                            as.character(confusion1$object_response), ]$Percent)
  rest.sd = sd(confusion1[as.character(confusion1$category) !=
                            as.character(confusion1$object_response), ]$Percent)
  if(binomial) {
    
    confusion.difference = get.z.for.binomial(confusion.difference,
                                              confusion1, confusion2,
                                              divide.alpha.by)
  } else {
    confusion.difference = get.z.for.confusion(confusion.difference,
                                               diag.sd, rest.sd)
  }
  
  result = plot.confusion(confusion.difference,
                          experiment.name = unique(dat1$experiment.name),
                          is.difference.plot = TRUE,
                          main=main,
                          plot.accuracies = plot.accuracies,
                          plot.x.y.labels = plot.x.y.labels,
                          plot.scale = plot.scale,
                          network.name = network.name)
  return(result)
}

entropy.helper = function(prob.distribution) {
  
  if(abs(1-sum(prob.distribution)) > 0.05) {
    warning(paste("difference too large for entropy: sum = ",
                  as.character(sum(prob.distribution)), " != 1.0", sep=""))
  }
  res = 0.0
  for(i in prob.distribution) {
    if(abs(i) > 1e-5) { #ignore values too close to zero
      res = res + i*log(i, base=2)
    }
  }
  res = 0-res
  return(res)
}

get.entropy = function(dat) {
  # Return: what is the fraction of responses
  # that fall onto the top n (=most frequent)
  # categories?
  
  return(entropy.helper(get.percent.answers.per.category(dat)))
}

get.conditional.entropy = function(dat) {
  # Compute H(R|X)
  
  cond.entropy.sum = 0.0
  for(x in unique(dat$category)) {
    for(r in unique(dat$object_response)) {
      # p(x, r)
      p.x.and.r = nrow(dat[dat$category==x & dat$object_response==r, ]) / nrow(dat)
      # p(x)
      p.x = nrow(dat[dat$category==x, ]) / nrow(dat)
      if(p.x.and.r > 1e-15 & p.x > 0) {
        cond.entropy.sum = cond.entropy.sum + p.x.and.r * log(p.x/p.x.and.r, base=2)
      }
    }
  }
  return(cond.entropy.sum)
}


get.percent.answers.per.category = function(dat) {
  # Return a list of percentages, one per category.
  # Each value indicates the percentage of object_response
  # for this category. The sum, thus, should be 1 (DNN)
  # or close to 1 (humans, 'na' not included)
  
  result = c()
  num.answers = length(dat$category)
  for(c in unique(dat$category)) {
    result = c(result, nrow(dat[dat$object_response==c, ])/num.answers)
  }
  return(result)
}


get.top.n.frac.of.responses = function(dat, n) {
  # Return: what is the fraction of responses
  # that fall onto the top n (=most frequent)
  # categories?
  
  return(sum(sort(get.percent.answers.per.category(dat), decreasing = T)[1:n]))
}

get.z.for.binomial = function(conf, conf1, conf2,
                              divide.alpha.by) {
  # Assign values within [-3, 3] indicating the 'significance color'
  # for a confusion difference plot (here, these color values are called z)
  #
  # Parameters:
  # - conf            -> confusion difference
  # - conf1           -> human confusion data
  # - conf2           -> network confusion data
  # - divide.alpha.by -> if > 1.0, Bonferroni correction will be applied
  #
  # z values:
  # -3 to -1 -> difference significant for alpha = 0.001, 0.01, 0.05; network more frequently
  # 0        -> no or no significant difference
  # 3 to 1   -> difference significant for alpha = 0.001, 0.01, 0.05; humans more frequently
  # These alpha values (0.001, 0.01, 0.05) are subject to a Bonferroni
  # correction if divide.alpha.by is assigned a value larger than 1.0
  
  conf$z = "0" # default value
  
  conf1$Freq = as.numeric(conf1$Freq)
  conf1$CategoryFreq = as.numeric(conf1$CategoryFreq)
  conf2$Freq = as.numeric(conf2$Freq)
  conf2$CategoryFreq = as.numeric(conf2$CategoryFreq)
  
  for(i in 1:nrow(conf1)) {
    if(conf1[i, ]$category != conf2[i, ]$category) {
      stop("category mismatch")
    }
    tmp = 0
    weight = 3
    for(alpha in sort(c(0.001, 0.01, 0.05), decreasing = F)) {
      val = is.in.CI(conf2[i, ]$Freq, conf2[i, ]$CategoryFreq,
                     conf1[i, ]$Freq, conf1[i, ]$CategoryFreq,
                     conf.level = 1.0-alpha/divide.alpha.by)
      if(abs(weight*val) > abs(tmp)) {
        tmp = weight*val
        break # shortcut: speed up computation and begin with most significant
      }
      weight = weight - 1
    }
    conf[i, ]$z = as.character(tmp)
  }
  return(conf)
}


get.z.for.confusion = function(conf, diag.sd, rest.sd) {
  conf$z = "0"
  conf = get.z.helper(conf, TRUE, diag.sd, rest.sd)
  conf = get.z.helper(conf, FALSE, diag.sd, rest.sd)
  return(conf)
}

get.z.helper = function(conf, is.diag=TRUE, diag.sd, rest.sd) {
  
  diagonal = as.character(conf$category) == as.character(conf$object_response)
  diag = ifelse(is.diag, diagonal, !diagonal)
  sd = ifelse(is.diag, diag.sd, rest.sd)
  sd.1  = diag & conf$Percent/sd > 1
  sd.2  = diag & conf$Percent/(2*sd) > 2
  sd.3  = diag & conf$Percent/(3*sd) > 3
  sd.m1 = diag & conf$Percent/sd < -1
  sd.m2 = diag & conf$Percent/(2*sd) < -2
  sd.m3 = diag & conf$Percent/(3*sd) < -2
  if(any(sd.1)) {
    conf[sd.1, ]$z = "1"
  }
  if(any(sd.2)) {
    conf[sd.2, ]$z = "2"
  }
  if(any(sd.3)) {
    conf[sd.3, ]$z = "3"
  }
  if(any(sd.m1)) {
    conf[sd.m1, ]$z = "-1"
  }
  if(any(sd.m2)) {
    conf[sd.m2, ]$z = "-2"
  }
  if(any(sd.m3)) {
    conf[sd.m3, ]$z = "-3"
  }
  return(conf)
}


plot.confusion = function(confusion, 
                          experiment.name,
                          subject=NULL,
                          is.difference.plot=FALSE,
                          main=NULL,
                          plot.accuracies=TRUE,
                          plot.x.y.labels=TRUE,
                          plot.scale = TRUE,
                          network.name=NULL) {
  # Plot confusion matrix
  
  subject.title = ifelse(is.null(subject), "- all subjects", get.matching.name(subject))
  
  if(is.difference.plot) {
    g = geom_tile(aes(x=category, y=object_response, fill=z),
                  data=confusion, color="black", size=0.1)
  } else {
    g = geom_tile(aes(x=category, y=object_response, fill=Percent),
                  data=confusion, color="black", size=0.1)
  }
  
  tile <- ggplot() + g +
    labs(x="presented category",y="response") + 
    if(is.null(main)) {
      ggtitle(paste("Confusion matrix", experiment.name, subject.title)) 
    } else {
      ggtitle(main)
    }
  
  # print accuracy in box; fill gradient
  if(plot.accuracies) {
    tile = tile + 
      geom_text(aes(x=category, y=object_response, label=sprintf("%.1f", Percent)),
                data=confusion, size=5, colour="black")
  }
  
  tile = tile +
    if((!is.null(confusion$z)) & !is.difference.plot) {
      if(is.null(network.name)) {
        stop("no network name, but confusion$z exists -> which color to use?")
      }
      
      net.cols = NULL
      if(network.name == "vgg") {
        net.cols = vgg.cols
      } else if (network.name == "alexnet") {
        net.cols = alexnet.cols
      } else if (network.name == "googlenet") {
        net.cols = googlenet.cols
      }
      scale_fill_manual(values = c("0" = rgb(230, 230, 230, maxColorValue = 255),
                                   human.cols, net.cols))
    } else if(is.difference.plot) {
      print("plotting difference matrix")
      scale_fill_manual(values = c("0" = rgb(127, 127, 127, maxColorValue = 255),
                                   confdiff.human.cols, confdiff.net.cols), guide=FALSE)
    } else {
      if(plot.scale) {
        scale_fill_gradient(low=rgb(250, 250, 250, maxColorValue = 255),
                            high=human.100)
      } else {
        scale_fill_gradient(low="grey", high=human.100, guide=FALSE)
      }
    }
  
  # diagonal
  tile = tile + 
    geom_tile(aes(x=category, y=object_response),
              data=subset(confusion, as.character(category)==as.character(object_response)),
              color="black",size=0.3, fill="black", alpha=0) 
  if(! plot.x.y.labels) {
    tile = tile +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
  }
  #render
  return(tile)
}

confusion.matrix = function(dat, subject=NULL, main=NULL, plot.scale=TRUE,
                            plot.x.y.labels=TRUE) {
  #Plot confusion matrix either for all or for a specific subject
  
  confusion = get.confusion(dat, subject)
  return(plot.confusion(confusion, unique(dat$experiment.name), subject,
                        main=main, plot.scale=plot.scale,
                        plot.x.y.labels = plot.x.y.labels))
}


get.confusion = function(dat, subject=NULL,
                         net.dat=NULL, human.dat=NULL) {
  # Sure you want to get confused? ;)
  # Return all data necessary to plot confusion matrix.
  
  if(is.null(subject)) {
    d = data.frame(dat$category,
                   dat$object_response)
  } else {
    d = data.frame(dat[dat$subj==subject, ]$category,
                   dat[dat$subj==subject, ]$object_response)
  }
  
  names(d) = c("category", "object_response") 
  
  category = as.data.frame(table(d$category))
  names(category) = c("category","CategoryFreq")
  
  confusion = as.data.frame(table(d$category, d$object_response))
  names(confusion) = c("category", "object_response", "Freq")
  
  confusion = merge(confusion, category, by=c("category"))
  confusion$Percent = confusion$Freq/confusion$CategoryFreq*100
  
  # make sure the order is correct, with 'na' in the end
  for(f in rev(c("airplane", "bear", "bicycle", "bird", "boat", "bottle",
                 "car", "cat", "chair", "clock", "dog", "elephant",
                 "keyboard", "knife", "oven", "truck", "na"))) {
    if(f %in% levels(confusion$object_response)) {
      # TODO this is less than perfect, as it excludes some categories
      confusion$object_response <- relevel(confusion$object_response, f)
    }
  }
  
  return(confusion)
}


plot.performance = function(dat, logarithmic.scale=FALSE, 
                            line.for.chance=TRUE,
                            h.of.line.for.chance = 1/16.0,
                            avg.human.data=TRUE,
                            normalize=FALSE,
                            legend.position=NULL,
                            plot.legend=TRUE,
                            top.n=0,
                            entropy=FALSE,
                            plot.dnn.range=!normalize & (top.n==0) & !isTRUE(entropy),
                            plot.observer.range=avg.human.data & !normalize & (top.n==0) & !isTRUE(entropy),
                            main=NULL,
                            ylab=NULL,
                            log.base=10,
                            inverse.x.axis=unique(dat$experiment.name) %in% c("contrast-experiment", "contrast-png-experiment", "highpass-experiment"),
                            corrdat = NULL,
                            x.range=range(as.numeric(dat$condition)),
                            ticks=NULL,
                            cex.lab=2.0,
                            cex.axis=1.5,
                            cex.legend=1.3,
                            ...) {
  #Plot performance for an experiment, split by subject
  
  # ASSERT INPUT IS CORRECT, ASSIGN DEFAULT VALUES
  if(plot.observer.range & ! avg.human.data) {
    stop("plot.observer.range and avg.human.data cannot be both TRUE -> overcrowded plot.")
  }
  if(isTRUE(entropy) & (top.n!=0)) {
    stop("entropy == TRUE and top.n != 0 cannot both be the case -> use either of them!")
  }
  
  if(logarithmic.scale) {
    x.range = log(x.range, base = log.base)
  }
  if(isTRUE(entropy)) {
    y.range = range(0, h.of.line.for.chance)
  } else {
    y.range = range(0, 1)
  }
  x.lim = x.range
  if(inverse.x.axis) {
    x.lim = rev(x.range)
  }
  
  if(is.null(main)) {
    expt.name = levels(dat$experiment.name)[1]
    main = ifelse(normalize, paste("Normalized accuracy for", expt.name),
                  paste("Accuracy for", expt.name))
  }
  if(is.null(ylab)) {
    if(top.n == 0 & !isTRUE(entropy)) {
      ylab = ifelse(normalize, "Normalized classification accuracy",
                    "Classification accuracy")
    } else if(isTRUE(entropy)) {
      ylab = "Entropy of response distribution [bits]"
    } else {
      ylab = ifelse(normalize, paste("Normalized fraction of top-", top.n, " decisions", sep=""),
                    paste("Fraction of top-", top.n, " decisions", sep=""))
    }
    
  }
  
  # PLOTTING
  plot(x.range, y.range, type="n", bty="n", main=main, ylab=ylab, xlim=x.lim, 
       xaxt="n", cex.lab=cex.lab, cex.axis=cex.axis, ...)
  axis(side=1, at=ticks, cex.axis=cex.axis)
  
  subjects = get.all.subjects(dat, avg.human.data)
  
  for(s in subjects) {
    
    if(s$data.name %in% ALL.NETWORKS | !avg.human.data) {
      acc = get.accuracy(dat[dat$subj==s$data.name, ], top.n, entropy)
    } else {
      if(top.n == 0) {
        acc = get.accuracy(dat[dat$is.human==TRUE, ], top.n, entropy)
      } else {
        # Plot average of single observers' fraction of responses.
        # This is different from the combined human observers' average!
        # If every observer chose 3 categories, but different ones, 
        # this wouldn't show in the combined data, only in the overall
        # average.
        acc = get.accuracy(dat[dat$is.human==TRUE, ], top.n, entropy)
        
        acc$y = rep(0.0, times=length(unique(dat$condition)))
        
        for(z in unique(dat[dat$is.human==TRUE, ]$subj)) {
          acc.for.subj = get.accuracy(dat[dat$subj==z, ], top.n, entropy)
          acc$y = acc$y + acc.for.subj$y
        }
        acc$y = acc$y / length(unique(dat[dat$is.human==TRUE, ]$subj))
      }
    }
    
    x = acc$x
    if(logarithmic.scale) {
      x = log(acc$x, base=log.base)
    }
    y = acc$y / 100
    if(normalize) {
      y = get.normalized.value(y)
    }
    
    # PLOT CORRELATION
    if(s$data.name %in% ALL.NETWORKS & !is.null(corrdat)) {
      correlations = c()
      conditions.all = c()
      if(unique(dat$experiment.name) == "noise-experiment") {
        conditions.all = as.character(sort(unique(dat$condition)))
      } else if(is.character(dat$condition)) {
        conditions.all = as.character(sort(as.numeric(unique(dat$condition))))
      } else {
        stop("unknown condition format")
      }
      
      for(c in conditions.all) {
        
        d = get.correlations(dat[dat$condition==as.character(c), ],
                             corrdat[corrdat$condition==as.character(c), ],
                             verbose = F)
        
        correlations = c(correlations, d[d$network==s$data.name, ]$avg.corr)
      }
      lines(x, correlations, type="l", col=s$color, lty=2, lwd=LINES.LWD)
    }
    
    # PLOT MAIN DATA
    lines(x, y, type="b", lwd=LINES.LWD,
          lty=1, col=s$color, pch=s$pch, cex=POINTS.CEX.VAL, bg=s$color)
    
    # PLOT RANGE
    if(((s$data.name %in% ALL.NETWORKS) & plot.dnn.range) | 
       (plot.observer.range & !(s$data.name %in% ALL.NETWORKS))) {
      # plot range of results - for DNNs the range of all sessions,
      # for human observers the range of all observers.
      
      if(s$data.name %in% ALL.NETWORKS) {
        acc.all.sessions = get.acc.for.range(dat[dat$subj==s$data.name, ])
      } else {
        acc.all.sessions = get.acc.for.range(dat[dat$is.human==TRUE, ], is.human = TRUE)
      }
      
      counter = 1
      for(c in sort(as.numeric(unique(dat$condition)))) {
        range = get.min.and.max(acc.all.sessions, counter)
        
        if(logarithmic.scale) {
          c = log(c, base=log.base)
        }
        if(normalize) {
          range$min = get.normalized.value(range$min, min(acc$y/100), max(acc$y/100))
          range$max = get.normalized.value(range$max, min(acc$y/100), max(acc$y/100))
        }
        
        if(range$min != range$max) {
          arrows(c, range$min, c, range$max,
                 length=0.05, angle=90, code=3, col=s$color, lwd = DNN.RANGE.LWD)
        }
        
        counter = counter+1
      }
    }
  }
  
  if(line.for.chance) {
    abline(h=h.of.line.for.chance, lty=5)
  }
  if(plot.legend) {
    add.legend(subjects, legend.position,
               cex=cex.legend)
  }
}

plot.binary.results = function(dat,
                               avg.human.data=FALSE,
                               normalize=F,
                               entropy=FALSE,
                               plot.dnn.range=!normalize & !isTRUE(entropy),
                               plot.observer.range=avg.human.data & !isTRUE(normalize) & !isTRUE(entropy),
                               legend.position=NULL,
                               plot.legend=TRUE,
                               line.for.chance = FALSE,
                               h.of.line.for.chance = 1/16.0,
                               condition.labels=c("Color", "Grayscale"),
                               main=NULL, ylim=NULL, ...) {
  # plot results of the color-experiment or other experiments with 2 conditions
  
  ylab = "Classification accuracy"
  if(is.null(main)) {
    main="Accuracy"
  }
  if(isTRUE(entropy)) {
    ylab = "Entropy of response distribution [bits]"
  } else if(normalize) {
    ylab="Normalized classification accuracy"
    main="Normalized accuracy for color-experiment"
  }
  if(is.null(legend.position)) {
    legend.position = "bottom"
  }
  if(is.null(ylim)) {
    if(isTRUE(entropy)) {
      ylim = range(0, h.of.line.for.chance)
    } else {
      ylim = c(0.7, 1.0)
    }
  }
  
  plot(c(0.7, 2.3), ylim, type="n", xaxt="n", bty="n",
       xlab="", ylab=ylab, ylim = ylim,
       main=main, ...)
  axis(1, at=c(1,2), labels=condition.labels)
  
  subjects = get.all.subjects(dat, avg.human.data)
  
  for(s in subjects) {
    if(avg.human.data && ! s$data.name %in% ALL.NETWORKS) {
      acc = get.accuracy(dat[dat$is.human==TRUE, ], entropy = entropy) 
    } else {
      acc = get.accuracy(dat[dat$subj==s$data.name, ], entropy = entropy)
    }
    y.value = acc$y / 100
    if(normalize) {
      y.value = get.normalized.value(y.value)
    }
    
    lines(acc$x, y.value, type="b", lwd=LINES.LWD,
          lty=1, col=s$color, pch=s$pch, bg=s$color,
          cex=POINTS.CEX.VAL)
    
    if((s$data.name %in% ALL.NETWORKS & plot.dnn.range) | 
       (! s$data.name %in% ALL.NETWORKS & plot.observer.range)) {
      # plot range of results for DNNs
      
      if(s$data.name %in% ALL.NETWORKS) {
        acc.all.sessions = get.acc.for.range(dat[dat$subj==s$data.name, ])
      } else {
        acc.all.sessions = get.acc.for.range(dat[dat$is.human==TRUE, ])
      }
      
      color.index = 1
      grayscale.index = 2
      
      color.range = get.min.and.max(acc.all.sessions, color.index)
      grayscale.range = get.min.and.max(acc.all.sessions, grayscale.index)
      
      if(normalize) {
        color.range$min = get.normalized.value(color.range$min, min(acc$y/100), max(acc$y/100))
        color.range$max = get.normalized.value(color.range$max, min(acc$y/100), max(acc$y/100))
        grayscale.range$min = get.normalized.value(grayscale.range$min, min(acc$y/100), max(acc$y/100))
        grayscale.range$max = get.normalized.value(grayscale.range$max, min(acc$y/100), max(acc$y/100))
      }
      
      if(color.range$min != color.range$max) {
        arrows(color.index, color.range$min, color.index, color.range$max,
               length=0.05, angle=90, code=3, col=s$color, lwd = DNN.RANGE.LWD)
      }
      if(grayscale.range$min != grayscale.range$max) {
        arrows(grayscale.index, grayscale.range$min, grayscale.index, grayscale.range$max,
               length=0.05, angle=90, code=3, col=s$color, lwd = DNN.RANGE.LWD)
      }
    }
  }
  if(line.for.chance) {
    abline(h=h.of.line.for.chance, lty=3)
  }
  if(plot.legend) {
    add.legend(subjects, legend.position = legend.position)
  }
}


add.legend = function(subjects, legend.position=NULL, cex=1.25, 
                      y=NULL, pt.cex=POINTS.CEX.VAL, ...) {
  # Add a legend to a plot 
  # -> will be usually called from within the plotting function's code
  # This function will not care for the actual
  # subject numbers but plots them as 1,2,...,n without gaps!
  
  name.values = c()
  color.values = c()
  pch.values = c()
  
  counter = 1
  for(s in subjects) {
    
    is.found = FALSE
    for(p in PARTICIPANTS) {
      if(p$data.name == s$data.name) {
        name.values = c(name.values, paste("subject-0", as.character(counter),
                                           sep=""))
        counter = counter + 1
        is.found = TRUE
        break()
      }
    }
    if(!is.found) {
      name.values = c(name.values, s$name)
    }
    color.values = c(color.values, s$color)
    pch.values = c(pch.values, s$pch)
  }
  if(is.null(legend.position)) {
    legend.position="topleft"
  }
  legend(legend.position, y=y, legend=name.values,
         cex=cex, col=color.values,
         pt.bg = color.values, pt.cex = pt.cex,
         pch=pch.values, lty=1, ...)
  
}


plot.percent.category = function(dat, avg.human.data=FALSE,
                                 main="Per-category split",
                                 legend.position=NULL,
                                 plot.legend=TRUE) {
  # one way of plotting the decisions: how much % of responses are dogs, cats,...?
  
  subjects = get.all.subjects(dat, avg.human.data)
  
  if(is.null(legend.position)) {
    if(unique(dat$experiment.name)=="noise-experiment") {
      legend.position="topleft"
    } else {
      legend.position="topright"
    }
  }
  
  plot(c(1.0, 16.0), c(0.0, 1.0), xaxt="n", bty="n",
       xlab="Categories", ylab="Fraction of total responses", type="n",
       main=main)
  axis(1, at=1:16, labels=rep("", times=16))
  text(1:16, par("usr")[3]-0.05, labels = unique(dat$category), srt = 45, pos = 1, xpd = TRUE)
  
  for(s in subjects) {
    if(s$data.name %in% ALL.NETWORKS | !avg.human.data) {
      answers = get.percent.answers.per.category(dat[dat$subj==s$data.name, ])
    } else {
      answers = get.percent.answers.per.category(dat[dat$is.human==TRUE, ])
    }
    
    lines(1:16, answers,
          col=s$color,
          type="p",
          bg=s$color,
          pch=s$pch,
          cex=POINTS.CEX.VAL)
  }
  if(plot.legend) {
    if(unique(dat$experiment.name)=="noise-experiment") {
      add.legend(subjects, legend.position)
    } else {
      add.legend(subjects, legend.position)
    }
  }
  lines(c(1,16), c(1/16, 1/16), lty=2)
}

###################################################################
#               Style transfer functionality
###################################################################

get.accuracy.per.category = function(dat, avg.human.data=TRUE,
                                     custom.subjects = NULL) {
  # Return data frame that shows accuracy split by subj and category.
  
  res = data.frame(subj=character(),
                   category=character(),
                   accuracy=numeric())
  if(is.null(custom.subjects)) {
    subjects = get.all.subjects(dat, avg.human.data = avg.human.data)
  } else {
    subjects = custom.subjects
  }
  
  for(s in subjects) {
    subj = s$data.name
    
    if(nrow(dat[dat$subj==subj, ]) <1 & subj != HUMAN.DATA.NAME) {
      stop(paste("no data for subject", subj, "found! Perhaps check data.name"))
    }
    
    for(category in CATEGORIES) {
      if(avg.human.data & subj==HUMAN.DATA.NAME) {
        a = dat[dat$is.human==TRUE & dat$category==category, ]
      } else {
        a = dat[dat$subj==subj & dat$category==category, ]
      }
      acc = get.single.accuracy(a)
      
      res = rbind(res, data.frame(subj=subj, category=category,
                                  accuracy=acc))
    }
  }
  return(res)
}

get.fractions.of.responses = function(dat, x.is.content=TRUE,
                                      avg.human.data=TRUE,
                                      custom.subjects=NULL) {
  # Return data frame that splits responses into content, style etc.
  # Utility function for style transfer results plotting.
  # The following convention is followed:
  # Trials in which shape category = texture category are excluded.
  # This enables one to have the following partition of the 
  # remaining data:
  # - texture decisions
  # - shape decisions
  # - 'other' decisions (!= shape and != texture)
  # ...those three cases then sum to 1.0, which is necessary.
  
  res = data.frame(subj=character(),
                   category=character(),
                   frac.content.decisions=numeric(),
                   frac.texture.decisions=numeric(),
                   frac.other.decisions=numeric())
  if(is.null(custom.subjects)) {
    subjects = get.all.subjects(dat, avg.human.data = avg.human.data)
  } else {
    subjects = custom.subjects
  }
  for(s in subjects) {
    subj = s$data.name
    
    if(nrow(dat[dat$subj==subj, ]) <1 & subj != HUMAN.DATA.NAME) {
      stop(paste("no data for subject", subj, "found! Perhaps check data.name"))
    }
    
    for(category in CATEGORIES) {
      if(x.is.content) {
        if(avg.human.data & subj==HUMAN.DATA.NAME) {
          a = dat[dat$is.human==TRUE & dat$category==category, ]
        } else {
          a = dat[dat$subj==subj & dat$category==category, ]
        }
      } else {
        if(avg.human.data & subj==HUMAN.DATA.NAME) {
          a = dat[dat$is.human==TRUE & dat$texture==category, ]
        } else {
          a = dat[dat$subj==subj & dat$texture==category, ]
        }
      }
      
      # exclude texture = shape case
      b = a[a$texture != a$category, ]
      num.images = nrow(b)
      
      # decision = category (= content, = shape)
      frac.content.decisions = nrow(b[as.character(b$object_response)==as.character(b$category), ]) / num.images
      
      # decision = texture
      frac.texture.decisions = nrow(b[as.character(b$object_response)==as.character(b$texture), ]) / num.images
      
      # decision = neither texture nor category/content/shape
      frac.other.decisions = nrow(b[as.character(b$object_response)!=as.character(b$texture) & as.character(b$object_response)!=as.character(b$category), ]) / num.images
      
      if(! all.equal(frac.other.decisions + frac.texture.decisions + frac.content.decisions, 1.0)) {
        print("the problematic values are:")
        print(frac.texture.decisions)
        print(frac.content.decisions)
        print(frac.other.decisions)
        warning("values do not sum to 1.0 as they should!")
      }
      
      res = rbind(res, data.frame(subj=subj, category=category,
                                  frac.content.decisions=frac.content.decisions,
                                  frac.texture.decisions=frac.texture.decisions,
                                  frac.other.decisions=frac.other.decisions))
    }
  }
  return(res)
}


plot.transfer.results = function(dat, x.is.content=TRUE,
                                 plot.average.line=TRUE,
                                 plot.frac.other.decisions=TRUE,
                                 categories=rev(CATEGORIES),
                                 plot.legend=FALSE,
                                 legend.position=0.66,
                                 avg.human.data=TRUE,
                                 custom.subjects=NULL,
                                 ...) {
  # plot results for cue conflict stimuli (generated by style transfer)
  orig.xpd = par()$xpd
  # hard-coded values for category images
  xright = 0.02
  img.height = 0.85
  img.width = 0.06
  unwanted.y.offset = 0.5
  barplot.max.width = 0.1
  barplot.dist.from.plot = 0.05
  xlab="Fraction of 'shape' decisions"
  
  if(x.is.content) {
    obj = list(ylab = "Shape categories", mtext.side=2,
               categories.xleft=0.0-xright-img.width,
               categories.xright=0.0-xright,
               x.values.plotted.from=1.0,
               inversion.sign=-1.0,
               table.col.name="frac.content.decisions")
  } else {
    obj = list(ylab = "Texture categories", mtext.side=4,
               categories.xleft=1+xright,
               categories.xright=1+xright+img.width,
               x.values.plotted.from=0.0,
               inversion.sign=1.0,
               table.col.name="frac.texture.decisions")
  }
  
  # create empty plot
  left.xlim = 0.0 - obj$x.values.plotted.from*(xright+img.width) - (1-obj$x.values.plotted.from)*(xright+barplot.dist.from.plot+barplot.max.width)
  right.xlim = 1.0 + (1-obj$x.values.plotted.from)*(xright+img.width) + (obj$x.values.plotted.from)*(xright+barplot.dist.from.plot+barplot.max.width)
  
  plot.xlim = c(left.xlim, right.xlim)
  plot(plot.xlim, xlim=plot.xlim,
       ylim = c(unwanted.y.offset, length(categories)-unwanted.y.offset-(1-0.85)),
       type="n", xaxt="n", yaxt="n", bty="n", ylab="", xlab="")
  
  # add axes
  x.axis.seq = seq(from=0.0, to=1.0, by=0.1)
  axis(side=1, at=x.axis.seq, tick = T,
       labels=rev(rev(x.axis.seq)))
  axis(side=3, at=x.axis.seq, tick = T,
       labels=rev(x.axis.seq))
  lines(c(0.0, 0.0), c(0.0, length(categories)), lty=1)
  #lines(c(0.5, 0.5), c(0.0, length(categories)), lty=3) # dased line at x=0.5
  lines(c(1.0, 1.0), c(0.0, length(categories)), lty=1)
  
  # axes label text
  par(xpd=NA)
  mtext("Fraction of 'texture' decisions", side=1, line=3)
  arrow.offset = 1.75
  arrows(x0=1, y0=length(categories)+arrow.offset, x1=0, y1=length(categories)+arrow.offset, length = 0.15, angle = 30)
  mtext("Fraction of 'shape' decisions", side=3, line=3)
  arrows(x0=0, y0=-arrow.offset, x1=1, y1=-arrow.offset, length = 0.15, angle = 30)
  mtext(obj$ylab, side=obj$mtext.side, line=0)
  par(xpd=orig.xpd)
  
  # dotted horizontal lines to separate categories
  for(i in 1:(length(categories)-1)) {
    lines(x=c(0, 1),
          y=rep((i - (1-img.height)/2.0), times=2), lty=3,
          col=alpha("black", 0.2))
  }
  
  counter = 0
  for(category in categories) {
    rasterImage(image.categories[[category]],
                xleft = obj$categories.xleft,
                ybottom = counter,
                xright = obj$categories.xright, 
                ytop = counter+img.height)
    counter = counter + 1
  }
  
  # get data in correct format
  table = get.fractions.of.responses(dat, x.is.content=x.is.content,
                                     avg.human.data = avg.human.data,
                                     custom.subjects = custom.subjects)
  table$x.pos = obj$x.values.plotted.from + obj$inversion.sign * 
    (table[[obj$table.col.name]] / (1.0 - table$frac.other.decisions))
  
  
  # plot average lines
  if(is.null(custom.subjects)) {
    subjects = get.all.subjects(dat, avg.human.data = avg.human.data)
  } else {
    subjects = custom.subjects
  }
  if(plot.average.line) {
    for(subj in subjects) {
      subj.data = table[table$subj==subj$data.name, ]
      mean.value = obj$x.values.plotted.from + obj$inversion.sign *
        (mean(subj.data[[obj$table.col.name]] /
                (mean(subj.data$frac.content.decisions + subj.data$frac.texture.decisions))))
      lines(c(mean.value, mean.value), c(0.0, length(categories)), lty=1,
            col=alpha(subj$color, 0.3), lwd=2.0)
      
    }
  }
  
  # plot data points
  subj.counter = 0
  for(subj in rev(subjects)) {
    counter = 0
    for(category in categories) {
      x.pos = table[table$subj==subj$data.name & table$category==category, ]$x.pos
      pch = subj$pch
      if(subj$name == HUMAN.DATA.NAME) {
        pch = 21 # hack to set human data plotting to filled circles
      }
      points(x.pos, counter+img.height/2.0, pch=pch, col=subj$color, 
             bg=subj$color, cex=POINTS.CEX.VAL)
      if(plot.frac.other.decisions) { # add horizontal barplots
        # if the '1.0' in the following line is deleted, wrong decisions are plotted - if it is added, correct decisions are plotted.
        bar.width = barplot.max.width * (1.0 - table[table$subj==subj$data.name & table$category==category, ]$frac.other.decisions)
        bar.block.distance = 0.1 # distance between groups of bars for different categories
        bar.height = (1.0 - 2*bar.block.distance) / length(subjects)
        
        bar.xleft = obj$x.values.plotted.from - obj$inversion.sign*barplot.dist.from.plot
        bar.xright = bar.xleft - obj$inversion.sign*bar.width
        subj.offset = subj.counter * bar.height
        ybottom = counter + subj.offset
        # grey
        rect(xleft=bar.xleft, xright=bar.xleft - obj$inversion.sign*barplot.max.width,
             ybottom=ybottom,
             ytop=ybottom + bar.height,
             col = alpha("black", 0.0), border = "grey")
        # colour
        rect(xleft=bar.xleft, xright=bar.xright,
             ybottom=ybottom,
             ytop=ybottom + bar.height,
             col = subj$col, border = T)
      }
      counter = counter + 1
    }
    subj.counter = subj.counter + 1
  }
  
  if(plot.legend) {
    legend.y = length(categories)-0.5
    add.legend(subjects, legend.position = legend.position,
               cex=0.9, pt.cex=2.0, y=legend.y)
  }
  #par(xpd=orig.xpd)
}

plot.all.barplots = function(img.list, barplot.values, bracket.points,
                             draw.brackets=FALSE, draw.numbers=TRUE,
                             draw.axis=FALSE, angle=0,
                             exp.names=c()) {
  # plot a list of images and on top, barplots indicating accuracy.
  # arguments:
  # - img.list: a list of image paths for the images to be plotted
  # - barplot values: list of lists with acc=c(...,...,...) and col= defined
  #                   (one per network / observer / ...)
  # - bracket.points: vector of integers indicating end indices of brackets.
  #                   E.g. a value of 2 indicates that the first bracket will
  #                   go from 1st to 2nd image.
  num.bars = length(barplot.values)
  space.between.imgs = 0.2
  img.width = 1.0-space.between.imgs
  
  if(length(barplot.values[[1]]$acc) != length(img.list)) {
    stop("both lists need to have same length")
  }
  if(length(exp.names) >= 1) {
    ylim.value.1 = -2
  } else {
    ylim.value.1 = 0
  }
  
  plot.xlim = c(0, length(img.list))
  plot.ylim = c(ylim.value.1, 2*img.width+space.between.imgs)
  plot(x=c(0,0),
       y=c(0,0),
       xlim=plot.xlim,
       ylim=plot.ylim,
       type="n", xaxt="n", yaxt="n", bty="n", ylab="", xlab="", asp = 1)
  counter = 0
  bar.start.y = 0.9
  
  for(img in img.list) {
    img.to.plot = readPNG(img)
    
    img.to.plot = rotate(img.to.plot, angle)
    
    xleft = 0+counter
    xright = xleft + img.width
    ybottom = 0
    ytop = 1-space.between.imgs
    rasterImage(img.to.plot,
                xleft = xleft,
                ybottom = ybottom,
                xright = xright, 
                ytop = ytop)
    # draw black rectangle around image
    rect(xleft=xleft, ybottom=ybottom, xright=xright,
         ytop=ytop, col = NA, border = "black")
    
    # draw bars with accuracy lines
    barplot.gap = 0.035 # gap between neighbouring bars
    barplot.width = (img.width - (num.bars-1)*barplot.gap) / num.bars
    for(item.counter in 1:num.bars) {
      accuracies = barplot.values[[item.counter]]$acc
      accuracy = accuracies[counter+1]
      offset.x = (barplot.width + barplot.gap)*(item.counter-1)
      rect(xleft=xleft + offset.x, ybottom=bar.start.y, xright=xleft + offset.x + barplot.width,
           ytop=bar.start.y + (img.width*accuracy),
           col = barplot.values[[item.counter]]$col)
      
      if(counter==0) { # 1st barplot: draw network / human names
        shadowtext(x=xleft + offset.x + ifelse(num.bars<=3,barplot.width/3.5,barplot.width/5), y=bar.start.y + 0.02,
                   labels=c(barplot.values[[item.counter]]$subj),
                   srt=90, pos=4, cex = 0.8, col="white")
      }
      
      if(draw.numbers) {
        # draw text (accuracy) in numbers
        text.offset.y = ifelse(angle==0, 0.075, 0.125)
        text(x=xleft + offset.x + barplot.width/2.0, y=bar.start.y + text.offset.y + 
               (img.width*accuracy), labels=c(as.character(round(100*accuracy))), srt=angle, cex = 0.8)
        item.counter = item.counter + 1
      }
      
    }
    
    # y=0 line at bottom of bar chart
    if(!is.na(barplot.values[[1]]$acc[counter+1])) { # plot only if data available
      lines(x = c(counter, counter+img.width), y = c(bar.start.y, bar.start.y))
    } else { # plot "see right" for paper. Hard-coded values here.
      pos.see.right = ifelse(angle==0, 2, 4)
      text(x = counter +0.35, y = bar.start.y, labels=c("see right"), pos=pos.see.right,
           srt=angle, cex=1.4)
    }
    
    counter = counter + 1
  }
  
  if(draw.axis) {  # plot y axis
    axis(side=2, at=seq(from=bar.start.y, to=bar.start.y+img.width, length.out = 5), tick = T,
         labels=seq(from=0.0, to=1.0, by=0.25), pos=-0.1)
  }
  if(draw.brackets) { # draw square brackets
    gap = space.between.imgs/3.0
    x1 = 0-gap
    brackets.y = 2*gap
    for(p in bracket.points) {
      x2 = p-2*gap
      brackets(x1=x1, y1=brackets.y, x2=p-2*gap, y2=brackets.y, lwd=1.5,
               type=4, h=-space.between.imgs, ticks=NA, xpd=TRUE)
      x1 = x2+gap
    }
  }
  for(i in 1:length(exp.names)) { # draw text with experiment names
    if(angle==0) {
      text.x = i-1
      pos.text = 4
      offset.text = 0.0
    } else {
      text.x = i-0.5
      pos.text = 2
      offset.text = 0.5
    }
    text(x=text.x, y=-0.1, labels=exp.names[i], srt=angle,
         cex=1.0, pos=pos.text, offset=offset.text)
  }
}


###################################################################
#               Helper functions
###################################################################
get.accuracies.for.experiments.helper = function(dat.list) {
  # return list of accuracies for different observers and experiments
  # note: all data needs to have the same subjects!
  
  accuracies.here = list()
  counter = 1
  for(s in get.all.subjects(dat.list[[1]], avg.human.data = TRUE)) {
    new.subject = list(acc=numeric(length(dat.list)), col=s$color,
                       subj=character(length(dat.list)))
    for(i in 1:length(dat.list)) {
      acc.fun = get.single.accuracy
      if(s$data.name %in% ALL.NETWORKS) {
        new.subject$acc[i] = acc.fun(dat.here[[i]][dat.here[[i]]$subj==s$data.name, ])
        new.subject$subj[i] = s$name
      } else {
        new.subject$acc[i] = acc.fun(dat.here[[i]][dat.here[[i]]$is.human==TRUE, ])
        new.subject$subj[i] = "Humans"
      }
      
    }
    accuracies.here[[counter]] = new.subject
    counter = counter + 1
  }
  return(accuracies.here)
}



get.nearest.condition = function(dat, p) {
  # Return list(c, diff).
  # c = the condition for which the accuracy will be closest to p
  # diff = the difference between this accuracy and p
  
  a.tmp = Inf
  c.tmp = NULL
  diff.tmp = Inf
  for(c in unique(dat$condition)) {
    acc = get.single.accuracy(dat[dat$condition==c, ])
    if(abs(p-acc) < diff.tmp) {
      a.tmp = acc
      c.tmp = c
      diff.tmp = abs(p-acc)
    }
  }
  return(list(c=c.tmp, diff=diff.tmp))
}



is.in.CI = function(a.num.successes, a.total,
                    b.num.successes, b.total,
                    conf.level,
                    default.for.p.equals.0 = 0.001) {
  # In this analysis, is it used as follows:
  # a: network (in general, reference)
  # b: human
  #
  # Return value will be 1 if b.num.successes / b.total larger than 
  # the CIs upper bound, -1 if it is smaller, and 0 otherwise
  # (i.e. if it is contained in the CI, the return value will be 0).
  
  p.a = a.num.successes / a.total
  p.b = b.num.successes / b.total
  
  p = ifelse(p.a != 0, ifelse(p.a != 1, p.a, 1-default.for.p.equals.0), default.for.p.equals.0)
  
  p.value = binom.test(b.num.successes, b.total,
                       p = p,
                       alternative = "two.sided",
                       conf.level = conf.level)$p.value
  
  if(p.value < (1.0 - conf.level)) {
    if(p.a > p.b) {
      return(-1)
    } else if (p.b > p.a) {
      return(1)
    } else {
      stop("this shouldn't occur!")
    }
  } else {
    return(0)
  }
}


get.eidolon.dat.preprocessed = function(dat, separating.condition) {
  # Eidolon data is a special case because condition is 3-dimensional
  # (compared to other 1-dimensional experiments). Therefore this function
  # can be used to extract the whole data for the middle condition.
  # Parameter separating.condition is one of 0, ..., 10 .
  
  dat.new = dat[grepl(paste("-", as.character(separating.condition), "-", sep=""), dat$condition), ]
  dat.new$condition = as.character(dat.new$condition)
  dat.new$condition = lapply(dat.new$condition, function(y){strsplit(y, "-")[[1]][1]})
  dat.new$condition = as.numeric(dat.new$condition)
  return(dat.new)
}


get.na.trials.fraction = function(dat) {
  # return fraction of trials for which human observers did not 
  # respond at all
  
  total.responses = nrow(dat[dat$is.human==TRUE, ])
  na.responses = nrow(dat[dat$is.human==TRUE & dat$object_response=="na", ])
  return(na.responses / total.responses)
}


get.percent.answers.per.category = function(dat) {
  # Return a list of percentages, one per category.
  # Each value indicates the percentage of object_response
  # for this category. The sum, thus, should be 1 (DNN)
  # or close to 1 (humans, 'na' not included)
  
  result = c()
  num.answers = length(dat$category)
  for(c in unique(dat$category)) {
    result = c(result, nrow(dat[dat$object_response==c, ])/num.answers)
  }
  return(result)
}

get.normalized.value = function(old.val, old.min=min(old.val), old.max=max(old.val)) {
  #Return vector of elements squeezed between old.min and 1.
  
  if(any(old.val < old.min)) {
    #stop("old.val needs to be >= old.min")
  }
  new.val = ((old.val-old.min)/(old.max-old.min))*(1-old.min)+old.min
  return(new.val)
}


get.min.and.max = function(acc.all.sessions, index) {
  # Return min and max y-value of acc.all.sessions
  
  min = Inf
  max = -Inf
  
  for(a in acc.all.sessions) {
    if(index > length(a$y)) {
      stop("index needs to be <= length")
    }
    
    if(a$y[index] < min)
      min = a$y[index]
    if(a$y[index] > max)
      max = a$y[index]
  }
  
  return(list(min=min, max=max))
}

get.accuracy = function(dat, top.n=0, entropy=FALSE) {
  # Return data.frame with x and y for condition and accuracy.
  # If top.n != 0, return fraction of top-n responses instead
  # - that is, the fraction of responses that fall onto the
  # top n most frequent response categories.
  
  tab = table(dat$is.correct, by=dat$condition)
  false.index = 1
  true.index = 2
  acc = tab[true.index, ] / (tab[false.index, ]+tab[true.index, ])
  d = as.data.frame(acc)
  
  if(length(colnames(tab)) != length(unique(dat$condition))) {
    stop("Error in get.accuracy: length mismatch.")
  }
  
  if((top.n != 0) | isTRUE(entropy)) {
    counter = 1
    for(c in colnames(tab)) {
      if(! c %in% unique(dat$condition)) {
        warning("Warning for get.accuracy: condition mismatch.")
      }
      if(isTRUE(entropy)) {
        resp = get.entropy(dat[dat$condition==c, ])
      } else {
        resp = get.top.n.frac.of.responses(dat[dat$condition==c, ], top.n)
      }
      d[counter, ] = resp
      counter = counter + 1
    }
  }
  
  #enforce numeric ordering instead of alphabetic (otherwise problem: 100 before 20)
  if(!is.factor(dat$condition)) {
    #condition is numeric
    
    d$order = row.names(d)
    d$order = as.numeric(d$order)
    d = d[with(d, order(d$order)), ]
    d$order = NULL
    e = data.frame(x = as.numeric(row.names(d)), y=100*d[ , ])
  } else {
    #condition is non-numeric
    e = data.frame(x = row.names(d), y=100*d[ , ])
  }
  return(e)
}

get.category.order = function(dat, x.is.content) {
  table = get.fractions.of.responses(dat[dat$is.human==TRUE, ], x.is.content=x.is.content, avg.human.data = TRUE)
  table$frac.shape.decisions.amongst.correct.decisions = table$frac.content.decisions / (table$frac.content.decisions+table$frac.texture.decisions)
  c = table[order(table$frac.shape.decisions.amongst.correct.decisions), ]$category
  return(rev(as.character(c)))
}



get.texture = function(imagename) {
  # Helper function: return 'style' category as character from imagename.
  # This is hard-coded to the imagename formatting.
  
  a = strsplit(strsplit(strsplit(imagename, "_")[[1]][7], "-")[[1]][2], ".png")[[1]]
  category = gsub('[[:digit:]]+', '', a)
  return(category)
}


get.expt.data = function(expt.name,
                         is.style.transfer=FALSE,
                         append.human.data=FALSE,
                         generalisation.project=FALSE) {
  # Read data and return in the correct format
  
  if(!exists("DATAPATH")) {
    stop("you need to define the DATAPATH variable")
  }
  
  dat = NULL
  expt.path = paste(DATAPATH, expt.name, sep="")
  files = list.files(expt.path)
  
  if(length(files) < 1) {
    warning(paste("No data for expt", expt.name, "found! Check DATAPATH."))
  }
  
  for (i in 1:length(files)) {
    if(!endsWith(files[i], ".csv")) {
      warning("File without .csv ending found (and ignored)!")
    } else {
      dat = rbind(dat, read.csv(paste(expt.path, files[i], sep="/")))
    }
  }
  
  # concatenate human data from different data path
  if(append.human.data & endsWith(expt.name, "-trained-CNNs")) {
    expt.name = gsub(pattern="-trained-CNNs", replacement = '', expt.name)
    expt.path = DATAPATH
    if(generalisation.project) {
      expt.path=GENERALISATION.DATAPATH
    }
    expt.path = paste(expt.path, expt.name, sep="")
    files = list.files(expt.path)
    
    if(length(files) < 1) {
      warning(paste("No data for expt", expt.name, "found! Check data paths (GENERALISATION.DATAPATH, DATAPATH)"))
    }
    
    for (i in 1:length(files)) {
      if(!endsWith(files[i], ".csv")) {
        warning("File without .csv ending found (and ignored)!")
      } else {
        dat.read.in = read.csv(paste(expt.path, files[i], sep="/"))
        if(grepl("subject", unique(dat.read.in$subj))) { # only append human data
          dat = rbind(dat, dat.read.in)
        }
      }
    }
  }
  
  dat$imagename = as.character(dat$imagename)
  dat$is.correct = as.character(dat$object_response) == as.character(dat$category)
  dat$is.human = ifelse(grepl("subject", dat$subj), TRUE, FALSE)
  if(is.style.transfer) { # make extra column with texture = style category
    dat$texture = lapply(dat$imagename, get.texture)
    dat$texture = as.factor(as.character(dat$texture))
  }
  
  return(data.frame(experiment.name = expt.name, dat))
}


drop.resp.levels = function(dat) {
  # Return data without "na" as a response
  
  dat = dat[dat$object_response!="na", ]
  dat$object_response = droplevels(dat$object_response)
  return(dat)
}

get.acc.for.range= function(dat, is.human=FALSE) {
  # Return list of accuracies for range, to be used for computing range.
  # if is.human: for each human observer, if not:
  # for each session of a netowrk.
  # Each entry contains the accuracies for all conditions.
  
  acc.all.sessions = list()
  counter = 1
  if(is.human) {
    subjects = get.all.subjects(dat, avg.human.data = FALSE)
    for(s in subjects) {
      if(! s$data.name %in% ALL.NETWORKS) {
        acc.all.sessions[[counter]] = get.accuracy(dat[dat$subj==s$data.name, ])
        acc.all.sessions[[counter]]$y = acc.all.sessions[[counter]]$y / 100
        counter = counter + 1
      }
      
    }
  } else {
    for(session.num in unique(dat$session)) {
      acc.all.sessions[[counter]] = get.accuracy(dat[dat$session==session.num, ])
      acc.all.sessions[[counter]]$y = acc.all.sessions[[counter]]$y / 100
      counter = counter + 1
    }
  }
  
  
  return(acc.all.sessions)
}

get.range = function(dat, FUN, is.human=TRUE) {
  range = list()
  counter = 1
  if(is.human) {
    subjects = get.all.subjects(dat, avg.human.data = FALSE)
    for(s in subjects) {
      if(! s$data.name %in% ALL.NETWORKS) {
        range[[counter]] = FUN(dat[dat$subj==s$data.name, ])
        counter = counter + 1
      }
    }
    return(range)
  } else {
    stop("Not implemented.")
  }
}

get.single.accuracy = function(dat) {
  # Return accuracy for whatever data as a single value.
  if(is.na(length(dat$is.correct)) | length(dat$is.correct)==0) {
    return(NaN)
  } else {
    return(sum(dat$is.correct) / length(dat$is.correct))
  }
}

get.fail.rate = function(dat) {
  return (nrow(dat[dat$object_response=="na", ]) / (nrow(dat)))
}

get.tex.shape.wrong.rate = function(dat) {
  # Return fraction of decisions that correspond to neither shape nor texture
  return(nrow(dat[!(as.character(dat$object_response)==as.character(dat$category) | 
                      as.character(dat$object_response)==as.character(dat$texture)), ]) / nrow(dat))
}

endsWith = function(argument, match, ignore.case = TRUE) {
  # Return: does 'argument' end with 'match'?
  # Code adapted from:
  # http://stackoverflow.com/questions/31467732/does-r-have-function-startswith-or-endswith-like-python
  
  if(ignore.case) {
    argument = tolower(argument)
    match = tolower(match)
  }
  n = nchar(match)
  
  length = nchar(argument)
  
  return(substr(argument, pmax(1, length - n + 1), length) == match)
}

crop.pdfs = function(dir.path) {
  # crop all PDFs in a directory to remove all white margins.
  files = list.files(path=dir.path, pattern="*.pdf", full.names=TRUE, recursive=TRUE,
                     include.dirs = TRUE)
  print(files)
  for(f in files) {
    if(! endsWith(f, "crop.pdf")) { # if not already cropped
      system(paste("pdfcrop", f))
    }
  }
}
