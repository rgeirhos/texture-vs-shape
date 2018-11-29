###################################################################
#   Helper script for automated data analysis
#   (texture-shape experiments)
#   -------------------------------------------------------
#   Authors:   Robert Geirhos, Patricia Rubisch
#   Based on: R version 3.2.3
###################################################################

print.confusion.matrices = function(dat, path) {
  # print confusion matrices for all subjects to file
  
  for(subj in unique(dat$subj)) {
    pdf(file=paste(path, "confusion-matrix_", subj, ".pdf", sep=""), 
        width=8.4, 
        height=8.2)
    print(confusion.matrix(dat, subject=subj))
    dev.off()
  }
  if(nrow(dat[dat$is.human==TRUE, ]) > 0) {
    pdf(file=paste(path, "confusion-matrix_human-average.pdf", sep=""), 
        width=8.4, 
        height=8.2)
    print(confusion.matrix(dat[dat$is.human==TRUE, ]))
    dev.off()
  }
  
}


print.function.outcome = function(dat, FUN, function.name="") {
  # convenience function: print FUN applied to dat for every subject.
  
  print("###########################################")
  print(function.name)
  network.sum = 0.0
  network.counter = 0.0
  human.sum = 0.0
  human.counter = 0.0
  for(subj in get.all.subjects(dat, avg.human.data = FALSE)) {
    if(subj$data.name %in% NETWORKS) {
      network.sum = network.sum + FUN(dat[dat$subj==subj$data.name, ])
      network.counter = network.counter+1
    } else {
      human.sum = human.sum + FUN(dat[dat$subj==subj$data.name, ])
      human.counter = human.counter+1
    }
    print(sprintf("%15s:  %-5s", subj$name,
                  round(FUN(dat[dat$subj==subj$data.name, ]), 3)))
  }
  print("-------------------------------------------")
  if(nrow(dat[dat$is.human==FALSE, ]) > 0) {
    print(sprintf("%15s:  %-5s", "network average",
                  round(network.sum / network.counter, 3)))
  }
  if(nrow(dat[dat$is.human==TRUE, ]) > 0) {
    print(sprintf("%15s:  %-5s", "human average",
                  round(human.sum / human.counter, 3)))
  }
}


get.barplot = function(dat, FUN, plot.range=TRUE, 
                       line.for.chance=TRUE, h.of.line.for.chance=1/16.0,
                       exclude.list=c(), ...) {
  # plot barplot of FUN(data), split by subjects
  # if any subject is contained in exclude.list, it will not be
  # counted towards the "bar" of the plot but rather be shown
  # as an outlier data point.
  
  excluded.dat = dat[dat$subj %in% exclude.list, ]
  dat = dat[! dat$subj %in% exclude.list, ]
  
  cols = c()
  row.names = c()
  subjects = get.all.subjects(dat, avg.human.data = TRUE)
  acc = matrix(nrow=length(subjects))
  i = 1
  for(subj in subjects) {
    if(subj$data.name %in% NETWORKS) {
      acc[i] = FUN(dat[dat$subj==subj$data.name, ])
    } else {
      range = get.range(dat[dat$is.human==TRUE, ], FUN)
      acc[i] = mean(as.numeric(range))
    }
    row.names = c(row.names, subj$name)
    cols = c(cols, subj$color)
    i = i+1
  }
  
  row.names(acc) = row.names
  barplot(acc, beside = T, col=cols, 
          main="",
          xlab="",
          space = 0.25,
          las=1,
          names.arg=row.names(acc), ...)
  
  if(plot.range) {
    # plot range of human results
    range = get.range(dat[dat$is.human==TRUE, ], FUN)
    min = Inf
    max = -Inf
    for(x in range) {
      if(x < min) {
        min = x
      }
      if(x > max) {
        max = x
      }
    }
    arrows(4.5, min, 4.5, max, angle=90, col="black", code = 3)
  }
  if(line.for.chance) {
    abline(h=h.of.line.for.chance, lty=3)
  }
  if(!is.null(exclude.list)) {
    # plot the outlier
    for(excl.subj in exclude.list) {
      points(4.5, FUN(excluded.dat[excluded.dat$subj==excl.subj, ]))
    }
  }
}

print.barplot = function(dat, path, FUN, function.name,
                         plot.range=TRUE, exclude.list=c(), ...) {
  # print barplot of FUN(data) to file.
  # if any subject is contained in exclude.list, it will not be
  # counted towards the "bar" of the plot but rather be shown
  # as an outlier data point. If empty, subjects with a failure
  # to respond rate > 5% will be excluded.
  
  if(is.null(exclude.list)) {
    exclude.list = get.subj.to.exclude(dat)
  }
  if(!is.null(exclude.list)) {
    print("Warning: excluding subject(s)")
  }
  
  pdf(file=paste(path, function.name, ".pdf", sep=""), 
      width=8, 
      height=6)
  get.barplot(dat, FUN, plot.range, exclude.list=exclude.list, ...)
  dev.off()
}

print.conflict.results = function(dat, path, x.is.content, ...) {
  # plot performance for cue conflict stimuli
  
  if(isTRUE(x.is.content)) {
    s = "content"
  } else {
    s = "texture"
  }
  pdf(file=paste(path, "", "conflict_", s, ".pdf", sep=""), 
      width=6.5, 
      height=6.5)
  par(mfrow=c(1,1))
  plot.transfer.results(dat, x.is.content = x.is.content, ...)
  dev.off()
}

get.lineplot = function(first_dat, second_dat, third_dat, fourth_dat, FUN, plot.range=TRUE, 
                       line.for.chance=TRUE, h.of.line.for.chance=1/16.0,
                       exclude.list=c(), ...) {
  # plot lineplot of FUN(data), split by subjects
  # if any subject is contained in exclude.list, it will not be
  # counted towards the "bar" of the plot but rather be shown
  # as an outlier data point.
  
  n_experiments= 4

  excluded.first_dat = first_dat[first_dat$subj %in% exclude.list, ]
  first_dat = first_dat[! first_dat$subj %in% exclude.list, ]
  
  cols = c()
  row.names = c()
  colours = c()
  plotchar = c()
  
  subjects = get.all.subjects(first_dat, avg.human.data = TRUE)
  acc1 = matrix(nrow=length(subjects))
  i = 1
  #Do this for all experiments
  for(subj in subjects) {
    if(subj$data.name %in% NETWORKS) {
      acc1[i] = FUN(first_dat[first_dat$subj==subj$data.name, ])
    } else {
  range = get.range(first_dat[first_dat$is.human==TRUE, ], FUN)
      acc1[i] = mean(as.numeric(range))
    }
    row.names = c(row.names, subj$name)
  cols = c(cols, subj$color)
    i = i+1
    colours=c(subj$color)
    plotchar= c(subj$pch)
  
  }
  #fuse acc to new matrix with needed data. 
  
  excluded.third_dat = third_dat[third_dat$subj %in% exclude.list, ]
  third_dat = third_dat[! third_dat$subj %in% exclude.list, ]
  
  yrange = c(0:1, 0.2)
  experiments= factor(c("original", "silhouette-filled","edge", "texture"))
  xrange= c(1, 1.63, 2.27,3)

  alexnet = c(1,0.44,0.4,1)
  googlenet = c(1,0.488,0.281,1)
  vgg= c(1,0.481,0.244,1)
  human_participants = c(0.989,0.753,0.871,0.9)
  
  
  plot(yrange, xaxt= "n", type="n", xlab="experiments" , ylab="classification accuracy")

  for (i in experiments){
    lines(xrange, alexnet, type = "b", lwd = 1.5, lty= "solid" , col = rgb(65, 90, 140, maxColorValue = 255), pch = 18)# or 23
    lines(xrange, googlenet, type = "b", lwd = 1.5, lty= "solid" , col = rgb(80, 170, 200, maxColorValue = 255), pch = 15) #or 22
    lines(xrange, vgg, type = "b", lwd = 1.5, lty= "solid" , col = rgb(50, 105, 170, maxColorValue = 255), pch = 17) #or 24
    lines(xrange, human_participants, type = "b", lwd = 1.5, lty= "solid" , col =rgb(165, 30, 55, maxColorValue = 255), pch = 16)#21
    }
    
   axis(1, at = c(1, 1.63, 2.27,3), labels = experiments)

  if(plot.range) {
    # plot range of human results
    # need to see how this is changed in line chart as the place os not given, but for every point on y acces
    range = get.range(first_dat[first_dat$is.human==TRUE, ], FUN)
    min = Inf
    max = -Inf
    for(x in range) {
      if(x < min) {
        min = x
      }
      if(x > max) {
        max = x
      }
    }
    arrows(1, min, 1, max, angle=90, col="black", code = 3)
  }
  
  if(plot.range) {
    # plot range of human results
    # need to see how this is changed in line chart as the place os not given, but for every point on y acces
    range = get.range(second_dat[second_dat$is.human==TRUE, ], FUN)
    min = Inf
    max = -Inf
    for(x in range) {
      if(x < min) {
        min = x
      }
      if(x > max) {
        max = x
      }
    }
    arrows(1.63, min, 1.63, max, angle=90, col="black", code = 3)
  }
  
  if(plot.range) {
    # plot range of human results
    # need to see how this is changed in line chart as the place os not given, but for every point on y acces
    range = get.range(third_dat[third_dat$is.human==TRUE, ], FUN)
    min = Inf
    max = -Inf
    for(x in range) {
      if(x < min) {
        min = x
      }
      if(x > max) {
        max = x
      }
    }
    arrows(2.27, min, 2.27, max, angle=90, col="black", code = 3)
  }
  
  if(plot.range) {
    # plot range of human results
    # need to see how this is changed in line chart as the place os not given, but for every point on y acces
    range = get.range(fourth_dat[fourth_dat$is.human==TRUE, ], FUN)
    min = Inf
    max = -Inf
    for(x in range) {
      if(x < min) {
        min = x
      }
      if(x > max) {
        max = x
      }
    }
    arrows(3, min, 3, max, angle=90, col="black", code = 3)
  }
  if(line.for.chance) {
    abline(h=h.of.line.for.chance, lty=3)
  }
  
  for (i in 1: n_experiments){
  if(!is.null(exclude.list)) {
    # plot the outlier
    for(excl.subj in exclude.list) {
      points(2.27, FUN(excluded.third_dat[excluded.third_dat$subj==excl.subj, ]))
      }
    }
  }
}

print.lineplot = function(first_dat,second_dat, third_dat, fourth_dat, path, FUN, function.name,
                         plot.range=TRUE, exclude.list=c(), ...) {
  # print barplot of FUN(data) to file.
  # if any subject is contained in exclude.list, it will not be
  # counted towards the "bar" of the plot but rather be shown
  # as an outlier data point. If empty, subjects with a failure
  # to respond rate > 5% will be excluded.
  
  if(is.null(exclude.list)) {
    exclude.list = get.subj.to.exclude(first_dat)
  }
  if(!is.null(exclude.list)) {
    print("Warning: excluding subject(s)")
  }
  
  if(is.null(exclude.list)) {
    exclude.list = get.subj.to.exclude(second_dat)
  }
  if(!is.null(exclude.list)) {
    print("Warning: excluding subject(s)")
  }
  
  if(is.null(exclude.list)) {
    exclude.list = get.subj.to.exclude(third_dat)
  }
  if(!is.null(exclude.list)) {
    print("Warning: excluding subject(s)")
  }
  
  if(is.null(exclude.list)) {
    exclude.list = get.subj.to.exclude(fourth_dat)
  }
  if(!is.null(exclude.list)) {
    print("Warning: excluding subject(s)")
  }
  
  pdf(file=paste(path, "comparison", ".pdf", sep=""), 
      width=8, 
      height=6)
  get.lineplot(first_dat, second_dat, third_dat, fourth_dat, FUN, plot.range, exclude.list=exclude.list, ...)
  dev.off()
}

get.subj.to.exclude = function(dat, threshold=0.05) {
  # return list of subjects to exclude, if failure
  # to respond rate > treshold
  
  exclude.list = c()
  subjects = get.all.subjects(dat, avg.human.data = FALSE) #crashed with texture-filled-rotated because no networks are tested
  for(s in subjects) {
  #print(get.fail.rate(dat[dat$subj==s$data.name,])) nAN
    if(get.fail.rate(dat[dat$subj==s$data.name, ]) > threshold ) {
      exclude.list = c(exclude.list, s$data.name)
    }
  }
  return(exclude.list)
}