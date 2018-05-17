blockBoot<-function(d, num.boots = 30, block.len = 36, date.name = 'date'){
  #This function takes a vector of dates, d, and outputs a data.frame of dates * nboot * samp
  library('plyr'); library('dplyr')
  d = unique(d)
  d = d[order(d)]
  boots=ldply(seq(num.boots), function(x){
    draws=sample(seq(length(d)-block.len), size=ceiling(length(d)/block.len), replace=T)
    boot=do.call(c, llply(draws, function(l) d[seq(block.len) + l]))
    data.frame(boot, samp=x, nboot=seq(length(boot)))
  })
  d = data.frame(boot = d, samp=0, nboot=seq(length(d))) %>%
    rbind(boots)
  names(d)[which(names(d) == 'boot')] <- date.name
  d
}

bootSample <- function(d, boot.num = 30) {
  ld(seq(boot.num) - 1, function(l) {
    cbind(samp = l, sample_frac(d, size = 1, replace = l > 0))
  })
}
