library(neuRosim)
library(fmri)
library(HH)
# New script for kyle
# milkshake: a milkshake-paired cue (2 sec), followed by a blank screen, and
# the paired milkshake will then be delivered (4 sec; 3mL).
# followed by a waiting period (2 secs) (blank),
# a rinse with the neutral solution (2 sec; 0.7mL),
# a variable delay (4-11 secs; mean 5 secs).
# h20: tasteless-paired 2 sec, blank
# tasteless admin (4 sec; 3mL)
# a variable delay (4-11 secs; mean 5 secs).
# Each run contains 32 randomly presented blocks, consisting of 16 blocks of each taste-cue.

# Prediction error (same stimulus timing and volumes; 30 events/run)
# In 60% of the blocks (18 of 30), the visual cue associated with the palatable milkshake will be paired milkshake taste, followed by the rinse sequence.
# In the other 40% (12 of 30) of blocks, the milkshake cue will be followed by receipt of the tasteless solution, creating a negative prediction error event (no rinse). (no paired-tasteless cue-taste in these runs)

## ------------------------------------------------------------------------

#############################
n.loop = 50000

###make sure these match the script
delivery_time=4.0
cue_time=2.0
wait_time=2.0
rinse_time=2.0
runin_time = 8.0 

water<-rep(0,16)
sweet<-rep(1,16)
all<-c(water, sweet)
ntrials.total = 32


dur = rep(delivery_time, ntrials.total)#length of stimulus of interest
#min and max jitter values possible
min=4
max=11

#everything not jitter and not of interest (cue+wait+rinse)
iti_inital_sweet = cue_time+wait_time+rinse_time
iti_inital_water = cue_time+wait_time

iti_hard_sweet = iti_inital_sweet+max #if using a random iti, you need to include this and the onsets into the loop
iti_hard_water = iti_inital_miss+max #if using a random iti, you need to include this and the onsets into the loop

iti_hard_comb_intm = iti_hard_sweet*all
iti_hard_comb_intm = replace(iti_hard_comb_intm, iti_hard_comb_intm==0, iti_hard_miss)

mockons.all = cumsum(c(0,dur+iti_hard_comb_intm))

mockons.all = mockons.all[1:(ntrials.total)]


run.length = max(mockons.all) 
run.length

tr = 1
eff.size = 1

# Things I'd like to save
eff.val = rep(0, n.loop) #efficiency
desmats = array(0,c(run.length, 3,  n.loop)) #all the model data
ons.save = array(0,c(ntrials.total,3, n.loop)) #the onsets

Sys.time()->start;
for (i in 1:n.loop){
  keep.looking = 1
  while (keep.looking == 1){#creating onsets
    trial.type = sample(all)
    length.repeats = rle(trial.type)$lengths 
    keep.looking = max(length.repeats) > 2
  }
  
 
  iti_hard_comb_intm = iti_inital_sweet*trial.type
  iti_hard_comb_intm = replace(iti_hard_comb_intm, iti_hard_comb_intm==0, iti_inital_water)
  
  ons.all = cumsum(c(0,dur+iti_hard_comb_intm))
  ons.all = ons.all[1:(ntrials.total)]
  
  
  
  iti.uni = runif(1000, min, max) #randomly generate 1000 numbers between 1 and 9, mean is 5 this is the jitter

  vr <- c(0) ### an empty vector for jitter
  jit<-c(0)
  for (j in 1:length(iti_hard_comb_intm)) {#generating the jitter, randomly selecting a number from my distribution above
    jitter<-sample(iti.uni, 1)
    jit[j]<-round(jitter,0) #rounding jitter and adding it to a vector
    vr[j] <- iti_hard_comb_intm[j]+jit[j] # adding it to my non-random interval and then vector
  }
  
  ons.all = cumsum(c(0,vr))
  ons.all = ons.all[1:(ntrials.total)]
  
  
  
  #taking the onsets and making them simulated activation
  sweet = specifydesign(ons.all[trial.type == 1], dur[trial.type == 1],
                        tail(ons.all, n=1), tr,
                        eff.size, conv = "double-gamma")
  
  water = specifydesign(ons.all[trial.type == 0], dur[trial.type == 0],
                       tail(ons.all, n=1), tr,
                       eff.size, conv = "double-gamma")
  
  #save the simulated activation in a array (TRxContrast)
  des.mat = cbind(rep(1, length(sweet)), water, sweet)
  #ons.save= cbind(ons.save,trial.type)
  #ons.save[,,i]=
  # making the contrast this is sweet>water
  con = c(0, -1, 1) 
  #solving for the efficiency matrix
  eff.val[i] = 1/(t(con)%*%solve(t(des.mat)%*%des.mat)%*%con)
  ons.save[,,i]=c(ons.all,jit,trial.type)
  #creating an array adding des.mat to the efficiency matrix
  X = array('NA',c(dim(desmats)[1] - dim(des.mat)[1], 3)) # this is to get the matrix the same size as the desmats, need this because the lack of rinse and the jitter can make them different
  Y = rbind(des.mat, X)
  desmats[,,i] = Y
}
print(Sys.time()-start)

# Plot design matrices with best and worst efficiencies
par(mfrow = c(2, 1), mar = c(4, 3, 2, 1))
# finding the most eff matrix to plot all of the columns simulated activation for each TR
water.best = desmats[,2,which(eff.val == max(eff.val))]
water.best <- water.best[!is.na(water.best)]

sweet.best = desmats[,3,which(eff.val == max(eff.val))]
sweet.best <- sweet.best[!is.na(sweet.best)]

plot(water.best, type = 'l', lwd = 2, col = 'red', xlab = "TR", 
     ylab = '', ylim = c(min(c(water.best, sweet.best)), max(c(water.best, sweet.best))),
     main = "Highest Efficiency")
lines(sweet.best, lwd = 2, col = 'cyan')

water.worst = desmats[,2,which(eff.val == min(eff.val), arr.ind = TRUE)]
sweet.worst = desmats[,3,which(eff.val == min(eff.val))]

plot(water.worst, type = 'l', lwd = 2, col = 'red', xlab = "TR", 
     ylab = '', ylim = c(min(c(water.worst, sweet.worst)), 1.3),
     main = "Lowest Efficiency")
lines(sweet.worst, lwd = 2, col = 'cyan')
legend('topleft', c("sweet 1", "water 0"), col = c("cyan", "red"), lwd = c(2,1), bty = 'n')

#desmats TRxcontrast_activationxloops
#there are n.loops of matrices, with timepoint rows and contrast columns
#trying to back out which onsets are associated with the most efficient design
#order the efficiency, the last one (highest efficiency) is what you want
ord.eff = order(eff.val)
#most efficient
best = tail(ord.eff, 1)
ons.save[,,best]
# Omit the intercept
#diag(solve(cor(desmats[,3:4,best])))
#VIF
fake.data = rnorm(length(water))
mod.fake = lm(fake.data ~ water.best + sweet.best)
vif(mod.fake)

write.table(ons.save[,,best][,1], "/Users/gracer/Documents/Bev_Task_reup/onset_files/train/onsets_run01", row.names = F,col.names = F, sep="\t")
write.table(ons.save[,,best][,2], "/Users/gracer/Documents/Bev_Task_reup/onset_files/train/jitter_run01", row.names = F, col.names = F, sep="\t")
write.table(ons.save[,,best][,3], "/Users/gracer/Documents/Bev_Task_reup/onset_files/train/conds_run01", row.names = F, col.names = F, sep="\t")
