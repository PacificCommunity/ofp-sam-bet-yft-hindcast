# Run from a dedicated projection working directory.
# MFCL intermediate files are written to the current directory.
yft_full_grid <- file.path("data", "yft", "grid", "full")
yft_template <- file.path("data", "yft", "template", "mix2")

# yft hindcast

#-------------------------------------------------------------------------------
# 02/04/24 - try to make indices fisheries projecting ON CATCH
# issues: the indices fisheries won't projecting on assign the catch 1! 
# solution (temporary) is to change fish flag 92,94 for indices fisheries to make them "non-indices" Line 157,158
# issues: can't force mfcl to use simulation recuritment in simulate number at age 
# solution: age flag 182 set to 0, but able to turn off parest flag 239 with age flag 183 to 0 
# everything else should be consistent with Rob's nuclear grid run (running everything on catch)
#-------------------------------------------------------------------------------
library(FLCore)
library(FLR4MFCL) # Use the latest one
library(dplyr)
library(tidyr)
library(ggplot2)

mfcl_version <- "mfclo64"  # must be available on PATH
## director for retrospective 
sadir <- file.path("data", "yft", "grid_result", "mix2")
model_name <-list.dirs(sadir,recursive = FALSE,full.names = FALSE)

for (model in model_name){
# Read IN full frq file
fullfrq <- read.MFCLFrq(file.path(yft_full_grid, model, "yft.frq"))
# read in real rep
realrep <- read.MFCLRep(file.path(yft_full_grid, model, "plot-final.par.rep"))
dir.create(file.path("result", "fix_rec", model))
for (yr in c(2014:2020)){
## 1. READ IN retrospective INPUT FILES
frq <- read.MFCLFrq(file.path(yft_template, yr, "yft.frq"))
ini <- read.MFCLIni(file.path(yft_template, yr, "yft.ini"))
tag <- read.MFCLTag(file.path(yft_template, yr, "yft.tag"))
par <- read.MFCLPar(paste(sadir,model, yr,"12.par",sep="/"), first.yr = range(frq)['minyear'])

obs_rec <-t(drop(qts(log(popN(realrep)[1,as.character((yr+1):2021)]))))

# Projection set up
nsims <- 1 # per OM
simyear <- 2 # running to 2021
firstyear <- 1952
firstsimyear <- range(frq)['maxyear']+1 # 2022
totyr <- (firstsimyear+simyear-firstyear)
numreg <- 5        # 9 region model
numyear <- firstsimyear - firstyear
nfish <- 37        # 9 region model, 9 index fisheries
nages <- 40        # 40 age classes


# BASELINE
# Years to average catch/effort over to generate future catches
avyrs <- as.character(yr)     # use 2017 as an example
projcontrols <- data.frame(
  name= c("LL.ALL.1",   "LL.ALL.2",     "LL.US.1",  "LL.ALL.4",  "LL.OS.4",    "LL.OS.2",      "LL.ALL.2",     "LL.ALL.3",    
         "LL.ALL.4",   "LL.AU.5",      "LL.ALL.5", "LL.ALL.5",  "PS.ASSOC.4a", "PS.UNASSOC.4", "PS.ASSOC.4b",   "PS.UNASSOC.4",
         "Dom.PH.2",   "HL.IDPH.2",    "PS.JP.1",  "PL.JP.1" ,  "PL.ALL.4",   "PL.ALL.3",     "Dom.ID.2",     "PS.IDPH.2",   
         "PS.ASSOC.3", "PS.UNASSOC.3", "LL.AU.5",  "PL.ALL.2",  "LL.ALL.5",   "PS.ASSOC.2",   "PS.UNASSOC.2", "Dom.VN.2",    
         "Index.1",    "Index.2",      "Index.3",  "Index.4" ,  "Index.5"),
         region=c(1, 1, 1, 4, 4, 2, 2, 3, 4, 5, 5, 5, 4, 4, 4, 4, 2, 2, 1, 1, 4, 3, 2, 2, 3, 3, 5, 2, 5, 2, 2, 2, 1:5),
         caeff = c(rep(1, n_fisheries(frq))), 
         scaler=as.vector(rep(1,nfish)),  
         ess_length   = NA, ess_weight = NA) # don't set the ESS yet - but set separately for length and weight.

projCtrl <- MFCLprojControl(nyears=simyear, nsims=nsims, avyrs=avyrs, fprojyr=firstsimyear, controls=projcontrols)
projfrq  <- generate(frq, projCtrl)
# swap the projfrq after 2019 with real catch 
freq(projfrq)  <- rbind(subset(freq(projfrq), year<=yr), subset(freq(fullfrq), year>yr))
lf_range(projfrq)['Datasets'] <- nrow(realisations(projfrq))
 
#-------------------------------------------------------------------------------


## 4. GENERATE PROJPAR
write(projfrq,  "proj.frq")
write(tag,      "proj.tag")
write(ini,      "proj.ini")

# Make 00 par

system(paste(mfcl_version, "proj.frq proj.ini 00.par -makepar"))
zero.par <- read.MFCLPar("00.par", first.yr = range(frq)['minyear'])
projpar <- generate(par, zero.par, projfrq)

# Create stochastic projection inputs (simyears, simulated_numbers_at_age)
flagval(projpar, 1,  1) <- nsims
flagval(projpar, 2, 96) <- 2                         # tag pooling
flagval(projpar, 1, c(186, 189)) <- 1                # turn off output files
flagval(projpar, 2, c(148, 155)) <- c(20, 4)         # periods for average F calculation
flagval(projpar, 2, 161) <- 1                        # SRR log-normal bias correction
flagval(projpar, -(1:32), 55) <- 1                   # impact analysis - on

# Recruitment options
flagval(projpar, 2, c(199,200)) <- c(212,6)
flagval(projpar, 1, c(232,233)) <- c(4, 274-(2021-yr)*4)        # srr range set to period over which SRR estimated (1962 to 2020.5)
##flagval(projpar, 1, 239) <- 1                        # stochastic recruitment deviates - on
## to turn off 239 has to turn age flag 183 together 
flagval(projpar, 1,239) <-1
flagval(projpar, 2, 182) <- 0                        # how SRR fitted - if 183 = 1...
flagval(projpar, 2,183) <- 0
# Set up stochastic projection inputs
# ** Rob has already Hessian 7 step - proj,dep is in the results folder - could just use that instead of running Hessian 7 here **
# Hessian option 7
flagval(projpar, 1, 145) <- 7                        # hessian option 7
flagval(projpar, 2, 20)  <- nsims                    # number of sims, but doesn't really affect step 7
write(projpar, 'proj7.par')
# COPY OVER yft.age_length to testdir        
# file.copy(from = paste0(sadir, "yft.age_length"), to=paste0( "proj.age_length"))
        
runmfcl <- system(paste(mfcl_version, "proj.frq proj7.par ttt"))

# Hessian option 8 (set parest flags 234 and 237 to 0 to turn off hessian inversion)
flagval(projpar, 1, 145) <- 8                        # hessian option 8
flagval(projpar, 1, 234) <- 0                  # activate the randomised effort deviates
flagval(projpar, 1, 237) <- 0                  # simulate random numbers at age in the 1st projections
flagval(projpar, 1, 241) <- 0                        # generate pseudo observations
flagval(projpar, 1, 238) <- 0                        # stochastic recruitment - off (which = 1)
flagval(projpar, 1, 239) <- 0                        # stochastic recruitment deviates on SRR - off - not using simyears! ; 209=1 use simyear

write(projpar,  'proj8.par')
        
runmfcl <- system(paste(mfcl_version, "proj.frq proj8.par ttt"))

# Generates simulated numbers etc 
# Copy files to new version with 'all' to ensure not overwritten
file.copy(from="simulated_numbers_at_age",to= "simulated_numbers_at_age_all",overwrite=T)
file.copy(from="simulated_numbers_at_age_noeff",to="simulated_numbers_at_age_noeff_all",overwrite=T)
file.copy(from="simyears",to="simyears_all",overwrite=T)

simyears_all                       <- readLines("simyears_all")
simulated_numbers_at_age_all       <- readLines('simulated_numbers_at_age_all')
simulated_numbers_at_age_noeff_all <- readLines('simulated_numbers_at_age_noeff_all')

writeLines(simulated_numbers_at_age_all[1:8], 'simulated_numbers_at_age')
cat(t(obs_rec), file='simulated_numbers_at_age', append=T)

writeLines(simulated_numbers_at_age_noeff_all[1:8], 'simulated_numbers_at_age_noeff')
cat(t(obs_rec), file='simulated_numbers_at_age_noeff', append=T)


# Final settings for the projection
flagval(projpar, 1, 145)        <- 0
flagval(projpar, 1, c(187,188)) <- 0
flagval(projpar, 1, 241)        <- 1    # generate pseudo obs  
flagval(projpar, 1, 242)        <- 0    # generate pseudo obs for historical period (parest 241 must = 1)   
flagval(projpar, 1, 244) <- 1 # Generate effort_sim_true - uses flag 1 242 so that historical effort sim true?
flagval(projpar, 2, 186)        <- 0.0   # No obs error sd effort 
flagval(projpar, 2, 187)        <- 0.0   # No obs error sd catch
#flagval(projpar, 2, 96)         <- 350
flagval(projpar, 2, 20)         <- 1 # Number of iterations
#flagval(projpar, 2, 20)         <- 6 # Number of iterations
flagval(projpar,1,239) <- 1 
# change fish flag setting to resign indices fisheries 33 to 41 to normal fisheries
## flagval(projpar,c(-37:-33),92) <- 0
## flagval(projpar,c(-37:-33),94) <- 0
##need to rethink this flag (this now group it with fisheries 1)
##flagval(projpar,c(-41:-33),99) <- 1

# Need to set up a fake pseudo tag release - even if not wanting to generate pseudo tag data
tag_release <- data.frame(region = 3, year=2020, month=5, fishery=13, n=1500)
tagproj <- MFCLTagProj(ptd=tag_release, reprate=array(0.6, dim=c(n_fisheries(frq), nrow(tag_release))))

itn<-1
# Even though we have set sd for 0, it's good practice to control the seed
extra.args   <- paste("-effort_seed ", itn, " -catch_seed ", itn, sep="") 
# Write stuff out
write(tag, 'proj.tag')
write(tagproj, 'proj.tag_sim') # future pseudo tag stuff not wanted but we need to include this file
write(projfrq, 'proj.frq')
write(projpar,  "proj.par")
    
system(paste0(mfcl_version, " proj.frq proj.par pseudo.par -switch ", extra.args))   


# Read outputs

file.copy(from="plot-pseudo.par.rep", to=file.path("result", "fix_rec", model, paste0(yr,".rep")))
file.copy(from="proj.par", to=file.path("result", "fix_rec", model, paste0(yr,".par")))
pseudo    <- read.MFCLPseudo(catch='catch_sim', effort='effort_sim',  lw_sim='test_lw_sim', 
                             projfrq=projfrq, ctrl=projCtrl, historical=FALSE)

save(pseudo,file=file.path("result", "fix_rec", model, paste0(yr,"_pseudo.Rdata")))
file.remove("simyears")
file.remove("simyears_all")
file.remove("simulated_numbers_at_age")
file.remove("simulated_numbers_at_age_all")
file.remove("simulated_numbers_at_age_noeff")
file.remove("simulated_numbers_at_age_noeff_all")

}

}
