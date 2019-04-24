#!/usr/bin/Rscript
suppressPackageStartupMessages(library('tidyverse'))
suppressPackageStartupMessages(library('ggplot2'))
suppressPackageStartupMessages(library('tikzDevice'))
suppressPackageStartupMessages(library('xtable'))
suppressPackageStartupMessages(library('lemon'))
suppressPackageStartupMessages(library('knitr'))
suppressPackageStartupMessages(library('scales'))

# Read input data
# For timeouts, "States" field is set to -1
input <- read_delim('results.csv', delim=";", col_names=FALSE, trim_ws=TRUE, col_types="cccdiiiidi")
colnames(input) <- c("Model", "Dataset", "Solver", "Time", "Done", "Nodes", "Edges", "Priorities", "Solving", "Metric")

# Add "Id" column
input <- input %>% mutate(Id = paste(Model, Solver, sep = "-"))

# Split into <times> and <timeouts> and remove timeouts for which we have any times
times <- input %>% filter(Done != 0) %>% select(-Done)
timeouts <- input %>% filter(!Id %in% (times %>% distinct(Id))$Id) %>% select(-Done)

# Report data on input set
kable(input %>% filter(Nodes!=0) %>% group_by(Dataset,Priorities,Nodes,Edges) %>% distinct(Model) %>% group_by(Dataset) %>% summarize(n_distinct(Model), mean(Nodes), max(Nodes), mean(Edges), max(Edges), mean(Edges/Nodes), max(Edges/Nodes)))
kable(input %>% filter(Nodes!=0) %>% group_by(Dataset,Priorities,Nodes,Edges) %>% distinct(Model) %>% group_by(Dataset, Priorities) %>% summarize(n_distinct(Model), mean(Nodes), max(Nodes), mean(Edges), max(Edges), mean(Edges/Nodes), max(Edges/Nodes)))

kable(input %>% filter(Nodes!=0) %>% group_by(Dataset,Priorities,Nodes,Edges) %>% distinct(Model) %>% group_by(Dataset) %>% summarize(n_distinct(Model), mean(Nodes), max(Nodes), mean(Edges), max(Edges), mean(Edges/Nodes), max(Edges/Nodes)) %>% gather("k","v",-Dataset) %>% spread(Dataset, v),format="latex",booktabs=TRUE,linesep="",digits=2)
kable(input %>% filter(Nodes!=0) %>% group_by(Dataset,Priorities,Nodes,Edges) %>% distinct(Model) %>% group_by(Dataset) %>% summarize(n_distinct(Model), mean(Nodes), max(Nodes), mean(Edges), max(Edges), mean(Edges/Nodes), max(Edges/Nodes)), format="latex", booktabs=TRUE, linesep="")
kable(input %>% filter(Nodes!=0) %>% group_by(Dataset,Priorities,Nodes,Edges) %>% distinct(Model) %>% group_by(Dataset, Priorities) %>% summarize(n_distinct(Model), mean(Nodes), max(Nodes), mean(Edges), max(Edges), mean(Edges/Nodes), max(Edges/Nodes)) %>% group_by(Dataset,Priorities) %>% gather("k","v",-Dataset,-Priorities) %>% ungroup %>% mutate(DP=paste(Dataset,Priorities)) %>% select(-Dataset,-Priorities) %>% spread(DP, v), digits=2, format="latex", linesep="")

modelinfo <- input %>% filter(Priorities!=0, Nodes!=0) %>% group_by(Dataset,Priorities,Nodes,Edges) %>% distinct(Model)


# Compute median/mean/sd for times, and highest timeout for timeouts
times <- times %>% group_by(Id, Model, Dataset, Solver) %>% summarize(MedianTime = median(Time), MeanTime = mean(Time), sd = sd(Time)) %>% ungroup
timeouts <- timeouts %>% group_by(Id, Model, Dataset, Solver) %>% summarize(Timeout = max(Time)) %>% ungroup

# Compute Model-Order that are solved (or timeout) by all Method-Worker combinations
times_s <- times %>% select(Solver, Dataset, Model, Time=MedianTime)
timeouts_s <- timeouts %>% select(Solver, Dataset, Model, Time=Timeout)
MODone <- bind_rows(times_s, timeouts_s) %>% spread(Solver, Time) %>% drop_na() %>% pull(Model)
MOAll <- times_s %>% spread(Solver, Time) %>% drop_na() %>% pull(Model)
times %>% filter(Model %in% MOAll) %>% group_by(Dataset) %>% summarize(Count=n_distinct(Model))

MOAllmc <- times %>% filter(Model %in% MOAll, Dataset == "modelchecking") %>% pull(Model) %>% unique()
MOAlleq <- times %>% filter(Model %in% MOAll, Dataset == "equivchecking") %>% pull(Model) %>% unique()
MOAllsy <- times %>% filter(Model %in% MOAll, Dataset == "synt") %>% pull(Model) %>% unique()

# Compute Model-Order that are solved before timeout by all Method-Worker combinations
# MOLong1 <- times %>% filter(MedianTime>=1) %>% mutate(MW = paste(Method, Workers)) %>% select(MW, Model, Order, MedianTime) %>% spread(MW, MedianTime) %>% drop_na() %>% mutate(MO = paste(Model, Order)) %>% pull(MO)
# MOLong2 <- times %>% filter(MedianTime>=1 | Workers!=1) %>% mutate(MW = paste(Method, Workers)) %>% select(MW, Model, Order, MedianTime) %>% spread(MW, MedianTime) %>% drop_na() %>% mutate(MO = paste(Model, Order)) %>% pull(MO)
# MODoneByLDDandMDD <- bind_rows(times_s, timeouts_s) %>% filter(MW == "ldd-sat 1" | MW == "mdd-sat 1") %>% spread(MW, Time) %>% drop_na() %>% select(Model, Order) %>% mutate(MO = paste(Model, Order)) %>% pull(MO)
# MOAllByLDDandMDD <- times_s %>% filter(MW == "ldd-sat 1" | MW == "mdd-sat 1") %>% spread(MW, Time) %>% drop_na() %>% select(Model, Order) %>% mutate(MO = paste(Model, Order)) %>% pull(MO)

# MODone  are those MO where all solvers solve or timeout (no error)
# MOAll   are those MO where all solvers solve (no timeout/error)
# MOLong1 are those MO where all solvers solve and require more than 1 second
# MOLong2 are those MO where all solvers solve and require more than 1 second with 1 worker
# MODoneByLDDandMDD are those where ldd-sat 1 and mdd-sat 1 solve or timeout
# MOAllByLDDandMDD  are those where ldd-sat 1 and mdd-sat 1 solve (no timeout)

cat(sprintf("MODone (all solvers solve/timeout) contains %d Models.\n", length(MODone)))
cat(sprintf("MOAll (all solvers solve, no timeout) contains %d Models.\n", length(MOAll)))
cat(sprintf("MOAllmc contains %d Models.\n", length(MOAllmc)))
cat(sprintf("MOAlleq contains %d Models.\n", length(MOAlleq)))
cat(sprintf("MOAllsy contains %d Models.\n", length(MOAllsy)))




timesSummary <- times %>% filter(Model %in% MOAll) %>% group_by(Dataset, Solver) %>% summarize(SumMeanTime = sum(MeanTime)) %>% spread(Solver, SumMeanTime) %>% select("Dataset", "fpi","zlk","pp","tl","psi","fpi-n","zlk-n","pp-n","tl-n","psi-n")
kable(timesSummary)

timesSummary <- bind_rows(times_s, timeouts_s) %>% group_by(Dataset, Solver) %>% summarize(SumMeanTime = sum(Time)) %>% spread(Solver, SumMeanTime) %>% select("Dataset", "fpi","zlk","pp","tl","psi","fpi-n","zlk-n","pp-n","tl-n","psi-n")
kable(timesSummary,format="latex",linesep="",booktabs=TRUE)

kable(times %>% group_by(Solver, Dataset) %>% summarize(Time=sum(MeanTime)) %>% spread(Solver, Time) %>% select(Dataset,`fpi`,`fpi-1`,`fpi-2`,`fpi-4`) %>% mutate(s2=`fpi-1`/`fpi-2`,s4=`fpi-1`/`fpi-4`))

kable(head(times %>% group_by(Model, Dataset) %>% select(Model, Dataset, Solver, MeanTime) %>% spread(Solver, MeanTime) %>% select(Dataset,`fpi`,`fpi-1`,`fpi-2`,`fpi-4`) %>% mutate(s2=`fpi-1`/`fpi-2`,s4=`fpi-1`/`fpi-4`) %>% arrange(desc(s4)) %>% ungroup %>% left_join(modelinfo, by=c("Model","Dataset")), n=20))



# A beautiful cactus plot


# Helper function that adds cumulative sum of the given solver for time...
CalcCumSum <- function(s, slvr) {
    s = s[s$Solver==slvr,]
    s = s[order(s$Time),]
    s$cumsum <- 1:nrow(s)
    s
}

# Helper function that creates the plots...
Plot <- function(s, slvrs) {
    data <- data.frame(Set = character(0), Model = character(0), Solver = character(0), Time = numeric(0))
    for (slvr in slvrs) { data = rbind(data, CalcCumSum(s, slvr)) }
    ggplot(data, aes(y=Time,x=cumsum,color=Solver,shape=Solver)) +
        geom_point(size=3) + geom_line() +
        scale_shape_manual(values=1:length(slvrs)) +
        scale_y_continuous(name="Time (sec)") + scale_x_continuous(name="Model count") +
        theme_bw(base_size=16)
}

# Helper function to make the TIKZs
MakeTIKZ = function(s,w,h,t) {
    tikz(s, width=w, height=h, standAlone=F)
    print(t)
    graphics.off()
}

MakePNG = function(s,t) {
    png(s, width=1000, height=1000, res=100)
    print(t)
    graphics.off()
}



plot <- Plot(times %>% select(Set=Dataset,Model,Solver,Time=MeanTime), c("fpi","zlk","pp","tl","psi")) + coord_cartesian(ylim=c(0,30),xlim=c(600,750))
plot_eq <- Plot(times %>% filter(Dataset=="equivchecking") %>% select(Set=Dataset,Model,Solver,Time=MeanTime), c("fpi","zlk","pp","tl","psi")) + coord_cartesian(ylim=c(0,100),xlim=c(160, 220))
plot_eq_n <- Plot(times %>% filter(Dataset=="equivchecking") %>% select(Set=Dataset,Model,Solver,Time=MeanTime), c("fpi-n","zlk-n","pp-n","tl-n","psi-n")) + coord_cartesian(ylim=c(0,100),xlim=c(160, 220))
plot_mc <- Plot(times %>% filter(Dataset=="modelchecking") %>% select(Set=Dataset,Model,Solver,Time=MeanTime), c("fpi","zlk","pp","tl","psi")) + coord_cartesian(ylim=c(0,20),xlim=c(260, 320))
plot_mc_n <- Plot(times %>% filter(Dataset=="modelchecking") %>% select(Set=Dataset,Model,Solver,Time=MeanTime), c("fpi-n","zlk-n","pp-n","tl-n","psi-n")) + coord_cartesian(ylim=c(0,20),xlim=c(260, 320))
plot_sy <- Plot(times %>% filter(Dataset=="synt") %>% select(Set=Dataset,Model,Solver,Time=MeanTime), c("fpi","zlk","pp","tl","psi")) + coord_cartesian(ylim=c(0,25),xlim=c(190, 225))
plot_sy_n <- Plot(times %>% filter(Dataset=="synt") %>% select(Set=Dataset,Model,Solver,Time=MeanTime), c("fpi-n","zlk-n","pp-n","tl-n","psi-n")) + coord_cartesian(ylim=c(0,25),xlim=c(190,225))


MakeTIKZ("plot_eq.tex", 6, 3, plot_eq)
MakeTIKZ("plot_eq_n.tex", 6, 3, plot_eq_n)
MakeTIKZ("plot_mc.tex", 6, 3, plot_mc)
MakeTIKZ("plot_mc_n.tex", 6, 3, plot_mc_n)
MakeTIKZ("plot_sy.tex", 6, 3, plot_sy)
MakeTIKZ("plot_sy_n.tex", 6, 3, plot_sy_n)

MakePNG("plot_eq.png", plot_eq)
MakePNG("plot_eq_n.png", plot_eq_n)
MakePNG("plot_mc.png", plot_mc)
MakePNG("plot_mc_n.png", plot_mc_n)
MakePNG("plot_sy.png", plot_sy)
MakePNG("plot_sy_n.png", plot_sy_n)




# Strategy improvement plots
# psi_mc <- Plot(mc_no_odd, c("psi-n","psi-1-n","psi-8-n","parsi-seq","parsi-mc1","parsi-mc8","pgsi")) + coord_cartesian(ylim=c(0,150),xlim=c(150,320))
# psi_eq <- Plot(eq_no_odd, c("psi-n","psi-1-n","psi-8-n","parsi-seq","parsi-mc1","parsi-mc8","pgsi")) + coord_cartesian(ylim=c(0,400),xlim=c(100,180))
# psi_rn <- Plot(rn_no_odd, c("psi-n","psi-1-n","psi-8-n","parsi-seq","parsi-mc1","parsi-mc8","pgsi")) + coord_cartesian(ylim=c(0,500),xlim=c(200,280))
# psi_in <- Plot(in_no_odd, c("psi-n","psi-1-n","psi-8-n","parsi-seq","parsi-mc1","parsi-mc8","pgsi")) + coord_cartesian(ylim=c(0,600),xlim=c(500,760))



# A list of number of models per Solver

quit()
exit()







# cat(sprintf("MOLong1 (all solvers solve, >1 sec for all) contains %d Model-Order combinations.\n", length(MOLong1)))
# cat(sprintf("MOLong2 (all solvers solve, >1 sec for worker 1) contains %d Model-Order combinations.\n", length(MOLong2)))
# cat(sprintf("MODoneByLDDandMDD (ldd-sat-1 and mdd-sat solve/timeout) contains %d Model-Order combinations.\n", length(MODoneByLDDandMDD)))
# cat(sprintf("MOAllByLDDandMDD (ldd-sat-1 and mdd-sat solve, no timeout) contains %d Model-Order combinations.\n", length(MOAllByLDDandMDD)))
# cat(sprintf("\n"))

if(FALSE){
# Keep separately: 
cat(sprintf("We now consider the models where ldd-sat-1 and mdd-sat solve or timeout.\n"))
timesDoneByLDDandMDD <- times_s %>% mutate(MO = paste(Model, Order)) %>% filter(MO %in% MODoneByLDDandMDD) %>% select(-MO)
ModelOrders <- bind_rows(times,timeouts) %>% mutate(MO = paste(Model, Order)) %>% filter(MO %in% MODoneByLDDandMDD) %>% select(Model, Order) %>% distinct()
ForceCount <- ModelOrders %>% filter(Order == "rf") %>% nrow()
SloanCount <- ModelOrders %>% filter(Order == "rbs") %>% nrow()
cat(sprintf("There are %d models with the Force variable ordering.\n", ForceCount))
cat(sprintf("Within 20 minutes, Sylvan/LDD solves %d/%d Force models (with 1 core), Meddly solves %d/%d models.\n", timesDoneByLDDandMDD %>% filter(MW=="ldd-sat 1" & Order=="rf") %>% nrow(), ForceCount, timesDoneByLDDandMDD %>% filter(MW=="mdd-sat 1" & Order=="rf") %>% nrow(), ForceCount))
cat(sprintf("There are %d models with the Sloan variable ordering.\n", SloanCount))
cat(sprintf("Within 20 minutes, Sylvan/LDD solves %d/%d Sloan models (with 1 core), Meddly solves %d/%d models.\n", timesDoneByLDDandMDD %>% filter(MW=="ldd-sat 1" & Order=="rbs") %>% nrow(), SloanCount, timesDoneByLDDandMDD %>% filter(MW=="mdd-sat 1" & Order=="rbs") %>% nrow(), SloanCount))

# Now only keep the times/timeouts for which we have results of all Method-Worker combinations
# ie restrict times/timeouts to MODone
cat(sprintf("We now restrict the input to the models that solve or timeout for all solvers.\n"))
times <- times %>% mutate(MO = paste(Model, Order)) %>% filter(MO %in% MODone) %>% select(-MO)
timeouts <- timeouts %>% mutate(MO = paste(Model, Order)) %>% filter(MO %in% MODone) %>% select(-MO)
timesAll <- times %>% mutate(MO = paste(Model, Order)) %>% filter(MO %in% MOAll) %>% select(-MO)
ModelOrders <- bind_rows(times,timeouts) %>% mutate(MO = paste(Model, Order)) %>% filter(MO %in% MODone) %>% select(Model, Order) %>% distinct()
ForceCount <- ModelOrders %>% filter(Order == "rf") %>% nrow()
SloanCount <- ModelOrders %>% filter(Order == "rbs") %>% nrow()
cat(sprintf("There are %d models with the Force variable ordering.\n", ForceCount))
cat(sprintf("There are %d models with the Sloan variable ordering.\n", SloanCount))
}
###
# Time to print results
###

# Print number of solved models with any / 1 worker
cat("\nNow follows the number of solved models in MODone displaying for Force/Sloan together.\n")
cat("This is Table 2 in the paper.\n")
solvedMO <- times %>% mutate(MO=paste(Model,Order)) %>% select(MO,Method,Workers)
solvedMOSummary <- solvedMO %>% group_by(Method, Workers) %>% summarize(Count = n_distinct(MO)) %>% spread(Workers, Count) %>% left_join(solvedMO %>% group_by(Method) %>% summarize(Any = n_distinct(MO)))
solvedMOSummary <- solvedMOSummary %>% arrange(match(Method, c("otf-ldd-sat","ldd-sat","ldd-chaining","ldd-bfs","bdd-sat","mdd-sat")))
kable(solvedMOSummary)
kable(solvedMOSummary, format="latex", booktabs=TRUE, linesep="")

# Print number of solved models with any / 1 worker
cat("\nNow follows the number of solved models in MODone, for Force/Sloan seperately.\n")
cat("This is not in the paper.\n")
times_s <- times %>% mutate(MW = paste(Method, Workers)) %>% select(MW, Model, Order, Time=MedianTime)
timeouts_s <- timeouts %>% mutate(MW = paste(Method, Workers)) %>% select(MW, Model, Order, Time=Timeout)
solvedSummary <- times %>% group_by(Method, Order, Workers) %>% summarize(Count = n_distinct(Model)) %>% spread(Workers, Count) %>% left_join(times %>% group_by(Method, Order) %>% summarize(Any = n_distinct(Model))) %>% left_join(bind_rows(times_s, timeouts_s) %>% group_by(Order) %>% summarize(Max = n_distinct(Model)))
solvedSummary <- solvedSummary %>% ungroup() %>% mutate(Order=str_replace(Order, "rbs", "Sloan")) %>% mutate(Order=str_replace(Order, "rf", "Force")) %>% arrange(match(Order, c("Sloan","Force")), match(Method, c("otf-ldd-sat","ldd-sat","ldd-chaining","ldd-bfs","bdd-sat","mdd-sat")))
kable(solvedSummary)
kable(solvedSummary, format="latex", booktabs=TRUE, linesep="")

# Print summary sums...
cat("\nTo more accurately compare solving speed, we look at the no-timeout set.\n")
cat("Now follows the summary of times and speedups on the entire no-timeout set.\n")
cat("This is Table 3 in the paper.\n")

timesSummary <- timesAll %>% group_by(Method, Order, Workers) %>% summarize(SumMeanTime = sum(MeanTime)) %>% group_by(Method, Order) %>% spread(Workers, SumMeanTime) %>% mutate(s1=`1`/`1`, s2=`1`/`2`, s4=`1`/`4`, s8=`1`/`8`, s16 = `1`/`16`) %>% ungroup() %>% mutate(Order=str_replace(Order, "rbs", "Sloan")) %>% mutate(Order=str_replace(Order,"rf", "Force"))
timesSummary <- timesSummary %>% arrange(match(Order, c("Sloan", "Force")), match(Method, c("otf-ldd-sat","ldd-sat","ldd-chaining","ldd-bfs","bdd-sat","mdd-sat")))
kable(timesSummary %>% select(-s1), digits=c(0,0,0,0,0,0,0,1,1,1,1))
kable(timesSummary %>% select(-s1), digits=c(0,0,0,0,0,0,0,1,1,1,1), format="latex", booktabs=TRUE, linesep="")

# Some more data (not in Tables in the paper, but in the text)
cat("\nNow follows the average speedup (s2/s4/s8/s16) and the average speedup for models taking more than 1 second (t2/t4/t8/t16).\n")
cat("This is in the text of the paper, but not in a separate Table.\n")

timesTHING1 <- times %>% filter(MeanTime > 0) %>% select(Model, Order, Method, Workers, Time=MeanTime) %>% spread(Workers, Time) %>% drop_na() %>% gather(Workers, Time, -Model, -Order, -Method) %>% group_by(Method, Order, Workers) %>% spread(Workers, Time) %>% mutate(s2=`1`/`2`, s4=`1`/`4`, s8=`1`/`8`, s16 = `1`/`16`) %>% drop_na() %>% summarize(s2=mean(s2),s4=mean(s4),s8=mean(s8),s16=mean(s16))
timesTHING2 <- times %>% filter((Workers != 1 | MeanTime >= 1) & MeanTime > 0) %>% select(Model, Order, Method, Workers, Time=MeanTime) %>% spread(Workers, Time) %>% drop_na() %>% gather(Workers, Time, -Model, -Order, -Method) %>% group_by(Method, Order, Workers) %>% spread(Workers, Time) %>% mutate(s2=`1`/`2`, s4=`1`/`4`, s8=`1`/`8`, s16 = `1`/`16`) %>% drop_na() %>% summarize(t2=mean(s2),t4=mean(s4),t8=mean(s8),t16=mean(s16))
kable(timesTHING1 %>% left_join(timesTHING2) %>% ungroup() %>% mutate(Order=str_replace(Order, "rbs", "Sloan")) %>% mutate(Order=str_replace(Order, "rf", "Force")), digits=c(0,0,1,1,1,1,1,1,1,1))


# Get speedups of ALL times (using mean)
# times -> select mean -> spread -> remove rows without `1` -> compute speedups -> sort descending by speedup with 16 workers
cat("\nNow follows the top 20 speedups with ldd-sat\n")
Speedups <- times %>% select(Model,Order,Method,Workers,MeanTime) %>% spread(Workers,MeanTime) %>% filter(!is.na(`1`)) %>% mutate(s1=`1`/`1`, s2=`1`/`2`, s4=`1`/`4`, s8=`1`/`8`, s16 = `1`/`16`) %>% arrange(desc(s16))
kable(head(Speedups %>% filter(Method == "ldd-sat"), n=20))

# kable(Speedups)
# kable(head(Speedups %>% filter(Model == "BridgeAndVehicles-PT-V20P10N20"), n=20))
# kable(head(Speedups %>% filter(Model == "SmallOperatingSystem-PT-MT0128DC0064"), n=Inf))
# kable(head(Speedups %>% filter(Model == "CloudDeployment-PT-7a"), n=Inf))
# kable(head(Speedups %>% filter(Model == "Dekker-PT-015"), n=Inf))

# Helper function to make the TIKZs
MakeTIKZ = function(s,t) {
    tikz(s, width=6, height=3, standAlone=F)
    print(t)
    graphics.off()
}

MakePNG = function(s,t) {
    png(s, width=1000, height=1000, res=100)
    print(t)
    graphics.off()
}

###
# "violin" speedup plots
###

plotLDDSat <- ggplot(
    Speedups %>% filter(Method == "ldd-sat") %>% select(Model,Order,`2`=s2,`4`=s4,`8`=s8,`16`=s16) %>% gather(Workers, Speedup, -Model, -Order) %>% mutate(Workers = as.numeric(Workers)),
    aes(x=factor(Workers), y=Speedup)) +
    geom_violin(alpha=0.2) +
    theme_bw() + theme(legend.position="none") +
    coord_cartesian(ylim=c(0,8)) +
    scale_y_continuous(breaks=seq(0,20,by=1), name="Speedup") + 
    scale_x_discrete(name="Number of workers")

plotLDDChaining <- ggplot(
    Speedups %>% filter(Method == "ldd-chaining") %>% select(Model,Order,`2`=s2,`4`=s4,`8`=s8,`16`=s16) %>% gather(Workers, Speedup, -Model, -Order) %>% mutate(Workers = as.numeric(Workers)),
    aes(x=factor(Workers), y=Speedup)) +
    geom_violin(alpha=0.2) +
    theme_bw() + theme(legend.position="none") +
    coord_cartesian(ylim=c(0,8)) +
    scale_y_continuous(breaks=seq(0,20,by=1), name="Speedup") + 
    scale_x_discrete(name="Number of workers")

plotLDDPar <- ggplot(
    Speedups %>% filter(Method == "ldd-bfs") %>% select(Model,Order,`2`=s2,`4`=s4,`8`=s8,`16`=s16) %>% gather(Workers, Speedup, -Model, -Order) %>% mutate(Workers = as.numeric(Workers)),
    aes(x=factor(Workers), y=Speedup)) +
    geom_violin(alpha=0.2) +
    theme_bw() + theme(legend.position="none") +
    coord_cartesian(ylim=c(0,8)) +
    scale_y_continuous(breaks=seq(0,20,by=1), name="Speedup") + 
    scale_x_discrete(name="Number of workers")


MakeTIKZ("plot-ldd-sat.tex", plotLDDSat)
MakeTIKZ("plot-ldd-chaining.tex", plotLDDChaining)
MakeTIKZ("plot-ldd-bfs.tex", plotLDDPar)
MakePNG("plot-ldd-sat.png", plotLDDSat)
MakePNG("plot-ldd-chaining.png", plotLDDChaining)
MakePNG("plot-ldd-bfs.png", plotLDDPar)

###
# Make a scatter plot for ldd-sat, Time1 v Speedup16
###

LDDSAT_Time1_Speedup16 <- times %>% filter(Method == "ldd-sat") %>% select(Model, Order, Workers, Time=MeanTime) %>% spread(Workers, Time) %>% mutate(Speedup=`16`/`1`) %>% select(Time=`1`,Speedup) %>% drop_na()
LDDSAT_Time1_Speedup16plot <- ggplot(LDDSAT_Time1_Speedup16, aes(Time, Speedup)) + geom_point(size=0.5) + scale_x_log10(name="Time with 1 worker (sec)", breaks = trans_breaks("log10", function(x) 10^x, n=4)) + scale_y_continuous(name="Speedup with 16 workers") + theme_bw()

MakeTIKZ("plot-ldd-sat-time1-speedup16.tex", LDDSAT_Time1_Speedup16plot)
MakePNG("plot-ldd-sat-time1-speedup16.png", LDDSAT_Time1_Speedup16plot)

###
# Make a scatter plot for ldd-sat Time1 v mdd-sat Time1
###

LDDSAT1_MDDSAT1 <- times %>% filter(Workers == 1) %>% filter(Method == "ldd-sat" | Method == "mdd-sat") %>% select(Model, Order, Method, Time=MeanTime) %>% spread(Method, Time) %>% select(lddsat=`ldd-sat`, mddsat=`mdd-sat`) %>% drop_na()
LDDSAT1_MDDSAT1plot <-
    ggplot(LDDSAT1_MDDSAT1, aes(y=lddsat, x=mddsat)) +
    geom_point(size=0.5) +
    geom_abline(slope=1,intercept=0,linetype="dashed") +
    geom_abline(slope=1,intercept=1,linetype="dashed") +
    geom_abline(slope=1,intercept=-1,linetype="dashed") +
    scale_x_log10(name="Time with Meddly MDDs (sec)", limits=c(0.0001,1200)) +
    scale_y_log10(name="Time with Sylvan LDDs (sec)", limits=c(0.001,1200)) + 
    theme_bw()
MakeTIKZ("plot-ldd-sat-mdd-sat.tex", LDDSAT1_MDDSAT1plot)
MakePNG("plot-ldd-sat-mdd-sat.png", LDDSAT1_MDDSAT1plot)
