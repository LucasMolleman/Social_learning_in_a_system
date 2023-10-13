library(igraph)

# Function to create a rooted tree which represents a cultural system
generate_rooted_tree <- function(num_nodes, branching_factor) {
  g <- graph.empty(n = num_nodes, directed = TRUE)
  
  edgeList<-c()
  for (i in 1:(num_nodes-1)){
#	outDegreeThisNode<-1+floor(runif(1)*branching_factor)
#	outDegreeThisNode<-max(1, round(rnorm(1,mean=branching_factor,sd=1)))
	outDegreeThisNode<-branching_factor
	k<-i+1
	numConn<-0
	for (j in (i+1):num_nodes){
		if (numConn<outDegreeThisNode && !j%in%edgeList[c(FALSE,TRUE)]) {
			edgeList<-c(edgeList, i,j)
			numConn<-numConn+1
		}
	}
  }
  g<-add_edges(g, c(edgeList))
  return(g)
}


#### simulation parameters
#num_nodes <- 50			# size of the cultural systems (number of nodes)
N<-100						# population size
M<-10						# number of demonstrators
timesteps<-10000			# number of time steps in the simulation
replicateSimulations<-10	# number of simulations per parameter setting

S<-0.9 						# reliance on social learning; (1-S) is innovation rate
reset_rate<-0.01			# probability that an individual is replaced by a naive individual

# parameters for plotting (number of time steps per printing event frequencies)
windowSize<-1000
library(colorRamps)
num_colors <- num_nodes		
colramp <- colorRampPalette(c("white", "blue","green","orange", "red"))
color_palette <- colramp(num_colors)
color <- color_palette[1:num_nodes]



### define a summary matrix with success of learning strategies
## it records the number of nodes, the branching factor, the learning strategy, the simulation replicates, and the mean payoff
strategySuccess<-matrix(nrow=0, ncol=5)


### PARAMETERS ARE SET ###

# loop over social learning strategies:
## 0=random; 1=payoff-based; 2=similarity-based; 3=age-based
for (learningStrategy in c(0,1,2,3,4)){
#	dev.new(width=20, height=40)
	# define plotting areas for diagnostic simulation inspection
	par(mfrow=c(7,3), mar=c(1,1,1,1), las=1, xaxs='i', yaxs='i')
	
	# loop over branching and number of nodes
	for (num_nodes in c(15)){
		for (branching_factor in c(1,2,3,4,5,10,15)){
		
			### set up environment (cultural system)
			# for diagnostic plotting, we only show an example tree
			# for the actual simulations, we make a new tree in each replicate
			tree <- generate_rooted_tree(num_nodes, branching_factor)

			# plot the tree with the specified layout
			layout <- layout_as_tree(tree, root = 1, rootlevel = 0) # fix some plotting parameters
			nodeDepths<-rep(0,num_nodes)
			# calculate for each node its 'depth' (distance from root)
			for (i in 1:num_nodes){
				nodeDepths[i]<-length(shortest_paths(tree, from=1, to=i)$vpath[[1]])
			}
			nodeDepths
			
			## for diagnostic plotting: 
			## make colours according to depth
			V(tree)$color <- color_palette[nodeDepths]
			## plot the tree
			plot(tree, layout = layout, edge.arrow.size = 0.5, edge.color='black',
				vertex.color = V(tree)$color, vertex.label=NA)

			## bookkeeping for some plotting 
			summmeanTraitsInSystem<-rep(0,timesteps)
			summSLpay<-matrix(nrow=0, ncol=timesteps)
			
			### start simulation
			for (repl in 1:replicateSimulations){
				## show simulation progress on screen
				flush.console()
				print(paste('numNodes=', num_nodes, '  strat=', learningStrategy, '  branching_factor=', branching_factor, '  repl=', repl, sep=''))
			
				## bookkeep the frequency of traits across tree depths (for checking stability over time)
				traitsAtDepth<-matrix(nrow=0, ncol=max(nodeDepths))
				## bookkeeping for output
				SLpay<-rep(NA,timesteps)				## payoff for social learning
				meanTraitsInSystem<-rep(NA,timesteps) 	## number of traits in the system

				
				### define the cultural system ###
				
				## generate rooted tree to represent the cultural system
				tree <- generate_rooted_tree(num_nodes, branching_factor)
				## derive square matrix of parent/child traits
				adj_matrix <- as_adjacency_matrix(tree, sparse = FALSE)
				## root trait (at position 1,1) is its own parent
				adj_matrix[1,1]<-1
				
				## for some settings we need to know the depth of each node			
				nodeDepths<-c()
				for (i in 1:num_nodes){
					nodeDepths<-c(nodeDepths, length(shortest_paths(tree, from=1, to=i)$vpath[[1]]) )
				}
				nodeDepths
			
				# set payoffs for each trait
		#		payoffs<-rep(1,num_nodes)  	# fixed payoffs
				payoffs<-runif(num_nodes)	# random payoffs from uniform distribution
		#		payoffs<-runif(num_nodes) * nodeDepths # payoffs INcrease with depth
		#		payoffs<-runif(num_nodes) * (max(nodeDepths) - nodeDepths + 1) # payoffs DEcrease with depth
				payoffs<-payoffs/max(payoffs)  ## normalize so that the max is 1 (doesn't matter much)

				# some population parameters
				popS<-rep(S,N) ## initial reliance on social learning (currently fixed across the population)
				
				### SYSTEM AND NODE PAYOFFS ARE SET

				####### INITIALIZE POPULATION #####
				## start with empty repertoires (but fill them up in the next step)

				repertoires<-matrix(0, nrow=N, ncol=num_nodes)
				## everyone has the root trait of the cultural system
				repertoires[,1]<-1
						
				### initialize the population with repertoires (all connected to the tree root) 
				### with sizes drawn from a uniform distribution between 1:num_nodes
				for (ind in 1:N){
					numTraits<- sample(1:num_nodes,1)	# number of traits of this agent
					for (tr in 2:numTraits){			# add (randomly chosen) learnable traits
						unknownTraits<-which(repertoires[ind,]==0)
						# for which of these traits is the parent trait in the repertoire?
						# these are the traits currently 'learnable' to the individual
						learnableTraits<-c()
						for (trait in unknownTraits){
							parent<-which(adj_matrix[,trait]==1)
							if (repertoires[ind,parent]==1) learnableTraits<-c(learnableTraits,trait)
						}
						
						if (length(learnableTraits)==1) repertoires[ind,learnableTraits]<-1
						if (length(learnableTraits)>1){
							addTrait<-sample(learnableTraits,1)
							repertoires[ind,addTrait]<-1
						}
					}
				}
				
				## for age-based social learning, we need to assume initial ages. 
				## let's assume the age is proportional to the repertoire size
				ageScalar<-1		
				popAge<- rep(0,N)
				for (ind in 1:N) popAge[i]<-ageScalar * sum(repertoires[ind,])
		
				### population is now initialized... start running the model
				for (t in 1:timesteps){
					## sample a random individual
					ind<-sample(1:N,1)
					## will they learn individually or socially?
					r<-runif(1)
					
					if (r<popS[ind]) {  # social learning
						# which traits are currently not in the individual's repertoire
						unknownTraits<-which(repertoires[ind,]==0)
										
						# for which of these traits is the parent trait in the repertoire?
						# these are the traits currently 'learnable' to the individual
						learnableTraits<-c()
						for (trait in unknownTraits){
							parent<-which(adj_matrix[,trait]==1)
							if (repertoires[ind,parent]==1) learnableTraits<-c(learnableTraits,trait)
						}

						## sample M random other individuals
						pool<-1:N
						poolOthers<-pool[-ind] # agents do not sample themselves
						models<-sample(poolOthers, M, replace=FALSE)
						
						## randomly pick 1 trait from each model
						## only consider traits the learning agent do not know yet
						observedBehaviours<-c()
						observedModels<-c()
						for (model in models){
							newTraits<-c()
							for (k in 1:num_nodes){
								if (repertoires[model,k]==1 && repertoires[ind,k]==0) newTraits<-c(newTraits,k)
							}
							if (length(newTraits)>1) {
								tr<-sample(newTraits,1)
								observedBehaviours<-c(observedBehaviours, tr)
								observedModels<-c(observedModels, model)
							}
							if (length(newTraits)==1) {
								observedBehaviours<-c(observedBehaviours, newTraits)
								observedModels<-c(observedModels, model)
							}
						}
						
						## check if the vectors are filled as expected
						observedBehaviours
						observedModels
						
						## if there's no trait to learn among the observed ones, skip
						## SLpay was initialized at NA so nothing happens in that case
						## if there IS something to learn:
						if (length(observedBehaviours)>0){
							## list of weights of each observed behaviour
							## weights depend on learning strategy
							## normalized weights are used for choice
							wList<-c()
							selectedTrait<-c()	

							##### STRATEGY 1: payoff-based social learning #####
							
							if (learningStrategy==1){
								wList<-payoffs[observedBehaviours]/sum(payoffs[observedBehaviours])
							}
							##################
							
							###### STRATEGY 2: similarity based learning ######
							if (learningStrategy==2){
								## check for all agents how similar they are to self
								for (mod in observedModels){
									simToFocal<-0
									for (k in 1:num_nodes){ ## loop over all traits and sum similarity
										if (repertoires[ind,k]==repertoires[mod,k]) simToFocal<-simToFocal+1;
									}
									wList<-c(wList, simToFocal)
								}
							}
							##################	

							######	STRATEGY 3: age-based social learning #####				
							if (learningStrategy==3){
								## based on age similarity
								## check for all agents how similar they are to self
								for (mod in observedModels){
									w<- 10^-5
									ageDif<-popAge[mod]-popAge[ind]								
									if (ageDif >=0) w<- 0.5^ageDif
									wList<-c(wList, w)
								}			
							}	
							################
							
							######	STRATEGY 4: conformist social learning #####				
							if (learningStrategy==4){
								## count the selected behaviours and weigh common ones more
								for (mod in 1:length(observedBehaviours)){
									w<- length(which(observedBehaviours==observedBehaviours[mod]))
									wList<-c(wList, w)
								}			
							}
							
							################
													
							
							###### STRATEGY 0:  random learning benchmark
							if (learningStrategy==0){
								## select a trait you dont have at random
								wList<-rep(1,length(observedBehaviours))							
							}
							############
							
							### MAKE CHOICE ###
							## normalize the list of weights
							wList<-wList/sum(wList)						
							# normalize
							cSim<-cumsum(wList)
							r<-runif(1)
							
							selectedTrait<-c()
							for (k in 1:length(cSim)) {
								if (r<cSim[k]){
									selectedTrait<-observedBehaviours[k]
									break
								}
							}		
																					
							######## calculate payoffs of learning
							focalPay<-0
							if (selectedTrait%in%learnableTraits) {
								repertoires[ind,selectedTrait]<-1
								focalPay<-payoffs[selectedTrait]
							}
							SLpay[t]<-focalPay
						}
					}
					else {	# individual learning (=innovation)
						# get a random parent trait that has already been acquired
						parent<-sample(which(repertoires[ind,]==1),1)
						# select one of its child traits to acquire
						
						#list children
						children<-which(adj_matrix[parent,]==1)
						## only acquire child trait if this parent node has any children
						if (length(children)>0) {
							child<- sample(children,1)
						### add child to the individual's repertoire
							repertoires[ind,child]<-1			
						}
					}
				
					## each time step the agent was sampled, their age increases by 1
					popAge[ind]<-popAge[ind]+1
					
					## replace an individual with a naive one at random
					## NB this is not appropriate for evolutionary sims
					if (runif(1) < reset_rate) {
						repertoires[ind,]<-c(1,rep(0,num_nodes-1))
						popAge[ind]<-0  ## reset the age of the agent to 0
					}
					
					# BOOKKEEPING
					# number of traits in the system
					meanTraitsInSystem[t]<-sum(repertoires) / (num_nodes * N)
					# distribution of traits across the depths (layers) of the tree
					trDepth<-c()
					for (d in 1:max(nodeDepths)){
						x<-which(nodeDepths==d)
						thisDepth<-sum(repertoires[,x], na.rm=TRUE)
						trDepth<-c(trDepth, thisDepth)
					}
					traitsAtDepth<-rbind(traitsAtDepth, trDepth/sum(repertoires))
				}
				
				
				## do some plotting for inspecting the simulations
				if (repl==1) {plot(1:timesteps, SLpay, type='n', ylim=c(0,1), col='red', 
					axes=FALSE)
					axis(1, labels=F)
					axis(2)
					box()
				}
			
				## plot payoffs for social leraning for time windows (to smoothen)
				cols<-c("green", "orange", "red")
				for (w in 1:(timesteps/windowSize)){
					wi<-SLpay[1+((w-1)*windowSize):(w*windowSize)]			
					meanPayBin<- mean(wi, na.rm=TRUE)
					x<-(w-0.5)*windowSize
					points(x, meanPayBin, col=adjustcolor("blue", alpha=0.5), pch=16, cex=0.5)
				}
				
				
				## bookkeep for summary across simulation replicates
				summSLpay<-rbind(summSLpay,SLpay)
				summmeanTraitsInSystem<-summmeanTraitsInSystem+meanTraitsInSystem
				
				
				### add summary statistics to the overall master matrix
				##number of nodes, the branching factor, the learning strategy, the simulation replicates, and the mean payoff
				## characterize mean payoffs for a strategy as the last 10% of timesteps in the simulation
				summThisSimulation<-c(num_nodes, branching_factor, learningStrategy, 
					repl, mean(SLpay[round(timesteps*0.9):timesteps], na.rm=TRUE))
				
				strategySuccess<-rbind(strategySuccess, summThisSimulation)
				
			}
			
			## plot some summary statistics for this simulation setting
			summSuc<-c()
			for (w in 1:(timesteps/windowSize)){
				wi<-1+((w-1)*windowSize):(w*windowSize)	
				x<-c()
				for (k in wi) {
					if (k<=ncol(summSLpay)) x<-c(x,summSLpay[,k])
				}
				summSuc<-c(summSuc, mean(x,na.rm=TRUE))
			}
			
			## add lines to the plot with mean payoffs of social learning
			## and the number of traits in the system  (proportion of total)
			lines((1:(timesteps/windowSize))*windowSize, summSuc, col='black', lwd=2)
			lines(1:timesteps, summmeanTraitsInSystem/replicateSimulations, col='red', lwd=2)

			## set the colours for each layer
			V(tree)$color <- color_palette[nodeDepths]
			
			## add a plot for the distributions across layers
			plot(1:timesteps, traitsAtDepth[,1], type='n', ylim=c(0,1))
			## calculate the areas for plotting
			plotMatTraitDepth<-matrix(1,nrow=1,ncol=timesteps)
			for (i in 1:ncol(traitsAtDepth)){
				yy<- 0;
				for (j in 1:i) yy<-yy+traitsAtDepth[,j]
				plotMatTraitDepth<-rbind(plotMatTraitDepth, 1-yy)
			}
			## do the plotting
			for (i in 1:(nrow(plotMatTraitDepth)-1)){
				yy0<-plotMatTraitDepth[i,]
				yy1<-plotMatTraitDepth[i+1,]
				polygon(c(1:timesteps, timesteps:1), c(yy0, rev(yy1)), col=unique(V(tree)$color)[i])
			}
		}
	}
}

## give names to the columns of the simulation summary matrix: number of nodes, the branching factor, the learning strategy, the simulation replicates, and the mean payoff
strategySuccess<-data.frame(strategySuccess)
names(strategySuccess)<-c('numNodes', 'branching', 'learningStrat', 'repl', 'meanPayoff')
rownames(strategySuccess) <- NULL
strategySuccess

## what are the relevant x values?
xx<-unique(strategySuccess$branching)


## create an overview plot
par(mfrow=c(1,1), xaxs='r', mar=c(5,4,4,2),lend=1)
plot(1:max(xx), 1:max(xx)*0+1, type='n', ylim=c(0,0.7), xlim=c(1,max(xx)),
	xlab='Branching factor', ylab='Success of social learning strategy')
stratCols<-c('black', 'deepskyblue', 'firebrick', 'violet', 'forestgreen')


for (learnStrat in 0:4){
	b<-subset(strategySuccess, learningStrat==learnStrat)
	
	summM<-c()
	for (bf in unique(b$branching)){
#	for (nNodes in xx){
		d<-subset(b, branching==bf)
#		d<-subset(b, nNodes==numNodes)
#		for (k in 1:nrow(d)) points(bf-0.2+0.2*learnStrat, d$meanPayoff[k], pch=15+learnStrat, cex=0.8)
		
		m<-mean(d$meanPayoff)
		sd1<-sd(d$meanPayoff)
		summM<-c(summM,m)
#		if (bf==1 || bf==2 || bf%%5==0) {
		if (1==1) {
			x<-bf
#			x<-nNodes
#			arrows(x,m-sd1,x,m+sd1,code=0)
			points(x,m, pch=15+learnStrat, cex=1.5, col=stratCols[learnStrat+1])
		}
	}
#	lines(unique(b$branching), summM, col=stratCols[learnStrat+1])
	lines(xx, summM, col=stratCols[learnStrat+1])
}
legend('bottomright', c('random', 'payoff', 'similarity', 'age', 'conformity'), col=stratCols, pch=15:19)

