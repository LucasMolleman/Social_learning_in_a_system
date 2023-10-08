library(igraph)

# Function to create a rooted tree
generate_rooted_tree <- function(num_nodes, branching_factor) {
  g <- graph.empty(n = num_nodes, directed = TRUE)
  
  edgeList<-c()
  for (i in 1:(num_nodes-1)){
#	outDegreeThisNode<-1+floor(runif(1)*branching_factor)
	outDegreeThisNode<-max(1, round(rnorm(1,mean=branching_factor,sd=1)))
#	outDegreeThisNode<-branching_factor
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

num_nodes <- 50
## population size
N<-100
### number of demonstrators
M<-10
timesteps<-10000
S<-0.9 ## reliance on social learning (1-S) is innovation rate

# probability that an individual is replaced by a naive individual
reset_rate<-0.01

# for plotting (number of time steps per printing event frequencies)
windowSize<-1000

# define colours for plotting
library(colorRamps)
num_colors <- num_nodes
colramp <- colorRampPalette(c("white", "blue","green","orange", "red"))
color_palette <- colramp(num_colors)
color <- color_palette[1:num_nodes]

# define plotting device settings
par(mfrow=c(3,3), mar=c(1,1,1,1), las=1, xaxs='i', yaxs='i')

replicateSimulations<-10

### summary matrix with success of learning strategies
## it records the number of nodes, the branching factor, the learning strategy, the simulation replicates, and the mean payoff
strategySuccess<-matrix(nrow=0, ncol=5)

## social learning strategies:
## 0=random; 1=payoff-based; 2=similarity-based
for (learningStrategy in c(0,1,2)){
	for (br in c(1,1.1,1.2,1.3,1.4,1.5,2,5,50)){

		### set up environment (cultural system)
		branching_factor <- br

		## for plotting, we only show an example tree
		tree <- generate_rooted_tree(num_nodes, branching_factor)
		layout <- layout_as_tree(tree, root = 1, rootlevel = 0)
		# plot the tree with the specified layout
		nodeDepths<-c()
		for (i in 1:num_nodes){
			nodeDepths<-c(nodeDepths, length(shortest_paths(tree, from=1, to=i)$vpath[[1]]) )
		}
		nodeDepths

		## make colours according to depth
		V(tree)$color <- color_palette[nodeDepths]
		## plot the tree
		plot(tree, layout = layout, edge.arrow.size = 0.5, edge.color='black',
			vertex.color = V(tree)$color, vertex.label=NA)
			
			
		### start simulation

		## bookkeeping for some plotting 
		summmeanTraitsInSystem<-rep(0,timesteps)
		summSLpay<-matrix(nrow=0, ncol=timesteps)

		for (repl in 1:replicateSimulations){
			## show simulation progress on screen
			flush.console()
			print(paste('strat=', learningStrategy, '  branching_factor=', br, '  repl=', repl, sep=''))
		
			## bookkeep the frequency of traits across tree depths (for checking stability over time)
			traitsAtDepth<-matrix(nrow=0, ncol=max(nodeDepths))

			## generate rooted tree to represent the cultural system
			tree <- generate_rooted_tree(num_nodes, branching_factor)
			## derive square matrix of parent/child traits
			adj_matrix <- as_adjacency_matrix(tree, sparse = FALSE)
			## for now I assume that the root trait (at position 1,1) is its own parent
			adj_matrix[1,1]<-1
			
			## assign payoffs to all cultural traits. 
			## for some settings we need to know the depth of each node			
			nodeDepths<-c()
			for (i in 1:num_nodes){
				nodeDepths<-c(nodeDepths, length(shortest_paths(tree, from=1, to=i)$vpath[[1]]) )
			}
			nodeDepths
		
			## randomly drawn from a uniform distribution
	#		payoffs<-rep(1,num_nodes)  	# fixed payoffs
			payoffs<-runif(num_nodes)	# random payoffs

	#		payoffs<-runif(num_nodes) * nodeDepths # payoffs INcrease with depth
	#		payoffs<-runif(num_nodes) * (max(nodeDepths) - nodeDepths + 1) # payoffs DEcrease with depth
			
			payoffs<-payoffs/max(payoffs)  ## normalize so that the max is 1

			## some population parameters
			popS<-rep(S,N) ## initial reliance on social learning (currently fixed across the population)
			
			## start with empty repertoires (but fill them up in the next step)
			repertoires<-matrix(0, nrow=N, ncol=num_nodes)
			## everyone has the root trait of the cultural system
			repertoires[,1]<-1
			
			## bookkeeping for output
			SLpay<-c()		## payoff for social learning
			meanTraitsInSystem<-c()  ## number of traits in the system
		
			### initialize the population with repertoires (all connected to the tree root) 
			### with sizes drawn from a uniform distribution between 1:num_nodes
			
			for (ind in 1:N){
				numTraits<- sample(1:num_nodes,1)
				for (tr in 2:numTraits){
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
					
					## pick 1 trait from each model
					## randomly choose a trait that the model expresses
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
					observedBehaviours
					observedModels
					
					
					if (length(observedBehaviours)==0){
						SLpay<-c(SLpay, NA)
					}
					else{			
						selectedTrait<-c()
						
						if (learningStrategy==1){
							##### payoff-based social learning #####
							payList<-payoffs[observedBehaviours]/sum(payoffs[observedBehaviours])
							cPay<-cumsum(payList)
							r<-runif(1)
							
							for (k in 1:length(cPay)) {
								if (r<cPay[k]){
									selectedTrait<-observedBehaviours[k]
									break
								}
							}
						}
						##################
						
						if (learningStrategy==2){
							###### similarity based learning ######
							similarityList<-c()
							## check for all agents how similar they are to self
							for (mod in observedModels){
								simToFocal<-0
								for (k in 1:num_nodes){
									if (repertoires[ind,k]==repertoires[mod,k]) simToFocal<-simToFocal+1;
								}
								similarityList<-c(similarityList, simToFocal)
							}

							## create a list and a cumulative list to select a model proportional to similarity
							simList<-similarityList/sum(similarityList)
							cSim<-cumsum(simList)
							r<-runif(1)
							
							selectedTrait<-c()
							for (k in 1:length(cSim)) {
								if (r<cSim[k]){
									selectedTrait<-observedBehaviours[k]
									break
								}
							}
						
						}
						#########################				
						
						### random learning benchmark
						if (learningStrategy==0){
							## select a trait you dont have at random
							selectedTrait<-sample(observedBehaviours,1)
							if (length(observedBehaviours)==1) selectedTrait<-observedBehaviours
							
						}
						############
						
						######## calculate payoffs of learning
						focalPay<-0
						if (selectedTrait%in%learnableTraits) {
							repertoires[ind,selectedTrait]<-1
							focalPay<-payoffs[selectedTrait]
						}
						SLpay<-c(SLpay, focalPay)
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
					SLpay<-c(SLpay, NA) # no payoff for social learning in this case
				}
			
				
				## replace an individual with a naive one at random
				## NB this is not appropriate for evolutionary sims
				if (runif(1) < reset_rate) repertoires[ind,]<-c(1,rep(0,num_nodes-1))
				
				# bookkeep
				meanTraitsInSystem<-c(meanTraitsInSystem, sum(repertoires) / (num_nodes * N))

				trDepth<-c()
				for (d in 1:max(nodeDepths)){
					x<-which(nodeDepths==d)
					thisDepth<-sum(repertoires[,x], na.rm=TRUE)
					trDepth<-c(trDepth, thisDepth)
				}
				traitsAtDepth<-rbind(traitsAtDepth, trDepth/sum(repertoires))
				
			}
			
			if (repl==1) {plot(1:timesteps, SLpay, type='n', ylim=c(0,1), col='red', 
				axes=FALSE)
				axis(1, labels=F)
				axis(2)
				box()
			}
		
			## plot portions for time windows
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
			
			
			### add summary statistics to the overall matrix
			##number of nodes, the branching factor, the learning strategy, the simulation replicates, and the mean payoff
			## characterize mean payoffs for a strategy as the last 10% of timesteps in the simulation
			summThisSimulation<-c(num_nodes, branching_factor, learningStrategy, 
				repl, mean(SLpay[round(timesteps*0.9):timesteps], na.rm=TRUE))
			
			strategySuccess<-rbind(strategySuccess, summThisSimulation)
			
		}
		
		summSuc<-c()
		for (w in 1:(timesteps/windowSize)){
			wi<-1+((w-1)*windowSize):(w*windowSize)	
			
			x<-c()
			for (k in wi) {
				if (k<=ncol(summSLpay)) x<-c(x,summSLpay[,k])
			}
			summSuc<-c(summSuc, mean(x,na.rm=TRUE))
		}
			
		lines((1:(timesteps/windowSize))*windowSize, summSuc, col='black', lwd=2)
		lines(1:timesteps, summmeanTraitsInSystem/replicateSimulations, col='red', lwd=2)

		V(tree)$color <- color_palette[nodeDepths]
		
		plot(1:timesteps, traitsAtDepth[,1], type='n', ylim=c(0,1))
		
		plotMatTraitDepth<-matrix(1,nrow=1,ncol=timesteps)
		
		for (i in 1:ncol(traitsAtDepth)){
			yy<- 0;
			for (j in 1:i) yy<-yy+traitsAtDepth[,j]
			plotMatTraitDepth<-rbind(plotMatTraitDepth, 1-yy)
		}
		
		for (i in 1:(nrow(plotMatTraitDepth)-1)){
			yy0<-plotMatTraitDepth[i,]
			yy1<-plotMatTraitDepth[i+1,]
			polygon(c(1:timesteps, timesteps:1), c(yy0, rev(yy1)), col=unique(V(tree)$color)[i])
		}
	}
	

}

##number of nodes, the branching factor, the learning strategy, the simulation replicates, and the mean payoff
strategySuccess<-data.frame(strategySuccess)
names(strategySuccess)<-c('numNodes', 'branching', 'learningStrat', 'repl', 'meanPayoff')
rownames(strategySuccess) <- NULL
strategySuccess

par(mfrow=c(1,1), xaxs='r', mar=c(5,4,4,2),lend=1)
plot(0:num_nodes, 0:num_nodes*0+1, type='n', ylim=c(0,1), xlim=c(1,2),
	xlab='branching factor', ylab='Success of social learning strategy')
stratCols<-c('black', 'blue', 'red')

for (learnStrat in 0:2){
	b<-subset(strategySuccess, learningStrat==learnStrat)
	
	summM<-c()
	for (bf in unique(b$branching)){
		d<-subset(b, branching==bf)
		
#		for (k in 1:nrow(d)) points(bf-0.2+0.2*learnStrat, d$meanPayoff[k], pch=15+learnStrat, cex=0.8)
		
		m<-mean(d$meanPayoff)
		sd1<-sd(d$meanPayoff)
		summM<-c(summM,m)
		arrows(bf,m-sd1,bf,m+sd1,code=0)
		points(bf,m, pch=15+learnStrat, cex=1.5, col=stratCols[learnStrat+1])
	}
	lines(unique(b$branching), summM, col=stratCols[learnStrat+1])
}
legend('topleft', c('random', 'payoff', 'similarity'), col=stratCols, pch=15:17)

