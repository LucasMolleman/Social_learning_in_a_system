library(igraph)

# Function to create a rooted tree
generate_rooted_tree <- function(num_nodes, branching_factor) {
  g <- graph.empty(n = num_nodes, directed = TRUE)
  
  edgeList<-c()
  for (i in 1:(num_nodes-1)){
	outDegreeThisNode<-1+floor(runif(1)*branching_factor)
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

num_nodes <- 12
## population size
N<-100
timesteps<-30000
LR<-0.3 ## some learning rate to adjust
S<-0.9 ## reliance on social learning

# for plotting (number of time steps per printing event frequencies)
windowSize<-300

# define colours for plotting
library(colorRamps)
num_colors <- num_nodes
colramp <- colorRampPalette(c("white", "blue","green", "red"))
color_palette <- colramp(num_colors)

par(mfrow=c(3,2), mar=c(1,1,1,1), las=1)

replicateSimulations<-5

for (br in c(1,5,100)){

	### set up environment (cultural system)
	branching_factor <- br

	## for plotting, we only show an example tree
	tree <- generate_rooted_tree(num_nodes, branching_factor)
	layout <- layout_as_tree(tree, root = 1, rootlevel = 0)
	# Plot the tree with the specified layout
	nodeDepths<-c()
	for (i in 1:num_nodes){
		nodeDepths<-c(nodeDepths, length(shortest_paths(tree, from=1, to=i)$vpath[[1]]) )
	}
	nodeDepths

	
	V(tree)$color <- color_palette[nodeDepths]

	plot(tree, layout = layout, edge.arrow.size = 0.5, edge.color='black',
		vertex.color = V(tree)$color, vertex.label=NA)
		
		
	### start simulation

	## bookkeeping: 
	summSLsuccess<-rep(0,timesteps)
	summmeanTraitsInSystem<-rep(0,timesteps)
	summKnewit<-rep(0, timesteps/windowSize)
	summCouldnt<-rep(0, timesteps/windowSize)
	summSuccess<-rep(0, timesteps/windowSize)
	
	for (repl in 1:replicateSimulations){

		## square matrix of parent/child traits
		tree <- generate_rooted_tree(num_nodes, branching_factor)
		
		adj_matrix <- as_adjacency_matrix(tree, sparse = FALSE)
		
		## for now I assume that the root trait (at position 1,1) is its own parent
		adj_matrix[1,1]<-1
		popS<-rep(S,N) ## initial reliance on social learning
		repertoires<-matrix(0, nrow=N, ncol=num_nodes)
		repertoires[,1]<-1
		
		## keep track of proportion successful learning events
		k<-0
		n<-0
		knewit<-0
		couldntlearn<-0
		learnEventList<-c()
		
		SLsuccess<-c()
		meanTraitsInSystem<-c()
		propKnewit<-c()
		propCouldntlearn<-c()
		
		learnEventList<-c()
		
		for (t in 1:timesteps){
			## sample a random individual
			ind<-sample(1:N,1)
			## will they learn individually or socially
			r<-runif(1)
			if (r<popS[ind]) {  # social learning
				## sample a random other individual
				model<-ind
				while (model==ind) model<-sample(1:N,1)
				## randomly choose a trait that the model expresses
				modelTrait<- sample(which(repertoires[model,]==1),1)
				
				## what is the parent trait of this model trait?
				parentOfModel<-which(adj_matrix[,modelTrait]==1)
				
				## check if the model trait is (1) not in the focal's repetoire yet and (2) if the focal has acquired its parent
				if (repertoires[ind,modelTrait]==1) {
					knewit<-knewit+1
					learnEventList<-c(learnEventList, 1)
					}
				if (!parentOfModel%in%repertoires[ind,] && repertoires[ind,modelTrait]==0) {
					couldntlearn<-couldntlearn+1
					learnEventList<-c(learnEventList, 2)
				}
				if (parentOfModel%in%repertoires[ind,]){
					if (repertoires[ind,modelTrait]==0) {
						repertoires[ind,modelTrait]<-1  ## successful social learning
						k<-k+1;

						learnEventList<-c(learnEventList, 3)						
					}
					
				}

				n<-n+1;
				
			}
			else {	# individual learning
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
				learnEventList<-c(learnEventList,0)
			}
			
			SLsuccess<-c(SLsuccess, k/n)
			meanTraitsInSystem<-c(meanTraitsInSystem, sum(repertoires)/ (N*num_nodes))
			propKnewit<-c(propKnewit, knewit/n)
			propCouldntlearn<-c(propCouldntlearn, couldntlearn/n)
			
		}
		if (repl==1) {plot(1:timesteps, SLsuccess, type='n', ylim=c(0,1), col='red', 
			axes=FALSE)
			axis(1, labels=F)
			axis(2)
			box()
		}
#		lines(1:timesteps, SLsuccess, col='pink', lwd=0.5)
#		lines(1:timesteps, meanTraitsInSystem, col='grey', lwd=0.5)
#		lines(1:timesteps, propKnewit, col='green', lwd=0.5)
#		lines(1:timesteps, propCouldntlearn, col='purple', lwd=0.5)
		
		## plot portions for time windows

		cols<-c("green", "orange", "red")
		for (w in 1:(timesteps/windowSize)){
			wi<-learnEventList[1+((w-1)*windowSize):(w*windowSize)]
			
			## already knew the trait
			knew<-length(which(wi==1))/windowSize
			couldnt<-length(which(wi==2))/windowSize
			success<- length(which(wi==3))/windowSize
			
			x<-(w-0.5)*windowSize
			points(x, knew*(1/S), col=adjustcolor("green", alpha=0.2), pch=16, cex=0.5)
			points(x, couldnt*(1/S), col=adjustcolor("purple", alpha=0.2), pch=16, cex=0.5)
			points(x, success*(1/S), col=adjustcolor("blue", alpha=0.2), pch=16, cex=0.5)
			
			summKnewit[w]<-summKnewit[w]+knew*(1/S)
			summCouldnt[w]<-summCouldnt[w]+couldnt*(1/S)
			summSuccess[w]<-summSuccess[w]+success*(1/S)
		}
		
		
		## bookkeep for summary across simulation replicates
		summSLsuccess<-summSLsuccess+SLsuccess
		summmeanTraitsInSystem<-summmeanTraitsInSystem+meanTraitsInSystem
	
	}
#	lines(1:timesteps, summSLsuccess/replicateSimulations, col='blue', lwd=2)
	lines(1:timesteps, summmeanTraitsInSystem/replicateSimulations, col='black', lwd=2)
	
	lines((0.5+1:(timesteps/windowSize))*windowSize, summKnewit/replicateSimulations, col="green", lwd=2)
	lines((0.5+1:(timesteps/windowSize))*windowSize, summCouldnt/replicateSimulations, col="purple", lwd=2)
	lines((0.5+1:(timesteps/windowSize))*windowSize, summSuccess/replicateSimulations, col="blue", lwd=2)

}
