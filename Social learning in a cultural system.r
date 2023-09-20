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
N<-10
timesteps<-1000
LR<-0.3 ## some learning rate to adjust
S<-0.5 ## reliance on social learning

# define colours for plotting
library(colorRamps)
num_colors <- num_nodes
colramp <- colorRampPalette(c("white", "blue","green", "red"))
color_palette <- colramp(num_colors)

par(mfrow=c(3,2), mar=c(1,1,1,1))



for (br in c(1,2,50)){

	### set up environment (cultural system)
	branching_factor <- br

	tree <- generate_rooted_tree(num_nodes, branching_factor)
	layout <- layout_as_tree(tree, root = root_node, rootlevel = 0)
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
	## square matrix of parent/child traits
	adj_matrix <- as_adjacency_matrix(tree, sparse = FALSE)
	
	## for now I assume that the root trait (at position 1,1) is its own parent
	adj_matrix[1,1]<-1
	popS<-rep(S,N) ## initial reliance on social learning
	repertoires<-matrix(0, nrow=N, ncol=num_nodes)
	repertoires[,1]<-1
	
	## keep track of proportion successful learning events
	k<-0
	n<-0
	SLsuccess<-c()
	meanTraitsInSystem<-c()
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
			if (parentOfModel%in%repertoires[ind,]){
				if (repertoires[ind,modelTrait]==0) {
					repertoires[ind,modelTrait]<-1  ## successful social learning
					k<-k+1;		
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
		}
		
		SLsuccess<-c(SLsuccess, k/n)
		meanTraitsInSystem<-c(meanTraitsInSystem, sum(repertoires)/ (N*num_nodes))
	}
	plot(1:timesteps, SLsuccess, type='l', ylim=c(0,1), col='red')
	lines(1:timesteps, meanTraitsInSystem, col='black')
	
	
}


# Get the adjacency matrix
adj_matrix <- as_adjacency_matrix(tree, sparse = FALSE)
