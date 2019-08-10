# Author: Vihar Manchala

1) Creating Random Adjacency Matrices
#
# Script Name: adjMatrix
# Input: 
#		n... The number of vertices in the graph
#		p... Probablity two vertices are connected
#		plot... whether or not the matrix should be plotted as a graph
# Output: The nxn matrix of zero and ones
# Error Checking: The dimension is postive (else return NULL)
#
# Description: The matrix is related to a simple, undirected graph of n vertices.
#  In the graph is Vertex i and Vectex j are joined by an edge, then in the matrix
#   A[i,j] = 1, if no edge exists then A[i,j]=0.  There are differenct ways of handling 
#   the diagonal.  We will require the diagonal elements to be NA.
#
#   Whether or not an entry in the matrix is 0 or 1, is determined by drawing numbers
#   from the binomial distribution.  This distribution will yield a 1 with probability p and a 
#   o with probability (1-p).  In R it will work like this, in the ith column (vertex i)
#   , we require A[i,i] to be NA.  This leaves n-1 entries to be determined. We
#   use the command rbinom(n-1,1,p) to general these entries all at once, or
#   rbinom(1,1,p) to generate them one at a time.
#
# Plot: Using igraph create a simple plot. Use a color easy on the eyes such
#   as something found in the Brewer color palette (discussed in class)
#

require(igraph)
require(RColorBrewer)
adjMatrix<-function(n,p,plot)
{
  if(n>0)
  {
  A<-matrix(NA,nrow=n,ncol=n)
  j<-1
	for(i in seq(from=n-1, to=1, by=-1))
  {
    k<-j+1
    randomBinom<-rbinom(i,1,p)
    for(sinrandomBinom in randomBinom)
    {
      A[k,j]<-sinrandomBinom
      A[j,k]<-sinrandomBinom
      k<-k+1
    }
  j<-j+1
  }
  if(plot)
  {  
    colpal<-brewer.pal(8,"Dark2")
    graphOfA<-graph.adjacency(A, mode="undirected", diag=FALSE)
    V(graphOfA)$color<-colpal[5] 
    plot(graphOfA, layout=layout.auto, main="Adjacency Matrix")
  }
  return(A)
  }
  else
  {
    return(NULL)
  }
}

#============================================================================
2) Person with the most friends in the network

# Script Name: vowelMax
# Input: 
#        A... An adjacency matrix
#        plot... Plot the graph with the "friends" network highlighted
# Output: The number of the vertex with the most edge connections
# Error Checking: None
#
# Description: The number of "friends" a person has in a network is the 
#   number of edges connected to that vertex.  The ith person
#   in the network, will be connected to the jth person if A[i,j]=1.  So the 
#   number of 1's in the ith column reports how many edges connect person i to others
#   In mathematical terms, this is called the "degree" of vertex i. 
#
#   So you must determine the number of ones in each column and then find
#   which column has the most ones.
#
# Hint:  There is a quick trick to use here.  Since a column has only
#  0 or 1 in it, the sum of all of the numbers in the column is the same
#  as the number of 1's in the column.  Prove that to yourself. 
#
# Plot: Use igraph to plot the graph. Use three colors for the vertices: one
# for the most friendly person, another for his/her immediate friends, and another
#  for the remaining vertices. Color red the edges from the most friendly person to
#  his/her friends.  All remaining edges are black.
#

mostFriends<-function(A,plot)
{
  mostFriendly<-which.max(colSums(A,na.rm=TRUE))
  neFriends<-which(A[,mostFriendly]==1)
  
  if(plot)
  {  
    colpal<-brewer.pal(8,"YlOrRd")
    graphOfA<-graph.adjacency(A, mode="undirected", diag=FALSE)
    V(graphOfA)$color<-colpal[2]
    V(graphOfA)[mostFriendly]$color<-colpal[8] 
    E(graphOfA)$color<-"black"
    E(graphOfA)[mostFriendly%--%neFriends]$color<-"red"
    V(graphOfA)[neFriends]$color<-colpal[5] 
    plot(graphOfA, layout=layout.auto, main="Most Friendly")
  }
  return(mostFriendly)  
}



#============================================================================
3) Find a lonely person

# Script Name: noFriends
# Input: 
#        A... An adjacency matrix
# Output: The number of the vertex with the no edge connections
# Error Checking: None
#
# Description: The number of "friends" a person has in a network is the 
#   number of edges connected to that vertex.  The ith person
#   in the network, will be connected to the jth person if A[i,j]=1.  So the 
#   number of 1's in the ith column reports how many edges connect person i to others
#   In mathematical terms, this is called the "degree" of vertex i. 
#
#   So you must determine the number of ones in each column and then find
#   which column has the most ones.
#
# Hint:  There is a quick trick to use here.  Since a column has only
#  0 or 1 in it, the sum of all of the numbers in the column is the same
#  as the number of 1's in the column.  Prove that to yourself. 
#
# Plot: Create a plot of the graph using igraph.  Make the "lonely" person/people a
# different color
#

require(RColorBrewer)

noFriends<-function(A,plot)
{
  noFriend<-which(colSums(A,na.rm=TRUE)==0)
  if(plot)
  {
    colpal<-brewer.pal(8,"YlOrRd")
    graphOfA<-graph.adjacency(A, mode="undirected", diag=FALSE)
    V(graphOfA)$color<-colpal[2]
    V(graphOfA)[noFriend]$color<-colpal[8] 
    plot(graphOfA, layout=layout.auto, main="Lonely Person(s)")
  }
  return(noFriend)
}
#============================================================================
4) Number of friendship relationships in the network

# Script Name: numFriendships
# Input: 
#        A... An adjacency matrix
# Output: The number of uniqe edges in the graph 
#
# Description:  The number of friendships in the network is the number of edges
#    in the graph.  Basically, count the ones in the adjacency matrix, but be
#    careful.  (Remember, an edge from i to j is the same as an edge from j to 1)
#
#  
#

numFriendships<-function(A)
{
  return(sum(A,na.rm=TRUE)/2)
}

#============================================================================
5) Add a person to the friendship network

# Script Name: addPerson
# Input: 
#        A... An adjacency matrix
#		 p... Probability a relationship forms
#		 plot... Show the new person and relationships
# Output: New adjacency matrix
#
# Description:  As in creating an adjacency matrix, a new column is added
#  and the binomail distribution is used to determine the 1 entries.
#
# Plot: Plot the graph using igraph. Use three colors for the vertices: one
# for the newly added person, another for his/her immediate friends, and another
#  for the remaining vertices. Color red the edges from the new person to
#  his/her friends.  All remaining edges are black.
#
#  
#

addPerson<-function(A,p,plot)
{
  n<-nrow(A)
  A<-cbind(A,rbinom(n,1,p))
  A<-rbind(A,t(c(A[,n+1],NA)))
  if(plot)
  {
    newNodeFriends<-which(A[nrow(A),]==1)
    colpal<-brewer.pal(8,"YlOrRd")
    graphOfA<-graph.adjacency(A, mode="undirected", diag=FALSE)
    V(graphOfA)$color<-colpal[2]
    V(graphOfA)$color[newNodeFriends]<-colpal[5]
    V(graphOfA)[nrow(A)]$color<-colpal[8] 
    E(graphOfA)$color<-"black"
    E(graphOfA)[nrow(A)%--%newNodeFriends]$color="red"
    plot(graphOfA, layout=layout.auto, main="Adjacency Matrix after adding new node")
    
  }
    return(A)
}
#============================================================================
6) Add several people to the friendship network

# Script Name: growNetwork
# Input: 
#        A... An adjacency matrix
#		 p... Probability a relationship forms
#		 n... the number of people to add
#		 plot... Show the new person and relationships
# Output: New adjacency matrix
#
# Plot: Plot the graph using igraph. Use three colors for the vertices: one
# for the newly added person, another for his/her immediate friends, and another
#  for the remaining vertices. Color red the edges from the new person to
#  his/her friends.  All remaining edges are black.
#  
#
growNetwork<-function(A,n,p,plot=FALSE)
{
  iniN<-nrow(A)
  for(i in seq(from=iniN,to=iniN+n-1,by=1))
  {
    A<-cbind(A,rbinom(i,1,p))
    A<-rbind(A,t(c(A[,i+1],NA)))
  }
  if(plot)
  {
    colpal<-brewer.pal(8,"YlOrRd")
    graphOfA<-graph.adjacency(A, mode="undirected", diag=FALSE)
    lowerPointer<-nrow(A)-n+1
    newNodeFriendsMatrix<-NULL
    for(i in lowerPointer:nrow(A))
    {
      newNodeFriends<-which(A[i,]==1)
      newNodeFriendsMatrix<-c(newNodeFriendsMatrix,newNodeFriends)
    }
    V(graphOfA)$color<-colpal[2]
    V(graphOfA)$color[newNodeFriendsMatrix]<-colpal[5]
    V(graphOfA)[seq(from=lowerPointer,to=nrow(A))]$color<-colpal[8] 
    E(graphOfA)$color<-"black"
    E(graphOfA)[seq(from=lowerPointer,to=nrow(A))%--%newNodeFriends]$color="red"  
    
    labelName<-paste("Adjacency Matrix after adding",n,"new node(s)")
    plot(graphOfA, layout=layout.auto, main=labelName )
  }
  return(A)
}


