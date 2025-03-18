/*
 * Author: Alex Zajichek
 * CS 340 Fall 2014
 * Topological Sorting
 */
public class AdjMatrixGraph extends DirectedGraph {

	int g[][];
	
	public AdjMatrixGraph(String filename) {
		
		super(filename);
		g = new int[numNodes][numNodes];
		buildGraph();
	}
	
	//Places a 1 in each cell in the matrix where an edge exists
	public void addEdge(int v1, int v2) {
		g[v1][v2] = 1;
	}
	
	public String topo() {
		String order = "";
		MyQueue<Integer> q = new MyQueue<Integer>();
		int[] inDegree = new int[numNodes];
		
		//Runs through the matrix and fills the 'inDegree' array with the total initial inDegree for each node
		for(int j = 0; j < numNodes; j++) {
			for(int i = 0; i < numNodes; i++) {
				inDegree[j] = inDegree[j] + g[i][j];
			}
		}
		
		//Scans through 'inDegree' and enqueues the in degrees that are initially 0, using the index of the node
		for(int i = 0; i < numNodes; i++) {
			if(inDegree[i] == 0) {
				q.enqueue(i);
				inDegree[i]--;
			 }
			}
		
		//If no in degrees are initially 0, there is no topological order
		if(q.empty()) {
			return "No Topological Order Found";
		}
		
			//Enters the queue, serves the contents, and looks in the matrix to see if there is an edge
			while(q.empty() == false) {
			int temp = q.serve();
			for(int n = 0; n < numNodes; n++) {
				if(g[temp][n] == 1) {
					//If there is an edge, it subtracts 1 from the in degree of that node
					inDegree[n]--;
					//If that in degree ends up being 0, it immediately adds it to the queue, and the process repeats
					if(inDegree[n] == 0) {
						q.enqueue(n);
						inDegree[n]--;
					}
			   }
			}
			//Each time a node finishes the full cycle, it is added to the order
			order = order + " " + vertexNames[temp] + ",";
	}
			//At the end of the ordering, if all nodes are not accounted for, there is no ordering
			for(int l = 0; l < numNodes; l++) {
				if(order.indexOf(vertexNames[l]) == -1) {
					return "No Topological Order Found";
				}
			}
			
		return order;
	
	  }
	}

