/*
 * Author: Alex Zajichek
 * CS 340 Fall 2014
 * Topological Sorting
 */
import java.util.ArrayList;

public class AdjListGraph extends DirectedGraph {

	ArrayList<DoubleLinkedList<Integer>> g;
	
	public AdjListGraph(String filename) {
		super(filename);
		g = new ArrayList<DoubleLinkedList<Integer>>();
		for(int i = 0; i < numNodes; i++)
			g.add(new DoubleLinkedList<Integer>());
		buildGraph();
	}
	
	//Adds an edge based on the file information
	public void addEdge(int v1, int v2) {
		g.get(v1).insertLast(v2);
	}
	
	public String topo() {
		String order = "";
		MyQueue<Integer> q = new MyQueue<Integer>();
		int[] inDegree = new int[numNodes];
		
		//Constructs the 'inDegree' array, by summing the initial total in degree for each node
		for(int i = 0; i < numNodes; i++) {
			for(int x = 0; x < g.get(i).getSize(); x++) {
				int temp = g.get(i).deleteFirst();
				inDegree[temp]++;
				g.get(i).insertLast(temp);
			} 
		}
		
		//Enqueues each node index that has an initial in degree of 0
		for(int m = 0; m < numNodes; m++) {
				if(inDegree[m] == 0) {
					q.enqueue(m);
					inDegree[m]--;
				}
			//Enters the queue, serves the contents, and looks in the matrix to see if there is an edge
			while(!q.empty()) {
				int temp = q.serve();
				while(g.get(temp).getSize() != 0) {
					int y = g.get(temp).deleteFirst();
					//If there is an edge, it subtracts 1 from the in degree of that node
					inDegree[y]--;
					//If that in degree ends up being 0, it immediately adds it to the queue, and the process repeats
					if(inDegree[y] == 0) {
						q.enqueue(y);
						inDegree[y]--;
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
		}
		return order;
	}
}
