/*
 * Author: Alex Zajichek
 * CS 340 Fall 2014
 * Topological Sorting
 */

import java.io.IOException;
import java.util.Scanner;
import java.io.BufferedReader;
import java.io.FileReader;
public abstract class DirectedGraph {
	
	protected String vertexNames[];
	protected BufferedReader b;
	protected int numNodes;
	
	public DirectedGraph(String filename) {
		
		try {
			b = new BufferedReader(new FileReader(filename));
			numNodes = Integer.parseInt(b.readLine());
			
		}
		catch(Exception e) {
			System.out.println("Error in file contents or file not found");
		}
	}
	
	protected void buildGraph() {
		//Stores the data from the text files into 'vertexNames'
		Scanner scan;
		String e, v1, v2;
		try{
		vertexNames = new String[numNodes];
		for(int i = 0; i < numNodes; i++) {
			vertexNames[i] = b.readLine();
		}
		e = b.readLine();
		while(e != null) {
			scan = new Scanner(e);
			v1 = scan.next();
			v2 = scan.next();
			addEdge(getNodeNum(v1), getNodeNum(v2));
			e = b.readLine();
		}
	}
		catch(Exception r) {
			System.out.println("Error in file contents or file not found");
		}
	}
	
	protected int getNodeNum(String v) {
		for(int i = 0; i < numNodes; i++) 
			if(vertexNames[i].equals(v)) 
				return i;
				return -1;
	}
	
	public abstract void addEdge(int v1, int v2);
	
	public abstract String topo();

}
