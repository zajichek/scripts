/*
 * Author: Alex Zajichek
 * Huffman Coding: This takes a text file and compresses it down to between 40-60% of its original size. It then takes a compress file and
 * attempts to decode the binary file. (Can't figure out why the decode won't return the correct characters).
 * CS 340 Fall 2014
 */
import java.io.*;
import java.util.*;

public class HuffmanASCIICompression {

	class Node {
		Node left;
		char ch;
		Node right;

		Node(Node L, char c, Node r) {
			left = L;
			ch = c;
			right = r;
		}
	}

	Node root;
	int frequency[];
	int totalChars;
	String encodings[];

	public HuffmanASCIICompression() {
		encodings = new String[128];
	}

				

	public void encode(String infile, String outfile)throws IOException {	
		//Vector to store the list of characters as they appear
		Vector<Character> characters = new Vector<>();
		BufferedReader reader = new BufferedReader(new FileReader(infile));
		frequency = new int[128];
		int t = reader.read();
		//Reading in file per character, adding frequency counts
		while(t != -1) {
			characters.add((char) t);
			frequency[t]++;
			t = reader.read();
		}
		
		//Calculating the total number of characters in file
		totalChars = 0;
		for(int i = 0; i < 128; i ++) {
			if(frequency[i] != 0) {
				totalChars++;
			}
		}
		
		//Made a priority object, and insert each frequency
		PriorityQueue queue = new PriorityQueue(totalChars);
		for(int i = 0; i < frequency.length; i++) {
			if(frequency[i] != 0) {
			queue.insert(frequency[i], (char) i);
			}
		}
		
		//Building Huffman Tree
		Node tempNode = new Node(null, ' ', null);
		while(queue.getSize() != 1) {
			int leftValue = 0;
			int rightValue = 0;
			int total = 0;
			tempNode = new Node(null, ' ', null);
			//If it is a node, give it to the left of tempNode
			if(queue.getMinData() instanceof Node) {
				leftValue = queue.value();
				tempNode.left = (Node) queue.deleteMin();
			} else {
				//If it is still a char,  creates a new node
				leftValue = queue.value();
			    tempNode.left = new Node(null, (char) queue.deleteMin(), null);
			}
			
			//Repeat process for the right child of each node
			if(queue.getMinData() instanceof Node) {
				rightValue = queue.value();
				tempNode.right = (Node) queue.deleteMin();
			} else {
				rightValue = queue.value();
			    tempNode.right = new Node(null, (char) queue.deleteMin(), null);
			}
			//Give the temporary node a char value that is unused, and insert its new total into priority queue
			total = leftValue + rightValue;
			tempNode.ch = (char) 128;
			queue.insert(total, tempNode);
		}
		
			//After everything is inserted, root is equal to the final object in the queue
		    root = (Node) queue.deleteMin();
		    
		    //Initializing each encoding value
		    for(int i = 0; i < 128; i++)
		    	encodings[i] = "";
		    
		    //Getting the paths for each character
		    eval(root, "");
		    
		    //The paths will be exactly backwards of how they need to be, so this will reverse the path to its correct orientation
		    for(int j = 0; j < encodings.length; j++) {
		    	String o = encodings[j];
		    	String r = "";
		    for(int i = o.length() - 1 ; i >= 0 ; i--) {
		         r = r + o.charAt(i);
		       }
		    	encodings[j] = r;
		    }
		    
		    //Create a new BitOutputStream and write the bits to the file
		    BitOutputStream out = new BitOutputStream("encode.txt");
		    out.writeBits(encodings, characters);
		    
		    
	}
	
	//Recursively finds paths to each node in the tree, adds that path to the corresponding encoding value
	public String eval(Node r, String path) {
		if(r.left == null) {
			encodings[(int) r.ch] = path;
			return path;
		}
		
		String x = eval(r.left, "1" + path);
		String y = eval(r.right, "0" + path);
		
		return x + y;
	}
	
	public void decode(String infile, String outfile)throws IOException {
		PrintWriter writer = new PrintWriter(new FileWriter("Decoded.txt"));
		BitInputStream in = new BitInputStream("encode.txt");
		//Grabs the string of binary values read from the binary file
		String values = in.readString();
		Vector<Character> n = new Vector<>();
		//Calls getValues method to convert the paths to the corresponding char in 'endcodings'
		n = getValues(values);
		for(int m = 0; m < n.size(); m++) {
			writer.print(n.elementAt(m));
		}
		writer.flush();
		writer.close();
	}
	
	public Vector<Character> getValues(String j) {
		Vector<Character> e = new Vector<>();
		String temp = j;
		int i = 1;
		//Searches encodings to find a match for a substring, adds it to a vector of Characters
		while(i != temp.length()) {
			String o = temp.substring(0,i);
			for(int z = 0; z < encodings.length; z++) {
				if(o.equals(encodings[z])) {
					e.add((char) z);
					temp = temp.substring(i, temp.length());
					i = 0;
					break;
				}
			}
			i++;
		}
		
		return e;
	}
	

	public static void main(String args[]) throws IOException {
		HuffmanASCIICompression h = new HuffmanASCIICompression();
		h.encode(args[0], args[1]);
		h.decode(args[1], args[0]+"_new");
	}
}