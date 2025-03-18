/*
 * Author: Alex Zajichek
 * CS 340, Fall 2014
 */
/*
 * This program creates a B-Tree with a specified order. It is stored in a RandomAccessFile, allowing the data to be accessed.
 */

import java.io.*;
import java.util.*;

public class BTree {
	
	int order;
	RandomAccessFile r;
	String fileName;
	BTreeNode root;
	BTreeNode freeRoot;
	long rootPosition;
	Stack<BTreeNode> nodes;
	long nodePosition;
	
	private class BTreeNode {
		int count;
		int[] keys;
		long[] children;
		long location;
		
		BTreeNode(int count, int[] keys, long[] children, long location) {
			this.count = count;
			this.keys = keys;
			this.children = children;
			this.location = location;
		}
	}
	
	public BTree(String n, int ord) throws IOException {
		fileName = n;
		File file = new File(fileName);
		if(file.exists()) {
			file.delete();
		}
		
		//Writes the order and initial root location to the file, also writes the root to file with nothing in it
		order = ord;
		r = new RandomAccessFile(fileName, "rw");
		r.seek(0);
		r.writeInt(order);
		r.seek(4);
		rootPosition = 12;
		r.writeLong(rootPosition);
		root = newNode(rootPosition);
		writeNode(root);
	}
	
	public BTree(String n) throws IOException, RuntimeException {
		fileName = n;
		File file = new File(fileName);
		if(!file.exists()) {
			throw new RuntimeException("File does not exist");
		}
		
		//If access needs to be made from an existing BTree, the order and root position will be found
		r = new RandomAccessFile(fileName, "rw");
		r.seek(0);
		order = r.readInt();
		r.seek(4);
		rootPosition = r.readLong();
		root = readNode(rootPosition);
	}
	
	//Search builds up a stack of the search path, and then inserts 'k'
	public void insert(int k) throws IOException{
			search(k);
			insert(k, 0 ,0);
	}
	
	
	//Recursively inserts a value throughout the tree. The stack 'nodes' will keep popping values, allowing for a stop
	public int insert(int k, long left, long right) throws IOException {
	//If there is a node in the stack, insert will proceed
	if(!nodes.isEmpty()) {
	BTreeNode n = nodes.pop();
		//If the currentNode is not full and a leaf, data will be added
		if(!full(n) && isLeaf(n)) {
			addData(n, k);
			return 0;
			
		//If it is not full, but not a leaf, data will be added, but children will then be rearranged
		} else if(!full(n) && !isLeaf(n)) {
			addData(n, k);
			BTreeNode temp = readNode(n.location);
			int place = 0;
			for(int i = 0; i < temp.keys.length; i++) {
				if(temp.keys[i] == k) {
					place = i;
					break;
				}
			}
			
			//Moving the children down to insert the new child references
			for(int i = place; i < order-1; i++) {
				temp.children[i+1] = temp.children[i];
			}
			
			//A leaf must have been split for this case to happen, so positions of the split nodes are given
			temp.children[place] = left;
			temp.children[place + 1] = right;
			writeNode(temp);
			return 0;
			
		//If the root is a leaf and is full, it will be split, making a new root
		} else if((n.location == rootPosition) && isLeaf(n)) {
			BTreeNode newRoot = newNode(r.length());
			writeNode(newRoot);
			BTreeNode rightChild = newNode(r.length());
			writeNode(rightChild);
			int splitValue = order / 2;
			
			//Creating a new array with all keys plus 'k'
			int[] temp = new int[order];
			for(int i = 0; i < n.keys.length; i++) {
				temp[i] = n.keys[i];
			}
			temp[order - 1] = k;
			Arrays.sort(temp);
			
			newRoot.keys[0] = temp[splitValue];
			newRoot.count++;
			newRoot.children[0] = n.location;
			newRoot.children[1] = rightChild.location;
			
			//Assigning keys to the right child
			int v = 0;
			for(int i = splitValue; i < order; i++) {
				rightChild.keys[v] = temp[i];
				rightChild.count++;
				v++;
			}
			
			n.count = splitValue;
			for(int i = 0; i < n.count; i++) {
				n.keys[i] = temp[i];
			}
			
			rootPosition = newRoot.location;
			r.seek(4);
			r.writeLong(rootPosition);
			
			writeNode(newRoot);
			writeNode(n);
			writeNode(rightChild);
			
			return 0;
			
		//If the root is full but it is not a leaf, a new root will also be made
		} else if(n.location == rootPosition) {
			BTreeNode newRoot = newNode(r.length());
			writeNode(newRoot);
			BTreeNode rightChild = newNode(r.length());
			writeNode(rightChild);
			
			int[] temp = new int[order];
			for(int i = 0; i < n.keys.length; i++) {
				temp[i] = n.keys[i];
			}
			
			temp[order - 1] = k;
			Arrays.sort(temp);
			
			int splitValue = order / 2;
			int place = 0;
			
			//Assigning values to left child
			for(int i = 0; i < splitValue; i++) {
				n.keys[i] = temp[i];
			}
			n.count = splitValue;
			
			//Assigning values to right child
			int v = 0;
			for(int i = splitValue + 1; i < temp.length; i++) {
				rightChild.keys[v] = temp[i];
				rightChild.count++;
				v++;
			}
			
			//Finding where in 'temp' 'k' was added
			for(int i = 0; i < temp.length; i++) {
				if(temp[i] == k) {
					place = i;
					break;
				}
			}
			
			newRoot.keys[0] = temp[splitValue];
			newRoot.children[0] = n.location;
			newRoot.children[1] = rightChild.location;
			newRoot.count++;
			
			//If this case holds, the children go on the left of the split
			if(place < splitValue) {
				int p = 0;
				for(int i = 0; i < n.count; i++) {
					if(n.keys[i] == k) {
						p = i;
						break;
					}
				}
				
				int x = 0;
				for(int i = n.count; i < order; i++) {
					rightChild.children[x] = n.children[i];
					x++;
				}
				
				for(int i = p; i < n.count; i++) {
					n.children[i+1] = n.children[i];
				}
				
				n.children[p] = left;
				n.children[p + 1] = right;
				
			//If this case holds, children go on the right of the split
			} else if(place > splitValue){
				int p = 0;
				for(int i = 0; i < rightChild.count; i++) {
					if(rightChild.keys[i] == k) {
						p = i;
						break;
					}
				}
				
				int y = 0;
				for(int i = splitValue + 1; i < order; i++) {
					rightChild.children[y] = n.children[i];
					y++;
				}
				
				for(int i = p; i < rightChild.count; i++) {
					rightChild.children[i+1] = rightChild.children[i];
				}
				
				rightChild.children[p] = left;
				rightChild.children[p + 1] = right;
			//If the inserted value is the split value, then children will be put on either side
			} else {
				n.children[n.count] = left;
				rightChild.children[0] = right;
				int x = 1;
				for(int i = splitValue + 1; i < order; i++) {
					rightChild.children[x] = n.children[i];
					x++;
				}
				}
			
			

			rootPosition = newRoot.location;
			r.seek(4);
			r.writeLong(rootPosition);
			
			writeNode(newRoot);
			writeNode(n);
			writeNode(rightChild);
			
			return 0;
			
			//If a leaf is full, it is split
		} else if(isLeaf(n)) {
			int splitValue = 0;
			long leftLocation = 0;
			long rightLocation = 0;
			int splitLocation = order / 2;
			
			BTreeNode rightChild = newNode(r.length());
			writeNode(rightChild);
			rightLocation = rightChild.location;
			leftLocation = n.location;
			
			int[] temp = new int[order];
			for(int i = 0; i < n.keys.length; i++) {
				temp[i] = n.keys[i];
			}
			temp[order - 1] = k;
			Arrays.sort(temp);
			splitValue = temp[splitLocation];
			
			for(int i = 0; i < splitLocation; i++) {
				n.keys[i] = temp[i];
			}
			n.count = splitLocation;
			
			int v = 0;
			for(int i = splitLocation; i < temp.length; i++) {
				rightChild.keys[v] = temp[i];
				rightChild.count++;
				v++;
			}
			
			writeNode(n);
			writeNode(rightChild);
			//returns the split value, as well as the location of the two new leaves
			return insert(splitValue, leftLocation, rightLocation);
			
		//If the node is full and it is not the root or a leaf, it will also be split
		} else if(full(n)){
			int splitValue = 0;
			long leftLocation = 0;
			long rightLocation = 0;
			int place = 0;;
			int splitLocation = order / 2;
			BTreeNode node = newNode(r.length());
			writeNode(node);
			leftLocation = n.location;
			rightLocation = node.location;
			
			int[] temp = new int[order];
			for(int i = 0; i < n.keys.length; i++) {
				temp[i] = n.keys[i];
			}
			temp[order - 1] = k;
			Arrays.sort(temp);
			splitValue = temp[splitLocation];
			
			for(int i = 0; i < temp.length; i++) {
				if(temp[i] == k) {
					place = i;
					break;
				}
			}
			
			for(int i = 0; i < splitLocation; i++) {
				n.keys[i] = temp[i];
			}
			n.count = splitLocation;
			
			int v = 0;
			for(int i = splitLocation + 1; i < temp.length; i++) {
				node.keys[v] = temp[i];
				node.count++;
				v++;
			}
			
			//Same process as above
			if(place < splitLocation) {
				int p = 0;
				for(int i = 0; i < n.count; i++) {
					if(n.keys[i] == k) {
						p = i;
						break;
					}
				}
				
				int x = 0;
				for(int i = n.count; i < order; i++) {
					node.children[x] = n.children[i];
					x++;
				}
				
				for(int i = p; i < n.count; i++) {
					n.children[i+1] = n.children[i];
				}
				
				n.children[p] = left;
				n.children[p + 1] = right;
				
			} else if(place > splitLocation){
				int p = 0;
				for(int i = 0; i < node.count; i++) {
					if(node.keys[i] == k) {
						p = i;
						break;
					}
				}
				
				int y = 0;
				for(int i = splitLocation + 1; i < order; i++) {
					node.children[y] = n.children[i];
					y++;
				}
				
				for(int i = p; i < node.count; i++) {
					node.children[i+1] = node.children[i];
				}
				
				node.children[p] = left;
				node.children[p + 1] = right;
			} else {
				n.children[n.count] = left;
				node.children[0] = right;
				int x = 1;
				for(int i = splitValue + 1; i < order; i++) {
					node.children[x] = n.children[i];
					x++;
				}
				}
			
			writeNode(n);
			writeNode(node);
			
			//Returns the value that it split at to insert it higher in the tree, as well as references to the location of the split node
			return insert(splitValue, leftLocation, rightLocation);
		}
		
		}
		//Doesn't have an effect
		return 0;
		}
	
	//Adds one data value to a node, and writes it to the file
	public void addData(BTreeNode b, int data) throws IOException{
		BTreeNode node;
		int count = b.count;
		b.keys[count] = data;
		count = count + 1;
		int[] temp = new int[count];
		for(int i = 0; i < count; i++) {
			temp[i] = b.keys[i];
		}
		
		Arrays.sort(temp);
		
		int[] k = new int[order - 1];
		for(int i = 0; i < count; i++) {
			k[i] = temp[i];
		}
		
		node = new BTreeNode(count, k, b.children, b.location);
		writeNode(node);
	}
	
	//Writes nodes to the RA file (count, keys, children)
	public void writeNode(BTreeNode b) throws IOException{
		long loc = b.location;
		r.seek(loc);
		r.writeInt(b.count);
		loc = loc + 4;
		r.seek(loc);
		for(int i = 0; i < b.keys.length; i++) {
			r.writeInt(b.keys[i]);
			//System.out.print(b.keys[i] + " ");
			loc = loc + 4;
			r.seek(loc);
		}
		
		for(int j = 0; j < b.children.length; j++) {
			if(j != b.children.length - 1) {
			r.writeLong(b.children[j]);
			loc = loc + 8;
			r.seek(loc);
			} else {
			 r.writeLong(b.children[j]);
			}
		}
	}
	
	//Reads nodes from the file, given their locations
	public BTreeNode readNode(long position) throws IOException{
		long loc = position;
		r.seek(loc);
		int count = r.readInt();
		
		loc = loc + 4;
		r.seek(loc);
		int[] k = new int[order - 1];
		long[] c = new long[order];
		for(int i = 0; i < k.length; i++) {
			k[i] = r.readInt();
			loc = loc + 4;
			r.seek(loc);
		}
		
		for(int i = 0; i < c.length; i++) {
			if(i != c.length -1) {
			c[i] = r.readLong();
			loc = loc + 8;
			r.seek(loc);
			} else {
			  c[i] = r.readLong();
			}
		}
		
		BTreeNode node = new BTreeNode(count, k, c, position);
		
		return node;
	}
	
	//Checks if the node is full
	public boolean full(BTreeNode r) {
		if(r.count == order - 1) {
			return true;
		}
			return false;
	}
	
	//Checks if the node is a leaf
	public boolean isLeaf(BTreeNode r) {
		if(r.children[0] == 0) {
		return true;
		}
		return false;
	}
	
	//Creates a new BTreeNode object at the specifed location in the file. Gives it 0's for all parameter values
	public BTreeNode newNode(long position) throws IOException{
		int[] data = new int[order - 1];
		long[] children = new long[order];
		BTreeNode temp = new BTreeNode(0, data, children, position);
		return temp;
	}
	
	//Searches for 'k' in the BTree, also builds up a stack for insertion
	public boolean search(int k) throws IOException{
		nodes = new Stack<>();
		BTreeNode tempNode = readNode(rootPosition);
		while(tempNode.children[0] != 0) {
			boolean found = false;
			for(int i = 0; i < tempNode.count; i++) {
				if(k < tempNode.keys[i]) {
					found = true;
					nodes.push(tempNode);
					tempNode = readNode(tempNode.children[i]);
					break;
				}
			}
			if(found == false) {
			if(tempNode.keys[tempNode.count - 1] < k) {
				nodes.push(tempNode);
				tempNode = readNode(tempNode.children[tempNode.count]);
			}
			}
		}
		
		nodes.push(tempNode);
		nodePosition = tempNode.location;
		for(int i = 0; i < tempNode.count; i++) {
			if(tempNode.keys[i] == k) {
				return true;
			}
		}
		
		return false;
	}
	
	//Iterator not implemented
	public class BTIterator implements Iterator<Integer> {
		
		public BTIterator(int low, int high) {
			
		}
		
		public boolean hasNext() {
			return true;
		}
		
		public Integer next() {
			return 1;
		}
		
		public void remove() {
			//Not implemented
		}
		
	}
	
	public Iterator<Integer> iterator(int low, int high) {
		return new BTIterator(low, high);
	}
	
	public void close() throws IOException{
		r.close();
	}
	
	
	public static void main(String[] args) throws IOException{
		BTree t = new BTree("he", 5);
		//BTree f = new BTree("he");
	}
	}