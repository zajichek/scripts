/*
 * Author: Alex Zajichek
 * CS 340, Fall 2014
 * 11/19/2014
 */

import java.util.*;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class SymbolTable {
	
	class Node {
		String key;
		Object data;
		Node next;
		
		Node(String k, Object d, Node x) {
			key = k;
			data = d;
			next = x;
		}
	}
	
	Node table[];
	
	public SymbolTable(int s) {
		//Initializing the size of the table, and the heads of each list
		table = new Node[s];
		for(int i = 0; i < table.length; i++)
			table[i] = new Node(null, null, null);
	}
	
	private int hash(String k) {
		//Hash function sums the char values and mods it by the length of the array
		int hashValue = 0;
		for(int i = 0; i < k.length(); i++) {
			hashValue = hashValue + (int) k.charAt(i);
		}
		
		hashValue = hashValue % table.length;
		return hashValue;
	}
	
	//Defining the entire function in the two parameter insert method. 
	public boolean insert(String k) {
		return insert(k, null);
	}
	
	public boolean insert(String k, Object d) {
		//If the key is already in the table, no insert will be made
		if(find(k) == true) {
			return false;
		}
		
		int hashValue = hash(k);
		//Initializing a temporary node
		Node tempRoot = new Node(null, null, null);
		//The temporary node is equal to what the current head is pointing to
		tempRoot = table[hashValue].next;
		//The head will point to the new node, which points to the node that used to be 'next'
		table[hashValue].next = new Node(k, d, tempRoot);
		return true;
	}
	
	public boolean find(String k) {
		int hashValue = hash(k);
		//Initializing a temporary node
		Node tempNode = new Node(null, null, null);
		//Temporary node is equal to the first place that k could be found
		tempNode = table[hashValue].next;
		//Will look through entire list for k, if it is found, return 'true'
		while(tempNode != null) {
			if(tempNode.key.equals(k)) {
				return true;
			} else {
				tempNode = tempNode.next;
			}
		}
		
		return false;
	}
	
	public Object getData(String k) {
		//If the string is not in the table, null will be returned
		if(find(k) == false) {
			return null;
		}
		//Getting the hash value for k
		int hashValue = hash(k);
		//Creating a temporary Node to search for k, and assigning it to the 'next' node in the correct row of the array
		Node tempNode = new Node(null, null, null);
		tempNode = table[hashValue].next;
		//While there are next nodes, each one will be checked for the correct key value
		while(tempNode != null) {
			if(tempNode.key.equals(k)) {
				return tempNode.data;
			} else {
				tempNode = tempNode.next;
			}
		}
		
		//Won't reach this point
		return null;
	}
	
	public void setValue(String k, Object d) {
		//Assumes find(k) is true
		int hashValue = hash(k);
		Node tempNode = new Node(null, null, null);
		tempNode = table[hashValue];
		//Finds k and adds d as the data associated with it
		while(tempNode.next != null) {
			if(tempNode.next.key.equals(k)) {
				tempNode.next.data = d;
				break;
			} else {
				tempNode = tempNode.next;
			}
		}
	}
	
	public class STIterator implements Iterator<String> {
				Node currentNode;
				int index;
				STIterator i;
				
		//The constructor instantiates the currentNode and finds the first available Node to take value of
		public STIterator() {
				currentNode = new Node(null, null, null);
				for(int i = 0; i < table.length; i++) {
					if(table[i].next != null) {
						currentNode = table[i].next;
						index = i;
						break;
					}
				}
		}
		
		//Returns the initial value of the currentNode pointer
		//next() will skip over this initial Node if not called 
		public String initialKey() {
			return currentNode.key;
		}
		
		//Returns true if there is another node is the table
		public boolean hasNext() {
			if(currentNode.next != null) {
				return true;
			} else if(index != table.length - 1) {
				for(int i = index; i < table.length; i++) {
					if(table[i].next != null) {
						return true;
					   }
				    }
				}
			
			return false;
		}
		
		
		public String next() {
			//Keeps re-positioning if there is a node next in the linked list
			if(hasNext() && currentNode.next != null) {
				currentNode = currentNode.next;
				return currentNode.key;
			}
			
			//If there is not a node next, currentNode will re-position to the next Node in the table
			//If no nodes are left, null is returned
			if(index != table.length - 1) {
				index = index + 1;
				for(int i = index; i < table.length; i++) {
					if(table[i].next != null) {
						currentNode = table[i].next;
						index = i;
						return currentNode.key;
					}
				}
			}
			return null;
		}
		
		public void remove() {
		//optional
		}
		
		//Deletes the specified Node (Not used in Part II)
		//If currentNode is currently the Node that contains 'k' as the key, the pointer will be moved to the next available node
		public boolean delete(String k) {
			if(find(k) == false) {
				return false;
			}
			
			int hashValue = hash(k);
			Node tempNode = new Node(null, null, null);
			tempNode = table[hashValue];
			while(tempNode.next != null) {
				if(tempNode.next.key.equals(k)) {
					if(currentNode.key.equals(k)) {
						next();
					}
					tempNode.next = tempNode.next.next;
					table[hashValue] = tempNode;
					break;
				} else {
					tempNode = tempNode.next;
				}
			}
			
			return true;
		}
		
		public Iterator<String> iterator() {
			i = new STIterator();
			return i;
		}
	}
	
		//Main method that tests the SymbolTable and STIterator
		public static void main(String[] args) throws IOException {
			SymbolTable t = new SymbolTable(5);
			//Test file contains random first and last names, inserts first name as key, last name as data 
			BufferedReader reader = new BufferedReader(new FileReader("hashdata.txt"));
			String line = reader.readLine();
			
			//Inserting the values from the file
			while(line != null) {
				Scanner scan = new Scanner(line);
				t.insert(scan.next(), scan.next());
				line = reader.readLine();
			}
			
			//Testing various methods by printing results from the table
			STIterator i = t.new STIterator();
			System.out.println(i.initialKey() + " " + t.getData(i.initialKey()) + " " + t.hash(i.initialKey()));
			String name = i.initialKey();
			while(i.hasNext()) {
				String c = i.next();
				System.out.println(c + " " + t.getData(c) + " " + t.hash(c));
			}
			
			//Changes and prints the data with key 'name' changed to the specified value
			System.out.println();
			t.setValue(name, "new data");
			System.out.println(name + " " + t.getData(name));
			
			//To test 'delete', a new STIterator was made
			STIterator i2 = t.new STIterator();
			//Returns 'true' while Jinny is in the table
			System.out.println(t.find("Jinny"));
			i2.delete("Jinny");
			//After deleted, 'false' is returned
			System.out.println(t.find("Jinny"));
			System.out.println();
			//When printing out the table again, "Jinny" does not appear in the list of names
			System.out.println(i2.initialKey());
			while(i2.hasNext()) {
				System.out.println(i2.next());
			}
	}
}
	
	
















