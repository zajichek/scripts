/*
 * Author: Alex Zajichek
 * Huffman Coding: This takes a text file and compresses it down to between 40-60% of its original size. It then takes a compress file and
 * attempts to decode the binary file. (Can't figure out why the decode won't return the correct characters).
 * CS 340 Fall 2014
 */
import java.util.Vector;

public class PriorityQueue {

	Vector<Integer> frequency;
	Vector<Object> values;
	int max;

	public PriorityQueue(int qsize) {
		//A vector to values, and one for frequencies, all values will be directly corresponded to its frequency by index of the vector
		max = qsize;
		values = new Vector<Object>();
		frequency = new Vector<Integer>();
	}
	
	public int value() {
		//A method returning the frequency at the front (smallest)
		return frequency.elementAt(0);
	}

	public Object deleteMin() {
		//Deletes the object at the front of values and freuquency (always will be the smallest)
		Object temp = values.remove(0);
		frequency.remove(0);
		return temp;
	}

	public Integer getMinKey() {
		return frequency.elementAt(0);
	}


	public boolean empty() {
		//Checks if there is nothing left in the vectors
		if(frequency.size() == 0 && values.size() == 0) {
			return true;
		} else {
		return false;
	  }
	}

	//Returns the minimum object in the vector
	public Object getMinData() {
		return values.elementAt(0);
	}

		//Checks if it is full
		public boolean full() {
			if(getSize() == max) {
			return true;
			} else {
				return false;
			}
	}

	//Each time an object is inserted, it will find its place in sorted order, so the minimum value will always be at the front
	public void insert(Integer k, Object d) {
		if(empty()) {
			frequency.addElement(k);
			values.addElement(d);
		} else if(getSize() == 1) {
			if(frequency.elementAt(0) >= k) {
				frequency.insertElementAt(k, 0);
				values.insertElementAt(d, 0);
				
			} else {
				frequency.addElement(k);
				values.addElement(d);
			}
		} else {
			for(int i = -1; i < values.size()-1; i++) {
				if(frequency.elementAt(i + 1) >= k) {
					frequency.insertElementAt(k, i + 1);
					values.insertElementAt(d, i + 1);
					break;
				  }
				}
				if(k > frequency.elementAt(values.size()-1)) {
					frequency.addElement(k);
					values.addElement(d);
				}
			}
		}

	//Returns size of the vectors
	public int getSize() {
	return values.size();
	}
}
