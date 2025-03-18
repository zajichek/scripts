/*
 * Author: Alex Zajichek
 * CS 340 Fall 2014
 * Topological Sorting
 */
public class MyQueue<T> implements FIFOQueue<T> {
	
	DoubleLinkedList<T> q;
	
	public MyQueue() {
		q = new DoubleLinkedList<>();
	}
	
	public boolean empty() {
		if(q.getSize() == 0) {
		return true;
		} else {
			return false;
		}
	}
	
	
	public void enqueue(T d) {
		q.insertLast(d);
		
	}
	
	public T serve() {
		T temp = q.deleteFirst();
		return temp;
	}

}
