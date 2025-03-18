/*
 * Author: Alex Zajichek
 * CS 340 Fall 2014
 * Topological Sorting
 */


public class DoubleLinkedList<T> {
	
	class Node {
		Node prev;
		T data;
		Node next;
		
		Node(Node p, T d, Node n) {
			prev = p;
			data = d;
			next = n;
		}
	}
	
	Node head;
	Node current;
	Node last;
	int position;
	int size;
	
	public DoubleLinkedList() {
		head = new Node(null, null, null);
		head.next = new Node(head, null, null);
		size = 0;
		position = 0;
		current = head.next;
		last = current;
	}
	
	public void insertLast(T d) {
		Node newNode = new Node(last.prev, d, last);
		last.prev.next = newNode;
		last.prev = newNode;
		size++;
	}
	
	public void insertAt(int p, T d) {
		int m = 0;
		if(p > size || p < 0){
			System.out.println("Invalid Entry: Index requested does not exist");
		}
		
		else{
			int l = position - p;
			
			if(l > 0){
				for(m = 0; m < l; m++){
					current = current.prev;
					position -= 1;
				}
			}
			else if(l < 0){
				for(m = 0; m > l; m--){
					current = current.next;
					position += 1;
				}
			}
		}
		
		Node newNode     = new Node(current.prev, d, current);
		current.prev.next = newNode;
		current.prev      = newNode;
		
		position ++;
		size ++;
	}
	
	public T deleteFirst() {
		T temp = head.next.data;
		if(getSize() == 1) {
			head.next = last;
			last.prev = head;
		} else {
		head.next = head.next.next;
		head.next.prev = head;
		}
		size--;
		return temp;
	}
	
	public int getSize() {
		return size;
	}
	
	public int getPosition() {
		return position;
	}
	
	public T getData(int p) {
		if(p < 0 || p > size-1) {
			return null;
		} else {
			return current.data;
		}
	}
	
	
}
