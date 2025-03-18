/*
 * Author: Alex Zajichek
 * CS 340 Fall 2014
 * Topological Sorting
 */
public class DoubleLinkedListTest {
	
		DoubleLinkedList<String> d;

	public static void main(String[] args) {
		DoubleLinkedList<Integer> d = new DoubleLinkedList<>();
		for(int i = 0; i < 5; i++) {
		d.insertLast(i + 10);
		}
		
		for(int k = 0; k < 4; k++) {
		d.insertAt(0, 99);
		}
		
		d.insertAt(-1, 5);
		
		for(int j = 0; j < 9; j++) {
			System.out.println("Value: " +  d.deleteFirst());
			System.out.println("Size: " + d.getSize());
			System.out.println("Position: " + d.getPosition());
		}
	}

}
