/*
 * Author: Alex Zajichek
 * CS 464
 * Database index implementation
 */

public class Bucket {
		int bucketSize;
		long[] locations;
		int[] primaryKeys;
		int count;
		int numBits;
		
	public Bucket(int size) {
		bucketSize = size;
		locations = new long[bucketSize];
		primaryKeys = new int[bucketSize];
		count = 0;
		numBits = 1;
	}
	
	public void insert(int pk, long loc) {
		primaryKeys[count] = pk;
		locations[count] = loc;
		count++;
	}
	
	public int count() {
		return count;
	}
	
	public int bitsUsed() {
		return numBits;
	}
	
}
