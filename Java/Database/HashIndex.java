/*
 * Author: Alex Zajichek
 * CS 464
 * Database index implementation
 */

import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.Vector;


public class HashIndex {

	RandomAccessFile r;
	int bucketSize;
	int bitsUsed;
	String file; 
    long[] directory;
    long dirLoc;
    int dirSize;
	
	//The first constructor is for creating a new hash index
	public HashIndex(int s, String filename) throws IOException {
		bucketSize = s;
	    file = filename;
		r = new RandomAccessFile(file, "rw");
		
		r.seek(0);
		r.writeInt(bucketSize);
		
		r.seek(4);
		bitsUsed = 1;
		r.writeInt(bitsUsed);
		dirSize = (int) Math.pow(2, bitsUsed);
		
		r.seek(8);
		dirLoc = 16;
		r.writeLong(dirLoc);
		
		directory = new long[2];
		directory[0] = 32;
		directory[1] = 32 + 4 + 4 + 4*bucketSize + 8*bucketSize;
		r.seek(dirLoc);
		r.writeLong(directory[0]);
		r.writeLong(directory[1]);
		Bucket B1 = new Bucket(bucketSize);
		Bucket B2 = new Bucket(bucketSize);
		writeBucket(B1, directory[0]);
		writeBucket(B2, directory[1]);
		
	}
	
	//The second constructor is for accessing existing hash indexes
	public HashIndex(String filename) throws IOException {
		file = filename;
		r = new RandomAccessFile(filename, "rw");
		r.seek(0);
		bucketSize = r.readInt();
		
		r.seek(4);
		bitsUsed = r.readInt();
		dirSize = (int) Math.pow(2, bitsUsed);
		
		r.seek(8);
		dirLoc = r.readLong();
		
		directory = readDirectory();
	}
	
	//Returns the index of the directory array in which the value should be located
	public int hash(int k) {
		String hashValue = Integer.toBinaryString(k);
		while(hashValue.length() < 20) {
			hashValue = "0" + hashValue;
		}
		hashValue = hashValue.substring(hashValue.length()-bitsUsed);
		int index = Integer.parseInt(hashValue, 2);
		return index;
	}
	
	//Inserts the primary key and location into the hash index according to the hash value
	public void insert(int pk, long loc) throws IOException {
		int index = hash(pk);
		long bucket = directory[index];
		Bucket temp = readBucket(bucket);
		
		if(full(temp)) {
			if(temp.numBits == bitsUsed) {
				bitsUsed++;
				r.seek(4);
				r.writeInt(bitsUsed);
				dirSize = (int) Math.pow(2, bitsUsed);
				long[] tempDirectory = new long[dirSize];
				int x=0;
				for(int i = 0; i < dirSize; i++) {
					tempDirectory[i] = directory[x];
					x++;
					if(x == directory.length) {
						x = 0;
					}
				}
				int[] k = new int[bucketSize + 1];
				long[] l = new long[bucketSize + 1];
				for(int i = 0; i < bucketSize; i++) {
					k[i] = temp.primaryKeys[i];
					l[i] = temp.locations[i];
				}
				k[bucketSize] = pk;
				l[bucketSize] = loc;
				Bucket newBucket1 = new Bucket(bucketSize);
				Bucket newBucket2 = new Bucket(bucketSize);
				newBucket1.numBits = bitsUsed;
				newBucket2.numBits = bitsUsed;
				
				newBucket1.insert(pk, loc);
				int h1 = hash(k[bucketSize]);
				int h2 = -1;
				
				for(int i = 0; i < k.length-1; i++) {
					if(hash(k[i]) == hash(k[bucketSize])) {
						newBucket1.insert(k[i], l[i]);
					} else {
						h2 = hash(k[i]);
						newBucket2.insert(k[i], l[i]);
					}
				}
				
				tempDirectory[h1] = bucket;
				writeBucket(newBucket1, bucket);
				
				long newLoc = r.length();
				writeBucket(newBucket2, newLoc);
				tempDirectory[h2] = newLoc;
				dirLoc = r.length();
				r.seek(8);
				r.writeLong(dirLoc);
				directory = new long[tempDirectory.length];
				directory = tempDirectory;
				writeDirectory();
				
			} else if(temp.numBits < bitsUsed) {
				long[] tempDirectory = new long[directory.length];
				tempDirectory = directory;
				Bucket newBucket1 = new Bucket(bucketSize);
				Bucket newBucket2 = new Bucket(bucketSize);
				newBucket1.insert(pk, loc);
				int h2 = -1;
				for(int i = 0; i < temp.primaryKeys.length; i++) {
					if(hash(temp.primaryKeys[i]) == index) {
						newBucket1.insert(temp.primaryKeys[i], temp.locations[i]);
					} else {
						h2 = hash(temp.primaryKeys[i]);
						newBucket2.insert(temp.primaryKeys[i], temp.locations[i]);
					}
				}
				
				newBucket1.numBits = temp.numBits + 1;
				newBucket2.numBits = temp.numBits + 1;
				long newLoc = r.length();
				tempDirectory[index] = bucket;
				tempDirectory[h2] = newLoc;
				writeBucket(newBucket1, bucket);
				writeBucket(newBucket2, newLoc);
				directory = new long[tempDirectory.length];
				directory = tempDirectory;
				writeDirectory();
			}
			
		} else {
			temp.insert(pk, loc);
			writeBucket(temp,bucket);
		}
	}
	
	//Writes the directory to the file
	public void writeDirectory() throws IOException {
		r.seek(dirLoc);
		for(int i = 0; i < directory.length; i++) {
			r.writeLong(directory[i]);
		}
	}
	
	//Checks if a bucket read in is full
	public boolean full(Bucket b) {
		if(b.count == bucketSize) {
			return true;
		} else {
			return false;
		}
	}
	
	//Finds a specified primary key in the hash index, returns -1 if it doesn't exist, or else returns location in table on pk
	public long find(int pk) throws IOException {
		long location = -1;
		int index = hash(pk);
		long bucket = directory[index];
		Bucket temp = readBucket(bucket);
		for(int i = 0; i < temp.count; i++) {
			if(temp.primaryKeys[i] == pk) {
				location = temp.locations[i];
				
			}
		}
		return location;
	}
	
	//Reads directory from the hash index file
	public long[] readDirectory() throws IOException{
		long[] temp = new long[dirSize];
		r.seek(dirLoc);
		for(int i = 0; i < dirSize; i++) {
			temp[i] = r.readLong();
		}
		
		return temp;
	}
	
	//Reads a bucket given its location
	public Bucket readBucket(long loc) throws IOException {
		Bucket b = new Bucket(bucketSize);
		r.seek(loc);
		b.count = r.readInt();
		b.numBits = r.readInt();
		for(int i = 0; i < b.bucketSize; i++) {
			b.primaryKeys[i] = r.readInt();
		}
		
		for(int j = 0; j < b.bucketSize; j++) {
			b.locations[j] = r.readLong();
		}
			return b;
	}
	
	
	//Writes a bucket given a bucket and the location to write it
	public void writeBucket(Bucket b, long loc) throws IOException{
		r.seek(loc);
		r.writeInt(b.count);
		r.writeInt(b.numBits);
		for(int i = 0; i < bucketSize; i++) {
			r.writeInt(b.primaryKeys[i]);
		}
		for(int i = 0; i < bucketSize; i++) {
			r.writeLong(b.locations[i]);
		}
	}
	
}