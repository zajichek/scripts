/*
 * Author: Alex Zajichek
 * CS 464
 * Database index implementation
 */

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.Vector;


public class DataBase {
	RandomAccessFile r;
	HashIndex h;
	
	public DataBase() {
	}
	
	//Tables and hash indexes are created here
	public void createTable(String name, Vector<String> att, int bucketSize) throws IOException {
		String filename = name + ".tab";
		int numAtt = att.size();
		File file = new File(filename);
		if(file.exists()) {
			System.out.println("Table already exists.");
			return;
		}
		HashIndex h = new HashIndex(bucketSize, name + ".ind");
		r = new RandomAccessFile(filename, "rw");
		r.seek(0);
		r.writeInt(numAtt);
		for(int i = 0; i < numAtt; i++) {
			if(att.elementAt(i).charAt(0) == '0') {
				r.writeInt(0);
			} else {
				r.writeInt(1);
			}
		}
		
		for(int i = 0; i < numAtt; i++) {
			r.writeUTF(att.elementAt(i).substring(1));
		}
	}
	
	
	//First step in the insert process
	public void insert(String tableName, Vector<String> values) throws IOException {
		String filename = tableName + ".tab";
		String hashname = tableName + ".ind";
		int pk = Integer.parseInt(values.elementAt(0));
		if(exists(tableName, pk)) {
			System.out.println("Primary key already exists.");
			return;
		}
		r = new RandomAccessFile(filename, "rw");
		
		writeRow(filename, hashname, values);
	}
	
	
	//Writes a row of values to the specified table, and calls the hash index for insert
	public void writeRow(String table, String index, Vector<String> values) throws IOException {
		//Location of primary key for new row
		long loc = 0;
		int pk = Integer.parseInt(values.elementAt(0));
		loc = r.length();
		r.seek(loc);
		for(int i = 0; i < values.size(); i++) {
			if(Character.isDigit(values.elementAt(i).charAt(0))) {
				int value = Integer.parseInt(values.elementAt(i));
				r.writeInt(value);
			} else {
				r.writeUTF(values.elementAt(i));
			}
		}
		
		h = new HashIndex(index);
		h.insert(pk, loc);
	}
	
	//Obtains the names of the attributes of a table
	public String attributes(String tableName) throws IOException {
		String filename = tableName + ".tab";
		r = new RandomAccessFile(filename, "rw");
		r.seek(0);
		int numAtt = r.readInt();
		r.seek((long) (numAtt*4 + 4));
		String att = "";
		for(int i = 0; i < numAtt; i++) {
			att = att + " " + r.readUTF();
		}
		
		return att;
	}
	
	//Reads a row from the table given its location in the file
	public String readRow(long location) throws IOException {
		String row = "";
		r.seek(0);
		int numAtt = r.readInt();
		int[] dataTypes = new int[numAtt];
		for(int i = 0; i < numAtt; i++) {
			dataTypes[i] = r.readInt();
		}
		r.seek(location);
		for(int i = 0; i < numAtt; i++) {
			if(dataTypes[i] == 0) {
				row = row + " " + r.readInt();
			} else {
				row = row + " " + r.readUTF();
			}
		}
		
		return row;
	}
	
	//Calls the hash index to find the primary key specified
	public String find(String tableName, int pk) throws IOException {
		if(!exists(tableName, pk)) {
			return "No matching row exists.";
		} 
		
		String filename = tableName + ".tab";
		r = new RandomAccessFile(filename, "rw");
		long location = h.find(pk);
		return readRow(location);
	}
	
	//Checks if a primary key exists in the hash index
	public boolean exists(String tableName, int pk) throws IOException {
		String hashname = tableName + ".ind";
		h = new HashIndex(hashname);
		long location = h.find(pk);
		if(location == -1) {
			return false;
		}
		return true;
	}
}
