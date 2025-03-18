/*
 * Author: Alex Zajichek
 * Huffman Coding: This takes a text file and compresses it down to between 40-60% of its original size. It then takes a compress file and
 * attempts to decode the binary file. (Can't figure out why the decode won't return the correct characters).
 * CS 340 Fall 2014
 */

import java.io.*;

public class BitInputStream {
	DataInputStream in;
	String file;

	public BitInputStream(String infile)  throws IOException {
		
		in = new DataInputStream(new FileInputStream(infile));
		
		//more initialization will be required
	}

	public int readInt()  throws IOException {
		return readBit();
	}

	public String readString() throws IOException  {
		String o = "";
		int temp = readInt();
		
		while(temp != -1) {
			//Converts the byte to its binary String representation, adds 0's to make it a length of 8
			o = o + Integer.toBinaryString(temp);
			while(o.length() < 8) {
				o = "0" + o;
			}
			
			temp = readInt();
		}
		return o;
	}

	public int readBit()  throws IOException {
		try {
			//Reads each byte from the file
			int i = in.readByte();	
			return i;
		}
		catch(Exception e) {
			return -1;
		}
	}

	public void close() throws IOException {
		in.close();
	}
}
		
