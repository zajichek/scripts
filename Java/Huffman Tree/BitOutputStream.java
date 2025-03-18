/*
 * Author: Alex Zajichek
 * Huffman Coding: This takes a text file and compresses it down to between 40-60% of its original size. It then takes a compress file and
 * attempts to decode the binary file. (Can't figure out why the decode won't return the correct characters).
 * CS 340 Fall 2014
 */

import java.io.*;
import java.util.Vector;

public class BitOutputStream {
	DataOutputStream out;

	public BitOutputStream(String filename) throws IOException {
		out = new DataOutputStream(new FileOutputStream(filename));
		//more initialization is needed
	}

	public void writeInt(int i)  throws IOException {
		out.writeByte(i);
	}

	public void writeString(String s) throws IOException {
		String temp = s;
		//Writes each group of 8 to the file as one byte
		while(temp.length() >= 8) {
			String t = temp.substring(0, 8);
			temp = temp.substring(8,temp.length());
			//Decodes the 8 bit String into its actual byte value
		    writeInt(Integer.decode(t));
		}
		
		close(temp);

	}

	public void writeBits(String[] s, Vector<Character> v)  throws IOException {
		//Creates a string of all the path representations in order in which the characters appear in the original file
		String b = "";
		for(int i = 0; i < v.size(); i++) {
			b = b + s[(int) v.elementAt(i)];
		}
		
		writeString(b);
	}

	public void close(String t)  throws IOException {
		//If there is left over bits in the file, this will close it out and write it
		if(t.length() > 0) {
			while(t.length() != 8) {
				t = t + "0";
			}
			writeInt(Integer.decode(t));
		}
		out.close();
	}
}
