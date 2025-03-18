/*
 * Author: Alex Zajichek
 * CS 464
 * Database index implementation
 */
/*
 * This program implements an extendible hashing index for managing a database. 
 * The indexes and tables are stored in RandomAccessFiles.
 */

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.Scanner;
import java.util.Vector;


public class Driver {
	
	//The Driver class acts as the user interface
	public static void main(String args[]) throws IOException, RuntimeException{
		DataBase d = new DataBase();
		
		System.out.println("What would you like to do?");
		System.out.println();
		System.out.println("	Create Table: Type 'create'");
		System.out.println("	Insert Row: Type 'insert'");
		System.out.println("	Find Row: Type 'find'");
		System.out.println("	Quit: Type 'quit'");
		
		Scanner scan = new Scanner(System.in);
		String choice = scan.next();
		
		while(!choice.equals("quit")) {
			if(choice.equals("create")) {
				System.out.println("Type the table name and bucketsize for index:");
				Scanner s1 = new Scanner(System.in);
				String tableName = s1.next();
				int bucketSize = Integer.parseInt(s1.next());
				
				System.out.println("Type attributes of '" + tableName + "'. (separate by white space with primary key first; 0 for integer, 1 for varchar)");
				System.out.println("Example: 0primarykey 1name 0age");
				Vector<String> v = new Vector<>();
				Scanner s2 = new Scanner(System.in);
				String attributes = s2.nextLine();
				Scanner s3 = new Scanner(attributes);
				String attribute = s3.next();
				while(s3.hasNext()) {
				v.addElement(attribute);
				attribute = s3.next();
				}
				v.addElement(attribute);
				
				d.createTable(tableName, v, bucketSize);
		  } 
			if(choice.equals("insert"))  {
				
				System.out.println("Type the table name:");
				Scanner s1 = new Scanner(System.in);
				String tableName = s1.next();
				
				System.out.println("Type '1' to insert from file, or '2' to insert a single row");
				Scanner f = new Scanner(System.in);
				String w = f.next();
				
				if(w.equals("1")) {
					BufferedReader reader = new BufferedReader(new FileReader(args[0]));
					String line = reader.readLine();
					while(line != null) {
						Vector<String> v = new Vector<>();
						Scanner m = new Scanner(line);
						String value = m.next();
						while(m.hasNext()) {
							v.addElement(value);
							value = m.next();
						}
						v.addElement(value);
						d.insert(tableName, v);
						line = reader.readLine();
					}
				} else {
				System.out.println("Type values for '" + tableName + "'. (separate by white space with primary key first)");
				System.out.println(d.attributes(tableName));
				
				Vector<String> v = new Vector<>();
				Scanner s2 = new Scanner(System.in);
				String values = s2.nextLine();
				Scanner s3 = new Scanner(values);
				String value = s3.next();
				while(s3.hasNext()) {
				v.addElement(value);
				value = s3.next();
				}
				v.addElement(value);
				
				d.insert(tableName, v);
			}
				
				
	      } 
			if(choice.equals("find")) {
				System.out.println("Type the table name:");
				Scanner s1 = new Scanner(System.in);
				String tableName = s1.next();
				
				System.out.println("Type primary key value:");
				Scanner s2 = new Scanner(System.in);
				String pk = s2.next();
				int value = Integer.parseInt(pk);
				
				System.out.println(d.attributes(tableName));
				System.out.println(d.find(tableName, value));
			}
			
			System.out.println("What would you like to do?");
			System.out.println();
			System.out.println("	Create Table: Type 'create'");
			System.out.println("	Insert Row: Type 'insert'");
			System.out.println("	Find Row: Type 'find'");
			System.out.println("	Quit: type 'quit'");
			choice = scan.next();
		}
	}
}
