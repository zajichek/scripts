/*
 * Author: Alex Zajichek
 * CS 364 Fall 2014
 * Dr. Gendreau
 */

import java.sql.Connection;
import java.util.Vector;
import java.sql.DriverManager;
import java.util.Scanner;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.io.BufferedReader;
import java.io.FileReader;
import java.sql.DatabaseMetaData;

public class SQLFunctions {
	
	Connection con;
	PreparedStatement p;
	Statement s;
	ResultSet r;
	DatabaseMetaData d;
	
	public SQLFunctions(String path) {
		try{
		Class.forName("org.sqlite.JDBC");
		con = DriverManager.getConnection(path);
		}
		
		catch(SQLException e1) {
			System.out.println(e1.getMessage());
		}
		catch(Exception e) {
			System.out.println(e.getMessage());
		}
	}
	
	//#1
	public void contents(String tableName, String[] columns) throws SQLException {
		String column = "";
		String table = "FROM " + tableName;
		//Grabbing the correct columns for the specified table
		//Accounting for last column name to remove the comma
		int last = columns.length-1;
		for(int i  = 0; i < columns.length; i++) {
			if(i == last) {
				column = column + columns[i] + " ";
			} else {
			column = column + columns[i] + ",";
			}
		}
		
		//Printing out headers
		System.out.println("TABLE: " + tableName);
		for(int m = 0; m < columns.length; m++) {
			System.out.print(columns[m] + " ");
		}
		System.out.println();
		
		//Finishing the full statement
		String statement = "SELECT " + column + table;
		s = con.createStatement();
		r = s.executeQuery(statement);
		
		System.out.println();
		while(r.next()) {
			for(int j = 1; j <= columns.length; j++) {
				System.out.print(r.getString(j) + " ");
			}
			System.out.println();
		}
	}
	
	//#2
	public void bookNums(String first, String last) throws SQLException {
		//Creates a Statement object to set up the query
		s=con.createStatement();
		//Makes a string value for the specified query
		String query = "SELECT B.booknum, title FROM Book B, Writes W, Author A WHERE A.aid = W.aid AND W.booknum = B.booknum AND A.first = '" + first +"' AND A.last = '" + last + "'";
		r = s.executeQuery(query);
		//Prints the values in the result set
		while(r.next()) {
			System.out.println(r.getString(1) + " " + r.getString(2));
		}
	}
	
	//#3
	public void availableCopies(String booknum) throws SQLException {
		Statement temp = con.createStatement();
		ResultSet r = temp.executeQuery("SELECT title FROM Book WHERE booknum ='" + booknum +"'");
		String title = r.getString(1);
		s = con.createStatement();
		//The query counts number of rows where due is 'null' for specified booknum
		String query = "SELECT count(DISTINCT C.copynum) FROM Loan L, Copy C, Book B WHERE B.booknum = '" + booknum + "' AND B.bookNum = C.bookNum AND C.copyNum = L.copyNum AND L.ret IS NOT NULL  AND L.copynum NOT IN(SELECT copynum FROM Loan WHERE ret IS NULL)";
		r = s.executeQuery(query);
		//r.next();
		System.out.println("There are currently " + r.getString(1) + " available copies of " + title + ".");
	}
	
	//#4
	public void currentBooksCheckedOut(String cid) throws SQLException {
		s = con.createStatement();
		ResultSet temp = s.executeQuery("SELECT first, last FROM Customer WHERE cid = " + cid);
		String name = temp.getString(1) + " " + temp.getString(2);
		//Creating a header for results
		System.out.println("Books currently checked out by " + name + ":");
		r = s.executeQuery("SELECT title FROM Book B, Copy C, Loan L, Customer X WHERE B.booknum = C.booknum AND C.copynum = L.copynum AND L.cid = X.cid AND X.cid = '" + cid + "' AND L.out IS NOT NULL AND L.ret IS NULL");
		while(r.next()) {
			System.out.println(r.getString(1));
		}
	}
	
	//#5
	public void overdueBooks(String cid) throws SQLException {
		Statement temp = con.createStatement();
		ResultSet temp2 = temp.executeQuery("SELECT first, last FROM Customer WHERE cid = '" + cid + "'");
		String name = temp2.getString(1) + " " + temp2.getString(2);
		s = con.createStatement();
		String query = "SELECT title FROM Book B, Copy C, Loan L, Customer X WHERE X.cid = '" + cid +"' AND X.cid = L.cid AND L.copynum = C.copynum AND C.booknum = B.booknum AND L.ret IS NULL AND L.due < date('now')";
		ResultSet r = s.executeQuery(query);
		System.out.println("Current books overdue for " + name);
		while(r.next()) {
			System.out.println(r.getString(1));
		}
	}
	
	//#6
	public void loadCustomers(String filename) throws SQLException{
		try {
		BufferedReader reader = new BufferedReader(new FileReader(filename));
		p = con.prepareStatement("INSERT INTO Customer VALUES(?, ?, ?, ?)");
		String line = reader.readLine();
		while(line != null) {
			System.out.println(line);
			Scanner scan = new Scanner(line);
			p.setString(1, scan.next());
			p.setString(2, scan.next());
			p.setString(3, scan.next());
			p.setString(4, scan.next());
			p.addBatch();
			line = reader.readLine();
		}
			p.executeBatch();
			System.out.println("Customers have been added.");
		}
		catch(Exception e) {
			System.out.println(e.getMessage());
		}
	}
	
	//#7
	public void returnBook(String copynum, String cid) throws SQLException {
		Statement temp = con.createStatement();
		r = temp.executeQuery("SELECT cid FROM Loan L WHERE L.copynum = '" + copynum + "' AND L.ret IS NULL");
		String id = r.getString(1);
		
		if(id.equals(cid) == false) {
			System.out.println("This book is not checked out by customer ID " + cid);
		} else {
			s = con.createStatement();
			System.out.println("Enter today's date: (YYYY-MM-DD)");
			String date = Keyboard.readString();
			s.executeUpdate("UPDATE Loan SET ret = '" + date + "' WHERE copynum = '" + copynum + "' AND ret IS NULL");
			System.out.println("Book has been returned.");
		}
	}
	
	public static void main(String[] args) throws SQLException {
		SQLFunctions s = new SQLFunctions("jdbc:sqlite://C:/Users/Alex/Desktop/BookDatabase");
		System.out.println("Choose a number for the corresponding function:");
		System.out.println("1) Display contents of a table.");
		System.out.println("2) Show book numbers and titles of books written by an author.");
		System.out.println("3) Show number of available copies of a book.");
		System.out.println("4) Display current books checked out by a customer.");
		System.out.println("5) Show over due books for a customer.");
		System.out.println("6) Add new customer information.");
		System.out.println("7) Return a book.");
		int menuChoice = Keyboard.readInt();
		
		if(menuChoice == 1) {
			System.out.println("Type a table name:");
			String tableName = Keyboard.readString();
			System.out.println("Which attributes would you like to see? (separate by a whitespace)");
			String columns = Keyboard.readString();
			Scanner scan = new Scanner(columns);
			Vector<String> v = new Vector<>();
			int size = 0;
			while(scan.hasNext()) {
				v.add(scan.next());
				size++;
			}
			String[] col = new String[size];
			for(int i = 0; i < v.size(); i++) {
				col[i] = v.elementAt(i);
			}
			s.contents(tableName, col);
			scan.close();
		} 
		
		if(menuChoice == 2) {
			System.out.println("Enter an author's name:");
			String name = Keyboard.readString();
			Scanner scan = new Scanner(name);
			String first = scan.next();
			String last = scan.next();
			s.bookNums(first, last);
			scan.close();
		}
		
		if(menuChoice == 3) {
			System.out.println("Enter a book number:");
			String bnum = Keyboard.readString();
			s.availableCopies(bnum);
		}
		
		if(menuChoice == 4) {
			System.out.println("Enter a customer's id:");
			String id = Keyboard.readString();
			s.currentBooksCheckedOut(id);
		}
		
		if(menuChoice == 5) {
			System.out.println("Enter a customer's id:");
			String id = Keyboard.readString();
			s.overdueBooks(id);
		}
		
		if(menuChoice == 6) {
			System.out.println("Enter the name of a text file with customer data to be added to the database:");
			String file = Keyboard.readString();
			s.loadCustomers(file);
		}
		
		if(menuChoice == 7) {
			System.out.println("Enter copy number and customer id:");
			String data = Keyboard.readString();
			Scanner scan = new Scanner(data);
			s.returnBook(scan.next(), scan.next());
			scan.close();
		}
	}
}
