/*
 * Author: Alex Zajichek
 * CS 340, Fall 2014
 * 11/19/2014
 */
/*
 * The 'VariableEvaluation' class evaluates expressions read from a file, stores variable values into a 'SymbolTable', 
 * and prints out the final values of the variables.
 */

import java.io.FileReader;
import java.io.BufferedReader;
import java.util.Scanner;
import java.io.IOException;
import java.util.Stack;

public class VariableEvaluation {
	
	Node root;
	SymbolTable table;
	SymbolTable.STIterator i;
	
	private class Node {
		Node left;
		String data;
		Node right;

		Node(Node l, String d, Node r) {
			left = l;
			data = d;
			right = r;
		}
	}
	
	public VariableEvaluation() {
		//Used a symbol table of size 10, though any size could be used.
		root = new Node(null, null, null);
		table = new SymbolTable(10);
	}
	
	//Builds a tree for each expression in the file, and stores variable values in the symbol table
	public boolean trees(String line) throws IOException {
				//Stack of operands and operators for use throughout the method
				Stack<Node> operands = new Stack();
				Stack<Node> operators = new Stack();
				//In order to keep precedence, uses temporary stack for the appropriate nodes
				Stack<Node> tempOperators = new Stack();
				Stack<Node> tempOperands = new Stack();
				//In the case of left associative operators (+,-,*,/,%), another stack is used
				//to ensure these are computed in the correct order
				Stack<Node> leftAssociative = new Stack();
				Scanner scan = new Scanner(line);
				//The first value in the expression will always be the variable
				String variable = scan.next();
				//The '=' sign is passed over here
				scan.next();
				while(scan.hasNext()) {
					root = new Node(null, null, null);
					String temp = scan.next();
					//If temp is an integer, push it in the operand stack
					if(Character.isDigit(temp.charAt(0))) {
						Node number = new Node(null, temp, null);
						operands.push(number);
					//If temp is a variable, this case is examined
					} else if(isVariable(temp.charAt(0))) {
					//If the variable in question has been given a value, it is put into the tree as an operand
						if((table.getData(temp) instanceof Integer) == true) {
							Node val = new Node(null, temp, null);
							operands.push(val);
						} else {
					//If the variable has not been defined, the 'error' method is called, and the variable is stored 
					//in the table with "unassigned" as its data
							error(line);
							System.out.println();
							table.insert(variable, "unassigned");
							return false;
						}
					//If temp is not a closed parenthesis, push it on the operator stack
					} else if(temp.charAt(0) != ')') {
						Node operator = new Node(null, temp, null);
						operators.push(operator);
					} else {
					//If temp is a closed parenthesis, it will be evaluated until it reaches the open parenthesis,
					//and will then push it on the operand stack
						while(operators.peek().data.charAt(0) != '(') {
							root = new Node(null, null, null);
					//Refer to method 'precedence' for classifications
							int preced = 1;
							while(preced < 5) {
					//Checks for unary minus operators
								if(preced == 1) {
									while(operators.peek().data.charAt(0) != '(') {
					//If the top operator does not have the same precedence, put it in the tempOperators stack
										if(precedence(operators.peek().data) != preced) {
										tempOperators.push(operators.pop());
										tempOperands.push(operands.pop());
										} else {
					//If it does have correct precedence, create a new root with the correct operand
											root.data = operators.pop().data;
											root.right = null;
											root.left = operands.pop();
											Node tempRoot = root;
											operands.push(tempRoot);
										}
									}
									
							} else
					//Follow same procedure, searching for '^' as second preference
								if(preced == 2) {
								while(operators.peek().data.charAt(0) != '(') {
								if(precedence(operators.peek().data) != preced) {
								tempOperators.push(operators.pop());
								tempOperands.push(operands.pop());
								} else {
									root = new Node(null, null, null);
									root.data = operators.pop().data;
									root.right = operands.pop();
									root.left = operands.pop();
									Node tempRoot = root;
									operands.push(tempRoot);
								}
							}
						 } else {
						//All others are left associative. * and / and % are precedence 3, and + and - are precedence 4
								while(operators.peek().data.charAt(0) != '(') {
								if(precedence(operators.peek().data) != preced) {
								tempOperators.push(operators.pop());
								tempOperands.push(operands.pop());
								} else {
						//To keep the left associativity in tact, we push these a separate stack
									leftAssociative.push(operators.pop());
									tempOperands.push(operands.pop());
								}
							}
						//Empties out the correct operators, creates and push subtrees
									while(leftAssociative.isEmpty() != true) {
										root = new Node(null, null, null);
										root.data = leftAssociative.pop().data;
										root.right = tempOperands.pop();
										root.left = operands.pop();
										Node tempRoot = root;
										operands.push(tempRoot);
									
							     }
						      }
						//Through each iteration of precedence, this will put everything back into the respective stacks
								while(tempOperators.isEmpty() != true) {
									operators.push(tempOperators.pop());
								}
								while(tempOperands.isEmpty() != true) {
									operands.push(tempOperands.pop());
								}
								
								preced++;
							}
						}
						//Pop's off the remaining '(' after everything is finished
						operators.pop();
					}
				}
				
				//If there are no operators, then there must only be 1 operand, which must be the root
				if(operators.isEmpty()) {
					root = operands.pop();
				}
				
				//After there are no more values in the line, and all parenthesis expressions are taken care of first,
				//the process is repeated for what is remaining in the stacks
						while(operators.isEmpty() != true) {
							root = new Node(null, null, null);
							int preced = 1;
							while(preced < 5) {
								if(preced == 1) {
									while(operators.isEmpty() != true) {
									if(precedence(operators.peek().data) != preced) {
										tempOperators.push(operators.pop());
										tempOperands.push(operands.pop());
									} else {
										root = new Node(null, null, null);
											root.data = operators.pop().data;
											root.left = operands.pop();
											root.right = null;
											Node tempRoot = root;
											operands.push(tempRoot);
										}
									}
							}else 
								if(preced == 2) {
									while(operators.isEmpty() != true) {
									if(precedence(operators.peek().data) != preced) {
									tempOperators.push(operators.pop());
									tempOperands.push(operands.pop());
									} else {
										root = new Node(null, null, null);
										root.data = operators.pop().data;
										root.right = operands.pop();
										root.left = operands.pop();
										Node tempRoot = root;
										operands.push(tempRoot);
									}
								}
							 } else {
									while(operators.isEmpty() != true) {
									if(precedence(operators.peek().data) != preced) {
									tempOperators.push(operators.pop());
									} else {
										leftAssociative.push(operators.pop());
										tempOperands.push(operands.pop());
									}
								}
										while(leftAssociative.isEmpty() != true) {
											root = new Node(null, null, null);
											root.data = leftAssociative.pop().data;
											root.right = tempOperands.pop();
											root.left = operands.pop();
											Node tempRoot = root;
											operands.push(tempRoot);
										
								     }
							      }
									while(tempOperators.isEmpty() != true) {
										operators.push(tempOperators.pop());
									}
									while(tempOperands.isEmpty() != true) {
										operands.push(tempOperands.pop());
									}
									
									preced++;
							}
						}	
		
		//If the variable on the left hand side is not in the table already,
		//it is inserted as a key, with its data being the evaluation of the root
		//If it is in the table already, the data is just updated to the key that already exists
		if(table.find(variable) == false) {
			table.insert(variable, evaluate(root));
		} else {
			table.setValue(variable, evaluate(root));
		}
		return true;
}
	
	//Checks the precedence of a given operator
	public int precedence(String operator) {
		if(operator.charAt(0) == '!') {
			return 1;
		} else {
			if(operator.charAt(0) == '^') {
				return 2;
			} else {
				if(operator.charAt(0) == '*' || operator.charAt(0) == '/' || operator.charAt(0) == '%') {
					return 3;
				} else {
					return 4;
				}
			}
		}
	}
	
	//For distinguishing characters in the file, determines what isn't a variable or a number
	public boolean isOperator(String ch) {
		switch(ch.charAt(0)) {
		case '!': return true;
		case '^': return true;
		case '*': return true;
		case '/': return true;
		case '%': return true;
		case '+': return true;
		case '-': return true;
		case '(': return true;
		case ')': return true;
		}
		return false;
	}
	
	//When reading from the file, if the character is not an operator or a number, it must be a variable
	//PRE: A variable does not begin with a number or an operator symbol, and does not have spaces
	public boolean isVariable(char c) {
		if(!isOperator(Character.toString(c)) && !Character.isDigit(c)) {
			return true;
		}
		
		return false;
	}
	
	//This method is called if a variable does not have an integer value as its data,
	//An error is printed because the expression can't be evaluated
	public void error(String line) {
		Scanner scan = new Scanner(line);
		System.out.println("Error: " + line);
		scan.next();
		scan.next();
		while(scan.hasNext()) {
			String q = scan.next();
			if(isVariable(q.charAt(0)) && !(table.getData(q) instanceof Integer)) {
				System.out.println(q + " has not been assigned a value");
			}
		}
	}
	
	public int evaluate(Node r) {
		//Recursively evaluates each subtree
		int x = 0;
		int y = 0;
		
		//Takes care of a unary minus
		if(r.data.charAt(0) == '!') {
			if(r.left==null && isVariable(r.data.charAt(0))) {
				return (int) table.getData(r.data);
			} else if(r.left == null) {
				return Integer.parseInt(r.data);
			} else {
				x = evaluate(r.left);
			}
				return x*-1;
		}
		
		//If it's a variable, gets the data associated with it, and applies operation
		if(r.left == null && r.right == null && isVariable(r.data.charAt(0))) {
			return (int) table.getData(r.data);
		} else if(r.left == null && r.right == null) {
			return Integer.parseInt(r.data);
		} else {
		x = evaluate(r.left);
		y = evaluate(r.right);
		}
		return operation(x, r.data, y);
	}
	
	//Gets two values to be operated on
	public int operation(int l, String o, int r) {
		int left = l;
		int right = r;
		
		switch(o.charAt(0)) {
		case '!': return left*-1;
		case '^': return (int) Math.pow(left, right);
		case '*': return left * right;
		case '/': return left / right;
		case '%': return left % right;
		case '+': return left + right;
		case '-': return left - right;
		}
		return 0;
		}
	
	//'printTables() is called once all lines have been read, and final variable values have been stored into the SymbolTable
	public void printTables() {
		SymbolTable.STIterator i = table.new STIterator();
		//initialKey() grabs the data of the first node that is pointed to in the constructor of STIterator
		System.out.println(i.initialKey() + " = " + table.getData(i.initialKey()));
		//Prints out remaining variables in the symbol table and their values 
		while(i.hasNext()) {
		String r = i.next();
		System.out.println(r + " = " + table.getData(r));
		}
	}
	
	
	public static void main(String[] args) throws IOException {
		VariableEvaluation n = new VariableEvaluation();
		//Reading the file 'expressions.txt' from the command line
		BufferedReader reader = new BufferedReader(new FileReader(args[0]));
		String line = reader.readLine();
		//For each line, the tree is built and information is stored in the table
		while(line != null) {
			n.trees(line);
			line = reader.readLine();
		}
		
		//Once all values are stored in the table, data is printed
		n.printTables();
	}
}
