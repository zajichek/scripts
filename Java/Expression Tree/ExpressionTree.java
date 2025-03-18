/*
 * Author: Alex Zajichek
 * CS 340 Fall 2014
 * UW - La Crosse
 */
/*
 * This program builds expression trees through implementation of the post order and in order traversals.
 */

import java.util.*;
import java.io.*;

public class ExpressionTree {

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

	public static final int INFIX = 1;
	public static final int POSTFIX = 2;

	Node root;

	public ExpressionTree(String exp, int format) {
		if (format == INFIX)
			buildInfix(exp);
		else 
			buildPostfix(exp);
	}

	private void buildPostfix(String exp) {
		//Each iteration resets the root to null
		root = new Node(null, null, null);
		Stack<Node> operands = new Stack();
		Scanner scan = new Scanner(exp);
		while(scan.hasNext()) {
			root = new Node(null, null, null);
			String temp = scan.next();
			//If 'temp' is an integer, push it on the stack, 
			//if it is not a unary minus operator, create a subtree and push it back in the stack
			//If it is a unary minus, creates a subtree with only a left node
			if(Character.isDigit(temp.charAt(0))) {
				Node value = new Node(null, temp, null);
				operands.push(value);
			} else if(temp.charAt(0) != '!') {
				root.data = temp;
				root.right = operands.pop();
				root.left = operands.pop();
				Node tempRoot = root;
				operands.push(tempRoot);
			} else {
				root.data = temp;
				root.left = operands.pop();
				root.right = null;
				Node tempRoot = root;
				operands.push(tempRoot);
			}
		}	
	}

	private void buildInfix(String exp) {
		//Stack of operands and operators for use throughout the method
		Stack<Node> operands = new Stack();
		Stack<Node> operators = new Stack();
		//In order to keep precedence, uses temporary stack for the appropriate nodes
		Stack<Node> tempOperators = new Stack();
		Stack<Node> tempOperands = new Stack();
		//In the case of left associative operators, use another stack to ensure these are computed in the correct order
		Stack<Node> leftAssociative = new Stack();
		Scanner scan = new Scanner(exp);
		while(scan.hasNext()) {
			root = new Node(null, null, null);
			String temp = scan.next();
			//If temp is an integer, push it in the operand stack
			if(Character.isDigit(temp.charAt(0))) {
				Node number = new Node(null, temp, null);
				operands.push(number);
			//If temp is not a closed parenthesis, push it on the operator stack
			} else if(temp.charAt(0) != ')') {
				Node operator = new Node(null, temp, null);
				operators.push(operator);
			} else {
			//If temp is a closed parenthesis, apply the following
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
		//After there are no more values in the file, and all parenthesis expressions are taken care of first,
		//Follow the same process as above, except it waits until the stack is empty
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
	}

	public int evaluate() {
		return evaluate(root);
	}
	
	public int evaluate(Node r) {
		//Recursively evaluates each subtree
		int x = 0;
		int y = 0;
		if(r.data.charAt(0) == '!') {
			if(r.left==null) {
				return Integer.parseInt(r.data);
			} else {
				x = evaluate(r.left);
			}
				return x*-1;
		}
		
		if(r.left == null && r.right == null) {
			return Integer.parseInt(r.data);
		} else {
		x = evaluate(r.left);
		y = evaluate(r.right);
		}
		return operation(x, r.data, y);
	}

	public String toPostfix() {
		return toPostfix(root);
	}

	private String toPostfix(Node r) {
	//Recursively outputs the post order expression of the tree
		String x;
		String y;
		if(r.data.charAt(0) == '!') {
			if(r.left == null) {
				return r.data;
			} else {
				x = toPostfix(r.left);
			}
				return x + " " + r.data;
		}
		
		if(r.left == null && r.right == null) {
			return r.data;
		} else {
		x = toPostfix(r.left);
		y = toPostfix(r.right);
		}
		return order(x, r.data, y);
	}

	public String toInfix() {
		return toInfix(root);
	}

	private String toInfix(Node r) {
		//Recursively outputs the in order expression of the tree with parenthesis
		String x;
		String y;
		if(r.data.charAt(0) == '!') {
			if(r.left == null) {
				return r.data;
			} else {
				x = toPostfix(r.left);
			}
				return x + " " + r.data;
		}
		
		if(r.left == null && r.right == null) {
			return r.data;
		} else {
			x = toInfix(r.left);
			y = toInfix(r.right);
		}
		return "(" + x + r.data + y + ")";
	}
	
	//Checks the precedence of a given operator, used in buildInfix
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
	
	//Returns the post ordering of a subtree
	public String order(String l, String o, String r) {
		return l + " " + r + " " + o + " ";
	}
	
	//Used in 'evaluate()', does the correct operation for the node value
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
	




	public static void main(String args[]) throws IOException{
		BufferedReader b1 = new BufferedReader(new FileReader(args[0]));
		ExpressionTree e;
		String exp = b1.readLine();
		while (!exp.equals("")) {
			e = new ExpressionTree(exp,ExpressionTree.POSTFIX);
			System.out.println("Infix format: " + e.toInfix());
			System.out.println("Postfix format: " + e.toPostfix());

			System.out.println("Expression value: "+e.evaluate());
			System.out.println();
			exp = b1.readLine();
		}
		exp = b1.readLine();
		while (exp != null) {
			e = new ExpressionTree(exp,ExpressionTree.INFIX);
			System.out.println("Infix format: " + e.toInfix());
			System.out.println("Postfix format: " + e.toPostfix());

			System.out.println("Expression value: "+ e.evaluate());
			System.out.println();
			exp = b1.readLine();
		}


	}
}