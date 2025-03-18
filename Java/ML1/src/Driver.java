import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.util.Vector;

public class Driver {
	HashVector h;
	Matrix m, q;
	String[] uniqueWords;
	File docs, queries;
	String[] docNames, queryNames;
	String docPath;
	String queryPath;
	int[][] dotProd;
	double[][] cos;
	Vector<SimilarityStorage>[] cosineSimilarity, dotProdSimilarity;
	
	public Driver(String dp, String qp) {
		docPath = dp;
		queryPath = qp;
		
		h = new HashVector(20);
		populateHashVector();

		populateMatrix();
		
		populateQueryMatrix();
		writeMatrices();
		multiply();
		//wordTotals();
		//printResults();
	}
	
	public void writeMatrices() {
		String h1 = "Word Q1 Q2 Q3 Q4 Q5";
		
		String h2 = "Word";
		for(int n = 0; n < docNames.length; n++) {
			h2 = h2 + " " + docNames[n]; 
		}
		
		try {
			PrintWriter w1 = new PrintWriter(new FileWriter("Q.txt"));
			PrintWriter w2 = new PrintWriter(new FileWriter("M.txt"));
			
			w1.println(h1);
			w2.println(h2);
			
			for(int i = 0; i < uniqueWords.length; i++) {
				String x = uniqueWords[i];
				String y = uniqueWords[i];
				for(int j = 0; j < queryNames.length; j++) {
					x = x + " " + q.matrix[i][j];
				}
				for(int k = 0; k < docNames.length; k++) {
					y = y +  " " + m.matrix[i][k];
				}
				
				w1.println(x);
				w2.println(y);
				
			}
			
			w1.flush();
			w2.flush();
			w1.close();
			w2.close();
		} catch(Exception e) {
			
		}
	}
	
	public void multiply() {
		dotProd = new int[queryNames.length][docNames.length];
		cos = new double[queryNames.length][docNames.length];
		int[][] temp1 = transpose(q.matrix);
		int[][] temp2 = transpose(m.matrix);
		for(int i = 0; i < temp1.length; i++) {
			for(int j = 0; j < temp2.length; j++) {
				dotProd[i][j] = dotProduct(temp2[j], temp1[i]);
				cos[i][j] = cosine(temp2[j], temp1[i]);
			}
		}
		
		findMaximums();
	}
	
	public void findMaximums() {
		//Need one for each query (make an array)
		cosineSimilarity = new Vector[queryNames.length];
		dotProdSimilarity = new Vector[queryNames.length];
		
		for(int i = 0; i < queryNames.length; i++) {
			cosineSimilarity[i] = new Vector<SimilarityStorage>();
			dotProdSimilarity[i] = new Vector<SimilarityStorage>();
		}
		
		for(int i = 0; i < queryNames.length; i++) {
			for(int j = 0; j < docNames.length; j++) {
				if(cos[i][j] != 0) {
				cosineSimilarity[i].addElement(new SimilarityStorage(docNames[j], cos[i][j]));
				}
				if(dotProd[i][j] != 0) {
					dotProdSimilarity[i].addElement(new SimilarityStorage(docNames[j], dotProd[i][j]));
				}
			}
			
		}
		
		try {
			PrintWriter writer = new PrintWriter(new FileWriter("Results.txt"));
		for(int g = 0; g < queryNames.length; g++) {
			writer.println("Query: " + queryNames[g] + "   Cosine Similarity");
			writer.println();
			int topTen = 0;
			while(topTen < 10) {
				
				double max = cosineSimilarity[g].elementAt(0).cosine;
				int place = 0;
					for(int z = 0; z < cosineSimilarity[g].size(); z++) {
						if(cosineSimilarity[g].elementAt(z).cosine >= max) {
							max = cosineSimilarity[g].elementAt(z).cosine;
							place = z;
						}
					}
					writer.println((topTen + 1) + ") " + cosineSimilarity[g].elementAt(place).word + ": " + cosineSimilarity[g].elementAt(place).cosine);
					cosineSimilarity[g].removeElementAt(place);
					topTen = topTen + 1;
			}
			
			writer.println("_____________________________________");
			writer.println();
			
			

		}
		writer.println("__________________________________________________________________");
		
		for(int g = 0; g < queryNames.length; g++) {
			writer.println("Query: " + queryNames[g] + "  Dot Product Similarity");
			writer.println();
			int topTen = 0;
			while(topTen < 10) {
				
				int max = dotProdSimilarity[g].elementAt(0).dotProd;
				int place = 0;
					for(int z = 0; z < dotProdSimilarity[g].size(); z++) {
						if(dotProdSimilarity[g].elementAt(z).dotProd >= max) {
							max = dotProdSimilarity[g].elementAt(z).dotProd;
							place = z;
						}
					}
					writer.println((topTen + 1) + ") " + dotProdSimilarity[g].elementAt(place).word + ": " + dotProdSimilarity[g].elementAt(place).dotProd);
					dotProdSimilarity[g].removeElementAt(place);
					topTen = topTen + 1;
			}
			
			writer.println("_____________________________________");
			writer.println();
		}
		
		writer.flush();
		writer.close();
		} catch(Exception e) {
			
		}
	}
	
	
	public int[][] transpose(int[][] matrix) {
		int[][] t = new int[matrix[0].length][matrix.length];
		
		for(int i = 0; i < t.length; i++) {
			for(int j = 0; j < t[i].length; j++) {
				t[i][j] = matrix[j][i];
			}
		}
		
		return t;
	}
	
	public void populateHashVector() {
		docs = new File(docPath);
		docNames = new String[docs.list().length];
		int docNum = 0;
		int numUniqueWords = 0;
		
		//Going through each file in the directory
		for(int i  = 0; i < docs.list().length; i++) {
			docNames[i] = docs.list()[i];
			try{
	
				BufferedReader reader = new BufferedReader(new FileReader(docPath + docs.list()[i]));
				String line = reader.readLine();
				while(line != null) {
					boolean wordFound = h.addString(line, docNum);
					line = reader.readLine();
					if(!wordFound) {
						numUniqueWords = numUniqueWords + 1;
					}
				}
				reader.close();
				
			} catch(Exception e) {
				System.out.println(e.getMessage());
			}
			docNum = docNum + 1;
		}
		uniqueWords = new String[numUniqueWords];

	}
	
	public void populateMatrix() {
		m = new Matrix(uniqueWords.length, docs.list().length);
		
		int word = 0;
		for(int i = 0; i < h.index.length; i++) {
			for(int j = 0; j < h.index[i].size(); j++) {
				uniqueWords[word] = h.index[i].elementAt(j).getWord();
					for(int k = 0; k < h.index[i].elementAt(j).documents.size(); k++) {
						m.matrix[word][h.index[i].elementAt(j).documents.elementAt(k)]++; 
					}
					word = word + 1;
			}
		}
	}
	
	public void populateQueryMatrix() {
		queries = new File(queryPath);
		q = new Matrix(uniqueWords.length, queries.list().length);
		queryNames = new String[queries.list().length];
		
		for(int i = 0; i < queries.list().length; i++) {
			queryNames[i] = queries.list()[i];
			
			try {
				BufferedReader reader = new BufferedReader(new FileReader(queryPath + queries.list()[i]));
				String line = reader.readLine();
				while(line != null) {
					for(int j  = 0; j < uniqueWords.length; j++) {
						if(line.equals(uniqueWords[j])) {
							q.matrix[j][i]++;
						}
					}
					line = reader.readLine();
				}
			} catch(Exception e) {
				System.out.println(e.getMessage());
			}
		}
	}
	
	public void printResults() {
		for(int i = 0; i < m.numRows; i++) {
			System.out.print(uniqueWords[i]);
			for(int j  = 0; j < m.numColumns; j++) {
				System.out.print(" " + m.matrix[i][j]);
			}
			System.out.println();
		}
	}
	
	public void wordTotals() {
		int sum;
		for(int i = 0; i < m.numRows; i++) {
			System.out.print(uniqueWords[i]);
			sum = 0;
			for(int j = 0; j < m.numColumns; j++) {
				sum = sum + m.matrix[i][j];
			}
			System.out.print(" " + sum);
			System.out.println();
		}
	}
	
	public int dotProduct(int[] x, int[] y)  {
		int dp = 0;
		
		for(int i = 0; i < x.length; i++) {
			dp = dp + x[i] * y[i];
		}
		
		return dp;
	}
	
	public double cosine(int[] x, int[] y) {
		double denom = norm(x)*norm(y);
		
		return ((double) dotProduct(x, y))/denom;
	}
	
	public double norm(int[] x) {
		double n = 0;
		
		for(int i = 0; i < x.length; i++) {
			n = n + x[i]*x[i];
		}
		
		return Math.sqrt(n);
	}
	
	public static void main(String[] args) {
		Driver d = new Driver("/Users/alexzajichek/Documents/workspace/ML1/Docs/", "/Users/alexzajichek/Documents/workspace/ML1/Queries/");
	}
}
