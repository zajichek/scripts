import java.io.*;
import java.util.*;

public class Model {
	
	int numCats;
	boolean parametric;
	double alpha;
	Category[] categories;
	double[] priorProbs;
	double[][] notNormProbs;
	
	public Model(String tr, String te, boolean type) {
		File[] train = new File(tr).listFiles();
		File[] test = new File(te).listFiles();
		parametric = type;
		categories = new Category[train.length];
		int total = createCategories(train);
		
		priorProbs = new double[train.length];
		for(int i = 0; i < priorProbs.length; i++) {
			priorProbs[i] = ((double)categories[i].totalDocs)/((double)total);
		}
		
		notNormProbs = new double[test.length][priorProbs.length];
		
		if(parametric) {
			predictParametric(test);
		} else {
			predictNonParametric(test);
		}
	}
	
	private int createCategories(File[] f) {
		int totalDocs = 0;
		for(int i = 0; i < f.length; i++) {
			categories[i] = new Category(f[i]);
			totalDocs = totalDocs + categories[i].totalDocs;
		}
		return totalDocs;
	}
	
	private void predictParametric(File[] f) {
		for(int i = 0; i < f.length; i++) {
			TestDocument d = new TestDocument(f[i]);
			for(int j = 0; j < priorProbs.length; j++) {
				notNormProbs[i][j] = Math.log(priorProbs[j]) +  
						(categories[j].getParametricProb(d));
			}
		}
	}
	
	private void predictNonParametric(File[] f) {
		for(int i = 0; i < f.length; i++) {
			TestDocument d = new TestDocument(f[i]);
			for(int j = 0; j < priorProbs.length; j++) {
				notNormProbs[i][j] = Math.log(priorProbs[j]) +  
						(categories[j].getNonParametricProb(d));
			}
		}
	}
	
	public static void main(String args[]) {
		Model m = new Model("Data/Training", "Data/Testing", true);
		for(int i = 0; i < m.priorProbs.length; i++) {
			System.out.println(m.priorProbs[i]);
		}
		for(int i = 0; i < m.notNormProbs.length; i++) {
			double total = 0;
			double cat[] = new double[2];
			for(int j = 0; j < m.notNormProbs[i].length; j++) {
				total += Math.exp(m.notNormProbs[i][j]);
				cat[j] = Math.exp(m.notNormProbs[i][j]);
			}
			System.out.println("Document" + i);
			System.out.println("Personal: " + cat[0] / total);
			System.out.println("Private: " + cat[1] / total);
			System.out.println("");
		}
	}
 }
