import java.io.*;
import java.util.*;

public class Model extends Object {
	
	int numCats;
	boolean parametric;
	double delta;
	Category[] categories;
	double[] priorProbs;
	double[][] notNormProbs;
	String[] labels;
	String[] testNames;
	
	public Model(String tr, String te, boolean type, double del) {
		File[] train = new File(tr).listFiles();
		File[] test = new File(te).listFiles();
		
		labels = new String[train.length];
		testNames = new String[test.length];
		
		parametric = type;
		delta = del;
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
			labels[i] = f[i].toString();
			totalDocs = totalDocs + categories[i].totalDocs;
		}
		return totalDocs;
	}
	
	private void predictParametric(File[] f) {
		for(int i = 0; i < f.length; i++) {
			TestDocument d = new TestDocument(f[i]);
			testNames[i] = f[i].toString();
			for(int j = 0; j < priorProbs.length; j++) {
				notNormProbs[i][j] = Math.log(priorProbs[j]) +  
						(categories[j].getParametricProb(d, delta));
			}
		}
	}
	
	private void predictNonParametric(File[] f) {
		for(int i = 0; i < f.length; i++) {
			TestDocument d = new TestDocument(f[i]);
			testNames[i] = f[i].toString();
			for(int j = 0; j < priorProbs.length; j++) {
				notNormProbs[i][j] = Math.log(priorProbs[j]) +  
						(categories[j].getNonParametricProb(d, delta));
			}
		}
	}
 }
