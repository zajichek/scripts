import java.io.*;
import java.util.*;

public class Category {

  Map<String,Integer> words;
  int totalWords, totalDocs;
  BufferedReader reader;
  File[] files;
 
 public Category(File f){
  words = new HashMap<String, Integer>();
  files = f.listFiles();
  totalWords = 0;
  totalDocs = 0;
  load();
 }
 
  public double getParametricProb(TestDocument d, double del) {
  double delta = del;
  double likelihood = 0;
  for (String key : d.doc.keySet()) {
    int observedCount = d.doc.get(key);
    double p_w = delta * (1/(double)totalWords);
    if(words.get(key) != null) {
      double lambda = ((double) words.get(key)) / ((double) totalDocs);
      double poisProb = Math.exp(Poisson.logPmf(observedCount, lambda))/(1-Math.exp(-lambda));
      p_w += (1-delta)*poisProb;
     }
    likelihood += Math.log(p_w);
  }
  return likelihood;
 }
  
  
 public double getNonParametricProb(TestDocument d, double del) {
  double delta = del;
  double likelihood = 0;
  for (String key : d.doc.keySet()) {
    double observedCount = d.doc.get(key);
    double p_w = delta * (1/(double)totalWords);
    if(words.get(key) != null) {
      p_w += (1-delta)*((double)words.get(key) / (double) (totalWords));
     }
    likelihood += observedCount*Math.log(p_w);
  }
  return likelihood;
 }
 
 private void load() {
  try{
   for(File f: files) {
    reader = new BufferedReader(new FileReader(f));
    String word = reader.readLine();
    while(word != null) {
      if(words.containsKey(word)) {
       words.put(word, words.get(word) + 1);
      } else {
       words.put(word, 1);
      }
      totalWords++;
      word = reader.readLine(); 
    }
    totalDocs++;
    reader.close();
   }
  } catch(Exception e) {
   e.getMessage();
  }
 }
}
