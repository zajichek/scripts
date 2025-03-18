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
 
 public double getParametricProb(TestDocument d) {
  double likelihood = 0;
  for (String key : words.keySet()) {
    double lambda = ((double) words.get(key)) / ((double) totalDocs);
    int observedCount = 0;
    if(d.doc.get(key) != null) {
     observedCount = d.doc.get(key);
    } 
    likelihood += Poisson.logPmf(observedCount, lambda);
  }
  return likelihood;
 }
 
 public double getNonParametricProb(TestDocument d) {
  double likelihood = 0;
  double delta = 0.01;
  for (String key : d.doc.keySet()) {
   if(words.get(key) != null) {
    double w = Math.log((1 - delta) * ((double)words.get(key) / (double)totalWords) + (delta) * 
                        (1 / (double)totalWords));
    likelihood += d.doc.get(key) * w;
   }
   else {
    double w = Math.log(delta) + Math.log(1 / (double)totalWords);
    likelihood += d.doc.get(key) * w;
   }
   
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