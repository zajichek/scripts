public class NaiveBayesDriver {
 
  public static void main(String args[]) {
  }
  
  public Model buildModel(String train, String test, boolean type, double delta) {
      Model m = new Model(train, test, type, delta);
      return m;
  }

  public double[][] probsArray(Model m) {
      return m.notNormProbs;
  }
 
  public String[] labelsArray(Model m) {
      return m.labels;
  }
 
  public String[] testNamesArray(Model m) {
      return m.testNames;
  }
}
