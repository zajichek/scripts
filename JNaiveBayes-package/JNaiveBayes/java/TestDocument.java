import java.io.*;
import java.util.*;

public class TestDocument {
		Map<String, Integer> doc;
		
		public TestDocument(File f) {
			doc = new HashMap<String,Integer>();
			load(f);
		}
		
		private void load(File f) {
			try {
				BufferedReader reader = new BufferedReader(new FileReader(f));
				String word = reader.readLine();
				while(word != null) {
					if(doc.containsKey(word)) {
						doc.put(word, doc.get(word) + 1);
					} else {
						doc.put(word, 1);
					}
					word = reader.readLine();	
				}
				reader.close();
			} catch(Exception e) {
				e.getMessage();
			}
		}
}