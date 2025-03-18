import java.util.Vector;

public class WordStorage {
		
		String word;
		Vector<Integer> documents;
	
	public WordStorage(String word, int firstDoc) {
		documents = new Vector<Integer>();
		this.word = word;
		documents.addElement(firstDoc);
	}
	
	public String getWord() {
		return word;
	}
	
	public void addDocument(int d) {
		documents.addElement(d);
	}	
}
