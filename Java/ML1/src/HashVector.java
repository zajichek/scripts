import java.util.Vector;

public class HashVector {
	Vector<WordStorage>[] index;
	
	public HashVector(int size) {
		index = new Vector[size];
		
		for(int i = 0; i < index.length; i++) {
			index[i] = new Vector<WordStorage>();
		}
	}
	
	public boolean addString(String s, int doc) {
		int ind = hash(s);
		
		boolean wordFound = false;
		for(int i = 0; i < index[ind].size(); i++) {
			if(index[ind].elementAt(i).getWord().equals(s)) {
				index[ind].elementAt(i).addDocument(doc);
				wordFound = true;
				break;
			}
		}
		
		if(!wordFound) {
			index[ind].addElement(new WordStorage(s, doc));
		}
		
		return wordFound;
	}
	
	public int hash(String s) {
		int sum = 0;
		
		for(int i = 0; i < s.length(); i++) {
			sum = sum + s.charAt(i);
		}
		
		return sum%index.length;
	}
}
