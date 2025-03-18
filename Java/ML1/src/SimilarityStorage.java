
public class SimilarityStorage {
	double cosine;
	int dotProd;
	String word;
	
	public SimilarityStorage(String word, int dotProd) {
		this.word = word;
		this.dotProd = dotProd;
	}
	
	public SimilarityStorage(String word, double cosine) {
		this.word = word;
		this.cosine = cosine;
	}
}
