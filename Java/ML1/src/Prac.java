import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;


public class Prac {
	public static void main(String[] args) {
		HashVector v = new HashVector(10);
		v.addString("Dad", 5);
		System.out.println(v.index[v.hash("Dad")].elementAt(0).documents.elementAt(0));
	}
}
