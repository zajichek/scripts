import java.io.File;
import java.io.RandomAccessFile;
import java.util.Vector;

public class RandomAccessTester {
	
	public static void main(String[] args) {
		 new RandomAccessTester();
	}
	
	public RandomAccessTester() {
			
		try {
			//Creating file for creation
			RandomAccessFile forest;
		
			forest = new RandomAccessFile("initial_forest_example_11-2-17.forest", "rw");
			
			//Setting position at beginning of file
			forest.seek(0);
			
			//Writing forest type (F = classification, T = regression)
			forest.writeBoolean(false);
			
			//Writing number of classes
			short[] classes = {1,2,3};
			forest.writeShort(classes.length);
			
			//Writing class labels
			for(int i = 0; i < classes.length; i++)
				forest.writeShort(classes[i]);
			
			//Writing number of variables
			boolean[] varTypes = {true,false,true,true,false};
			forest.writeShort(varTypes.length);
			
			//Writing variable types (F = numeric, T = factor)
			for(int i = 0; i < varTypes.length; i++)
				forest.writeBoolean(varTypes[i]);
			
			//Closing file 
			forest.close();
			
			
		} catch(Exception e) {
			System.out.println(e.getMessage());
			
		}
	}
	
	

}
