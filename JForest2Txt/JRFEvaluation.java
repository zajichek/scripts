import java.io.File;
import java.io.FileNotFoundException;
import java.io.RandomAccessFile;

/*
 * Author: Alex Zajichek
 * Date: 10-17-2017
 * Biostatistician, Cleveland Clinic
 * Description: This class takes will be used in the forest2txt.R function to 
 * write out a random forest R object to a RandomAccessFile.
 * evaluateForest will then evaluate the file for new observations.
 */

public class JRFEvaluation {

	//rJava package looks for class containing main method
	public static void main(String[] args) {
	}
	
	//First method that writes meta data for classification forest
		public boolean initializeClassificationForest(boolean[] varTypes, short[] classes, File source) {
			try {
				//Creating file for creation
				RandomAccessFile forest;
			
				//If file already exists, throw an exception
				if(!source.exists()) {
					forest = new RandomAccessFile(source, "rw");
				} else {
					throw new Exception("File already exists");
				}
				//Setting position at beginning of file
				forest.seek(0);
				
				//Writing forest type (F = classification, T = regression)
				forest.writeBoolean(false);
				
				//Writing number of classes
				forest.writeShort(classes.length);
				
				//Writing class labels
				for(int i = 0; i < classes.length; i++)
					forest.writeShort(classes[i]);
				
				//Writing number of variables
				forest.writeShort(varTypes.length);
				
				//Writing variable types (F = numeric, T = factor)
				for(int i = 0; i < varTypes.length; i++)
					forest.writeBoolean(varTypes[i]);
				
				//Current location to start writing trees
				int location = (int) forest.getFilePointer();
				if(location != 5 + varTypes.length + 2*classes.length + 8) {
					extracted();
				}
				
				//Closing file 
				forest.close();
				
				//Returning indicator that file initialized
				return true;
				
			} catch(Exception e) {
				System.out.println(e.getMessage());
				return false;
			}
		}

	
	//First method that writes meta data for a regression forest
	public boolean initializeRegressionForest(boolean[] varTypes, File source) {
		try {
			//Creating file for creation
			RandomAccessFile forest;
		
			//If file already exists, throw an exception
			if(!source.exists()) {
				forest = new RandomAccessFile(source, "rw");
			} else {
				throw new Exception("File already exists");
			}
			//Setting position at beginning of file
			forest.seek(0);
			
			//Writing forest type (F = classification, T = regression)
			forest.writeBoolean(true);
			
			//Writing number of variables
			forest.writeShort(varTypes.length);
			
			//Writing variable types (F = numeric, T = factor)
			for(int i = 0; i < varTypes.length; i++)
				forest.writeBoolean(varTypes[i]);
			
			//Current location to start writing trees
			int location = (int) forest.getFilePointer();
			if(location != 3 + varTypes.length + 8) {
				extracted();
			}
			
			//Closing file 
			forest.close();
			
			//Returning indicator that tree was initialized
			return true;
			
		} catch(Exception e) {
			System.out.println(e.getMessage());
			return false;
		}
	}
	
	//Creates and returns a ForestWriter object
	public ForestWriter createForestWriter(File source) {
		return new ForestWriter(source);
	}
	

	private void extracted() throws Exception {
		throw new Exception("Location mismatch");
	}
}
