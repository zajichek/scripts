import java.io.File;
import java.io.FileNotFoundException;
import java.io.RandomAccessFile;
import java.util.Vector;


//After a JRFEvaluation.initialize<Class/Regression>Forest is called, JREvaluation.createForestWriter returns this object
public class ForestWriter {
	boolean type;
	RandomAccessFile forest;
	short numClasses, numVars;
	short[] classes;
	boolean[] varTypes;
	long currentLocation;
	long lastTree;
	double[][] currentTree;
	Vector<Integer> remainingNodes;
	
	public ForestWriter(File source) {
		try {
			if(source.exists()) {
				forest = new RandomAccessFile(source, "rw");
			} else {
				throw new FileNotFoundException("File not found");
			}
			//Forest type
			forest.seek(0);
			type = forest.readBoolean();
			
			//Gathering information to be readily available (automatically advances pointer)
			//For type == true, regression forest, false = classification
			if(!type) {
				//Reading number of classes
				numClasses = forest.readShort();
				
				//Reading class labels
				classes = new short[numClasses];
				for(int i = 0; i < classes.length; i++)
					classes[i] = forest.readShort();
			}
			//Reading number of variables
			numVars = forest.readShort();
			
			//Reading in data types
			varTypes = new boolean[numVars];
			for(int i = 0; i < varTypes.length; i++)
				varTypes[i] = forest.readBoolean();
			
			//Storing current location where trees can start writing
			currentLocation = forest.getFilePointer();
			
		} catch(Exception e) {
			System.out.println(e.getMessage());
		}
	}
	
	public long getLocation() {
		try {
			currentLocation = forest.getFilePointer();
			return currentLocation;
		} catch(Exception e) {
			return -1;
		}
	}
	
	public long getLastTreeLocation() {
		return lastTree;
	}
	
	//Pass tree matrix; write to file 
	public void writeClassificationTree(double[][] tree) {
		try {
			currentTree = new double[tree.length][4];
			currentTree = tree;
			
			//Location of next tree going to be written at the start (seek here and write the 'currentLocation' after writing all nodes)
			long nextTree = getLocation();
			
			//Seeking 8 bytes further to start writing tree nodes
			forest.seek(nextTree + 8);
			
			//Vector to eliminate nodes already written
			remainingNodes = new Vector<>();
			for(int i = 0; i < tree.length; i++)
				remainingNodes.addElement(i);
			
			//While nodes are still needing to be written, write them. Passing first element always to initiate a path being written
			while(remainingNodes.size() > 0) {
				writeClassificationNode(remainingNodes.elementAt(0));
			}
			
			//Writing location of next tree at the beginning of this tree
			forest.seek(nextTree);
			forest.writeLong(currentLocation);
			forest.seek(currentLocation);
			
			forest.close();
			} catch(Exception e) {
					System.out.println(e.getMessage());
			}
	}
	
	//Writes nodes semi-recursively
	private long writeClassificationNode(int index) {
		try {
			long nodeLocation = getLocation();
			
			//Current node needed to be written based on index of tree
			double[] node = currentTree[index];
		
			//Gathering node information
			//Index of tree nodes to travel to if new value is <= value. Adjusting indices from R to Java
			int leftDaughter = (int) node[0] - 1;
			
			//Index of tree nodes to travel to if new value is > value. Adjusting indices from R to Java
			int rightDaughter = (int) node[1] - 1;
			
			//Variable being considered in split. Adjusting indices from R to Java
			int var = (int) node[2] - 1;
			
			//Writing variable number
			forest.writeShort((short) var);
			
			//Writing split value
			if(varTypes[var]) {
				forest.writeInt((int) node[3]);
			} else {
				forest.writeDouble(node[3]);
			}
			
			//Removing node after being written
			remainingNodes.remove(remainingNodes.indexOf(index));
			
			//Recursively writing left daughter
			if(leftDaughter > 0) {
				forest.writeLong(writeClassificationNode(leftDaughter));
			} else {
				forest.writeLong(2 + -1*leftDaughter);
			}
			
			//Recursively writing right daughter
			if(rightDaughter > 0) {
				forest.writeLong(writeClassificationNode(rightDaughter));
			} else {
				forest.writeLong(2 + -1*rightDaughter);
			}
			
			return nodeLocation;
			
	   } catch(Exception e) {
		   System.out.println(e.getMessage());
		   return -1;
	   }
	}
}
