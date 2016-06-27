// This class runs SPCAccumulator on the data in each .csv file in a specified directory
// and saves the output as a new .csv file in the same directory.

package uk.ac.ic.doc.wishnwl.tw;

import java.util.Iterator;
import java.util.List;
import java.util.Vector;
import java.io.*;

import org.eclipse.birt.data.engine.core.DataException;

import com.opencsv.CSVReader;
import com.opencsv.CSVWriter;

public class SPCIO {

	 CSVReader reader;
     List<String[]> myEntries;
     Vector<Double[]> csvVals;

	public void loadCsv(String filename) {

		try {
			this.reader = new CSVReader(new FileReader(filename));
			this.myEntries = reader.readAll();}
		catch(IOException e) {
			e.printStackTrace();
			}
		finally {
			try{this.reader.close();}
			catch(IOException e){
				e.printStackTrace();
			}

			}


	}

	public void makeVector() {

		this.csvVals = new Vector<Double[]>();
		Double x = new Double(0);

		for (String[] s : myEntries) {
			try {
				x = Double.valueOf(String.valueOf(s[0].toCharArray()));
			}
			catch (NumberFormatException e) {
//				System.out.println(String.valueOf(s[0].toCharArray())
//						+ " is not a Double - replaced with null.");
				x = null;
			}
			finally {
				csvVals.add(new Double[]{x});
			}

		}

	}

	public static void csvSPC(String fName) throws DataException {
		SPCIO testIO = new SPCIO();
		testIO.loadCsv(fName);
		testIO.makeVector();

//		for (Double[] xD : testIO.csvVals) {
//			System.out.println(xD[0]);
//		}

		System.out.println("Begin Accumulator");

		SPCAccumulator spca = new SPCAccumulator(false);

		//First pass - load data into vals, pass to SPCCalculator spcCalc
		spca.start();
		for (Iterator<Double[]> i = testIO.csvVals.iterator(); i.hasNext();) {
			spca.onRow(i.next());
		}
		spca.finish();

		//Get the mean or average moving range from spcCalc and add it to ret
		Vector ret = new Vector();
		spca.start();
		for (Iterator<Double[]> i = testIO.csvVals.iterator(); i.hasNext();) {
			spca.onRow(i.next());
			ret.add(spca.getValue());
		}
		spca.finish();

		System.out.println("End Accumulator");

		System.out.println("Begin Accumulator");

		SPCAccumulator spcm = new SPCAccumulator(true);

		//First pass - load data into vals, pass to SPCCalculator spcCalc
		spcm.start();
		for (Iterator<Double[]> i = testIO.csvVals.iterator(); i.hasNext();) {
			spcm.onRow(i.next());
		}
		spcm.finish();

		//Get the mean or average moving range from spcCalc and add it to ret
		Vector retm = new Vector();
		spcm.start();
		for (Iterator<Double[]> i = testIO.csvVals.iterator(); i.hasNext();) {
			spcm.onRow(i.next());
			retm.add(spcm.getValue());
		}
		spcm.finish();

		System.out.println("End Accumulator");

		//Print the returned results out to the console

//		for (Iterator i = ret.iterator(); i.hasNext();) {
//			System.out.println(i.next());
//		}

		//structure the output
		int n = ret.size();
		Vector<Double[]> vOut = new Vector<Double[]>(n);



		System.out.println("Structure Output");
		for (int i = 0; i < n; i++) {
			Double[] vItem = new Double[3];
			vItem[0] = testIO.csvVals.get(i)[0];
			if (ret.get(i) != null) {
				vItem[1] = Double.valueOf(String.valueOf(ret.get(i)));
			} else {
				vItem[1] = null;
			}
			if (retm.get(i) != null) {
				vItem[2] = Double.valueOf(String.valueOf(retm.get(i)));
			} else {
				vItem[2] = null;
			}


			vOut.add(i, vItem);
//			System.out.println(String.valueOf(i) + ": " + String.valueOf(vOut.get(i)[0]) + ", " +
//					String.valueOf(vOut.get(i)[1]) + ", " + String.valueOf(vOut.get(i)[2]));
		}


		CSVWriter writer;
		boolean fOpen;
		String[] sOut = new String[3];
		String saveName = new String();

		try {
			// TODO: use a regex to do this properly
			saveName = fName.substring(0, fName.length() - 4) + "_OUT.csv";
			writer = new CSVWriter(new FileWriter(saveName));
			fOpen = true;
			System.out.println("File Open");
		}
		catch(IOException e) {
			e.getStackTrace();
			fOpen = false;
			writer = null;
		}
		finally {
			//
		}

	     if (fOpen == true) {try{
	    	 System.out.println("Writing...");
//	    	 for (Iterator i = ret.iterator(); i.hasNext();) {
//	 			sOut[0] = String.valueOf(i.next());
//	    		writer.writeNext(sOut);
//	 		}

	    	 for (int j = 0; j < n; j++) {
	    		 sOut[0] = String.valueOf(vOut.get(j)[0]);
	    		 sOut[1] = String.valueOf(vOut.get(j)[1]);
	    		 sOut[2] = String.valueOf(vOut.get(j)[2]);
	    		 //System.out.println(String.valueOf(i) + ": " + sOut[0] + ", " + sOut[1] + ", " + sOut[2]);
//	    		 System.out.println(String.valueOf(j) + ": " + vOut.get(j)[0] + ", " + vOut.get(j)[1] + ", " + vOut.get(j)[2]);
	    		 writer.writeNext(sOut);
	    	 }
	    	 //writer.writeAll(ret);
	    	 	writer.close();
	    	 	System.out.println("File Closed");
	     	}
	     	catch (IOException e) {
	    	 	e.getStackTrace();
	     	}
	     	finally {
				//
			}
	     }


	}


	public static void main(String[] args) throws DataException {

		File folder = new File("C:\\Users\\tw299\\git\\spc-algorithm\\SPCalgorithm1\\data");
		File[] listOfFiles = folder.listFiles();
		String fileName = new String();

		for (int i = 0;i < listOfFiles.length; i++) {
			fileName = listOfFiles[i].getAbsolutePath();
			SPCIO.csvSPC(fileName);
		}



	}

}

