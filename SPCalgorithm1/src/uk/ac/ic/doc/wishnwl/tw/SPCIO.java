// This class runs SPCCalculator on the data in each .csv file in a specified directory
// and saves the output as a new .csv file in the same directory.

package uk.ac.ic.doc.wishnwl.tw;

import java.util.List;
import java.util.Vector;
import java.io.*;

import com.opencsv.CSVReader;
import com.opencsv.CSVWriter;

public class SPCIO {
	
	CSVReader reader;
    List<String[]> myEntries;
    Vector<Double> csvVals;


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

		this.csvVals = new Vector<Double>();
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
				csvVals.add(new Double(x));
			}

		}

	}

	public static Vector<Double[]> analyseCsv(String fName, int minimumPeriodLength, int runRuleLength, boolean forceRecalc) {
		SPCIO testIO = new SPCIO();
		testIO.loadCsv(fName);
		testIO.makeVector();
		
		SPCCalculator spcc = new SPCCalculator(testIO.csvVals, minimumPeriodLength, runRuleLength, forceRecalc);
		spcc.calculate();

		//Get the mean and add it to ret
		Vector ret = new Vector();
		for (double m : spcc.getCentres()) {
			ret.add(new Double(m));
		}

		//Get the average moving range and add it to retm
		Vector retm = new Vector();
		for (double a : spcc.getAmrs()) {
			retm.add(new Double(a));
		}

		//structure the output
		int n = ret.size();
		Vector<Double[]> vOut = new Vector<Double[]>(n);

		//System.out.println("Structure Output");
		for (int i = 0; i < n; i++) {
			Double[] vItem = new Double[3];
			vItem[0] = testIO.csvVals.get(i);
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
		}

		return vOut;
	}


	public static void saveSpcToCsv(String fName, String label, Vector<Double[]> v) {
		int n = v.size();

		CSVWriter writer;
		boolean fOpen;
		String[] sOut = new String[3];
		String saveName = new String();

		try {
			// TODO: use a regex to do this properly
			saveName = fName.substring(0, fName.length() - 4) + "_" + label + "_OUT.csv";
			writer = new CSVWriter(new FileWriter(saveName));
			fOpen = true;
			//System.out.println("File Open: " + saveName);
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
	    	 //System.out.println("Writing...");

	    	 for (int j = 0; j < n; j++) {
	    		 sOut[0] = String.valueOf(v.get(j)[0]);
	    		 sOut[1] = String.valueOf(v.get(j)[1]);
	    		 sOut[2] = String.valueOf(v.get(j)[2]);

	    		 writer.writeNext(sOut);
	    	 }

	    	 	writer.close();
	    	 	//System.out.println("File Closed.");
	     	}
	     	catch (IOException e) {
	    	 	e.getStackTrace();
	     	}
	     	finally {
				//
			}
	     }
	}


	public static void csvSPC(String fName, int minimumPeriodLength, int runRuleLength, boolean forceRecalc) {

		Vector<Double[]> vOut = new Vector<Double[]>();
		vOut = analyseCsv(fName, minimumPeriodLength, runRuleLength, forceRecalc);
		saveSpcToCsv(fName, "", vOut);

	}

	public static boolean equalVectors (Vector<Double[]> u, Vector<Double[]> v) {


		if(u.size() != v.size()) return false;

		boolean status = true;
		for (int j = 0; j < u.size(); j++) {
			if (u.get(j)[0] == null || u.get(j)[1] == null || u.get(j)[2] == null) {
				if (!(u.get(j)[0] == null && u.get(j)[1] == null && u.get(j)[2] == null)) {
					status = false;
				}
			} else {
			status = status && u.get(j)[0].equals(v.get(j)[0]) && u.get(j)[1].equals(v.get(j)[1]) && u.get(j)[2].equals(v.get(j)[2]);
			}
		}

		return status;
	}

	public static void main(String[] args) {
		
		int minimumPeriodLength = Integer.parseInt(args[1]);
		int runRuleLength = Integer.parseInt(args[2]);
		boolean forceRecalc = Boolean.parseBoolean(args[3]);
		// "/Users/Thomas/code/eclipse-workspace/spc-algorithm/SPCalgorithm1/data"
		File folder = new File(args[0]);
		File[] listOfFiles = folder.listFiles();
		String fileName = new String();

		
		for (int i = 0;i < listOfFiles.length; i++) {
			fileName = listOfFiles[i].getAbsolutePath();
			//SPCIO.csvSPC(fileName, 0);
			Vector<Double[]> endResult = new Vector<Double[]>();
			System.out.println("\n*********************************************************************************************************");
			System.out.println("Analysing data in file: " + fileName + " ...");
			endResult = analyseCsv(fileName, minimumPeriodLength, runRuleLength, forceRecalc);
			saveSpcToCsv(fileName, "endresult", endResult);
			System.out.println("...data in file " + fileName + " analysed.");
			endResult = null;
			System.out.println("*********************************************************************************************************");
		}



	}

}

