package uk.ac.ic.doc.wishnwl.tw;

import static org.junit.Assert.*;

import java.util.Arrays;
import java.util.ArrayList;
import java.util.List;
import java.util.Iterator;
import java.util.Vector;
import java.io.File;

import org.eclipse.birt.data.engine.core.DataException;
import org.junit.Test;

public class SPCIOTest {

	@Test
	public void testAnalyseCsv() {

		//Get a list of all the files in the testdata folder
		File folder = new File("C:\\Users\\tw299\\git\\spc-algorithm\\SPCalgorithm1\\testdata");
		List<File> listOfFiles = new ArrayList<>(Arrays.asList(folder.listFiles()));
		String fileName = new String();
		String testOutputFileName = new String();
		String correctOutputFileName = new String();
		int count = 0;

		//Remove any files from the list where their filename ends with "OUT.csv"
		// - this should just leave the raw test data files in the list
		for (Iterator<File> iter = listOfFiles.iterator(); iter.hasNext();) {
			fileName = iter.next().getAbsolutePath();

			//System.out.println(fileName.substring(fileName.length()-7));
			if (fileName.length()>7 && fileName.substring(fileName.length()-7).equals("OUT.csv")) {
				//System.out.println("Yes");
				iter.remove();
			}
		}

		for (Iterator<File> iter = listOfFiles.iterator(); iter.hasNext();) {

			Vector<Double[]> result = new Vector<Double[]>();

			//Get the next filename in the list of test data files
			fileName = iter.next().getAbsolutePath();
			//System.out.println(fileName);

			//Run the current algorithm on fileName...
			try {
				result = SPCIO.analyseCsv(fileName);
			} catch (DataException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				fail("Data Exception.");
			}

			//...and save the result.
			//TODO: Shouldn't have to pass the maximum number of iterations to saveSpcToCsv method
			//TODO: Consider making saveSpcToCsv return the actual file name used if successful?
			SPCIO.saveSpcToCsv(fileName, "TEST", 0, result);

			//Reconstruct the filename that the test result will have been saved as.
			testOutputFileName = fileName.substring(0, fileName.length() - 4) + "_" + "TEST" + "_OUT.csv";
			correctOutputFileName = fileName.substring(0, fileName.length() - 4) + "_endresult_OUT.csv";

			//Load in the results of the test run...
			SPCIO testIO = new SPCIO();
			testIO.loadCsv(testOutputFileName);
			//testIO.makeVector();

			//...and the original results they should match.
			SPCIO correctIO = new SPCIO();
			correctIO.loadCsv(correctOutputFileName);
			//correctIO.makeVector();

			//Assert that the test run gave the same results as the original.
			assertTrue(csvContentEqual(testIO.myEntries,correctIO.myEntries));

			count++;
			result = null;
		}

		System.out.println(count);

	}

	private boolean csvContentEqual(List<String[]> myEntries,
			List<String[]> myEntries2) {
		// TODO Write csvContentEqual method to compare contents of two csv files, passed as List<String[]>


		return false;
	}

}
