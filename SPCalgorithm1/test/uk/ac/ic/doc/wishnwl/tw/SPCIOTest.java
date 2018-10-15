package uk.ac.ic.doc.wishnwl.tw;

import java.util.Arrays;
import java.util.Collection;
import java.util.ArrayList;
import java.util.List;
import java.util.Iterator;
import java.util.Vector;
import java.io.File;

import org.eclipse.birt.data.engine.core.DataException;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

@RunWith(value = Parameterized.class)
public class SPCIOTest {

	String dataFileName;
	SPCIO testIO;
	SPCIO correctIO;

	@Before
	public void initialize() {
		this.testIO = new SPCIO();
		this.correctIO = new SPCIO();
	}

	//Constructor takes parameter from list and passes to test
	public SPCIOTest (String fileName) {
		this.dataFileName = fileName;
	}

	//Returns a list of data files to test the algorithm on.
	//For each file in the list there must be a pre-analysed output file
	//in the same folder, against which the algorithm's output
	//will be tested.
	@Parameterized.Parameters( name = "{index}: {0}")
	public static Collection<Object[]> dataSets() {
		Collection<Object[]> fileListResult = new ArrayList<Object[]>();
		String fileName;
		File folder = new File("/Users/Thomas/code/eclipse-workspace/spc-algorithm/SPCalgorithm1/test-data");
		List<File> listOfFiles = new ArrayList<>(Arrays.asList(folder.listFiles()));

		//Remove any files from the list where their filename ends with "OUT.csv"
		// - this should just leave the raw test data files in the list
		for (Iterator<File> iter = listOfFiles.iterator(); iter.hasNext();) {
			fileName = iter.next().getAbsolutePath();
			if (fileName.length()>7 && fileName.substring(fileName.length()-7).equals("OUT.csv")) {
				iter.remove();
			}
		}

		// Populate the output Collection with the filenames
		for (Iterator<File> iter = listOfFiles.iterator(); iter.hasNext();) {
			fileName = iter.next().getAbsolutePath();
			if(!fileName.contains("DS_Store")) {
				String[] paramArray = new String[1];
				paramArray[0] = fileName;
				fileListResult.add(paramArray);
			}
		}
		// Return the list of filenames to be used as parameters for the test.
		return fileListResult;
	}

	@Test
	public void testSPCIO() {
		String testOutputFileName = new String();
		String correctOutputFileName = new String();
		Vector<Double[]> result = new Vector<Double[]>();

		//Run the current algorithm on fileName...
		try {
			result = SPCIO.analyseCsv(dataFileName, 20, 8, false);
		} catch (DataException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			fail("Data Exception.");
		}

		//...and save the result.
		//TODO: Shouldn't have to pass the maximum number of iterations to saveSpcToCsv method
		//TODO: Consider making saveSpcToCsv return the actual file name used if successful?
		SPCIO.saveSpcToCsv(dataFileName, "TEST", result);

		//Reconstruct the filename that the test result will have been saved as.
		testOutputFileName = dataFileName.substring(0, dataFileName.length() - 4) + "_" + "TEST" + "_OUT.csv";
		correctOutputFileName = dataFileName.substring(0, dataFileName.length() - 4) + "_endresult_OUT.csv";

		//Load in the results of the test run...
		testIO.loadCsv(testOutputFileName);

		//...and the original results they should match.
		correctIO.loadCsv(correctOutputFileName);
		//correctIO.makeVector();

		//Assert that the test run gave the same results as the original.
		assertTrue(csvContentEqual(testIO.myEntries,correctIO.myEntries));

	}

	//Method to test whether the contents of two csv files are the same.
	private boolean csvContentEqual(List<String[]> myEntries,
			List<String[]> myEntries2) {
		// If there are a different number of rows, the two csv files
		// cannot be equal.
		if (myEntries.size() != myEntries2.size()) return false;

		int i;
		int l;
		int l2;
		String[] a, a2;

		Iterator<String[]> iter = myEntries.iterator();
		Iterator<String[]> iter2 = myEntries2.iterator();

		while (iter.hasNext() && iter2.hasNext()) {

			a = iter.next();
			a2 = iter2.next();

			l = a.length;
			l2 = a2.length;

			// If there are a different number of entries in a given row, the
			// two csv files cannot be equal.
			if (l != l2) return false;

			for (i=0; i < l; i++) {
				// If any individual cell differs, the two csv files cannot
				// be equal.
				if (!a[i].equals(a2[i])) return false;

			}
		}
		// If we haven't already returned false, it must be the case that the
		// two csv files have the same contents.
		return true;
	}
}
