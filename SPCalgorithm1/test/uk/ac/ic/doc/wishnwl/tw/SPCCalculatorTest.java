package uk.ac.ic.doc.wishnwl.tw;

import static org.junit.Assert.*;
import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;

import org.apache.commons.lang3.Range;

import org.junit.Before;
import org.junit.Test;


public class SPCCalculatorTest {
	
	SPCCalculator spcCalc;
	
	@Before
	public void setUp() {
		this.spcCalc = new SPCCalculator(getTestVals());
	}
	
	@Test
	public void testSigTrace() {
		int [] exampleSig1 = new int[] {1,1,-1};
		int [] exampleSig2 = new int[] {1,1,1,1};
		int [] exampleSig3 = new int[] {-1,-1,-1,-1,-1};
		
		int result1 = SPCCalculator.sigTrace(exampleSig1);
		int result2 = SPCCalculator.sigTrace(exampleSig2);
		int result3 = SPCCalculator.sigTrace(exampleSig3);
		
		assertEquals(-1, result1);
		assertEquals(1, result2);
		assertEquals(1, result3);
	}
	
	@Test
	public void testRemoveSignatureZeros() {
		int [] exampleSig1 = new int[] {1,1,0,-1};
		int [] exampleSig2 = new int[] {1,0,1,1,1,0};
		int [] exampleSig3 = new int[] {0,-1,0,0,1,-1,1,0,0,0,0,-1};
		int [] exampleSig4 = new int[] {0,0,0,0,0};
		
		int [] expectedResult1 = new int[] {1,1,-1};
		int [] expectedResult2 = new int[] {1,1,1,1};
		int [] expectedResult3 = new int[] {-1,1,-1,1,-1};
		int [] expectedResult4 = new int[] {};
		
		int [] result1 = SPCCalculator.removeSignatureZeros(exampleSig1);
		int [] result2 = SPCCalculator.removeSignatureZeros(exampleSig2);
		int [] result3 = SPCCalculator.removeSignatureZeros(exampleSig3);
		int [] result4 = SPCCalculator.removeSignatureZeros(exampleSig4);
		
		assertArrayEquals(expectedResult1, result1);
		assertArrayEquals(expectedResult2, result2);
		assertArrayEquals(expectedResult3, result3);
		assertArrayEquals(expectedResult4, result4);
	}
	
	@Test
	public void testIsRuleBreakingRun() {
		
		SPCCalculator.Period P = spcCalc.periods.get(0);
		
		// A run of 8 below the mean
		Range<Integer> p1 = Range.between(0,7);
		// A non-run (mixture of above and below)
		Range<Integer> p2 = Range.between(28,35);
		// A run of 12 above the mean
		Range<Integer> p3 = Range.between(61,72);
		// A non-run (last point below)
		Range<Integer> p4 = Range.between(61,73);
		// A run of 5 above the mean
		Range<Integer> p5 = Range.between(51,55);
		
		int result1 = spcCalc.isRuleBreakingRun(p1, P, 8);
		int result2 = spcCalc.isRuleBreakingRun(p2, P, 8);
		int result3 = spcCalc.isRuleBreakingRun(p3, P, 8);
		int result4 = spcCalc.isRuleBreakingRun(p4, P, 8);
		int result5 = spcCalc.isRuleBreakingRun(p5, P, 8);
		
		assertEquals(-1, result1);
		assertEquals(0, result2);
		assertEquals(1, result3);
		assertEquals(0, result4);
		assertEquals(0, result5);
		
	}
	
	@Test
	public void testRunEnd() {
		
		SPCCalculator.Period P = spcCalc.periods.get(0);
		
		assertEquals(29, spcCalc.runEnd(0, P));
		assertEquals(29, spcCalc.runEnd(2, P));
		assertEquals(49, spcCalc.runEnd(41, P));
		assertEquals(30, spcCalc.runEnd(30, P));
		assertEquals(31, spcCalc.runEnd(31, P));
		assertEquals(34, spcCalc.runEnd(32, P));
		assertEquals(78, spcCalc.runEnd(76, P));

	}
	
	@Test
	public void testRuleBreakingRuns() {
		
		SPCCalculator.Period P = spcCalc.getPeriods().get(0);
		
		List<Range<Integer>> ruleBreakingRuns = spcCalc.ruleBreakingRuns(P, 8, false);
		
		// Correct result is: [(0,29),(41,49), (61,72)]
		Range<Integer> correct_p1 = Range.between(0,29);
		Range<Integer> correct_p2 = Range.between(41,49);
		Range<Integer> correct_p3 = Range.between(61,72);
		
		assertEquals(3, ruleBreakingRuns.size());
		assertEquals(correct_p1.getMinimum(), ruleBreakingRuns.get(0).getMinimum());
		assertEquals(correct_p1.getMaximum(), ruleBreakingRuns.get(0).getMaximum());
		assertEquals(correct_p2.getMinimum(), ruleBreakingRuns.get(1).getMinimum());
		assertEquals(correct_p2.getMaximum(), ruleBreakingRuns.get(1).getMaximum());
		assertEquals(correct_p3.getMinimum(), ruleBreakingRuns.get(2).getMinimum());
		assertEquals(correct_p3.getMaximum(), ruleBreakingRuns.get(2).getMaximum());
		
	}
	
	@Test
	public void testConstructorMean() {
		
		double[] defaultMean = new double[spcCalc.getRawVals().length];
		Arrays.fill(defaultMean, (double) 0.201256823);
		
		assertArrayEquals(defaultMean, spcCalc.getCentres(), 1e-6d);
		
	}
	
	@Test
	public void testSetMean() {
		
		double testValue = 3.14159d;
		Range<Integer> testRange = Range.between(5,9);
		
		double[] testMeans = new double[spcCalc.getRawVals().length];
		Arrays.fill(testMeans, (double) 0.201256823);
		for (int i = 5; i<=9; i++) {
			testMeans[i] = testValue;
		}
		
		spcCalc.setMean(testRange, testValue);
		
		assertArrayEquals(testMeans, spcCalc.getCentres(), 1e-6d);
		
	}
	
	@Test
	public void testMaxRangeLength() {
		
		Range<Integer> p1 = Range.between(0,5);
		Range<Integer> p2 = Range.between(19,30);
		Range<Integer> p3 = Range.between(41,49);
		
		List<Range<Integer>> ranges = new ArrayList<Range<Integer>>();
		ranges.add(p1);
		ranges.add(p2);
		ranges.add(p3);
		
		int result = spcCalc.maxRangeLength(ranges);
		
		assertEquals(30 - 19 + 1, result);
		
	}
	
	
	@Test
	public void testRecalculate() {
		
		int dataLength = spcCalc.getRawVals().length;
		
		double[] defaultMean = new double[dataLength];
		Arrays.fill(defaultMean, 0.201256823d);
		
		double[] recalcMean1 = new double[dataLength];
		Arrays.fill(recalcMean1, 0.121413093d);
		for (int i=40; i<dataLength; i++) {recalcMean1[i] = 0.283147828d;}
		
		double[] recalcMean2 = new double[dataLength];
		Arrays.fill(recalcMean2, 0.06374923d);
		for (int i=40; i<dataLength; i++) {recalcMean2[i] = 0.283147828d;}
		
		// Check that recalculation with the default period does not change the mean
		spcCalc.recalculate();

		assertArrayEquals(defaultMean, spcCalc.getCentres(), 1e-6d);
		
		// Now artificially change the periods, and check recalculation works accordingly
		SPCCalculator.Period P1 = spcCalc.new Period(0,39);
		SPCCalculator.Period P2 = spcCalc.new Period(40, dataLength - 1);
		ArrayList<SPCCalculator.Period> testPeriods = new ArrayList<SPCCalculator.Period>();
		testPeriods.add(P1);
		testPeriods.add(P2);
		
		spcCalc.periods = testPeriods;
		
		spcCalc.recalculate();
		
		assertArrayEquals(recalcMean1, spcCalc.getCentres(), 1e-6d);
		
		// Check an example with a period with differing display and calculation periods
		SPCCalculator.Period P1a = spcCalc.new Period(0,19,0,39);
		ArrayList<SPCCalculator.Period> testPeriods2 = new ArrayList<SPCCalculator.Period>();
		testPeriods2.add(P1a);
		testPeriods2.add(P2);
		
		spcCalc.periods = testPeriods2;
		
		spcCalc.recalculate();
		
		assertArrayEquals(recalcMean2, spcCalc.getCentres(), 1e-6d);

	}
	
	
	public double[] getTestVals() {
		double[] testVals = {0.1875,
				0.0625,
				0.0217391,
				0.0227273,
				0.0169492,
				0.0487805,
				0.0909091,
				0.025641,
				0,
				0.0217391,
				0.0491803,
				0.0357143,
				0.0333333,
				0.0344828,
				0.157895,
				0.12,
				0.113636,
				0.108108,
				0.0833333,
				0.0408163,
				0.0759494,
				0.0888889,
				0.172414,
				0.130435,
				0.0689655,
				0.118644,
				0.104167,
				0.0638298,
				0.0689655,
				0.183673,
				0.28,
				0.155556,
				0.268293,
				0.3125,
				0.285714,
				0.175,
				0.333333,
				0.232143,
				0.181818,
				0.28125,
				0.0833333,
				0.218182,
				0.285714,
				0.333333,
				0.254902,
				0.323077,
				0.230769,
				0.266667,
				0.366667,
				0.345455,
				0.178571,
				0.285714,
				0.714286,
				0.25,
				0.214286,
				0.224138,
				0.2,
				0.219512,
				0.421053,
				0.126761,
				0.166667,
				0.257576,
				0.272727,
				0.32,
				0.619048,
				0.20339,
				0.25,
				0.253333,
				0.314815,
				0.5,
				0.295775,
				0.347826,
				0.291667,
				0.191176,
				0.244444,
				0.146341,
				0.273973,
				0.25,
				0.301587};
		return testVals;
	}

}
