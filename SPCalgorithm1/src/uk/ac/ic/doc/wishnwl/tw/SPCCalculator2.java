package uk.ac.ic.doc.wishnwl.tw;

import java.util.Iterator;
import java.util.Vector;

import uk.ac.ic.doc.wishnwl.tw.SPCCalculator.Pair;

public class SPCCalculator2 {

	//public int maximumNumberOfLoops;
	//private int defaultBreakPadding = 5;
	//public int breakPadding;
	Vector<Double> vals;
	double[] rawVals;
	double[] deltas;
	double[] means;
	double[] amrs;
	boolean[] breakPoints;

	public SPCCalculator2(Vector vals2, int maxLoops, int padding) {
		//this.maximumNumberOfLoops = maxLoops;
		//this.breakPadding = padding;
		this.vals = vals2;
		rawVals = new double[vals.size()];
		for (int i = 0; i < vals.size(); i++) {
			double v = vals.elementAt(i).doubleValue();
			rawVals[i] = v;
		}
		initArrays();
	}

	public SPCCalculator2(double[] vals2, int maxLoops, int padding) {
		//this.maximumNumberOfLoops = maxLoops;
		//this.breakPadding = padding;
		this.rawVals = vals2;
		initArrays();
	}

	public SPCCalculator2(Vector vals2) {
		this(vals2, 0, 5);
	}

	public SPCCalculator2(double[] vals2) {
		this(vals2, 0, 5);
	}

	private void initArrays() {
		double sum = 0.0D;
		means = new double[rawVals.length];
		breakPoints = new boolean[rawVals.length];
		deltas = new double[rawVals.length];
		amrs = new double[rawVals.length];
		if (deltas.length > 0) deltas[0] = 0.0D;
		double prev = 0.0D;
		for (int i = 0; i < rawVals.length; i++) {
			double v = rawVals[i];
			sum += v;
			if (i > 0) deltas[i] = Math.abs(v - prev);
			breakPoints[i] = false;
			prev = v;
		}
		double mean = sum / rawVals.length;
		for (int i = 0; i < means.length; i++) {
			means[i] = mean;
		}
	}

	public String toString() {
		StringBuffer sb = new StringBuffer();
		sb.append("rawVals:[");
		for (int i = 0; i < rawVals.length; i++) {
			if (i < rawVals.length - 1) sb.append(rawVals[i] + ",");
			else sb.append(rawVals[i] + "]\n");
		}
		sb.append("means:[");
		for (int i = 0; i < means.length; i++) {
			if (i < means.length - 1) sb.append(means[i] + ",");
			else sb.append(means[i] + "]\n");
		}
		sb.append("amrs:[");
		for (int i = 0; i < amrs.length; i++) {
			if (i < amrs.length - 1) sb.append(amrs[i] + ",");
			else sb.append(amrs[i] + "]\n");
		}
		sb.append("breakPoints:[");
		for (int i = 0; i < breakPoints.length; i++) {
			if (i < breakPoints.length - 1) sb.append(breakPoints[i] + ",");
			else sb.append(breakPoints[i] + "]\n");
		}
		return sb.toString();
	}

	public Object get(int index) {
		return new Double(means[index]);
	}

	public Object getLimit(int index) {
		return new Double(amrs[index]);
	}

	//public void add(double rawValue) {
		// TODO Auto-generated method stub
	//	vals.add(new Double(rawValue));
	//}

	/**
	 * Calculate the mean of a segment defined by start and end points.
	 * @param start Start of segment (inclusive).
	 * @param end End of segment (exclusive).
	 */
	private double calcMean(int start, int end) {
		double sum = 0.0D;
		for (int i = start; i < end; i++) {
			sum += rawVals[i];
		}
		double m = sum / (end - start);
		return m;
	}
	private double calcLimit(int start, int end) {
		double sum = 0.0D;
		//Original ...int i = start...
		//Should be ...int i = start + 1... because
		//Amr should ignore the meaningless first value

		for (int i = start + 1; i < end; i++) {
			sum += deltas[i];
		}
		//Original:
		//double m = sum / (end - start);
		//I think this is a mistake - it should be:
		double m = sum / (end - start - 1);
		// - because the average moving range is found
		//by dividing by one less than the number of data
		return m;
	}
	private void recalculateMeans(int calcLen) {
		// TODO: Need to modify this to work with two "types" of break point
		// i) recalculating break: as before
		// ii) extending break: mean persists from previous period.
		int segmentStart = 0;
		int segmentEnd = 0;
		for (int i = 0; i < rawVals.length; i++) {
			if (breakPoints[i]) {
				if (calcLen == 0) {
					segmentEnd = i;
				} else {
					segmentEnd = Math.min(segmentStart + calcLen, i);
				}
				double m = calcMean(segmentStart, segmentEnd);
				double l = calcLimit(segmentStart, segmentEnd);
				for (int j = segmentStart; j < i; j++) {
					means[j] = m;
					amrs[j] = l;
				}
				segmentStart = i;
			}
		}
		// have to do the last segment manually
		if (calcLen == 0) {
			segmentEnd = rawVals.length;
		} else {
			segmentEnd = Math.min(segmentStart + calcLen, rawVals.length);
		}

		double m = calcMean(segmentStart, segmentEnd);
		double l = calcLimit(segmentStart, segmentEnd);

		for (int j = segmentStart; j < rawVals.length; j++) {
			means[j] = m;
			amrs[j] = l;
		}
	}

	/**
	 *  Checks for a break based on the rule 2a.
	 *
	 * @param startIndex The point from which the method starts looking for the breakpoint.
	 * @return null if no Break2a, otherwise the start and end indices in the arrays where the run occurs
	 */
	private Pair existsBreak2a(int startIndex, int maxRunLength) {
		for (int i = startIndex; i < rawVals.length; i++){
			int breakEnd = break2a(i, maxRunLength);

			if (breakEnd > -1) return new Pair(i, breakEnd);
		}
		return null;
	}

	private int break2a(int i, int maxRunLength) {
		int seq = 0;
		boolean breakDuringRun = false;
		while ((i + seq < rawVals.length) && (rawVals[i + seq] > means[i+seq]) && !breakDuringRun){
			if (seq != 0) breakDuringRun = breakDuringRun || breakPoints[i + seq];
			seq++;
		}
		if (seq >= maxRunLength) // there is a run of at least maxRunLength points
			return i+seq-1; //return the index of the last point
		seq = 0;
		while ((i + seq < rawVals.length) && (rawVals[i + seq] < means[i+seq]) && !breakDuringRun){
			if (seq != 0) breakDuringRun = breakDuringRun || breakPoints[i + seq];
			seq++;
		}
		if (seq >= maxRunLength) // there is a run of at least maxRunLength points
			return i+seq-1; //return the index of the last point

		return -1;
	}

	/**
	 * Alternative version of the algorithm, with provided mean, used for hypothetical testing.
	 * @param i
	 * @return
	 */
	private Pair existsBreak2aM(int startIndex, int endIndex, double m, int maxRunLength) {
		// Modified this function so rule breaks are only returned if they fall
		// entirely within the specified range.

		for (int i = startIndex; i < endIndex; i++){
			int breakEnd = break2aM(i, m, maxRunLength);
			if ((breakEnd > -1) && (endIndex - i >= maxRunLength)) return new Pair(i, breakEnd);
		}
		return null;
	}


	private int break2aM(int i, double m, int maxRunLength) {

		int seq = 0;
		while ((i + seq < rawVals.length) && (rawVals[i + seq] > m)){
			seq++;
		}
		if (seq >= maxRunLength) // there is a run of at least maxRunLength points
			return i+seq-1; //return the index of the last point
		seq = 0;
		while ((i + seq < rawVals.length) && (rawVals[i + seq] < m)){
			seq++;
		}
		if (seq >= maxRunLength) // there is a run of at least maxRunLength points
			return i+seq-1; //return the index of the last point

		return -1;
	}

	private boolean existsBreakPointBefore(int breakEnd) {
		for (int i = 0; i < breakEnd; i++) {
			if (breakPoints[i]) return true;
		}
		return false;
	}
	private int getSecondBreakPointBefore(int p) {
		int last = 0;
		int secondLast = 0;
		for (int i = 0; i < p; i++) {
			if (breakPoints[i]) {
				secondLast = last;
				last = i;
			}
		}
		return secondLast;
	}

	private int getBreakPointBefore(int p) {
		int last = 0;
		for (int i = 0; i < p; i++) {
			if (breakPoints[i]) {
				last = i;
			}
		}
		return last;
	}

	//*************************************************************************
	//
	//The algorithm itself is specified by this method.
	//Its effect is to modify the breakPoints attribute to its desired state
	//reflecting the result of the recalculation algorithm.
	//
	//*************************************************************************
	public void calculate() {
		//feature - extension of baseline/period mean&limits - done by virtue
		//of ability to restrict period used for calculation to length calcLen
		//feature - wait for additional data before recalculating
		//TODO: add feature - only recalculate if (i) new period average in same dir as break
		//TODO: add feature - only recalculate if (ii) same rule break no longer present after recalc.
		// NB (ii) => (i) but the converse is not true
		// so any recalculation under (ii) is also under (i), but not vice-versa.
		// In other words, there exist series/recalculation pairs for which the
		// mew period average is in the same direction as the break, but where
		// the rule break persists - in these cases (i) would still recalculate
		// but (ii) would not. So (ii) is more conservative.

		// So three cases given a rule break a) same direction, rule break disappears b) same direction, rule break persists
		// c) different direction, rule break persists

		// Three level parameter of the algorithm
		// 0: recalculate in a, b & c = never remove the rule break
		// 1: recalculate in a, b = remove rule break if rule break persists
		// 2: recalculate in a = remove rule break if new average is in opposite direction

		// Need to refactor to parametrise this.

		//Note 2017-11-01: (ii) is too conservative. e.g. efit R1F_571n. ?only do not recalc if a) stillbreak starts within first 7? b) enough non-stillbreak points to form limits?

		// Specify the minimum period length n_b
		int periodMin = 12;
		// Specify the run length to trigger rule break n_r
		int maxRunLength = 7;
		// Specify how many data points required in new period before recalculating n_p
		int recalcPeriodMin = 12;
		// Specify whether to prevent recalculation if there the rule break persists afterwards
		boolean checkStillBreaks = false;

		// Calculate the mean and amr based on current break points
		recalculateMeans(periodMin);

		// Start looking for rule breaks after the pseudo baseline period
		Pair breakIndexRun = null;
		int breakSearchStart = periodMin;
		double testMean;
		Pair stillBreaks;

		for (int s = breakSearchStart; s < rawVals.length; s++) {
			System.out.println(s);
			breakIndexRun = existsBreak2a(s, maxRunLength);

			// On finding a rule break starting at position s...
			if (breakIndexRun != null) {
				int breakStart = breakIndexRun.a;
				int breakEnd = breakIndexRun.b;
				// figure out the direction of the rule break
				int breakDir = (int) Math.signum(rawVals[breakStart] - means[breakStart]);
				System.out.println("RB@" + breakStart + "-" + breakEnd + ":" + breakDir);

				// ... check to see whether there are sufficient data points
				// to form a new period...
				if (rawVals.length - breakStart >= recalcPeriodMin) {
					System.out.println("Sufficient data");
					// calculate the new mean for the new period
					testMean = calcMean(breakStart, rawVals.length);
					// check for any rule breaks occuring within the same
					// interval as the triggering rule break.
					stillBreaks = null;
					for (int br = breakStart; br <= breakEnd - maxRunLength; br++) {
						System.out.println("stillBreaks check" + breakStart + "-" + (breakEnd - maxRunLength) + ": " + br);
						System.out.println(br + ":" + breakEnd + ":" + testMean);
						stillBreaks = existsBreak2aM(br, breakEnd, testMean, maxRunLength);
						if (stillBreaks != null) break;
					}

					if (stillBreaks == null || checkStillBreaks == false) {
						System.out.println("RB does not persist or we aren't checking" + (stillBreaks == null) + (checkStillBreaks == false));
						// ... if now no rule break, or if we're not checking, insert the breakpoint ...
						breakPoints[breakStart] = true;
						break;
					} else {
						int testBreakStart = stillBreaks.a;
						// figure out the direction of the rule break
						int testBreakDir = (int) Math.signum(rawVals[testBreakStart] - testMean);

						// ... or if there is now a rule break but it is in the
						// opposite direction to the triggering rule break,
						// also insert the breakpoint ...
						if (breakDir*testBreakDir == -1) {
							System.out.println("Rule break opposite dir");
							breakPoints[breakStart] = true;
							break;
						}

						System.out.println("Rule break same dir - no breakpoint inserted");
						s = breakEnd;
					}
				} else {
					// ...otherwise keep looking.
					continue;
				}
			}
		}

		recalculateMeans(periodMin);

		return;

	}
	//*************************************************************************
	//*************************************************************************

	private int countBreakPoints(int maxRunLength) {
		int numBreaks = 0;
		int j = 0;
		while (j < breakPoints.length - 1) {
			Pair pCount = existsBreak2a(j, maxRunLength);
			if (pCount == null) {
				break;
			}
			else {
				numBreaks++;
				j = pCount.b;
			}
		}

		return numBreaks;
	}

	private int calcOverlap(Pair x, Pair y) {

		Pair p = new Pair(0,0);
		Pair q = new Pair(0,0);

		if (x.a <= y.a) {
			p = x;
			q = y;
		} else {
			p = y;
			q = x;
		}

		if (q.a > p.b) return 0;
		return Math.min(q.b-q.a+1,p.b-p.a+1);

}

	private boolean existsBreakPointWithin(int p, int tol, boolean forwards) {

		if (!forwards) {
			if (p < tol) return true;
		} else {
			if (p >= breakPoints.length - tol) return true;
		}
		int direction = -1;
		if (forwards) direction = 1;
		for (int i = 0; i <= tol; i++) {
			if (breakPoints[p + direction*i]) return true;
		}
		return false;
	}

	class Pair
	{
		int a;
		int b;
		Pair (int a, int b) {
			this.a = a;
			this.b = b;
		}
		public String toString() { return "("+ a + ", " + b + ")"; }
	}

	public static void main(String[] args) {
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
//		Vector<Double> testVals2 = new Vector<Double>();
//		testVals2.add(new Double(0.1875));
//		testVals2.add(new Double(0.0625));
//		testVals2.add(new Double(0.0217391));
//		testVals2.add(new Double(0.0227273));
//		testVals2.add(null);
//		testVals2.add(new Double(0.0169492));
//		testVals2.add(new Double(0.0487805));
//		testVals2.add(new Double(0.0909091));

		SPCCalculator2 calc = new SPCCalculator2(testVals, 0, 5);
		System.out.println("Before:\n" + calc);
		calc.calculate();
		System.out.println("After:\n" + calc);
	}

}
