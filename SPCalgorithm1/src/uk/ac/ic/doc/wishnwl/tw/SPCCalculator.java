package uk.ac.ic.doc.wishnwl.tw;

import java.util.Iterator;
import java.util.Vector;
import java.util.List;
import java.util.ArrayList;

//import uk.ac.ic.doc.wishnwl.tw.SPCCalculator.Pair;

public class SPCCalculator {

	Vector<Double> vals;
	double[] rawVals;
	double[] deltas;
	double[] means;
	double[] amrs;
	ArrayList<Period> periods;

	public SPCCalculator(Vector vals2) {

		this.vals = vals2;
		rawVals = new double[vals.size()];
		for (int i = 0; i < vals.size(); i++) {
			double v = vals.elementAt(i).doubleValue();
			rawVals[i] = v;
		}
		initArrays();
	}

	public SPCCalculator(double[] vals2) {

		this.rawVals = vals2;
		initArrays();
	}

	private void initArrays() {
		double sum = 0.0D;
		means = new double[rawVals.length];
		periods = new ArrayList<Period>();
		if (rawVals.length > 0) {periods.add(new Period(0, rawVals.length - 1));}
		deltas = new double[rawVals.length];
		amrs = new double[rawVals.length];
		if (deltas.length > 0) deltas[0] = 0.0D;
		double prev = 0.0D;
		for (int i = 0; i < rawVals.length; i++) {
			double v = rawVals[i];
			sum += v;
			if (i > 0) deltas[i] = Math.abs(v - prev);
			prev = v;
		}
		if (rawVals.length > 0) {
			double mean = sum / rawVals.length;
			for (int i = 0; i < means.length; i++) {
				means[i] = mean;
			}
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
		sb.append("Periods:[");
		for (int i = 0; i < periods.size(); i++) {
			sb.append(periods.get(i).toString());
		}
		return sb.toString();
	}

	public Object get(int index) {
		return new Double(means[index]);
	}

	public Object getLimit(int index) {
		return new Double(amrs[index]);
	}


	/**
	 * Calculate the mean of a segment defined by start and end points.
	 * @param start Start of segment (inclusive).
	 * @param end End of segment (inclusive).
	 */
	private double calcMeanInclusive(int start, int end) {
		double sum = 0.0D;
		for (int i = start; i <= end; i++) {
			sum += rawVals[i];
		}
		double m = sum / (end - start + 1);
		return m;
	}

	
	private double calcAmrInclusive(int start, int end) {
		double sum = 0.0D;
		
		for (int i = start + 1; i <= end; i++) {
			sum += deltas[i];
		}
		
		double amr = sum / (end - start);
		return amr;
	}
	
	//PRIVATE
	public void recalculate() {

		for (Period P : periods) {
			setMean(P.dispInterval, P.mean());
			setAmr(P.dispInterval, P.amr());
		}
		
	}
	
	//PRIVATE
	public void setMean(Pair p, double v) {
		int i = p.a;
		while(i <= p.b) {
			this.means[i] = v;
			i++;
		}
		
	}
	
	//PRIVATE
	public void setAmr(Pair p, double v) {
		int i = p.a;
		while(i <= p.b) {
			this.amrs[i] = v;
			i++;
		}
		
	}
	

	//*************************************************************************
	//
	//The algorithm itself is specified by this method.
	//Its effect is to modify the breakPoints attribute to its desired state
	//reflecting the result of the recalculation algorithm.
	//
	//*************************************************************************
	public void calculate() {
		System.out.println("Calculate commencing ---------------------------------------------------------------------------");
		// Specify the minimum period length n_p
		int periodMin = 20;
		// Specify the run length to trigger rule break n_r
		int maxRunLength = 8;
		// Check for potential run starts at the end of a new period?
		boolean newPeriodEndCheck = false;

		// Step -1: Check there is sufficient data to calculate limits
		if (rawVals.length < periodMin) {
			// Insufficient data
			// For now, ignore this fact, and leave as one period
			System.out.println("Insufficient data");
			return;
		}
		
		// Step 0: Form the baseline period and check if sufficient data for any
		// additional periods
		Period P0 = new Period(0, periodMin - 1, 0, rawVals.length - 1);
		periods.clear();
		periods.add(P0);
		// Step 1: Calculate mean
		this.recalculate();
		if (rawVals.length < 2*periodMin) {
			// Only enough data for one period.
			System.out.println("Enough data for one period only");
			return;
		}
		
		// Counter to keep track of how much of the dataset has been analysed
		int s = periodMin;
		
		// Loop whilst still enough data to potentially form new period
		while (rawVals.length - s + 1 >= periodMin) {
			// First refresh the mean/limits
			System.out.println("Algorithm loop: s=" + s + "=========================================================");
			System.out.println("Periods: " + periods);
			this.recalculate();
			
			int lastPeriod = periods.size() - 1;
			
			Pair intervalToCheck = new Pair(s, periods.get(lastPeriod).dispInterval.b);
			List<Pair> ruleBreakingRuns = getRuleBreakingRuns(periods.get(lastPeriod), maxRunLength, intervalToCheck);
			
			if (ruleBreakingRuns.size() == 0) {
				System.out.println("No rule-breaking runs.");
				// Step 2: If there are no rule-breaking-runs, finish
				// otherwise continue...
				return;
			}
			
			// There is at least one rbr; the first of these triggers consideration of a recalculation...
			Pair firstRBRun = ruleBreakingRuns.get(0);
			int startIndex = firstRBRun.a;
			int runLength = firstRBRun.length();
			int runDirection = isRuleBreakingRun(firstRBRun, periods.get(lastPeriod), maxRunLength);
			
			// Check enough data after start of run to potentially
			// form new period
			if (rawVals.length - startIndex < periodMin) {
				System.out.println("Run too close to end of data");
				return;
			}
			
			// Identify new candidate period, and check for any rule-breaking-runs within it
			Period newP = new Period(startIndex, startIndex + periodMin - 1);
			System.out.println("Checking for RBRs against candidate period " + newP);
			List<Pair> rbrsNewP = getRuleBreakingRuns(newP, maxRunLength, false);
			List<Pair> rbrsNewPTowardsOriginal = new ArrayList<Pair>();
				for (Pair r : rbrsNewP) {
					// Step 4: Check to see if any of these rbrs are in the opposite direction to the triggering rule-break,
					// i.e. back towards the original process.
					int runDirection2 = isRuleBreakingRun(r, newP, maxRunLength);
					if (runDirection2 == -runDirection) {
						rbrsNewPTowardsOriginal.add(r);
					}
				}
			if (rbrsNewPTowardsOriginal.size() == 0) {
				System.out.println("No RBRs back to original process within candidate period, adding...");
				// If no rule-breaks back towards the original process,
				// put the new period in and continue.
				
				//TODO: Here implement "Strict no-regret" option
				//if newPeriodEndCheck = true
				
				// Add newP to periods
				periods.get(lastPeriod).dispInterval.b = startIndex - 1;
				periods.add(new Period(startIndex, startIndex + periodMin - 1, startIndex, rawVals.length - 1));
				
				// Recalculate
				this.recalculate();
				
				// Increment counter
				s = newP.calcInterval.b + 1;
				
				//go to next while loop
				continue;
			} else {
				// Now we know that there is at least one rule-break back towards
				// the original process, and rbrsNewPTowardsOriginal is a list of these
				
				// Check whether all of these are rule-breaking against the original process
				// in the same direction as the triggering run
				boolean runConsistentWithOriginalProcess = false;
				for (Pair r : rbrsNewPTowardsOriginal) {
					if(isRuleBreakingRun(r, periods.get(lastPeriod), maxRunLength) != runDirection) {
						runConsistentWithOriginalProcess = true;
					}
				}
				if (!runConsistentWithOriginalProcess) {
				System.out.println("All RBRs in candidate period are RB wrt original, adding...");
				// If so, put the new period in and continue.
				
				// TODO: DNRYS - this is same as above!
				
				//TODO: Here implement "Strict no-regret" option
				//if newPeriodEndCheck = true
				
				// Add newP to periods
				periods.get(lastPeriod).dispInterval.b = startIndex - 1;
				periods.add(new Period(startIndex, startIndex + periodMin - 1, startIndex, rawVals.length - 1));
				
				// Recalculate
				this.recalculate();
				
				// Increment counter
				s = newP.calcInterval.b + 1;
				
				//go to next while loop
				continue;
				} else {
					// If not, do not add this period, and start looking from after the
					// triggering run.
					System.out.println("At least one RBR in new period consistent with original process. si=" + startIndex + " rl=" + runLength);
					s = startIndex + runLength;
					continue;
				}
			}
		} 
		
		return;

	}
	//*************************************************************************
	//*************************************************************************


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


	public class Pair
	{
		int a;
		int b;
		Pair (int a, int b) {
			this.a = a;
			this.b = b;
		}
		public int length() { return Math.max(a, b) - Math.min(a, b) + 1; }
		public String toString() { return "("+ a + ", " + b + ")"; }
	}
	
	public class Period
	{
		Pair calcInterval;
		Pair dispInterval;
		Period (Pair c, Pair d) {
			this.calcInterval = c;
			this.dispInterval = d;
		}
		Period (int c0, int c1, int d0, int d1) {
			this.calcInterval = new Pair(c0, c1);
			this.dispInterval = new Pair(d0, d1);
		}
		Period (int a, int b) {
			this.calcInterval = new Pair(a,b);
			this.dispInterval = new Pair(a, b);
		}
		public double mean() {
			return calcMeanInclusive(this.calcInterval.a, this.calcInterval.b);
		}
		public double amr() {
			return calcAmrInclusive(this.calcInterval.a, this.calcInterval.b);
		}
		
		public String toString() {return "[" + calcInterval.toString() + "," + dispInterval.toString() + "]";}
	}
	
	
	public List<Pair> getRuleBreakingRuns(Period P, int runLength, Pair within) {
		
		List<Pair> runList = new ArrayList<Pair>();
		int i = within.a;
		
		Pair run;
		
		while(i <= within.b) {
			run = new Pair(i, getRunEnd(i, P));
			if (run.length() >= runLength) {
				runList.add(run);
			}
			i = run.b;
			i++;
		}
		
		return runList;
	}
	
	
	public List<Pair> getRuleBreakingRuns(Period P, int runLength, boolean startPostCalc) {
		int start;
		if (startPostCalc) {
			start = P.calcInterval.b + 1;
		} else {
			start = P.dispInterval.a;
		}
		Pair pWithin = new Pair(start, P.dispInterval.b);
		
		return getRuleBreakingRuns(P, runLength, pWithin);

	}
	
	
	public int getRunEnd(int i, Period P) {
		double m = P.mean();
		int initDir;
		int dir;
		int j = i;
		initDir = (int) Math.signum(rawVals[i] - m);
		dir = initDir;
		while(dir == initDir || dir == 0) {
			j++;
			if (j == rawVals.length) {break;}
			dir = (int) Math.signum(rawVals[j] - m);
		}
		j--;
		
		return j;
	}
	
	public int isRuleBreakingRun(Pair candidateRun, Period P, int runLength) {
		// Returns 0 if the data points covered by candidateRun do not form a
		// rule-breaking-run w.r.t. P; if they do,
		// return 1 if the run is above P's mean and
		// return -1 if the run is below P's mean.
		if (candidateRun.length() < runLength) {return 0;} else {
		
			int[] signature;
			int[] sigNoZeros;
			int sigTrace;
			double m = P.mean();
			signature = new int[candidateRun.length()];
			for (int i = candidateRun.a; i <= candidateRun.b; i++) {
				signature[i - candidateRun.a] = (int) Math.signum(rawVals[i] - m);
			}
			sigNoZeros = removeSignatureZeros(signature);
			if (sigNoZeros.length < runLength) {return 0;} else {
				sigTrace = sigTrace(sigNoZeros);
				if (sigTrace == -1) { return 0; } else {
					return sigNoZeros[0];
				}
			}
		}
	}
	
	public static int[] removeSignatureZeros(int[] sig) {
		int j = 0;
		int[] sigNoZeros;
			for (int i = 0; i < sig.length; i++) {
				if(sig[i] == 1 || sig[i] == -1) {j++;}
			}
			sigNoZeros = new int[j];
			int k = 0;
			for (int i = 0; i < sig.length; i++) {
				if(sig[i] == 1 || sig[i] == -1) {
					sigNoZeros[k] = sig[i];
					k++;
				}
			}
		// TODO: Handle case where all elements of sig are 0
		return sigNoZeros;
	}
	
	public static int sigTrace(int[] sig) {
		int sameSign = 1;
		for (int i = 1; i < sig.length; i++) {
			if (sig[i] != sig[0]) {
				sameSign = -1;
				break;
			}
		}
		return sameSign;
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

		SPCCalculator calc = new SPCCalculator(testVals);
		System.out.println("Before:\n" + calc);
		calc.calculate();
		System.out.println("After:\n" + calc);
	}


	
}



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
		
		//Note 2017-12-12: I'm thinking go with (i) for now. Need to carefully consider disadvantages of this.
		//Note 2017-12-12: Motivated by AMHS_CNWL_GP65 - even rule-breaks after the period of the original
		//recalculation-inducing rule-break (RB1) can indicate a return to previous process.
		// SO if enough data following RB1 to form new period, do so, otherwise leave as special cause RB
		// against prevailing process.
		
		//***NEXT BEST IDEA***
		//AH so the problem occurs if the n_p points after RB1-start *end* with a run (however long)
		//in the opposite direction to RB1 against its original context process - i.e. back towards that process
		//This is problematic because it *could* turn into a rule-breaking run back to the old process.
		// So potential solution: wait until enough points to recalculate *that do not end in such a run*
		// Then 'label' RB1 as resolved - a special cause signal against the prevailing process.
		
		//Note 2017-12-13 Will we still need to deal with case of persisting rule break? Possibly.
		//Up down up? But then imagine RB1 hugely higher, slight but persistent down, ending above new mean.
		//This would be better recalculated, whereas for a small leap at RB1, better not. That depends on whether
		//The period beyond RB1 constitutes a rule break against RB1's original context process.
		//If so, keep break at RB1-start, if not, don't insert break at RB1-start??
	
		
		//TODO: Once the above is resolved, need to deal with multiple recalculation points
		//(as of v2, so far have only dealt with first. Would be great to do this by making it recursive).



		