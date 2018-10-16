package uk.ac.ic.doc.wishnwl.tw;

import java.util.Vector;
import java.util.List;
import java.util.ArrayList;

import org.apache.commons.lang3.Range;

public class SPCCalculator {
	
	int periodMin;
	int maxRunLength;
	boolean forceNewPeriodOnRBR;
	private Vector<Double> vals;
	private double[] rawVals;
	private double[] deltas;
	private double[] means;
	private double[] amrs;
	ArrayList<Period> periods;
	//ArrayList<double[]> output;
	
	//
	// Constructors
	//
	public SPCCalculator(Vector vals2, int minimumPeriodLength, int runRuleLength, boolean alwaysRecalc) {

		this.periodMin = minimumPeriodLength;
		this.maxRunLength = runRuleLength;
		this.forceNewPeriodOnRBR = alwaysRecalc;
		this.vals = vals2;
		rawVals = new double[vals.size()];
		for (int i = 0; i < vals.size(); i++) {
			double v = vals.elementAt(i).doubleValue();
			rawVals[i] = v;
		}
		initArrays();
	}

	public SPCCalculator(double[] vals2, int minimumPeriodLength, int runRuleLength, boolean alwaysRecalc) {
		
		this.periodMin = minimumPeriodLength;
		this.maxRunLength = runRuleLength;
		this.forceNewPeriodOnRBR = alwaysRecalc;
		this.rawVals = vals2;
		initArrays();
	}
	
	public SPCCalculator(Vector vals2) {
		this(vals2, 20, 8, false);
	}
	
	public SPCCalculator(double[] vals2) {
		this(vals2, 20, 8, false);
	}
	
	//
	// Initialisation
	//
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
	
	//
	// Print to text
	//
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

	//
	// Getters and setters
	//
	public Object get(int index) {
		return new Double(means[index]);
	}

	public Object getLimit(int index) {
		return new Double(amrs[index]);
	}
	
	public double[] getRawVals() {
		return this.rawVals;
	}
	
	public double[] getDeltas() {
		return this.deltas;
	}
	
	public double[] getCentres() {
		return this.means;
	}
	
	public double[] getAmrs() {
		return this.amrs;
	}
	
	public ArrayList<Period> getPeriods() {
		return this.periods;
	}

	//PRIVATE
	public void setMean(Range<Integer> p, double v) {
		int i = p.getMinimum();
		while(i <= p.getMaximum()) {
			this.means[i] = v;
			i++;
		}
		
	}
	
	//PRIVATE
	public void setAmr(Range<Integer> p, double v) {
		int i = p.getMinimum();
		while(i <= p.getMaximum()) {
			this.amrs[i] = v;
			i++;
		}
		
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
		// int this.periodMin = 20;
		// Specify the run length to trigger rule break n_r
		// int this.maxRunLength = 8;
		// Check for potential run starts at the end of a new period?
		boolean newPeriodEndCheck = true;
		// This first one overrides the below parameters
		// boolean this.forceNewPeriodOnRBR = false;
		// Only disqualify a new Period if the run back to original process is longer than
		// the triggering run
		boolean onlyScreenReboundRBRsLongerThanTrigger = false;
		// A run back to the original process disqualifies a new Period even if
		// it doesn't reach this.maxRunLength until after the end of the new calc period.
		boolean screenReboundRBRsEvenIfTheyTriggerAfterCalcPeriod = true;
		// 
		boolean doNotAddPeriodIfTriggerIsStillRBRInSameDir = true;
		

		System.out.println("Algorithm parameters:");
		System.out.println("n_p = " + this.periodMin + ", n_r = " + this.maxRunLength);
		System.out.println("this.forceNewPeriodOnRBR: " + this.forceNewPeriodOnRBR);
		System.out.println("onlyScreenReboundRBRsLongerThanTrigger: " + onlyScreenReboundRBRsLongerThanTrigger +
				". screenReboundRBRsEvenIfTheyTriggerAfterCalcPeriod: " + screenReboundRBRsEvenIfTheyTriggerAfterCalcPeriod);
		System.out.println("doNotAddPeriodIfTriggerIsStillRBRInSameDir: " + doNotAddPeriodIfTriggerIsStillRBRInSameDir);
		// Step -1: Check there is sufficient data to calculate limits
		if (rawVals.length < this.periodMin) {
			// Insufficient data
			// For now, ignore this fact, and leave as one period
			System.out.println("Insufficient data");
			return;
		}
		
		// Step 0: Form the baseline period and check if sufficient data for any
		// additional periods
		Period P0 = new Period(0, this.periodMin - 1, 0, rawVals.length - 1);
		periods.clear();
		periods.add(P0);
		// Step 1: Calculate mean
		this.recalculate();
		if (rawVals.length < 2*this.periodMin) {
			// Only enough data for one period.
			System.out.println("Enough data for one period only");
			return;
		}
		
		// Counter to keep track of how much of the dataset has been analysed
		int s = this.periodMin;
		
		// Loop whilst still enough data to potentially form new period
		while (rawVals.length - s + 1 >= this.periodMin) {
			// First refresh the mean/limits
			System.out.println("Algorithm loop: s=" + s + "=========================================================");
			System.out.println("Periods: " + periods);
			this.recalculate();
			
			int lastPeriod = periods.size() - 1;
			
			Range<Integer> intervalToCheck = Range.between(s, periods.get(lastPeriod).dispInterval.getMaximum());
			List<Range<Integer>> ruleBreakingRuns = ruleBreakingRuns(periods.get(lastPeriod), this.maxRunLength, intervalToCheck);
			
			if (ruleBreakingRuns.size() == 0) {
				System.out.println("No rule-breaking runs.");
				// Step 2: If there are no rule-breaking-runs, finish
				// otherwise continue...
				return;
			}
			
			// There is at least one rbr; the first of these triggers consideration of a recalculation...
			Range<Integer> firstRBRun = ruleBreakingRuns.get(0);
			int startIndex = firstRBRun.getMinimum();
			int runLength = rangeLength(firstRBRun);
			int runDirection = isRuleBreakingRun(firstRBRun, periods.get(lastPeriod), this.maxRunLength);
			
			// Check enough data after start of run to potentially
			// form new period
			if (rawVals.length - startIndex < this.periodMin) {
				System.out.println("Run too close to end of data");
				return;
			}
			
			// Identify new candidate period, and check for any rule-breaking-runs within it
			Range<Integer> newPcalc = Range.between(startIndex, startIndex + this.periodMin - 1);
			Range<Integer> newPdisp = Range.between(startIndex, rawVals.length - 1);
			Period newP = new Period(newPcalc, newPdisp);
			
			if(this.forceNewPeriodOnRBR) {
				System.out.println("RBR forces new period");
				// Add newP to periods
				Range<Integer> lastPeriodDisp = periods.get(lastPeriod).dispInterval;
				Range<Integer> newLastPeriodDisp = Range.between(lastPeriodDisp.getMinimum(), startIndex - 1);
				Period newLastPeriod = new Period(periods.get(lastPeriod).calcInterval, newLastPeriodDisp);
				periods.set(periods.size() - 1, newLastPeriod);
				periods.add(new Period(startIndex, startIndex + this.periodMin - 1, startIndex, rawVals.length - 1));
				
				// Recalculate
				this.recalculate();
				
				// Increment counter
				s = newP.calcInterval.getMaximum() + 1;
				
				//go to next while loop
				continue;
			}
			
			
			System.out.println("Checking for RBRs against candidate period " + newP);
			List<Range<Integer>> rbrsNewP = ruleBreakingRuns(newP, this.maxRunLength, false);
			// First check that the triggering rule break is not still a RBR in the same direction...
			boolean triggerIsStillRBRSameDir;
			if (rbrsNewP.size() == 0) {
				triggerIsStillRBRSameDir = false;
			} else {
			triggerIsStillRBRSameDir = rbrsNewP.get(0).getMinimum() <= startIndex &&
					rbrsNewP.get(0).getMaximum() >= firstRBRun.getMaximum() &&
					isRuleBreakingRun(rbrsNewP.get(0), newP, this.maxRunLength) == runDirection;
			}
			if (doNotAddPeriodIfTriggerIsStillRBRInSameDir && triggerIsStillRBRSameDir) {
				// If it is, do not add this period, and start looking from after the
				// triggering run.
				System.out.println("Triggering run is still RBR in same direction. si=" + startIndex + " rl=" + runLength);
				s = startIndex + runLength;
				continue;
			}
			// Now check for RBRs back towards the original process...
			List<Range<Integer>> rbrsNewPTowardsOriginal = new ArrayList<Range<Integer>>();
				for (Range<Integer> r : rbrsNewP) {
					// Step 4: Check to see if any of these rbrs are in the opposite direction to the triggering rule-break,
					// i.e. back towards the original process.
					// 
					int runDirection2 = isRuleBreakingRun(r, newP, this.maxRunLength);
					if (runDirection2 == -runDirection && r.getMinimum() <= newP.calcInterval.getMaximum() &&
							(screenReboundRBRsEvenIfTheyTriggerAfterCalcPeriod ||
									r.getMinimum() <= newP.calcInterval.getMaximum() - this.maxRunLength + 1)) {
						rbrsNewPTowardsOriginal.add(r);
					}
				}
			if (rbrsNewPTowardsOriginal.size() == 0 || 
					(onlyScreenReboundRBRsLongerThanTrigger && maxRangeLength(rbrsNewPTowardsOriginal) < runLength)) {
				System.out.println("No RBRs, or none longer than triggering run, back to original process within candidate period, adding...");
				System.out.println("[Number of RBRs in direction of original:" + rbrsNewPTowardsOriginal.size() + "]");
				// If no rule-breaks back towards the original process,
				// put the new period in and continue.
				
				//"Strict no-regret" option

				if (newPeriodEndCheck && 
						(int) Math.signum(rawVals[newP.calcInterval.getMaximum()] - newP.mean()) == -runDirection) {
					int startLastRunInNewPcalc = runStart(newP.calcInterval.getMaximum(), newP);
					int numRemaining = rawVals.length - startLastRunInNewPcalc;
					if (numRemaining < this.maxRunLength) {
						int endLastRunInNewPcalc = runEnd(newP.calcInterval.getMaximum(), newP);
						if (endLastRunInNewPcalc >= rawVals.length - 1) {
							System.out.println("Last run in new calc. period may trigger in future: " + startLastRunInNewPcalc + ", " + endLastRunInNewPcalc);
							s = startIndex + runLength;
							continue;
						}	
					} 
				}
				
				// Add newP to periods
				Range<Integer> lastPeriodDisp = periods.get(lastPeriod).dispInterval;
				Range<Integer> newLastPeriodDisp = Range.between(lastPeriodDisp.getMinimum(), startIndex - 1);
				Period newLastPeriod = new Period(periods.get(lastPeriod).calcInterval, newLastPeriodDisp);
				periods.set(periods.size() - 1, newLastPeriod);
				periods.add(new Period(startIndex, startIndex + this.periodMin - 1, startIndex, rawVals.length - 1));
				
				// Recalculate
				this.recalculate();
				
				// Increment counter
				s = newP.calcInterval.getMaximum() + 1;
				
				//go to next while loop
				continue;
			} else {
				// Now we know that there is at least one rule-break back towards
				// the original process, and rbrsNewPTowardsOriginal is a list of these
				
				// Check whether all of these are rule-breaking against the original process
				// in the same direction as the triggering run
				boolean runConsistentWithOriginalProcess = false;
				for (Range<Integer> r : rbrsNewPTowardsOriginal) {
					if(isRuleBreakingRun(r, periods.get(lastPeriod), this.maxRunLength) != runDirection) {
						runConsistentWithOriginalProcess = true;
					}
				}
				if (!runConsistentWithOriginalProcess) {
				
				System.out.println("All RBRs in candidate period are RB wrt original");
				
				// TODO: DNRYS - this is same as above!
				
				//"Strict no-regret" option
				if (newPeriodEndCheck && 
						(int) Math.signum(rawVals[newP.calcInterval.getMaximum()] - newP.mean()) == -runDirection) {
					int startLastRunInNewPcalc = runStart(newP.calcInterval.getMaximum(), newP);
					int endLastRunInNewPcalc = runEnd(newP.calcInterval.getMaximum(), newP);
					int numRemaining = rawVals.length - startLastRunInNewPcalc;
					if (numRemaining < this.maxRunLength) {
						if (endLastRunInNewPcalc >= rawVals.length - 1 ||
								isRuleBreakingRun(Range.between(startLastRunInNewPcalc, endLastRunInNewPcalc), newP, this.maxRunLength) == -runDirection) {
							System.out.println("Last run in new calc. period rebounds or may rebound in future: " + startLastRunInNewPcalc + ", " + endLastRunInNewPcalc);
							s = startIndex + runLength;
							continue;
						}	
					} 
				}
				
				
				// put the new period in and continue.
				
				// Add newP to periods
				Range<Integer> lastPeriodDisp = periods.get(lastPeriod).dispInterval;
				Range<Integer> newLastPeriodDisp = Range.between(lastPeriodDisp.getMinimum(), startIndex - 1);
				Period newLastPeriod = new Period(periods.get(lastPeriod).calcInterval, newLastPeriodDisp);
				periods.set(periods.size() - 1, newLastPeriod);
				periods.add(new Period(startIndex, startIndex + this.periodMin - 1, startIndex, rawVals.length - 1));
				
				// Recalculate
				this.recalculate();
				
				// Increment counter
				s = newP.calcInterval.getMaximum() + 1;
				
				//go to next while loop
				continue;
				} else {
					// If not, do not add this period, and start looking from after the
					// triggering run.
					System.out.println("At least one RBR in new period consistent with original process. si=" + startIndex + " rl=" + runLength);
					System.out.println("RBRs in new period towards original process: ");
					for (Range<Integer> p : rbrsNewPTowardsOriginal) {
						System.out.println(p);
						System.out.println(isRuleBreakingRun(p, periods.get(lastPeriod), this.maxRunLength));
					}
					s = startIndex + runLength;
					continue;
				}
			}
		} 
		
		return;

	}
	//*************************************************************************
	//*************************************************************************
	
	
	public static int rangeLength(Range<Integer> r) {
		return r.getMaximum() - r.getMinimum() + 1;
	}
	
	
	public class Period {

		Range<Integer> calcInterval;
		Range<Integer> dispInterval;
		Period (Range<Integer> c, Range<Integer> d) {
			this.calcInterval = c;
			this.dispInterval = d;
		}
		Period (int c0, int c1, int d0, int d1) {
			this.calcInterval = Range.between(c0, c1);
			this.dispInterval = Range.between(d0, d1);
		}
		Period (int a, int b) {
			this.calcInterval = Range.between(a, b);
			this.dispInterval = Range.between(a, b);
		}
		public double mean() {
			return calcMeanInclusive(this.calcInterval.getMinimum(), this.calcInterval.getMaximum());
		}
		public double amr() {
			return calcAmrInclusive(this.calcInterval.getMinimum(), this.calcInterval.getMaximum());
		}
		
		public String toString() {return "[" + calcInterval.toString() + "," + dispInterval.toString() + "]";}
	}
	
	
	public List<Range<Integer>> ruleBreakingRuns(Period P, int runLength, Range<Integer> within) {
		
		List<Range<Integer>> runList = new ArrayList<Range<Integer>>();
		int i = within.getMinimum();
		
		Range<Integer> run;
		
		while(i <= within.getMaximum()) {
			run = Range.between(i, runEnd(i, P));
			if (rangeLength(run) >= runLength) {
				runList.add(run);
			}
			i = run.getMaximum();
			i++;
		}
		
		return runList;
	}
	
	
	public List<Range<Integer>> ruleBreakingRuns(Period P, int runLength, boolean startPostCalc) {
		int start;
		if (startPostCalc) {
			start = P.calcInterval.getMaximum() + 1;
		} else {
			start = P.dispInterval.getMinimum();
		}
		Range<Integer> pWithin = Range.between(start, P.dispInterval.getMaximum());
		
		return ruleBreakingRuns(P, runLength, pWithin);

	}
	
	
	public int runEnd(int i, Period P) {
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
	
	public int runStart(int i, Period P) {
		double m = P.mean();
		int initDir;
		int dir;
		int j = i;
		initDir = (int) Math.signum(rawVals[i] - m);
		dir = initDir;
		while(dir == initDir || dir == 0) {
			j--;
			if (j == -1) {break;}
			dir = (int) Math.signum(rawVals[j] - m);
		}
		j++;
		
		return j;
	}
	
	public int maxRangeLength(List<Range<Integer>> rangeList) throws IllegalArgumentException {
		
		if (rangeList.size() == 0) {
			throw new IllegalArgumentException();
		}
		int [] rangeLengths = new int[rangeList.size()];
		int i = 0;
		int maxLength = rangeLength(rangeList.get(0));
		
		for (Range<Integer> p : rangeList) {
			rangeLengths[i] = rangeLength(p);
			i++;
		}
		for (i = 0; i < rangeList.size(); i++) {
			maxLength = Math.max(maxLength, rangeLengths[i]);
		}
		
		return maxLength;
	}
	
	public int isRuleBreakingRun(Range<Integer> candidateRun, Period P, int runLength) {
		// Returns 0 if the data points covered by candidateRun do not form a
		// rule-breaking-run w.r.t. P; if they do,
		// return 1 if the run is above P's mean and
		// return -1 if the run is below P's mean.
		if (rangeLength(candidateRun) < runLength) {return 0;} else {
		
			int[] signature;
			int[] sigNoZeros;
			int sigTrace;
			double m = P.mean();
			signature = new int[rangeLength(candidateRun)];
			for (int i = candidateRun.getMinimum(); i <= candidateRun.getMaximum(); i++) {
				signature[i - candidateRun.getMinimum()] = (int) Math.signum(rawVals[i] - m);
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



		