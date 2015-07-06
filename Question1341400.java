import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import algebra.FiniteField;
import algebra.Matrix;
 
public class Question1341400 {
	public static void main (String [] args) {
		n = 50;
		k = 31;
		System.out.println (test (Long.parseLong ("00101111111110001011010001011000111011100110100111",2)));
	}
	
	static List<Integer> getEquivalenceClasses (int n) {
		List<Integer> result = new ArrayList<Integer> (); 
		boolean [] done = new boolean [1 << n];
		for (int i = 0;i < done.length;i++)
			if (!done [i]) {
				result.add (i);
				for (int j = 0,k = i;j < n;j++,k = rotate (k,n))
					done [k] = true;
			}
		return result;
	}
	
	static int rotate (int k,int n) {
		return (k >> 1) | ((k & 1) << (n - 1));
	}
	
	// encode (-1,0,1) as (2,3,1), which allows addition mod 3 to be encoded as 3 & (x + (x >> 2)) or (3 & x) + (x >> 2), where x is the raw sum
	
	static int encode (int i) {
		return (i + 2) % 3 + 1;
	}
	
	static int decode (int i) {
		return (i + 1) % 3 - 1;
	}
 
	static byte [] counts = new byte [1 << 30];
	static int [] values = {0,1,-1,0};
	
	static void recurse (int i,int index,int count) {
		if (i == 10)
			counts [index] = (byte) count;
		else
			for (int j = 0;j < 4;j++)
				recurse (i + 1,(index << 3) + j,count + values [j]);
	}
 
	static {
		recurse (0,0,0);
	}
	
	static boolean test (long bits) {
		Matrix<Integer> b = new Matrix<Integer> (new FiniteField (3),k,n);
 
		for (int i = 0;i < k;i++)
			for (int j = 0;j < n;j++)
				b.set (i,j,(int) ((bits >> ((i + j) % n)) & 1));
 
		Matrix<Integer> bKernelBasis = b.getKernelBasis ();
		dim = bKernelBasis.n;
		
		vector0 = vector1 = vector2 = vector3 = vector4 = mask3;
		
		int [] [] rows = new int [5] [k];
		int [] [] basis = new int [5] [dim];
		
		for (int i = 0;i < k;i++)
			for (int j = 0;j < n;j++)
				if (b.get (i,j) == 1)
					rows [j / 10] [i] |= 3 << (3 * (j % 10));
		
		for (int i = 0;i < dim;i++)
			for (int j = 0;j < n;j++)
				basis [j / 10] [i] |= encode (bKernelBasis.get (j,i)) << (3 * (j % 10));
 
		basis0 = basis [0];
		basis1 = basis [1];
		basis2 = basis [2];
		basis3 = basis [3];
		basis4 = basis [4];
		
		rows0 = rows [0];
		rows1 = rows [1];
		rows2 = rows [2];
		rows3 = rows [3];
		rows4 = rows [4];
		
		return recurse (0,false);
	}
	
	static int dim;
	
	static int [] rows0,rows1,rows2,rows3,rows4;
	static int [] basis0,basis1,basis2,basis3,basis4;
	static int vector0,vector1,vector2,vector3,vector4;
	
	static int k,n;
	
	final static int mask1 = 0x9249249;
	final static int mask3 = 3 * mask1;
	
	static int add (int x,int y) {
		int z = x + y;
		return (z & mask3) + ((z >> 2) & mask1);
	}
	
	static boolean recurse (int i,boolean hadNonZero) {
		if (i == dim) {
			if (!hadNonZero)
				return false;
			for (int j = 0;j < k;j++)
				if (counts [vector0 & rows0 [j]] + counts [vector1 & rows1 [j]] + counts [vector2 & rows2 [j]] + counts [vector3 & rows3 [j]] + counts [vector4 & rows4 [j]] != 0)
					return false;
			return true;
		}
		
		for (int j = 0;j < 3;j++) {
			if (hadNonZero || j != 2)
				if (recurse (i + 1,hadNonZero || j != 0))
					return true;
	
			vector0 = add (vector0,basis0 [i]);
			vector1 = add (vector1,basis1 [i]);
			vector2 = add (vector2,basis2 [i]);
			vector3 = add (vector3,basis3 [i]);
			vector4 = add (vector4,basis4 [i]);
		}
		
		return false;
	}
 
	// This is the code that I used to optimize the encoding for arithmetic with vectors over F_3 
	
	static void optimizeCodes () {
		int [] [] codes = new int [12] [3];
 
		for (int add = 0,k = 0;add <= 1;add++)
			for (int sign = -1;sign <= 1;sign += 2)
				for (int shift = 0;shift < 3;shift++,k++)
					for (int i = 0;i < 3;i++)
						codes [k] [i] = (3 + sign * i + shift) % 3 + add;
 
		int [] [] table = new int [9] [2];
 
		for (int [] code1 : codes)
			for (int [] code2 : codes) {
				for (int i = 0,k = 0;i < 3;i++)
					for (int j = 0;j < 3;j++,k++) {
						table [k] [0] = code1 [i] + code2 [j];
						table [k] [1] = code1 [(i + j + 2) % 3];
					}
				Collection<Function> functions = findFunction (table);
				if (!functions.isEmpty ()) {
					System.out.println (Arrays.toString (code1) + " x " + Arrays.toString (code2));
					System.out.println (Arrays.deepToString (table));
					System.out.println ("  " + functions);
				}
			}
	}
 
	static interface Function {
		int f (int x);
	}
	
	static abstract public class UnaryFunction implements Function {
		Function f1;
		String s;
 
		public UnaryFunction (String s,Function f1) {
			this.f1 = f1;
			this.s = s;
		}
 
		abstract int op (int a);
		
		public int f (int x) {
			return op (f1.f (x));
		}
		
		public String toString () {
			return s + f1;
		}
	}
 
	static public class NegationFunction extends UnaryFunction {
		public NegationFunction (Function f1) {
			super ("~",f1);
		}
 
		int op (int a) {
			return ~a;
		}
	}
	
	static abstract public class BinaryFunction implements Function {
		Function f1;
		Function f2;
		String s;
 
		public BinaryFunction (String s,Function f1,Function f2) {
			this.f1 = f1;
			this.f2 = f2;
			this.s = s;
		}
 
		abstract int op (int a,int b);
		
		public int f (int x) {
			return op (f1.f (x),f2.f (x));
		}
		
		public String toString () {
			return "(" + f1 + " " + s + " " + f2 + ")";
		}
	}
	
	static public class SumFunction extends BinaryFunction {
		public SumFunction (Function f1,Function f2) {
			super ("+",f1,f2);
		}
 
		int op (int a,int b) {
			return a + b;
		}
	}
	
	static public class ProductFunction extends BinaryFunction {
		public ProductFunction (Function f1,Function f2) {
			super ("*",f1,f2);
		}
 
		int op (int a,int b) {
			return a * b;
		}
	}
 
	static public class DifferenceFunction extends BinaryFunction {
		public DifferenceFunction (Function f1,Function f2) {
			super ("-",f1,f2);
		}
 
		int op (int a,int b) {
			return a - b;
		}
	}
 
	static public class AndFunction extends BinaryFunction {
		public AndFunction (Function f1,Function f2) {
			super ("&",f1,f2);
		}
 
		int op (int a,int b) {
			return a & b;
		}
	}
 
	static public class OrFunction extends BinaryFunction {
		public OrFunction (Function f1,Function f2) {
			super ("|",f1,f2);
		}
 
		int op (int a,int b) {
			return a | b;
		}
	}
 
	static public class XorFunction extends BinaryFunction {
		public XorFunction (Function f1,Function f2) {
			super ("^",f1,f2);
		}
 
		int op (int a,int b) {
			return a ^ b;
		}
	}
 
	static public class LeftShiftFunction extends BinaryFunction {
		public LeftShiftFunction (Function f1,Function f2) {
			super ("<<",f1,f2);
		}
 
		int op (int a,int b) {
			return a << b;
		}
	}
 
	static public class RightShiftFunction extends BinaryFunction {
		public RightShiftFunction (Function f1,Function f2) {
			super (">>",f1,f2);
		}
 
		int op (int a,int b) {
			return a >> b;
		}
	}
	
	static public class ConstantFunction implements Function {
		int c;
 
		public ConstantFunction (int c) {
			this.c = c;
		}
 
		public int f (int x) {
			return c;
		}
		
		public String toString () {
			return String.valueOf (c);
		}
	}
 
	static public class IdentityFunction implements Function {
		public int f (int x) {
			return x;
		}
		
		public String toString () {
			return "x";
		}
	}
	
	final static int depth = 3; 
	
	public static Collection<Function> findFunction (int [] [] table) {
		List<Function> result = new ArrayList<Function> ();
		List<Function> [] functions = new ArrayList [depth + 1];
		for (int i = 0;i < functions.length;i++)
			functions [i] = new ArrayList<Function> ();
		
		functions [0].add (new IdentityFunction ());
		for (int c = 1;c <= 4;c++)
			functions [0].add (new ConstantFunction (c));
		for (int i = 0;i < depth;i++) {
			for (Function f : functions [i])
				functions [i + 1].add (new NegationFunction (f));
			for (int j = 0;j <= i;j++)
				for (Function f : functions [j]) {
					boolean symmetric = true;
					for (Function g : functions [i - j]) {
						if (symmetric && j <= i - j) {
							functions [i + 1].add (new SumFunction (f,g));
							functions [i + 1].add (new DifferenceFunction (f,g));
							functions [i + 1].add (new AndFunction (f,g));
							functions [i + 1].add (new OrFunction (f,g));
							functions [i + 1].add (new XorFunction (f,g));
						}
						if (g instanceof ConstantFunction) {
							functions [i + 1].add (new LeftShiftFunction (f,g));
							functions [i + 1].add (new RightShiftFunction (f,g));
						}
						symmetric &= f != g;
					}
				}
			for (Function f : functions [i + 1])
				if (test (f,table))
					result.add (f);
		}
		
		return result;
	}
	
	static boolean test (Function f,int [] [] table) {
		for (int [] result : table)
			if (f.f (result [0]) != result [1])
				return false;
		return true;
	}
}
