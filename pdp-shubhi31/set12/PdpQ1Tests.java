import java.lang.reflect.Method;
import java.util.Arrays;

public class PdpQ1Tests {
	
	static long call(String methodName, long ... args) {
		Class<?> cls = Sumsq2.class;
		try {
			Method m = cls.getDeclaredMethod(methodName, Long.TYPE, Long.TYPE);
			Object[] args1 = Arrays.stream(args).mapToObj((n) -> n).toArray();
			return (long) m.invoke(null, args1);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public static void main(String[] args) {
		PdpTestSuite tests = new PdpTestSuite(10);
		
		long[][] sums = {
				{       0,       0},
				{       1,       1},
				{       2,       3},
				{       3,       6},
				{       4,      10},
				{       7,      28},
				{      14,     105},
				{      21,     231},
				{      28,     406},
				{      35,     630},
				{      42,     903},
				{      49,    1225},
				{      56,    1596},
				{      63,    2016},
				{      70,    2485},
		};
		
		for (long[] s : sums) {
			tests.addTestCase("mainLoop(" + s[0] + ", 3)",
					() -> call("mainLoop", s[0], 3),
					s[1]);
			tests.addTestCase("sumSquaresLoop(" + s[0] + ", 91)",
					() -> call("sumSquaresLoop", s[0], 91),
					s[1] + 91);
			tests.addTestCase("sumSquaresLoop2(" + s[0] + ", 37)",
					() -> call("sumSquaresLoop2", s[0], 37),
					-s[1] + 37);
		}
		
		tests.runTests();
	}

}
