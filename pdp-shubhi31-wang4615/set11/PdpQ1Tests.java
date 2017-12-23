
public class PdpQ1Tests {

	public static void main(String[] args) {
		PdpTestSuite tests = new PdpTestSuite(60);
		String java = "java -Xss100M -ea ";
		tests.addExternalTestCase(
		        "ack(3,3)",
				java + " Programs PS11testPrograms/ack.ps11 3 3",
				"61");
        tests.addExternalTestCase(
                "bst(64,49,2)",
                java + " Programs PS11testPrograms/bst.ps11 64 49 2",
                "2401");
		tests.addExternalTestCase(
		        "bst(64,81,2)",
				java + " Programs PS11testPrograms/bst.ps11 64 81 2",
				"1000000000000000000");
        tests.addExternalTestCase(
                "church(5)",
                java + " Programs PS11testPrograms/church.ps11 5",
                "120");
        tests.addExternalTestCase(
                "fact(11)",
                java + " Programs PS11testPrograms/fact.ps11 11",
                "39916800");
        tests.addExternalTestCase(
                "fact2(11)",
                java + " Programs PS11testPrograms/fact2.ps11 11",
                "39916800");
        tests.addExternalTestCase(
                "fib(7,1)",
                java + " Programs PS11testPrograms/fib.ps11 7 1",
                "13");
        tests.addExternalTestCase(
                "insertionsort(2,20)",
                java + " Programs PS11testPrograms/insertionsort.ps11 2 20",
                "907091414");
        tests.addExternalTestCase(
                "mergesort(1,20)",
                java + " Programs PS11testPrograms/mergesort.ps11 1 20",
                "900010104");
        tests.addExternalTestCase(
                "quicksort(3,12)",
                java + " Programs PS11testPrograms/quicksort.ps11 3 12",
                "910262731");
        tests.addExternalTestCase(
                "selectionsort(3,14)",
                java + " Programs PS11testPrograms/selectionsort.ps11 3 14",
                "910262731");
        tests.addExternalTestCase(
                "sieve(1,100)",
                java + " Programs PS11testPrograms/sieve.ps11 1 100",
                "25");
        tests.addExternalTestCase(
                "sumsq(25,3)",
                java + " Programs PS11testPrograms/sumsq.ps11 25 3",
                "325");
        tests.addExternalTestCase(
                "tail(why was 6 afraid of 7?)",
                java + " Programs PS11testPrograms/tail.ps11 7 8 9",
                "504");
        tests.addExternalTestCase(
                "vectors(618)",
                java + " Programs PS11testPrograms/vectors.ps11 618",
                "78485485");
		tests.runTests();
	}

}
