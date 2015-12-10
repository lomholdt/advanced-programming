// Week 3
// sestoft@itu.dk * 2015-09-09

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.IntSummaryStatistics;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.lang.StringBuilder;
import java.util.stream.IntStream;
import java.util.stream.DoubleStream;

public class TestWordStream {
  public static void main(String[] args) {
    String filename = "/usr/share/dict/words";
    // String filename = "/Users/Lomholdt/Dropbox/skole/itu/3semester/parallel/practical-concurrent-and-parallel-programming/pcpp-week03/usr/share/dict/words";
    System.out.println(readWords(filename).count());


    // readWords(filename).limit(100).forEach(System.out::println); // 6.3.2
    // readWords(filename).filter(x -> x.length() > 22).forEach(System.out::println); // 6.3.3
    // System.out.println(readWords(filename).filter(x -> x.length() > 22).findAny().get()); // 6.3.4
    readWords(filename).filter(x -> isPalindrome(x)).forEach(System.out::println); // 6.3.5
    readWords(filename).parallel().filter(x -> isPalindrome(x)).forEach(System.out::println); // 6.3.6
    // System.out.println(readWords(filename).mapToInt(x -> x.length()).summaryStatistics()); // 6.3.7
    // System.out.println(readWords(filename).collect(Collectors.groupingBy(String::length))); // 6.3.8
    // readWords(filename).map(x -> letters(x)).limit(100).forEach(System.out::println); // 6.3.9
    // System.out.println(readWords(filename).map(x -> letters(x).get('e')).filter(x -> x != null).reduce((a,b) -> a+b).get()); 6.3.10

    // System.out.println(readWords(filename).collect(Collectors.groupingBy(x -> letters(x)))); // 6.3.11
    // System.out.println(readWords(filename).parallel().collect(Collectors.groupingBy(x -> letters(x)))); // 6.3.12 
    
    // 6.4.1
    final int N = 999_999_999;
    // long startTime = System.nanoTime();
    // System.out.println(IntStream.range(1,N).mapToDouble(i -> 1.0/i).sum());
    // long estimatedTime = System.nanoTime() - startTime;
    // System.out.printf("No Parallel: %10.d%n", estimatedTime);
    // 6.4.2
    // long startTimeParallel = System.nanoTime();
    // System.out.println(IntStream.range(1,N).parallel().mapToDouble(i -> 1.0/i).sum());
    // long estimatedTimeParallel = System.nanoTime() - startTimeParallel;
    // System.out.printf("With Parallel: %s%n", estimatedTimeParallel);
    // 6.4.3
    // double sum = 0.0;
    // long startTimeSequentialLoop = System.nanoTime();
    // for (int i=1; i<N; i++){ sum += 1.0/i; }
    // System.out.println(sum);
    // long estimatedTimeSequentialLoop = System.nanoTime() - startTimeSequentialLoop;
    // System.out.printf("Sequential Loop: %s%n", estimatedTimeSequentialLoop);

    // 6.4.4
    // long startTimeImperativeLoop = System.nanoTime();
    // final double[] next = { 1 };
    // System.out.println(DoubleStream.generate(() -> 1.0/next[0]++).limit(N).sum());
    // long estimatedTimeImperativeLoop = System.nanoTime() - startTimeImperativeLoop;
    // System.out.printf("Imperative: %s%n", estimatedTimeImperativeLoop);

    // 6.4.5
    // long startTimeImperativeLoopParallel = System.nanoTime();
    // final double[] nextParallel = { 1 };
    // System.out.println(DoubleStream.generate(() -> 1.0/nextParallel[0]++).limit(N).parallel().sum());
    // long estimatedTimeImperativeLoopParallel = System.nanoTime() - startTimeImperativeLoopParallel;
    // System.out.printf("Imperative Parallel: %s%n", estimatedTimeImperativeLoopParallel);

  }

  // 6.3.1
  public static Stream<String> readWords(String filename) {
    try {
      BufferedReader reader = new BufferedReader(new FileReader(filename));
      return reader.lines();
    } catch (IOException exn) { 
      return Stream.<String>empty();
    }
  }

  public static boolean isPalindrome(String s) {
    StringBuilder sb = new StringBuilder();
    sb.append(s);
    return sb.reverse().toString().equals(s); 
  }

  // 6.3.9
  public static Map<Character,Integer> letters(String s) {
    Map<Character,Integer> res = new TreeMap<>();
    for (char c : s.toCharArray()) {
      int val = res.get(c) == null ? 1 : res.get(c) + 1;
      res.put(c, val);
    }
    return res;
  }
}
