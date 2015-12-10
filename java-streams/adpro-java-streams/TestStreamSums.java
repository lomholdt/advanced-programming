// Week 3
// sestoft@itu.dk * 2015-08-28

// Outputs, excluding BigDecimal (which takes ca 250 seconds):

// # OS:   Mac OS X; 10.9.5; x86_64
// # JVM:  Oracle Corporation; 1.8.0_51
// # CPU:  null; 8 "cores"
// # Date: 2015-09-28T21:52:14+0200

// Execution times, milliseconds:
// forwardStream                      8570.7 ms     163.90          2
// backwardStream                     8523.4 ms      52.04          2
// parallelStream                     2044.3 ms      54.26          2
// limitStream                        8224.4 ms      83.10          2
// kahanLoop                          9074.9 ms      81.10          2
// forwardFor                         5408.5 ms     538.11          2
// backwardFor                        7140.1 ms     693.89          2

// Numerical results:
// Mono decimal       21.300481501347944016685100637
// BigDecimal,50      21.300481501347944016685101848908346966127087030566
// precise        =   21.3004815013479440166851
// forwardStream  =   21.300481501347942
// backwardStream =   21.300481501347946
// parallelStream =   21.300481501347942
// limitStream    =   21.300481501347942
// kahanLoop      =   21.300481501347942
// forwardFor     =   21.300481501348550
// backwardFor    =   21.300481501346148


// Same, run on AMD Opteron

// # OS:   Linux; 3.2.0-76-generic; amd64
// # JVM:  Oracle Corporation; 1.8.0_60
// # CPU:  null; 32 "cores"
// # Date: 2015-09-30T21:11:37+0200
// forwardStream                     16887.8 ms    2572.46          2
// backwardStream                    16011.8 ms    1423.62          2
// parallelStream                      758.9 ms      80.56          2
// limitStream                       24061.6 ms     139.80          2
// kahanLoop                         15729.7 ms      83.76          2
// forwardFor                         6707.7 ms      91.85          2
// backwardFor                        6488.9 ms     149.22          2
// precise        =   21.3004815013479440166851
// forwardStream  =   21.300481501347942
// backwardStream =   21.300481501347946
// parallelStream =   21.300481501347942
// limitStream    =   21.300481501347942
// kahanLoop      =   21.300481501347942
// forwardFor     =   21.300481501348550
// backwardFor    =   21.300481501346148

import java.util.stream.DoubleStream;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import java.util.function.IntToDoubleFunction;
import java.math.BigDecimal;
import java.math.MathContext;

public class TestStreamSums {
  public static void main(String[] args) {
    int N = 1_000_000_000;
    SystemInfo();
    timeAll(N);
    exerciseAll(N);
  }

  public static void exerciseAll(int N) {
    System.out.printf("precise        =   21.3004815013479440166851%n");
    //    System.out.printf("bigDecimalSum  =  %s%n", bigDecimalSum(N));
    System.out.printf("forwardStream  = %20.15f%n", forwardStream(N));
    System.out.printf("backwardStream = %20.15f%n", backwardStream(N));
    System.out.printf("parallelStream = %20.15f%n", parallelStream(N));
    System.out.printf("limitStream    = %20.15f%n", limitStream(N));
    System.out.printf("kahanLoop      = %20.15f%n", kahanLoop(N));
    System.out.printf("forwardFor     = %20.15f%n", forwardFor(N));
    System.out.printf("backwardFor    = %20.15f%n", backwardFor(N));
  }

  public static void timeAll(final int N) {
    //    Mark7("bigDecimalSum  =  %s%n", bigDecimalSum(N));
    Mark7("forwardStream",  i -> forwardStream(N));
    Mark7("backwardStream", i -> backwardStream(N));
    Mark7("parallelStream", i -> parallelStream(N));
    Mark7("limitStream",    i -> limitStream(N));
    Mark7("kahanLoop",      i -> kahanLoop(N));
    Mark7("forwardFor",     i -> forwardFor(N));
    Mark7("backwardFor",    i -> backwardFor(N));
  }

  public static double forwardFor(int N) {
    double sum = 0.0;
    for (int i=1; i<N; i++)
      sum += 1.0/i;
    return sum;
  }

  public static double backwardFor(int N) {
    double sum = 0.0;
    for (int i=1; i<N; i++)
      sum += 1.0/(N-i);
    return sum;
  }

  public static double forwardStream(int N) {
    return IntStream.range(1, N).mapToDouble(i -> 1.0/i).sum();
  }
 
  public static double backwardStream(int N) {
    return IntStream.range(1, N).mapToDouble(i -> 1.0/(N-i)).sum();
  }

  public static double parallelStream(int N) {
    return IntStream.range(1, N).parallel().mapToDouble(i -> 1.0/i).sum();
  }

  public static double limitStream(int N) {
    final int[] next = { 1 };
    return DoubleStream.generate(() -> 1.0/next[0]++).limit(N-1).sum();
  }

  public static double kahanLoop(int N) {
    // Precise result (to within 2 ulp) using Kahan sum:
    double sum = 0.0, C = 0.0;
    for (int i=1; i<N; i++) {
      double Y = 1.0/i - C, T = sum + Y;
      C = (T - sum) - Y;
      sum = T;
    }
    return sum;
  }

  public static BigDecimal bigDecimalSum(int N) {
    final MathContext mc = new MathContext(40);
    BigDecimal sum = BigDecimal.ZERO;
    for (int i=1; i<N; i++)
      sum = sum.add(BigDecimal.ONE.divide(new BigDecimal(N-i), mc));
    return sum;
  }

  // ========== Infrastructure code ==========

  public static void SystemInfo() {
    System.out.printf("# OS:   %s; %s; %s%n", 
                      System.getProperty("os.name"), 
                      System.getProperty("os.version"), 
                      System.getProperty("os.arch"));
    System.out.printf("# JVM:  %s; %s%n", 
                      System.getProperty("java.vendor"), 
                      System.getProperty("java.version"));
    // The processor identifier works only on MS Windows:
    System.out.printf("# CPU:  %s; %d \"cores\"%n", 
                      System.getenv("PROCESSOR_IDENTIFIER"),
                      Runtime.getRuntime().availableProcessors());
    java.util.Date now = new java.util.Date();
    System.out.printf("# Date: %s%n", 
      new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ").format(now));
  }

  // NB: result in milliseconds

  public static double Mark7(String msg, IntToDoubleFunction f) {
    int n = 10, count = 1, totalCount = 0;
    double dummy = 0.0, runningTime = 0.0, st = 0.0, sst = 0.0;
    do { 
      count *= 2;
      st = sst = 0.0;
      for (int j=0; j<n; j++) {
        Timer t = new Timer();
        for (int i=0; i<count; i++) 
          dummy += f.applyAsDouble(i);
        runningTime = t.check();
        double time = runningTime * 1e3 / count;
        st += time; 
        sst += time * time;
        totalCount += count;
      }
    } while (runningTime < 0.25 && count < Integer.MAX_VALUE/2);
    double mean = st/n, sdev = Math.sqrt((sst - mean*mean*n)/(n-1));
    System.out.printf("%-25s %15.1f ms %10.2f %10d%n", msg, mean, sdev, count);
    return dummy / totalCount;
  }
}
