// Counting prime factors using parallel Linq and other techniques, a
// la Java 8 streams.

// sestoft@itu.dk * 2015-03-11

using System;
using System.Collections.Generic;
using System.Linq;

public class TestCountPrimesStream {
  public static void Main(String[] args) {
    SystemInfo();
    int range = 200000;
    Timer t = new Timer();
    var factors200000 = countStreamParallelChunk(range);
    // PrintFew(15, factors200000);
    int[] histogram = FactorFrequencies(range, factors200000);
    Console.WriteLine("Time = {0,8:F3} sec", t.Check());
    PrintFew(15, histogram);

    factors200000 = countStreamParallelRange(range);    
    histogram = FactorFrequencies(range, factors200000);
    Console.WriteLine("Time = {0,8:F3} sec", t.Check());
    PrintFew(15, histogram);
  }

  private static void PrintFew(int range, IEnumerable<List<int>> factorLists) {
    foreach (List<int> factors in factorLists) {
      if (range-- <= 0)
	break;
      foreach (int factor in factors)
	Console.Write("{0} ", factor);
      Console.Write("; ");
    }
    Console.WriteLine(); Console.WriteLine();
  }

  private static void PrintFew(int range, int[] histogram) {
    foreach (int i in histogram) {
      if (range-- <= 0)
	break;
      Console.Write("{0} ", i);
    }
    Console.WriteLine(); Console.WriteLine();
  }

  private static bool isPrime(int n) {
    int k = 2;
    while (k * k <= n && n % k != 0)
      k++;
    return n >= 2 && k * k > n;
  }

  static List<int> Factors(int n) {
    List<int> factors = new List<int>();
    int d = 2;
    while (n > 1) {
      if (n % d == 0) {
	factors.Add(d);
	n /= d;
      } else
	d++;      
    }
    return factors;
  }

  // Parallel stream solution with (supposedly efficient) range partitioning
  private static IEnumerable<List<int>> countStreamParallelRange(int range) {
    return ParallelEnumerable.Range(0, range)
      .Select(i => Factors(i));
  }

  // Parallel stream solution with (supposedly inefficient) chunk partitioning
  private static IEnumerable<List<int>> countStreamParallelChunk(int range) {
    return Enumerable.Range(0, range)
      .AsParallel()
      .Select(i => Factors(i));
  }

  private static int[] FactorFrequencies(int range, IEnumerable<List<int>> factorLists) {
    int[] histogram = new int[range];
    foreach (List<int> factors in factorLists)
      foreach (int factor in factors)
        histogram[factor]++;
    return histogram;
  }

  private static void SystemInfo() {
    Console.WriteLine("# OS          {0}", 
      Environment.OSVersion.VersionString);
    Console.WriteLine("# .NET vers.  {0}",   
      Environment.Version);
    Console.WriteLine("# 64-bit OS   {0}",   
      Environment.Is64BitOperatingSystem);
    Console.WriteLine("# 64-bit proc {0}",   
      Environment.Is64BitProcess);
    Console.WriteLine("# CPU         {0}; {1} \"cores\"",   
      Environment.GetEnvironmentVariable("PROCESSOR_IDENTIFIER"),
      Environment.ProcessorCount); 
    Console.WriteLine("# Date        {0:s}", 
      DateTime.Now);
  }
}

// Crude timing utility ----------------------------------------

public class Timer {
  private readonly System.Diagnostics.Stopwatch stopwatch
    = new System.Diagnostics.Stopwatch();
  public Timer() { Play(); }
  public double Check() { return stopwatch.ElapsedMilliseconds / 1000.0; }
  public void Pause() { stopwatch.Stop(); }
  public void Play() { stopwatch.Start(); }
}
