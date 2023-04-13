// Experiment, prime factors in parallel, like parallel2017.fs
// sestoft@itu.dk * 2016-03-16

using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using System.Diagnostics;	// Stopwatch

class MyTest {
  private static int[] histogram = new int[200000];

  public static void Main(String[] args) {
    SystemInfo();
    {
      Timer t = new Timer();
      List<int>[] factors200000 = new List<int>[200000];
      for (int n=0; n<200000; n++) 
	factors200000[n] = Factors(n);
      PrintHistogram();
      Console.WriteLine("{0:F3} sec", t.Check());
    }
    histogram = new int[200000];
    {
      Timer t = new Timer();
      List<int>[] factors200000 = new List<int>[200000];
      Parallel.For(0, 200000, 
		   n => { factors200000[n] = Factors(n); });
      PrintHistogram();
      Console.WriteLine("{0:F3} sec", t.Check());
    }
  }

  static void PrintHistogram() {
    for (int i=0; i<40; i++)
      Console.Write("{0}; ", histogram[i]);
    Console.WriteLine();
  }

  static List<int> Factors(int n) {
    List<int> factors = new List<int>();
    int d = 2;
    while (n > 1) {
      if (n % d == 0) {
	factors.Add(d);
	  lock (histogram)  //Lock necessary for atomic update
	  histogram[d]++;
	n /= d;
      } else
	d++;      
    }
    return factors;
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

public class Timer {
  private Stopwatch stopwatch;

  public Timer() {
    stopwatch = new Stopwatch();
    stopwatch.Reset();
    stopwatch.Start();
  }
  
  public double Check() {
    return stopwatch.ElapsedMilliseconds / 1000.0;
  }
}
