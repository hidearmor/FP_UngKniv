// Experiments with F# asynchronous HTTP
// sestoft@itu.dk 2012-04-23

open System;;      // Uri
open System.Net;;  // WebClient
open System.Xml;;  // XmlDocument, XmlNode

let urls = ["http://www.itu.dk"; "http://www.miele.dk";
            "http://www.amazon.com"; "http://www.dr.dk";
            "http://www.vg.no"; "http://www.tv2.dk"; "http://www.google.dk";
            "http://www.twitter.com"; "http://www.ing.dk"; "http://www.dtu.dk"];;

// string -> int
let lengthSync (url : string) =
    printf ">>>%s>>>\n" url    
    let wc = new WebClient()
    let html = wc.DownloadString(Uri(url))
    printf "<<<%s<<<\n" url
    html.Length;;

lengthSync("http://www.itu.dk");;

[ for url in urls do yield lengthSync url];;

// Naive parallelization:

let lens = 
  let tasks = [ for url in urls do yield async { return lengthSync url } ]
  Async.RunSynchronously(Async.Parallel tasks);;

// string -> Async<int>
let lengthAsync (url : string) = 
    async {
        printf ">>>%s>>>\n" url
        let wc = new WebClient()
        let! html = wc.AsyncDownloadString(Uri(url))
        printf "<<<%s<<<\n" url
        return html.Length
        };;
    
Async.RunSynchronously(lengthAsync("http://www.itu.dk"));;

let lens = 
  let tasks = [ for url in urls do yield lengthAsync url]
  Async.RunSynchronously(Async.Parallel tasks);;

// ----------------------------------------------------------------------
// These timing examples do not demonstrate much gain from async,
// possibly because the web access is performed on individual threads 
// rather than by the underlying nonblocking IO machinery.

// Wall-clock time in seconds to execute f():
let duration (f : unit -> 'a) : float =
    let t1 = DateTime.Now
    let result = f()
    let t2 = DateTime.Now
    t2.Subtract(t1).TotalMilliseconds / 1000.0;;

// string -> float
let timeSync (url : string) = 
    let wc = new WebClient()
    duration(fun () -> wc.DownloadString(Uri(url)))

timeSync("http://www.diku.dk");;

[ for url in urls do yield timeSync url];;

// string -> Async<float>
let timeAsync (url : string) = 
    async { return timeSync(url) };;

Async.RunSynchronously(timeAsync("http://www.diku.dk"));;

Async.RunSynchronously(Async.Parallel [ for url in urls do yield timeAsync url]);;

// ----------------------------------------------------------------------
// NCBI examples in F# (from C# Precisely 2nd ed chapter 23)
// File cs/ex-async-protein.cs

let server = "http://www.ncbi.nlm.nih.gov/entrez/eutils/";;

// NcbiEntrezAsync : string -> Async<string>
let NcbiEntrezAsync(query : string) =
    async {
        let wc = new WebClient()
        let! response = wc.AsyncDownloadString(Uri(server + query))
        return response
        };;

// val NcbiProteinAsync : string -> Async<string>
let NcbiProteinAsync(id : string) =
    NcbiEntrezAsync("efetch.fcgi?rettype=fasta&retmode=text&db=protein&id=" + id)


// NcbiProteinParallelAsync : string list -> Async<string []>
let NcbiProteinParallelAsync(ids : string list) =
    async {
        let tasks = [ for id in ids do yield NcbiProteinAsync(id) ]
        return! Async.Parallel(tasks);
        };;

let prots = ["P01308"; "P01315"; "P01317"];;

Async.RunSynchronously(NcbiProteinParallelAsync(prots));;

