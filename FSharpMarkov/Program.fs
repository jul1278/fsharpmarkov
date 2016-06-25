// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System
open System.IO
open System.Threading
open System.Windows.Forms
open System.Text.RegularExpressions
open FTwitter

let readLines filePath = 
        File.ReadLines(filePath) 
        |> Seq.map(fun a -> a.Split([|' '|]) |> Seq.toArray)
        |> Seq.toList

let defaultTextLines = readLines "C:\\Projects\\fsharpmarkov\\FSharpMarkov\\bin\\Debug\\b_1140_19062016.txt" 

let words (lns : String) = lns.Split([|' '|])

let wordsFollowingWord word (lines : seq<string[]>) = 
    seq {
        for line in lines -> 
            line |> Seq.windowed 2
                |> Seq.filter(fun w -> (Seq.head w) = word)
                |> Seq.map(fun w -> (Seq.last w)) 
        }
    |> Seq.concat

let distinctWords (wordLst : seq<string[]>) = 
    seq {for words in wordLst -> words |> Seq.distinct }
        |> Seq.concat
        |> Seq.filter(fun w -> w.Equals("") = false)
        |> Seq.distinct

let markovChain (lines : seq<string[]>) =
    let words =  distinctWords lines
    seq {for word in words do
            let wordsFollowing = wordsFollowingWord word lines
            yield (word, wordsFollowing)}

let selectRandomSeeded (lst : seq<string>) (seed : int) = 
    match Seq.length lst with
    | 0 -> ""
    | _ -> lst |> Seq.nth (Math.Abs(seed) % (Seq.length lst))

let selectRandom (lst : seq<string>) (rnd : Random) = 
    match Seq.length lst with
    | 0 -> ""
    | _ -> let len = Seq.length lst 
           lst |> Seq.nth (rnd.Next(len))
       
let rec printWords (chain : seq<string * seq<string>>) word counter (seed : int) = 
    if counter = 0 then " "
    else
        let wordOrNone = chain |> Seq.tryFind(fun w -> if (fst w) = word then true else false)
        match wordOrNone with
        | None -> " "
        | Some (_, w) -> word + " " + printWords chain (selectRandomSeeded w seed) (counter - 1) (seed + 1)

let rec generateNewRec (chain : seq<string * seq<string>>) = 
    let rnd = Random()
    let seedNum = rnd.Next()
    let seed = selectRandom ( chain |> Seq.map(fun w -> fst w)) (Random())
    let text = printWords chain seed 100 seedNum
    Console.Clear()
    Console.WriteLine(text)
    Console.ReadLine() |> ignore
    generateNewRec chain

let generateNew (chain : seq<string * seq<string>>) = 
    let rnd = Random()
    let seedNum = rnd.Next()
    let seed = selectRandom ( chain |> Seq.map(fun w -> fst w)) (Random())
    let text = printWords chain seed 25 seedNum
    text

let tweetAt user text = ".@" + user + " " + text

let rec doTweet (twitter : FSharp.Data.Toolbox.Twitter.Twitter) textChain = 
    let newText = generateNew textChain
    let tweetText = tweetAt " " newText
    try
        twitter.Tweets.Post(tweetText.Substring(0, Math.Min(100, tweetText.Length))) |> ignore
    with
    | e -> Console.WriteLine(e.Message)  

    Console.WriteLine(DateTime.Now.ToShortTimeString())
    Console.WriteLine(tweetText) |> ignore
    let r = new Random()
    Thread.Sleep(60*1000*r.Next(5))
    doTweet twitter textChain

[<EntryPoint>]
[<System.STAThread>]
let main argv = 

    let text = readLines "C:\\Projects\\fsharpmarkov\\vote_hillary.txt"
    let textChain = markovChain text

    // store it as a list so we don't query tweets twice
    let tweets = statuses interestingHashTags |> Seq.toList

    let user = tweets |> Seq.filter(fun t -> t.Text.Contains("RT") = false)
                    |> Seq.map(fun t -> t.User.ScreenName) |> twitter.Users.Lookup 
                    |> Seq.filter(fun u -> u.FollowersCount < 1000) 
                    |> Seq.head

    let userTweetId = tweets |> Seq.filter(fun t -> t.User.Id = user.Id) |> Seq.map(fun t -> t.IdStr) |> Seq.head

    let tweetBody = generateNew textChain
    let tweet = tweetAt user.ScreenName tweetBody

    try
        twitter.Tweets.Post(tweet.Substring(0, Math.Min(139, tweet.Length)), in_reply_to_status_id = userTweetId) |> ignore
    with
    | e -> Console.WriteLine(e.Message)
    Console.WriteLine(tweet)
    0

//    if argv.Length > 0 then
//        if File.Exists(argv.[0]) then
//            let readText = readLines argv.[0]
//            let textChain = markovChain readText
//            generateNewRec textChain
//        else
//           Console.WriteLine("Not a valid file.") 
//    else
//        let chain = markovChain defaultTextLines
//        generateNewRec chain
    //0



