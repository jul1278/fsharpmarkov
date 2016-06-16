// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System
open System.IO
open System.Text.RegularExpressions

let lines = 
    File.ReadLines("C:\\Projects\\fsharpmarkov\\text.txt") 
        |> Seq.map(fun l -> l + "\n")
        |> Seq.toList 
        |> Seq.map(fun w -> Regex.Replace(w, "[a-z]\.[a-z]", ". "))
        |> Seq.map(fun w -> Regex.Replace(w, "[)]", ") "))
        |> Seq.map(fun w -> Regex.Replace(w, "[\\t\\n\\r]+"," "))
        |> Seq.map(fun w -> Regex.Replace(w, "[>]", "\n>"))
        |> Seq.map(fun w -> Regex.Replace(w, "[?]", "\n"))        
        //|> Seq.map(fun w -> Regex.Replace(w, "[,.?()]", ""))  
        //|> Seq.map(fun w -> w.ToLower())  
        |> Seq.fold(fun acc a -> String.Format("{0}{1}", acc, a)) "" 
     
let words = lines.Split([|' '|])

let scentences = seq {for l in lines.Split([|'\n'|]) do yield l.Split([|' '|]) }

let wordsFollowingWord word wordLst = 
    wordLst
    |> Seq.windowed 2
    |> Seq.filter(fun w -> (Seq.head w) = word)
    |> Seq.map(fun w -> (Seq.last w))
    |> Seq.distinct

let uniqueWords wordLst = 
    wordLst 
    |> Seq.distinct 
    |> Seq.filter(fun w -> (String.IsNullOrEmpty w) = false)

let markovChain wordLst = 
    seq {for word in uniqueWords wordLst do
            let wordsFollowing = wordsFollowingWord word wordLst
            yield (word, wordsFollowing)}

let selectRandomSeeded (lst : seq<string>) (seed : int) = 
    match Seq.length lst with
    | 0 -> ""
    | _ -> lst |> Seq.nth (seed % (Seq.length lst))

let selectRandom (lst : List<string>) (rnd : Random) = 
    match List.length lst with
    | 0 -> ""
    | _ -> List.nth lst (rnd.Next(List.length lst))
       
let rec printWords (chain : seq<string * seq<string>>) word counter (seed : int) = 
    if counter = 0 then " "
    else
        let wordOrNone = chain |> Seq.tryFind(fun w -> if (fst w) = word then true else false)
        match wordOrNone with
        | None -> " "
        | _ -> word + " " + printWords chain (selectRandomSeeded (snd (Option.get wordOrNone)) seed) (counter - 1) (seed + 1)

let rec generateNew (chain : seq<string * seq<string>>) = 
    let rnd = Random()
    let seedNum = rnd.Next()
    let seed = selectRandom ( chain |> Seq.map(fun w -> fst w) |> Seq.toList) (Random())
    let text = printWords chain seed 100 seedNum
    Console.WriteLine(text)
    let a = Console.ReadLine()
    generateNew chain

[<EntryPoint>]
let main argv = 
    let chain = markovChain words
    generateNew chain
    0



