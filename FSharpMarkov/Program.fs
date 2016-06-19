// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System
open System.IO
open System.Text.RegularExpressions

let readLines filePath = 
        File.ReadLines(filePath) 
        |> Seq.map(fun a -> a.Split([|' '|]) |> Seq.toArray)
        |> Seq.toList
        //|> Seq.map(fun l -> l + "\n")
        //|> Seq.map(fun w -> Regex.Replace(w, "[a-z]\.[a-z]", ". "))
        //|> Seq.map(fun w -> Regex.Replace(w, "[)]", ") "))
        //|> Seq.map(fun w -> Regex.Replace(w, "[\\t\\n\\r]+"," "))
        //|> Seq.map(fun w -> Regex.Replace(w, "[>]", "\n>"))
        //|> Seq.map(fun w -> Regex.Replace(w, "[?]", "\n"))        
        //|> Seq.map(fun w -> Regex.Replace(w, "[,.?()]", ""))  
        //|> Seq.map(fun w -> w.ToLower())  
        //|> Seq.fold(fun acc a -> String.Format("{0}{1}", acc, a)) "" 


let defaultTextLines = readLines "C:\\Projects\\fsharpmarkov\\FSharpMarkov\\bin\\Debug\\b_1140_19062016.txt" 

let words (lns : String) = lns.Split([|' '|])

let wordsFollowingWord word (lines : seq<string[]>) = 
    seq {for line in lines -> 
            line |> Seq.windowed 2
                |> Seq.filter(fun w -> (Seq.head w) = word)
                |> Seq.map(fun w -> (Seq.last w)) }
        |> Seq.concat

    //|> Seq.distinct

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
        | _ -> word + " " + printWords chain (selectRandomSeeded (wordOrNone |> Option.get |> snd) seed) (counter - 1) (seed + 1)

let rec generateNew (chain : seq<string * seq<string>>) = 
    let rnd = Random()
    let seedNum = rnd.Next()
    let seed = selectRandom ( chain |> Seq.map(fun w -> fst w)) (Random())
    let text = printWords chain seed 100 seedNum
    Console.Clear()
    Console.WriteLine(text)
    Console.ReadLine() |> ignore
    generateNew chain

[<EntryPoint>]
let main argv = 
    if argv.Length > 0 then
        if File.Exists(argv.[0]) then
            let readText = readLines argv.[0]
            let textChain = markovChain readText
            generateNew textChain
        else
           Console.WriteLine("Not a valid file.") 
    else
        let chain = markovChain defaultTextLines
        generateNew chain
    0



