// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System
open System.IO

let punctuation = [",";"\"";"'";";";"."]

let lines = 
    File.ReadLines("C:\\book.txt") 
        |> Seq.toList
        |> Seq.fold(fun acc a -> String.Format("{0}{1}", acc, a)) "" 

let words = 
    lines.Split([|' '|]) 
    |> Seq.filter(fun c -> (List.exists ((=) c) punctuation) = false) // remove punctuation
    |> Seq.toList

let wordFreq wordLst = 
    wordLst |> Seq.groupBy(fun x -> x)
            |> Seq.map(fun (word, sq) -> word, Seq.length sq)
            |> Seq.toList

let numWordIsNext word nextWord wordLst = 
    wordLst 
    |> Seq.windowed 2
    |> Seq.filter(fun w -> (Seq.head w) = word && (Seq.last w) = nextWord) 
    |> Seq.length

let numOccurences word wordLst = 
    wordLst
    |> Seq.filter(fun w -> w = word)
    |> Seq.length

let prob word nextWord wordLst = ((numWordIsNext word nextWord wordLst) |> float) / ((numOccurences word wordLst) |> float)

let allPairs (uniqueWords : List<String>) = 
    [for word1 in uniqueWords do 
        for word2 in uniqueWords do yield (word1, word2)]

let uniqueWords wordLst = wordLst |> Seq.distinct |> Seq.toList

let allPairsProb wordLst =
    let uniqueWords = uniqueWords wordLst
    [for word1 in uniqueWords do 
        for word2 in uniqueWords do 
            let p = prob word1 word2 wordLst
            yield (word1, word2, p)] |> Seq.filter(fun t -> match t with | (_, _, 0.0) -> false | _ -> true)

[<EntryPoint>]
let main argv = 

    let result1 = numWordIsNext "cat" "dog" ["cat";"dog";"cat";"cow"]
    let result2 = numWordIsNext "cat" "dog" ["cat";"dog";"cat";"cow";"cat";"dog";]

    let c = numOccurences "cat" ["cat";"dog";"cat";"cow";"cat";"dog";]
    let prob1 = prob "cat" "dog" ["cat";"dog";"cat";"cow"]

    let pairs = allPairsProb (uniqueWords words)

    for p in pairs do
        printfn "%s %s %f" <||| p 

//    for pair in pairs do  
//        let p = prob (fst pair) (snd pair) words
//
//        if p > 0.0 then
//            printfn "%s %s %f" <||| (fst pair, snd pair, p)  
    0



