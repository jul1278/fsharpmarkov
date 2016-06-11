﻿// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System
open System.IO
open System.Text.RegularExpressions

let lines = 
    File.ReadLines("C:\\Projects\\fsharpmarkov\\text.txt") 
        |> Seq.toList 
        //|> Seq.map(fun w -> Regex.Replace(w, "[,.?()]", ""))  
        //|> Seq.map(fun w -> w.ToLower())  
        |> Seq.fold(fun acc a -> String.Format("{0}{1}", acc, a)) "" 
     
let words = lines.Split([|' '|]) |> Seq.toList

let numWordIsNext word nextWord wordLst = 
    wordLst 
    |> Seq.windowed 2
    |> Seq.filter(fun w -> (Seq.head w) = word && (Seq.last w) = nextWord) 
    |> Seq.length

let wordsFollowingWord word wordLst = 
    wordLst
    |> Seq.windowed 2
    |> Seq.filter(fun w -> (Seq.head w) = word)
    |> Seq.map(fun w -> (Seq.last w))
    |> Seq.toList

let uniqueWords wordLst = 
    wordLst 
    |> Seq.distinct 
    |> Seq.toList

let occurencesOfEachWordAfterWord currWord wordLst = 
    [for word in uniqueWords wordLst do
        yield (word, numWordIsNext currWord word wordLst) ]  
    |> Seq.filter(fun a -> match a with | (_, 0) -> false | _ -> true)  
    |> Seq.toList

let normalized (numOccurences : List<String * int>) = 
     let largest= numOccurences |> Seq.fold(fun acc a -> acc + (snd a)) 0 |> Convert.ToDouble
     [for pair in numOccurences do 
        let word = fst pair
        let norm = (Convert.ToDouble(snd pair)) / largest
        yield (word, norm)]
        
let markovChainNorm wordLst = 
    [for word in uniqueWords wordLst do
        let numOccurences = occurencesOfEachWordAfterWord word wordLst
        let normNumOccurences = normalized numOccurences 
        yield (word, normNumOccurences)]

let markovChain wordLst = 
    [for word in uniqueWords wordLst do
        let wordsFollowing = wordsFollowingWord word wordLst
        yield (word, wordsFollowing)]
  
let pickNextWord words =
    let rand = Random()
    let r = rand.NextDouble()*Convert.ToDouble(List.length words)
    let index = Convert.ToInt32(Math.Floor(r)) 
    match List.length words with
    | 0 -> ""
    | _ -> List.nth words index 
    
                
let rec printWords (chain : List<string * List<string>>) word counter = 
    if counter = 0 then " "
    else
        let words = snd (List.find(fun w -> if (fst w) = word then true else false) chain)
        let nextWord = pickNextWord words
        word + " " + printWords chain nextWord (counter - 1)

[<EntryPoint>]
let main argv = 
    let chain = markovChain words
    let seed = List.head( chain |> Seq.map(fun w -> fst w) |> Seq.toList)
    let text = printWords chain seed 100
    Console.Write(text)
    let a = Console.ReadLine()

    0



