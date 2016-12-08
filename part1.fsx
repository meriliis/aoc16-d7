open System
open System.Text.RegularExpressions

let path = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"input.txt")
let input = System.IO.File.ReadAllLines path

let findHypernets (IP : string)=  
    Regex.Matches(IP, "(?<=\[)\w*(?=\])")
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> List.ofSeq

let findSupernets (IP : string) =
    Regex.Matches(IP, "((?<=\])|^)\w+((?=\[)|$)")
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> List.ofSeq

let rec containsABBA (s : string) =
    match s with
    | s when s.Length < 4 -> false
    | s                   -> if s.Chars(0) = s.Chars(3) && s.Chars(1) = s.Chars(2) && s.Chars(0) <> s.Chars(1) then true
                             else containsABBA (s.Substring(1))
                             
let isValid (IP : string) =
    let ABBAinSupernets = IP |> findSupernets |> List.fold (fun acc s -> acc || containsABBA s) false
    let ABBAinHypernets = IP |> findHypernets |> List.fold (fun acc s -> acc || containsABBA s) false

    ABBAinSupernets && not ABBAinHypernets 

input
|> Array.filter (fun IP -> isValid IP)
|> Array.length


     
