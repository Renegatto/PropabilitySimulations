// Learn more about F# at https://fsharp.org
// See the 'F# Tutorial' project for more help.
open PropabilityCalculations

[<EntryPoint>]
let main argv =
    printfn "%A" <| calculate 20. 5. 7. 3
    printfn "%A" argv
    System.Console.ReadLine () |> ignore
    0 // return an integer exit code
