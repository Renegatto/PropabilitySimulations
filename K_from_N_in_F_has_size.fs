module K_from_N_in_F_has_size
open Infrastructure
//tools
let T x y = x
let F x y = y
let showme s x =
    printfn "%A%A" s x
    x
    
//randoms
let shuffle (rng: System.Random) (xs: 'a list): list<'a>*System.Random = 
    List.sortBy (fun x -> rng.Next (0, List.length xs)) xs, rng
let sample (count:int) (xs:'a list) (rng: System.Random): (list<'a>*System.Random) IO =
    shuffle rng xs
    |> fun (shuffled, rng') -> List.take (count) shuffled, rng'
    |> IO
    
//logic
let isHypothesisRight (size:int) (k:int Set) (m: int Set): bool =
    Set.intersect k m
    |> Set.count
    |> fun x -> x > size
//usage
let checkHypothesis (n_length: int) (k_length: int) (m_length: int) (s_length: int): bool IO =
    let rng: System.Random = new System.Random ()

    let n: int list = [1..n_length]

    let k': (list<int>*System.Random) IO = sample k_length n rng
    let m': (list<int>*System.Random) IO = IO.bind (sample m_length n << snd) k'

    let k: Set<int> IO = IO.map (fst>>Set) k'
    let m: Set<int> IO = IO.map (fst>>Set) m'
    IO.map2 (isHypothesisRight s_length) k m

let simulate (times:int): float IO =
    List.map (fun _ -> checkHypothesis 20 5 7 3) [1..times]
    |> List.map (IO.map (fun x -> if x then 1 else 0))
    |> List.reduce (IO.map2 (+)) |> showme "successes ="
    |> IO.map (fun successes -> float successes / float times )
