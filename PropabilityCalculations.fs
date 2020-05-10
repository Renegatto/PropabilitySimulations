module PropabilityCalculations
open Infrastructure

let sum_independent (a:float) (b:float):float = a+b- (a*b)
let sum_indep_many (times:int) (prop:float): float = 
    List.map (lambda.k prop) [1..times] 
    |> List.reduce sum_independent

let facs: bigint seq = seq {
    let mutable acc = 1I
    let mutable i = 0I
    while true do 
        i <- (i + 1I)
        acc <- (acc * i)
        yield acc
}

let rec fac n = Seq.take n facs |> Seq.last
let ffac: float -> float = round >> int >> fac >> float 

let C (k:float) (n:float) = 
    lambda.ks (printfn "%A %A" k n) (ffac n / ffac (n - k))
    |> lambda.s lambda.k (printfn "%A")

let foo (n:float) k m =
    let pks = List.map (fun i -> (k-i)/(n-i)) (List.map float [0..(round m |> int)-1])
    let pms = List.map (fun i -> (m-i)/(n-i)) (List.map float [0..(round m |> int)-1])
    List.zip pks pms
    |> lambda.s lambda.k (printfn "so %A")
    |> List.map (fun (k',m') -> k'*m')
    |> List.reduce sum_independent

let calculate n k m s =
    let P_k = k/n
    let P_m = m/k 
    let P_f = P_k * P_m
    let P_s = sum_indep_many s P_f

    (C m n - C (m-1.) (n - k)) / C (k-1.0) k / C k n
    |> printfn "%A or %A and %A" (sum_indep_many (round m |> int) P_f) (foo n k m )

    P_s
