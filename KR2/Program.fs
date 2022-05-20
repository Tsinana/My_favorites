// Learn more about F# at http://fsharp.org

open System

//
// КР0 Вариант 1    
//
let quantity_div_digit digit = 
    let rec q_d_d_rec_up digit i = 
        if digit = i-1 then 0
        elif 0 = digit % i then 
            let i1 = i + 1
            1 + q_d_d_rec_up digit i1
        else
            let i1 = i + 1 
            q_d_d_rec_up digit i1

    let rec q_d_d_rec_down digit i qu =
        if digit = i-1 then qu
        elif 0 = digit % i then
            let i1 = i + 1
            let qu1 = qu + 1 
            q_d_d_rec_down digit i1 qu1
        else
            let i1 = i + 1
            q_d_d_rec_down digit i1 qu

    let up = q_d_d_rec_up digit 1
    let down = q_d_d_rec_down digit 2 1
    if up = down then up
    else 0

let sum_digit digit = 
    let rec s_d_rec digit sum = 
        match digit with
        |0 -> sum
        |_ -> 
            let sum1 = sum + (digit % 10)
            let d1 = digit / 10
            s_d_rec d1 sum1
    s_d_rec digit 0

let func_02 list p f =
    let rec f_02 list p f newList =
        match list with 
        | [] -> newList
        | H::T -> 
            if p H then 
                let a = f H
                let nlist = newList @ [a]
                f_02 T p f nlist
            else f_02 T p f newList
    f_02 list p f []

let func_02_ultra list = 
    let p  =  fun x -> x>0 
    let f = fun x -> (sum_digit x)
    func_02 list p f

let func_03 list =
    let rec create_list list newlist =
        match list with 
        |[] -> newlist
        |(el,q)::T ->
            let a = List.init q (fun x -> el)
            let nl = List.append newlist a
            create_list T nl

    let a = list|>List.countBy id |>List.sortByDescending (fun (_,x) -> x)
    create_list a []
//
//  КР0 Вариант 1    
//
let func_011 list =
    let rec f_001_rec list pr =
        match list with
        |[]->pr
        |H::T->
            if 0 = H % 2 then 
                let pr1 = pr * H
                f_001_rec T pr1
            else f_001_rec T pr

    let rec f_001_rec_up list = 
        match list with
        |[]->1
        |H::T->
            if 0 = H % 2 then 
                H*f_001_rec_up T
            else 1*f_001_rec_up T

    let b = f_001_rec_up list
    let a = f_001_rec list 1
    if a = b then a
    else 0

let func_002 digit pr acc =
    let rec f_002 digit pr acc =
        match digit with
        |0->acc
        |_->
            let last = digit % 10
            let H = digit / 10
            if pr last then 
                let acc1 = acc @ [last]
                f_002 H pr acc1
            else f_002 H pr acc
    f_002 digit pr acc

let pr = fun x -> 0 = x % 10

let func_002_ultra digit = 
    func_002 digit pr []|>func_011
//
// Вариат 6
//
let max_up (list:int list):int =
    let rec max_up_rec (list:int list):int =
        match list with
        |[] -> -1111
        |H::[] -> H
        |H::T -> 
            let maxf = max_up_rec T
            if maxf > H then maxf
            else H
    max_up_rec list

let func_61 digit pr6 f6 =
    let rec f_61 digit div pr6 f6 list =
        if digit = div - 1 then list
        elif 0 = digit % div then 
            if pr6 div then 
                let a = f6 div
                let nlist = list @ [a]
                let div1 = div + 1
                f_61 digit div1 pr6 f6 nlist
            else
                let div1 = div + 1
                f_61 digit div1 pr6 f6 list
        else
            let div1 = div + 1
            f_61 digit div1 pr6 f6 list
    f_61 digit 1 pr6 f6 []

let func_63 list =
    list|>List.countBy id|>List.unzip
//
//Вариант 4
//
let func_43 list =
    let rec f_43 list newlist =
        match list with 
        |[]->newlist
        |H::T->
            let a = quantity_div_digit H
            if a = 2||a = 1 then 
                let nlist = newlist @ [H]
                f_43 T nlist
            else 
                f_43 T newlist
    let a,b = f_43 list []|>List.countBy id|>List.unzip
    a

[<EntryPoint>]
let main argv =
    let a = 8
    let list0 = [-1;22;-11]
    let list = [1;1;1;2;3;3;3;3;6;6]
    let a = func_43 list
    Console.WriteLine(a)

    
    //list|>max_up|>Console.WriteLine
    //a|>quantity_div_digit|>Console.WriteLine
    //list0|>func_02_ultra|>Console.WriteLine
    //list |> func_03|>Console.WriteLine
    printfn "Hello World from F#!"
    0 // return an integer exit code
