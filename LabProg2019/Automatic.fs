(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Automatic.fs: risluzione automatica
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Automatic

open Maze

let mutable stck = []
let mutable lt = []

let rec deleteWall l =
    match l with
    | [] -> []
    | (x,y)::xs -> if wallbang.[x,y].wall = true then deleteWall xs else (x,y)::deleteWall xs

let rec visitA lst = 
    match lst with 
    | [] -> []
    | (x,y)::xs -> if wallbang.[x, y].visite = true then visitA xs else (x,y)::visitA xs


let rec supreme x y =
    
    // preparo stack per backtrackimg
    if wallbang.[x,y].visite = false then
               wallbang.[x,y].visite <- true;
               stck <- [(x,y)]@stck

    // Controllo la presenza di muri o celle visitate            
    lt <- intorno x y
    lt <- visitA lt
    lt <- deleteWall lt
    let x1, y1 = pick lt 0

    if (x,y) = (((2*H)-1), ((2*W)-1)) then stck
    else if x1 = 100 then stck <- List.tail stck ; if stck = [] then stck else supreme (first (List.head stck)) (second (List.head stck))
    else supreme x1 y1

    
