module Program

open System
open Core

//Test cases
let test1=Compile "5/3+62"
let test2=Compile "5+2*2"
let test3=Compile "5-(4+10)"
let test4=Compile "((10+5-10)*45)+66"
let test5=Compile "10*66/44"
let test6=Compile "10+(-40+10)"


//Round if it necessary
Console.WriteLine(Math.Round(Run test1,2))
Console.WriteLine(Math.Round(Run test2,2))
Console.WriteLine(Math.Round(Run test3,2))
Console.WriteLine(Math.Round(Run test4,2))
Console.WriteLine(Math.Round(Run test5,2))
Console.WriteLine(Math.Round(Run test6,2))
Console.ReadLine()


