module Core

#light
  
open System
open Types

    let Compile (statement :string) : Lexem list =
      let rec lexem s =
        match s with   
        | '+' :: tail -> Operator Add :: lexem tail | '-' :: tail -> Operator Sub :: lexem tail
        | '*' :: tail -> Operator Mul :: lexem tail | '/' :: tail -> Operator Div :: lexem tail
        | '(' :: tail -> OpenPs :: lexem tail       | ')' :: tail -> ClosePs :: lexem tail
        | ' ' :: tail -> lexem tail //Space char
        | n :: tail when Char.IsDigit(n) ->
          let rec ConvertList (statement:double list) e c =
            match statement with
            | [] -> e
            | n :: tail -> ConvertList tail (e + (n * (10.0 ** double c))) (c + 1)
          let rec tc (statement:double list) (n:char) (s:char list) =
            let value = Double.Parse(string n)
            match s with
            | n :: tail when Char.IsDigit(n) -> tc (value::statement) n tail
            | x :: tail -> (Const (ConvertList statement value 1)) :: lexem (x :: tail)
            | [] -> [Const (ConvertList statement value 1)]
          tc [] n tail
    
        | [] -> []
        | n :: _ -> failwith ("Parse error - invalid character detected: " + n.ToString())
      let statement=String.Format("({0})",statement) //For cases -1+4, -1-10 etc.
      lexem (Seq.toList statement)
    
     let Run input =
      let PerformPop (stack:'a list) = (stack.Head, stack.Tail.Head, stack.Tail.Tail)
    
      let PerformCalc x y op =
        match op with
        | Operator Add -> y + x
        | Operator Sub -> y - x
        | Operator Mul -> y * x
        | Operator Div -> y / x
        | _ -> failwith "error parsing input"
    
      let rec ProcessRecords input operands (operators:Lexem list) =
        match input with
        | Const n :: tail -> ProcessRecords tail (n::operands) operators
    
        | Operator op :: tail ->
          if operators.Length <> 0 && operators.Head > (Operator op) then
            let x, y, operandsRem = PerformPop operands
            let e = PerformCalc x y operators.Head
            ProcessRecords tail (e::operandsRem) (Operator op::operators.Tail)
          else
            ProcessRecords tail operands (Operator op::operators)
    
        | OpenPs :: tail -> ProcessRecords tail operands (OpenPs::operators)
    
        | ClosePs :: tail ->
          match operators with
          | Operator op :: opsTail ->
            let x, y, operandsRem = PerformPop operands
            let e = PerformCalc x y (Operator op)
            ProcessRecords input (e::operandsRem) opsTail
          | OpenPs :: _ ->
            ProcessRecords tail operands operators.Tail
          | _ -> failwith "error parsing input"
    
        | [] ->
          match operators with
          | Operator op :: tail ->
            let x, y, operandsRem = PerformPop operands
            let e = PerformCalc x y (Operator op)
            ProcessRecords [] (e::operandsRem) tail
          | [] -> operands.Head
          | _ -> failwith "error parsing input"
    
      ProcessRecords input [] []

