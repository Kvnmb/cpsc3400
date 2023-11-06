// Kevin Bui
// hw4.fsx
// 5/9/23
// This program uses list and dictionary operations implemented in fsharp

let rec replace list origVal newVal =
    match list with
    // empty list
    | [] -> []
    // extracts first value and remaining list
    | hd :: tl ->
        if hd = origVal then // checks if the value matches the value to be replaced
            newVal::replace tl origVal newVal
        else
            hd::replace tl origVal newVal

// define list type to avoid error when both lists are empty
let rec mergeList (listA:int list) (listB:int list) =
    match listA, listB with
    | [], [] -> []
    | _, [] -> listA
    | [], _ -> listB
    | aHead::aTail, bHead::bTail -> // grab the first value from each list
        aHead::bHead::mergeList aTail bTail     // merges the value from listA, then listB, then calls recursion with remaining lists


let rec search dict key =
    match dict with
    | [] -> None    // none if the list is empty / does not have key
    | (x, y)::tail -> 
        if x = key then
            Some y      // returns value if key is in dictionary
        else
            search tail key     // recursion to search rest of list

let insert dict key value =
    if search dict key = None then      // uses search function to traverse dictionary and then add tuple to list, else does not modify
        (key, value)::dict
    else dict

let rec remove dict key = 
    match dict with
    | [] -> []
    | (x, y)::tail ->
        if x = key then tail    // if key is found, rest of list is added without the tuple
        else (x, y)::remove tail key

let rec count dict func = 
    match dict with
    | [] -> 0
    | (key, value)::tail ->     // if the function returns true, +1 to the count then call recursion to check other nodes
        (if func value then 1 else 0) + count tail func

let twoDigitCount dict =
    count dict (fun value -> value >= 10 && value <= 99)    // single call to cound with a lambda function to check for two digits


