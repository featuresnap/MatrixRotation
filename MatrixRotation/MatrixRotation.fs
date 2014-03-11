module MatrixRotation

let values = array2D [[1;2];[3;4]]

let private getColumnAsArray n (array:'a[,])  =
    let colN = n + Array2D.base2 array
    array.[*, colN..colN] 

let getColumn n (array: 'a[,]) = 
    let columnN = array |> getColumnAsArray n
    let maxRow = (array |> Array2D.length1) - 1
    seq {for i in 0..maxRow do yield columnN.[i,0] }
    |> List.ofSeq

let reverseColumn n (array:'a[,]) = array |> getColumn n |> List.rev 
            
let rotateClockwise array = 
    let maxColumn = (Array2D.length2 array) - 1
    [   for col in 0..maxColumn do 
        yield array |> reverseColumn col ]
    |> array2D

let rotateCounterClockwise array = 
    let maxColumn = (Array2D.length2 array) - 1
    [   for col in [maxColumn .. -1 .. 0] do 
        yield array |> getColumn col ]
    |> array2D

