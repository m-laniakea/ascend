type t =
    { row : int
    ; col : int
    }

type dim_2D =
    { rows : int
    ; cols : int
    }

type dir =
    { dr : int
    ; dc : int
    }

let zero =
    { row = 0
    ; col = 0
    }

let add a b =
    { row = a.row + b.row
    ; col = a.col + b.col
    }

let diff a b =
    { row = b.row - a.row
    ; col = b.col - a.col
    }

let dir p =
    (* normalizes delta. WARN: only works for lines on x or + shapes *)
    assert (p.row <> 0 || p.col <> 0);
    let den = max (abs p.row) (abs p.col) in
    { dr = p.row / den
    ; dc = p.col / den
    }

let step p dir =
    { row = p.row + dir.dr
    ; col = p.col + dir.dc
    }

let dirRevCol d = { d with dc = -d.dc }
let dirRevRow d = { d with dr = -d.dr }
let dirOpposite d = dirRevCol d |> dirRevRow

let distance2 b a =
    let dr = b.row - a.row in
    let dc = b.col - a.col in
    dr * dr + dc * dc

let distanceManhattan f t =
    abs (t.row - f.row) + abs (t.col - f.col)

let closestTo p p1 p2 = Int.compare (distance2 p p1) (distance2 p p2)

let areLinedUp a b =
    (* x or + shaped lines only *)
    let pd = diff a b in
    abs(pd.row) = abs(pd.col) || (pd.row = 0) <> (pd.col = 0)

