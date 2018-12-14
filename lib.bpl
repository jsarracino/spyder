function prev(vs: [int] int, b: int, i: int) returns (int) {
    if (i != 0) then vs[i-1] else b
}

function prev_var(v: int, b: int, i: int) returns (int) {
    if (i != 0) then v else b
}
