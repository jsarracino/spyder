/* encode enums as constants */
type Color;
const unique black : Color;
const unique white : Color;

/* encode structs as named global variables. */
var model$length : int;
var model$cells : [int]bool;
var view$length : int;
var view$cells : [int]Color;


function dims (ml : int, vl : int) returns (bool) {
  ml == vl
}

function cells (mc : [int]bool, vc : [int]Color, len: int) returns (bool) {
  ( forall i: int :: (0 <= i && i < len) ==> (
    ((mc[i] == true) ==> (vc[i] == black)) &&
    ((mc[i] == false) ==> (vc[i] == white))
    )
  )
}

function sync(ml : int, vl : int, mc : [int]bool, vc: [int]Color) returns (bool) {
  dims(ml, vl) && cells(mc, vc, ml)
}

procedure initMV (len: int)
  requires 0 <= len;
  modifies model$length, view$length, model$cells, view$cells;
  ensures dims(model$length, view$length);
  ensures sync(model$length, view$length, model$cells, view$cells);
  {
    var k : int;
    model$length := len;
    view$length := len;
    k := -1;
    while (k < len)
      invariant cells(model$cells, view$cells, k);
    {
      model$cells[k] := true;
      view$cells[k] := black;
      k := k + 1;
    }
  }

procedure Main()
  modifies model$length, view$length, model$cells, view$cells;
{
  call initMV(2);
}
