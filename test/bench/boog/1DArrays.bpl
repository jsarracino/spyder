/* encode enums as constants */

/* encode structs as named global variables. */
var model$length : int;
var model$cells : [int]int;
var view$length : int;
var view$cells : [int]int;


function dims (ml : int, vl : int) returns (bool) {
  ml == vl
}

function cells (mc : [int]int, vc : [int]int, len: int) returns (bool) {
  ( forall i: int :: (0 <= i && i < len) ==> (
    mc[i] == vc[i]
    )
  )
}

function sync(ml : int, vl : int, mc : [int]int, vc: [int]int) returns (bool) {
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
    k := 0;
    while (k < len)
      invariant cells(model$cells, view$cells, k);
      invariant k <= len;
    {
      assert(cells(model$cells, view$cells, k-1));
      model$cells[k] := k;
      view$cells[k] := k;
      assert(cells(model$cells, view$cells, k));
      k := k + 1;
    }
  }

procedure Main()
  modifies model$length, view$length, model$cells, view$cells;
{
  call initMV(2);
}
