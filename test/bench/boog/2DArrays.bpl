/* encode enums as constants */

/* encode structs as named global variables. */
var model$length : int;
var model$width : int;
var model$cells : [int][int]int;
var view$length : int;
var view$width : int;
var view$cells : [int][int]int;


function dims (l : int, r : int) returns (bool) {
  l == r
}

function rows (model :[int][int]int, view: [int][int]int, len: int, width: int) returns (bool) {
  ( forall i: int :: (0 <= i && i < len) ==> (
      cells(model[i], view[i], width)
    )
  )
}

function cells (mc : [int]int, vc : [int]int, len: int) returns (bool) {
  ( forall i: int :: (0 <= i && i < len) ==> (
    mc[i] == vc[i]
    )
  )
}

function sync(ml : int, vl : int, mw : int, vw : int, mc : [int][int]int, vc: [int][int]int) returns (bool) {
  dims(ml, vl) && dims(mw, vw) && rows(mc, vc, ml, mw)
}

procedure initMV (len : int, width : int)
  requires 0 <= len;
  requires 0 <= width;
  modifies model$length, view$length, model$width, view$width, model$cells, view$cells;
  ensures dims(model$length, view$length);
  ensures dims(model$width, view$width);
  ensures sync(model$length, view$length, model$width, view$width, model$cells, view$cells);
  {
    var i : int;
    var j : int;
    model$length := len;
    view$length := len;
    model$width := width;
    view$width := width;
    i := 0;
    j := 0;
    while (i < len)
      invariant rows(model$cells, view$cells, i, width);
      invariant i <= len;
    {
      j := 0;
      while (j < width)
        invariant rows(model$cells, view$cells, i, width);
        invariant cells(model$cells[i], view$cells[i], j);
        invariant j <= width;
      {
        model$cells[i][j] := 1;
        view$cells[i][j] := 1;
        assert(cells(model$cells[i], view$cells[i], j));
        j := j + 1;
      }

      i := i + 1;
    }
  }

procedure Main()
  modifies model$length, view$length, model$width, view$width, model$cells, view$cells;
{
  call initMV(2, 3);
}
