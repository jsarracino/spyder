/* encode enums as constants */
type Color;
const unique black : Color;
const unique white : Color;

/* encode structs as named global variables. */
var model$length : int;
var model$width : int;
var model$cells : [int, int]bool;
var view$length : int;
var view$width : int;
var view$cells : [int, int]Color;

/* procedure synchronizationInvariant()  */

function dims (ml : int, vl : int, mw : int, vw : int) returns (bool) {
  ml == vl && mw == vw
}

function cells (mc : [int, int]bool, vc : [int, int]Color, len: int, wid : int) returns (bool) {
  ( forall i: int, j: int :: (0 <= i && i < len && 0 <= j && j < wid) ==> (
    (mc[i, j] ==> (vc[i, j] == black)) &&
    (!mc[i, j] ==> (vc[i, j] == white))
    )
  )
}

function sync(ml : int, vl : int, mw : int, vw: int, mc : [int, int]bool, vc: [int, int]Color) returns (bool) {
  dims(ml, vl, mw, vw) && cells(mc, vc, ml, mw)
}

procedure initMV (len: int, wid: int)
  requires 0 < len;
  requires 0 < wid;
  modifies model$length, view$length, model$width, view$width, model$cells, view$cells;
  ensures dims(model$length, view$length, model$width, view$width);
  ensures sync(model$length, view$length, model$width, view$width, model$cells, view$cells);
  {
    var i, j: int;
    model$length := len;
    view$length := len;
    model$width := wid;
    view$width := wid;
    i := 0;
    j := 0;
    while (i < len)
      invariant (forall k, p: int :: 0 < k && k < j && 0 < p && p < i ==>
        (model$cells[p, k] ==> (view$cells[p, k] == black)) &&
        (!model$cells[p, k] ==> (view$cells[p, k] == white))
        );
        {
      while (j < wid)
      {
        model$cells[i, j] := true;
        view$cells[i, j] := black;
        j := j + 1;
      }
      i := i + 1;
    }
  }

procedure Main()
  modifies model$length, view$length, model$width, view$width, model$cells, view$cells;
{
  call initMV(2,2);
}
/* procedure toggleVCell(i: int)
  requires i < view$length;
  requires sync(model$length, view$length, model$cells, view$cells);
  modifies view$cells;
  modifies model$cells;
  ensures sync(model$length, view$length, model$cells, view$cells);
{
  var oldColor : Color;
  oldColor := view$cells[i];
  if (oldColor == black) {
    view$cells[i] := white;
    model$cells[i] := false;
  } else {
    view$cells[i] := black;
    model$cells[i] := true;
  }
} */



/* function synchMV () returns (bool)
{ ( forall i: int :: 0 <= i && i <  ==> (a, N, a[i]) ) } */

/* var model : [int]bool; */
/* var view : [int]Color; */


/* procedure toggleView() */
