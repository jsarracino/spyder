/* encode enums as constants */
type Color; // apparently I need finite? but boogie complains.
const unique black : Color;
const unique white : Color;
axiom (forall c: Color :: c == black || c == white);

/* encode structs as named global variables. */
var model$length : int;
var model$width : int;
var model$cells : [int][int]bool;
var view$length : int;
var view$width : int;
var view$cells : [int][int]Color;



// dimensions are equal
function dims (l : int, r : int) returns (bool) {
  l == r
}

// each row of the cells are equal
function rows (model :[int][int]bool, view: [int][int]Color, len: int, width: int) returns (bool) {
  ( forall i: int :: (0 <= i && i < len) ==> (
      cells(model[i], view[i], width)
    )
  )
}

// each individual cell in a row is equal
function cells (mc : [int]bool, vc : [int]Color, len: int) returns (bool) {
  ( forall i: int :: (0 <= i && i < len) ==> (
      (mc[i] ==> vc[i] == black) && (!mc[i] ==> vc[i] == white)
    )
  )
}

// dimensions are equal, and cells are equal
function sync(ml : int, vl : int, mw : int, vw : int, mc : [int][int]bool, vc: [int][int]Color) returns (bool) {
  dims(ml, vl) && dims(mw, vw) && rows(mc, vc, ml, mw)
}

// encode history as follows:
// history is an array of model cells
// history[index] points to the current cells
// model and view index are identical
// histLength is the length of history
// forward and back are buttons. when clicked, they a) move the history around and b) enable/disable based on the index + length.
var view$index : int;
var model$index : int;
var model$history : [int][int][int]bool;
var model$histLength : int;
var view$forward : bool;
var view$backward : bool;

// indices are equal
function indices(l: int, r: int) returns (bool) {
  l == r
}

// same as above with rows and cells, but for the history/cells arrays
function histEq(l : [int]bool, r : [int]bool, len: int) returns (bool) {
  (forall i: int :: 0 <= i && i < len ==> l[i] == r[i])
}

function rowEq(l : [int][int]bool, r : [int][int]bool, len : int, wid : int) returns (bool) {
  (forall i: int :: 0 <= i && i < len ==> histEq(l[i], r[i], wid) )
}

function history(idx: int, cs: [int][int]bool, hist: [int][int][int]bool, cLen : int, cWid : int, histLen : int) returns (bool) {
  0 <= idx && idx < histLen ==> rowEq(hist[idx], cs, cLen, cWid)
}

// buttons and indices work out
function buttons(back : bool, forward : bool, idx : int, hLen : int) returns (bool) {
  (idx > 0 <==> back) &&
  (idx < hLen-1 <==> forward)
}

function histIdx(idx : int, hLen : int) returns (bool) {
  idx >= 0 && idx < hLen
}

function histCorrect(midx : int, vidx : int, cs: [int][int]bool, hist: [int][int][int]bool, cLen : int, cWid : int, histLen : int, bck: bool, forward : bool) returns (bool) {
  indices(midx, vidx) &&
  history(midx, cs, hist, cLen, cWid, histLen) &&
  buttons(bck, forward, midx, histLen) &&
  histIdx(midx, histLen) &&
  histIdx(vidx, histLen)
}


procedure initMV (len : int, width : int)
  requires 0 <= len;
  requires 0 <= width;
  modifies model$length, view$length, model$width, view$width, model$cells, view$cells;
  modifies model$index, model$histLength, view$index, view$forward, view$backward, model$history;
  ensures dims(model$length, view$length);
  ensures dims(model$width, view$width);
  ensures sync(model$length, view$length, model$width, view$width, model$cells, view$cells);
  ensures histCorrect(model$index, view$index, model$cells, model$history, model$length, model$width, model$histLength, view$backward, view$forward);
  {
    var i : int;
    var j : int;
    var cells : [int][int]bool;
    model$length := len;
    view$length := len;
    model$width := width;
    view$width := width;
    i := 0;
    j := 0;
    view$backward := false;
    view$forward := false;
    model$index := 0;
    view$index := 0;
    while (i < len)
      invariant rows(model$cells, view$cells, i, width);
    {
      j := 0;
      while (j < width)
        invariant rows(model$cells, view$cells, i, width);
        invariant cells(model$cells[i], view$cells[i], j);
      {
        model$cells[i][j] := false;
        view$cells[i][j] := white;
        j := j + 1;
      }

      i := i + 1;
    }

    call cells := copy2DArr(model$cells, model$length, model$width);
    model$history[0] := cells;
    model$histLength := 1;
    assert(history(model$index, model$cells, model$history, model$length, model$width, model$histLength));
    /* assert(buttons(view$back, view$forward)) */
  }

procedure toggleView(i : int, j : int)
  requires sync(model$length, view$length, model$width, view$width, model$cells, view$cells);
  requires histCorrect(model$index, view$index, model$cells, model$history, model$length, model$width, model$histLength, view$backward, view$forward);
  modifies view$cells;
  modifies model$cells; // @inferred
  modifies model$history, model$histLength, view$forward, view$backward, view$index, model$index; // @inferred
  ensures sync(model$length, view$length, model$width, view$width, model$cells, view$cells);
  ensures histCorrect(model$index, view$index, model$cells, model$history, model$length, model$width, model$histLength, view$backward, view$forward);
  {
    var cells : [int][int]bool; // @inferred
    if (view$cells[i][j] == white) {
      view$cells[i][j] := black;
      // inferred condition; find program s.t.
      model$cells[i][j] := true; // @inferred
    } else {
      view$cells[i][j] := white;
      // find program s.t.
      model$cells[i][j] := false; // @inferred
    }

    // push the latest history onto the end, and reset the end
    // this would all be @inferred...
    view$index := view$index + 1;
    model$index := view$index;
    model$histLength := model$index + 1;

    call cells := copy2DArr(model$cells, model$length, model$width);
    model$history[model$index] := cells;

    view$backward := true;
    view$forward := false;

  }

procedure copyArr(in: [int]bool, len: int) returns (out : [int]bool)
  ensures (forall i: int :: 0 <= i && i < len ==> out[i] == in[i]);
{
  var i : int;
  i := 0;
  while (i < len)
    invariant (forall j: int :: 0 <= j && j < i ==> out[j] == in[j]);
  {
    out[i] := in[i];
    i := i + 1;
  }
}

procedure copy2DArr(in: [int][int]bool, len: int, wid: int) returns (out : [int][int]bool)
  ensures (forall i,j: int :: 0 <= i && i < len && 0 <= j && j < wid ==> out[i][j] == in[i][j]);
{
  var i : int;
  var arOut : [int]bool;
  i := 0;
  while (i < len)
    invariant (forall p, q: int :: 0 <= p && p < i &&  0 <= q && q < wid ==> out[p][q] == in[p][q]);
  {
    call arOut := copyArr(in[i], wid);
    out[i] := arOut;
    i := i + 1;
  }
}

procedure countCell(cell : bool) returns (out : int)
  ensures ((cell ==> out == 1) && (!cell ==> out == 0));

  {
    if (cell) {
      out := 1;
    } else {
      out := 0;
    }
  }

// given an array of cells, a position, and bounds, calculate the live neighbors of the cell.
procedure liveNeighbors(cells: [int][int]bool, i : int, j: int, len: int, wid: int ) returns (out : int)

{
  // special cases
  var xLo, xHi, yLo, yHi : int;
  var p, q : int;
  var cout : int;
  xLo := i-1;
  xHi := i+1;
  yLo := j-1;
  yHi := j+1;
  if (i == 0) {xLo := i;}
  if (i == (len - 1)) { xHi := i;}
  if (j == 0) {yLo := j;}
  if (j == (wid - 1)) { yHi := j;}

  p := xLo;
  q := yLo;
  while (p <= xHi) {
    q := yLo;
    while (q <= yHi) {
      call cout := countCell(cells[p][q]);
      out := out + cout;
      q := q + 1;
    }
    p := p + 1;
  }

}

procedure updateModelTightSynth()
  requires sync(model$length, view$length, model$width, view$width, model$cells, view$cells);
  requires histCorrect(model$index, view$index, model$cells, model$history, model$length, model$width, model$histLength, view$backward, view$forward);
  modifies model$cells;
  modifies view$cells;
  modifies model$histLength, model$history, view$index, model$index, view$forward, view$backward;
  ensures sync(model$length, view$length, model$width, view$width, model$cells, view$cells);
  ensures histCorrect(model$index, view$index, model$cells, model$history, model$length, model$width, model$histLength, view$backward, view$forward);
{
  var cells, oldModel : [int][int]bool;
  var i, j : int;
  var neighbors : int;
  call oldModel := copy2DArr(model$cells, model$length, model$width); //already present
  i := 0;
  j := 0;

  while (i < model$length)
    invariant rows(model$cells, view$cells, i, model$width); // @compiled
  {
    j := 0; // boogie caught this for me ;)
    while (j < model$width)
      invariant rows(model$cells, view$cells, i, model$width); // @compiled
      invariant cells(model$cells[i], view$cells[i], j); // @compiled
    {
      call neighbors := liveNeighbors(oldModel, i, j, model$length, model$width);
      if (oldModel[i][j]) { // old cell is live: rules 1-3
        if (neighbors < 2 || neighbors > 3) { // rule 1, 3
          model$cells[i][j] := false;
          view$cells[i][j] := white; // @inferred
        } else {
          model$cells[i][j] := true; // rule 2
          view$cells[i][j] := black; // @inferred
        }
      } else {
        if (neighbors == 3) {
          model$cells[i][j] := true; // rule 4
          view$cells[i][j] := black; // @inferred
        } else {
          model$cells[i][j] := false; // preserve dead cells (implicit rule 0)
          view$cells[i][j] := white; // @inferred
        }
      }

      j := j + 1;
    }
    i := i + 1;
  }

  // push the latest history onto the end, and reset the end
  // this would all be @inferred...
  view$index := view$index + 1;
  model$index := view$index;
  model$histLength := model$index + 1;

  call cells := copy2DArr(model$cells, model$length, model$width);
  model$history[model$index] := cells;

  view$backward := true;
  view$forward := false;
}

procedure moveForward()
  requires sync(model$length, view$length, model$width, view$width, model$cells, view$cells);
  requires histCorrect(model$index, view$index, model$cells, model$history, model$length, model$width, model$histLength, view$backward, view$forward);
  requires view$forward;
  modifies model$cells;
  modifies view$cells;
  modifies view$index, model$index, view$forward, view$backward;
  ensures sync(model$length, view$length, model$width, view$width, model$cells, view$cells);
  ensures histCorrect(model$index, view$index, model$cells, model$history, model$length, model$width, model$histLength, view$backward, view$forward);
{

}


procedure Main()
  modifies model$length, view$length, model$width, view$width, model$cells, view$cells;
  modifies model$index, model$histLength, view$index, view$forward, view$backward, model$history;
  ensures sync(model$length, view$length, model$width, view$width, model$cells, view$cells);
  ensures histCorrect(model$index, view$index, model$cells, model$history, model$length, model$width, model$histLength, view$backward, view$forward);
{
  call initMV(2, 3);
}
