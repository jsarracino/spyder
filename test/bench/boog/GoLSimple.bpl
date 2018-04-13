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


function dims (l : int, r : int) returns (bool) {
  l == r
}

function rows (model :[int][int]bool, view: [int][int]Color, len: int, width: int) returns (bool) {
  ( forall i: int :: (0 <= i && i < len) ==> (
      cells(model[i], view[i], width)
    )
  )
}

function cells (mc : [int]bool, vc : [int]Color, len: int) returns (bool) {
  ( forall i: int :: (0 <= i && i < len) ==> (
      (mc[i] ==> vc[i] == black) && (!mc[i] ==> vc[i] == white)
    )
  )
}

function sync(ml : int, vl : int, mw : int, vw : int, mc : [int][int]bool, vc: [int][int]Color) returns (bool) {
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
        model$cells[i][j] := false;
        view$cells[i][j] := white;
        j := j + 1;
      }

      i := i + 1;
    }
  }

procedure toggleView(i : int, j : int)
  requires sync(model$length, view$length, model$width, view$width, model$cells, view$cells);
  modifies view$cells;
  modifies model$cells; // @inferred
  ensures sync(model$length, view$length, model$width, view$width, model$cells, view$cells);
  {
    if (view$cells[i][j] == white) {
      view$cells[i][j] := black;
      // inferred condition; find program s.t.
      // sync(model$length, view$length, model$width, view$width, model$cells, view$cells);
      model$cells[i][j] := true; // @inferred
    } else {
      view$cells[i][j] := white;
      // find program s.t.
      // sync(model$length, view$length, model$width, view$width, model$cells, view$cells)
      model$cells[i][j] := false; // @inferred
    }
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
  /* ensures (forall i,j: int :: 0 <= i && i < len && 0 <= j && j < wid ==> out[i][j] == in[i][j]); */
{
  var i : int;
  var arOut : [int]bool;
  i := 0;
  while (i < len)
    /* invariant (forall p: int :: 0 <= p && p < i ==> out[p][wid] == in[p][wid]); */
  {
    /* assert(forall p: int :: 0 <= p && p < i ==> out[p][wid] == in[p][wid]); */
    call arOut := copyArr(in[i], wid);
    out[i] := arOut;
    /* assert(forall p: int :: 0 <= p && p < i+1 ==> out[p][wid] == in[p][wid]); */
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
  modifies model$cells;
  modifies view$cells;
  ensures sync(model$length, view$length, model$width, view$width, model$cells, view$cells);
{
  var oldModel : [int][int]bool;
  var i, j : int;
  var neighbors : int;
  call oldModel := copy2DArr(model$cells, model$length, model$width);
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
}

procedure updateModelLooseSynth()
  requires sync(model$length, view$length, model$width, view$width, model$cells, view$cells);
  modifies model$cells;
  modifies view$cells;
  ensures sync(model$length, view$length, model$width, view$width, model$cells, view$cells);
{
  var oldModel : [int][int]bool;
  var i, j : int;
  var neighbors : int;
  call oldModel := copy2DArr(model$cells, model$length, model$width);
  i := 0;
  j := 0;

  while (i < model$length)
    invariant rows(model$cells, view$cells, i, model$width); // @compiled
  {
    j := 0;
    while (j < model$width)
      invariant rows(model$cells, view$cells, i, model$width); // @compiled
      invariant cells(model$cells[i], view$cells[i], j); // @compiled
    {
      call neighbors := liveNeighbors(oldModel, i, j, model$length, model$width);
      if (oldModel[i][j]) { // old cell is live: rules 1-3
        if (neighbors < 2 || neighbors > 3) { // rule 1, 3
          model$cells[i][j] := false;
        } else {
          model$cells[i][j] := true; // rule 2
        }
      } else {
        if (neighbors == 3) {
          model$cells[i][j] := true; // rule 4
        } else {
          model$cells[i][j] := false; // preserve dead cells (implicit rule 0)
        }
      }

      // @inferred from ``the loop doesn't satisfy the property, try repair at the end, with the loop contents + sync condition + maybe knowledge that view$cells[i][j] should be changed''
      // i.e., the postcondition of the inside of cells, with the forall instantiated to i, j.
      if (model$cells[i][j]) {
        view$cells[i][j] := black;
      } else {
        view$cells[i][j] := white;
      }

      j := j + 1;
    }
    i := i + 1;
  }
}

procedure updateModelVeryLooseSynth()
  requires sync(model$length, view$length, model$width, view$width, model$cells, view$cells);
  modifies model$cells;
  modifies view$cells;
  ensures sync(model$length, view$length, model$width, view$width, model$cells, view$cells);
{
  var oldModel : [int][int]bool;
  var i, j : int;
  var ip : int;
  var neighbors : int;
  call oldModel := copy2DArr(model$cells, model$length, model$width);
  i := 0;
  j := 0;

  while (i < model$length)
    invariant rows(model$cells, view$cells, i, model$width); // @compiled
  {
    j := 0;
    while (j < model$width)
      invariant rows(model$cells, view$cells, i, model$width); // @compiled
    {
      call neighbors := liveNeighbors(oldModel, i, j, model$length, model$width);
      if (oldModel[i][j]) { // old cell is live: rules 1-3
        if (neighbors < 2 || neighbors > 3) { // rule 1, 3
          model$cells[i][j] := false;
        } else {
          model$cells[i][j] := true; // rule 2
        }
      } else {
        if (neighbors == 3) {
          model$cells[i][j] := true; // rule 4
        } else {
          model$cells[i][j] := false; // preserve dead cells (implicit rule 0)
        }
      }

      j := j + 1;
    }

    // option 3! the loop doesn't sync, so generate a new VC over a loop, i.e., find a program with the postcondition of
    // cells(model$cells[i], view$cells[i], model$width).

    ip := 0;
    while (ip < view$width)
      invariant rows(model$cells, view$cells, i, model$width); // @compiled
      invariant cells(model$cells[i], view$cells[i], ip); // @compiled
    {
      if (model$cells[i][ip]) {
        view$cells[i][ip] := black;
      } else {
        view$cells[i][ip] := white;
      }
      ip := ip + 1;
    }
    i := i + 1;
  }
}

procedure Main()
  modifies model$length, view$length, model$width, view$width, model$cells, view$cells;
  ensures sync(model$length, view$length, model$width, view$width, model$cells, view$cells);
{
  call initMV(2, 3);
}
