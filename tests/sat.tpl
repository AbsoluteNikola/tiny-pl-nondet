// x, y, z boolean, aka 0 or 1.
(
    (x := 0 U x := 1);
    (y := 0 U y := 1);
    (z := 0 U z := 1);
    ((!(x = 1)) && ((!(x = 1)) && (z = 1)))?
)