(
    a := 1;
    b := 2;
    c := 1; // -3 or +1
    D := ((b * b) - ((4 * a) * c));

    // find sqrt of D
    D_sqrt := 0;
    (
        D_sqrt := (D_sqrt + 1);
        (D_sqrt <= D)?
    )*;
    ((D_sqrt * D_sqrt) = D)?;
    (D>=0)?;
    ( (x := (((0 - b) + D_sqrt) / (2 * a)))
    U (x := (((0 - b) - D_sqrt) / (2 * a)))
    )
)
