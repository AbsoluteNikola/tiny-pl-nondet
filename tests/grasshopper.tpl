// за какое кол-во шагов кузнечик дойдет от точки 1 до точки N,
// с учетом того что может перемещаться на 1, 2 и 3 клетки

(
    N := 10; // длина поля;
    ix := 1; // где стоит кузнечик
    count := 1; // кол-во
    (
        (
            ( (ix := (ix + 1); count := (count + 1))
            U (ix := (ix + 2); count := (count + 1))
            )
        U   (ix := (ix + 3); count := (count + 1))
        );
        (ix <= N)?
    )*;
    (ix = N)?
)
