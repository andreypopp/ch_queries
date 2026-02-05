
  $ ./compile_and_run_simple '
  > let e1 = [%e "not(true)"] |> Ch_queries.expr_to_syntax;;
  > let e2 = [%e "not(true)"] |> Ch_queries.expr_to_syntax;;
  > let () = Printf.printf "%b\n" (Ch_queries_syntax.Syntax.equal_expr e1 e2);;
  > let e1 = e2;; (* shadow e1 so it is collected *)
  > Gc.full_major ();;
  > let e3 = [%e "not(true)"] |> Ch_queries.expr_to_syntax;;
  > let () = Printf.printf "%b\n" (Ch_queries_syntax.Syntax.equal_expr e2 e3);;
  > ' --run-only
  >>> RUNNING
  true
  true

Values are collected when no longer referenced:

  $ ./compile_and_run_simple '
  > let w = Weak.create 1;;
  > let () =
  >   let e = [%e "not(true)"] |> Ch_queries.expr_to_syntax in
  >   Weak.set w 0 (Some e);;
  > Gc.full_major ();;
  > let () = Printf.printf "collected: %b\n" (not (Weak.check w 0));;
  > ' --run-only
  >>> RUNNING
  collected: true
