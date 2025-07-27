module Database = struct
  open Queries

  module Public = struct
    let users =
      let scope ~alias =
        object
          method id : (non_null, int number) expr = unsafe_expr (alias ^ ".id")
          method x : (non_null, string) expr = unsafe_expr (alias ^ ".x")

          method is_active : (non_null, bool) expr =
            unsafe_expr (alias ^ ".is_active")
        end
      in
      from_table ~db:"public" ~table:"users" scope

    let profiles =
      let scope ~alias =
        object
          method user_id : (non_null, int number) expr =
            unsafe_expr (alias ^ ".user_id")

          method name : (non_null, string) expr = unsafe_expr (alias ^ ".name")
        end
      in
      from_table ~db:"public" ~table:"profiles" scope
  end
end

[@@@ocaml.warning "-27"]

let users ~condition =
  [%query "SELECT u.x AS x, u.id AS id FROM public.users as u WHERE condition"]

let x =
  let users =
    users ~condition:(fun u -> [%expr "u.is_active OR true"])
    |> Queries.from_select
  in
  [%query
    {|SELECT u.x AS name, p.name as pname
      FROM users as u
      LEFT JOIN public.profiles as p
      ON u.id = p.user_id
      WHERE p.name = toNullable('Alice') AND toNullable(false)|}]
