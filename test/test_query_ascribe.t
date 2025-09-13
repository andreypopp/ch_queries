queries can be ascribed with a type:
  $ ./compile_and_run '
  > let users = {%q|(SELECT users.x AS x FROM public.users)::(x String)|}
  > ' --run-only
  >>> RUNNING

one can ascribe a subqery:
  $ ./compile_and_run '
  > let users = {%q|SELECT u.x FROM (SELECT users.x AS x FROM public.users)::(x String) AS u|};;
  > ' --run-only
  >>> RUNNING

or a CTE
  $ ./compile_and_run '
  > let users = {%q|
  >   WITH u AS (SELECT users.x AS x FROM public.users)::(x String)
  >   SELECT u.x FROM u
  > |};;
  > ' --run-only
  >>> RUNNING

  $ ./compile_and_run '
  > let users = {%q|
  >   WITH u AS MATERIALIZED (SELECT users.x AS x FROM public.users)::(x String)
  >   SELECT u.x FROM u
  > |};;
  > ' --run-only
  >>> RUNNING

or a query parameter:
  $ ./compile_and_run '
  > let users u = {%q|SELECT u.x FROM $u::(x String, y Nullable(Int32))|};;
  > #show users
  > ' --run-only
  >>> RUNNING
  val users :
    (alias:string ->
     < x : (Ch_queries.non_null, string) Ch_queries.expr;
       y : (Ch_queries.null, int Ch_queries.number) Ch_queries.expr >
     Ch_queries.scope Ch_queries.from_one) ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select

  $ ./compile_and_run '
  > let users u = {%q|WITH u AS ($u)::(x String) SELECT u.x FROM u|};;
  > #show users
  > ' --run-only
  >>> RUNNING
  val users :
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select

  $ ./compile_and_run '
  > let users u = {%q|WITH u AS $u::(x String) SELECT u.x FROM u|};;
  > #show users
  > ' --run-only
  >>> RUNNING
  val users :
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select

  $ ./compile_and_run '
  > let users u = {%q|WITH u AS MATERIALIZED $u::(x String) SELECT u.x FROM u|};;
  > #show users
  > ' --run-only
  >>> RUNNING
  val users :
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select

or in IN operator:

  $ ./compile_and_run '
  > let users u = {%q|SELECT u.x FROM public.users AS u WHERE u.x IN $u::(_1 String)|};;
  > #show users
  > ' --run-only
  >>> RUNNING
  val users :
    < _1 : ('a, string) Ch_queries.expr > Ch_queries.scope Ch_queries.select ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select

  $ ./compile_and_run '
  > let users u = {%q|SELECT u.x FROM public.users AS u WHERE u.x IN ($u)::(_1 String)|};;
  > #show users
  > ' --run-only
  >>> RUNNING
  val users :
    < _1 : ('a, string) Ch_queries.expr > Ch_queries.scope Ch_queries.select ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select
