This test suite exercises `ch_queries_catalog` library which allows to define
parametrized views over database tables. See `./test_catalog.ml` for view
definitions.

basic view rewrite:
  $ ./test_catalog.exe "SELECT backlinks.src_domain FROM views.backlinks(dst_domain='example.com') AS backlinks"
  SELECT backlinks.src_domain
  FROM backlinks.backlinks AS backlinks FINAL
  PREWHERE backlinks.dst_domain = 'example.com'

view rewrite with existing prewhere:
  $ ./test_catalog.exe "SELECT backlinks.src_domain FROM views.backlinks(dst_domain='example.com') AS backlinks PREWHERE backlinks.src_domain = 'test.com'"
  SELECT backlinks.src_domain
  FROM backlinks.backlinks AS backlinks FINAL
  PREWHERE
  backlinks.src_domain = 'test.com' AND backlinks.dst_domain = 'example.com'

view rewrite with existing where:
  $ ./test_catalog.exe "SELECT backlinks.src_domain FROM views.backlinks(dst_domain='example.com') AS backlinks WHERE backlinks.src_domain = 'test.com'"
  SELECT backlinks.src_domain
  FROM backlinks.backlinks AS backlinks FINAL
  PREWHERE backlinks.dst_domain = 'example.com'
  WHERE backlinks.src_domain = 'test.com'

view with no params (with parens):
  $ ./test_catalog.exe "SELECT users.name FROM views.users() AS users"
  SELECT users.name FROM public.users AS users FINAL

view with no params (without parens):
  $ ./test_catalog.exe "SELECT users.name FROM views.users AS users"
  SELECT users.name FROM public.users AS users FINAL

default alias (table name when AS omitted):
  $ ./test_catalog.exe "SELECT backlinks.src_domain FROM views.backlinks(dst_domain='example.com')"
  SELECT backlinks.src_domain
  FROM backlinks.backlinks AS backlinks FINAL
  PREWHERE backlinks.dst_domain = 'example.com'

view that expands into a JOIN:
  $ ./test_catalog.exe "SELECT ub.user_name, ub.src_domain FROM views.user_backlinks(dst_domain='example.com') AS ub"
  SELECT ub_u.name, ub_bl.src_domain
  FROM public.users AS ub_u
  INNER JOIN backlinks.backlinks AS ub_bl
  ON ub_u.id = ub_bl.user_id
  PREWHERE ub_bl.dst_domain = 'example.com'

view that expands into a JOIN, on LHS of another JOIN:
  $ ./test_catalog.exe "SELECT ub.user_name, d.info FROM views.user_backlinks(dst_domain='example.com') AS ub JOIN public.domains AS d ON ub.src_domain = d.domain"
  SELECT ub_u.name, d.info
  FROM public.users AS ub_u
  INNER JOIN backlinks.backlinks AS ub_bl
  ON ub_u.id = ub_bl.user_id
  INNER JOIN public.domains AS d
  ON ub_bl.src_domain = d.domain
  PREWHERE ub_bl.dst_domain = 'example.com'

view with subquery in FROM:
  $ ./test_catalog.exe "SELECT bl.src_domain FROM views.backlink_counts(dst_domain='example.com') AS bl"
  SELECT agg.src_domain
  FROM (
    SELECT backlinks.src_domain AS src_domain, backlinks.dst_domain AS dst_domain
    FROM backlinks.backlinks AS backlinks FINAL
    PREWHERE backlinks.dst_domain = 'example.com') AS agg

view with subquery in FROM, on RHS of a JOIN:
  $ ./test_catalog.exe "SELECT u.name, bl.src_domain FROM public.users AS u JOIN views.backlink_counts(dst_domain='example.com') AS bl ON u.id = bl.src_domain"
  SELECT u.name, bl.src_domain
  FROM public.users AS u
  INNER JOIN
  (
    SELECT agg.src_domain AS src_domain
    FROM (
      SELECT
        backlinks.src_domain AS src_domain,
        backlinks.dst_domain AS dst_domain
      FROM backlinks.backlinks AS backlinks FINAL
      PREWHERE backlinks.dst_domain = 'example.com') AS agg) AS bl
  ON u.id = bl.src_domain

view with ORDER BY, LIMIT, OFFSET:
  $ ./test_catalog.exe "
  > SELECT backlinks.src_domain
  > FROM views.backlinks(dst_domain='example.com') AS b
  > JOIN public.users AS u ON b.user_id = u.id
  > ORDER BY b.src_domain, u.id ASC LIMIT 10 OFFSET 5"
  SELECT backlinks.src_domain
  FROM backlinks.backlinks AS backlinks FINAL
  INNER JOIN public.users AS u
  ON backlinks.user_id = u.id
  PREWHERE backlinks.dst_domain = 'example.com'
  ORDER BY backlinks.src_domain ASC, u.id ASC
  LIMIT 10
  OFFSET 5

view with group_by and having, no outer GROUP BY (inlined):
  $ ./test_catalog.exe "SELECT bs.src_domain, bs.cnt FROM views.backlink_stats(dst_domain='example.com') AS bs"
  SELECT backlinks.src_domain, count(backlinks.user_id)
  FROM backlinks.backlinks AS backlinks FINAL
  PREWHERE backlinks.dst_domain = 'example.com'
  GROUP BY backlinks.src_domain
  HAVING count(backlinks.user_id) > 1

view with group_by and having, outer query also has GROUP BY (wrapped as subquery):
  $ ./test_catalog.exe "
  > SELECT bs.cnt, count(1) AS count
  > FROM views.backlink_stats(dst_domain='example.com') AS bs
  > WHERE bs.cnt > 1
  > GROUP BY bs.cnt
  > HAVING count(1) > 0"
  SELECT bs.cnt, count(1) AS count
  FROM (
    SELECT count(backlinks.user_id) AS cnt
    FROM backlinks.backlinks AS backlinks FINAL
    PREWHERE backlinks.dst_domain = 'example.com'
    GROUP BY backlinks.src_domain
    HAVING count(backlinks.user_id) > 1) AS bs
  WHERE bs.cnt > 1
  GROUP BY bs.cnt
  HAVING count(1) > 0

view with group_by on RHS of a JOIN (always wrapped as subquery):
  $ ./test_catalog.exe "SELECT u.name, bs.cnt FROM public.users AS u JOIN views.backlink_stats(dst_domain='example.com') AS bs ON u.name = bs.src_domain"
  SELECT u.name, bs.cnt
  FROM public.users AS u
  INNER JOIN
  (
    SELECT count(backlinks.user_id) AS cnt, backlinks.src_domain AS src_domain
    FROM backlinks.backlinks AS backlinks FINAL
    PREWHERE backlinks.dst_domain = 'example.com'
    GROUP BY backlinks.src_domain
    HAVING count(backlinks.user_id) > 1) AS bs
  ON u.name = bs.src_domain

view with default param (provided):
  $ ./test_catalog.exe "SELECT bl.src_domain FROM views.backlinks_limited(dst_domain='example.com', max_rows=10) AS bl"
  SELECT backlinks.src_domain
  FROM backlinks.backlinks AS backlinks FINAL
  PREWHERE backlinks.dst_domain = 'example.com'
  WHERE backlinks.user_id < toInt64(10)

view with default param (omitted, uses default):
  $ ./test_catalog.exe "SELECT bl.src_domain FROM views.backlinks_limited(dst_domain='example.com') AS bl"
  SELECT backlinks.src_domain
  FROM backlinks.backlinks AS backlinks FINAL
  PREWHERE backlinks.dst_domain = 'example.com'
  WHERE backlinks.user_id < toInt64(100)

join where only rhs is a view:
  $ ./test_catalog.exe "SELECT u.name, bl.src_domain FROM public.users AS u JOIN views.backlinks(dst_domain='example.com') AS bl ON u.id = bl.user_id"
  SELECT u.name, bl.src_domain
  FROM public.users AS u
  INNER JOIN
  (
    SELECT backlinks.src_domain AS src_domain, backlinks.user_id AS user_id
    FROM backlinks.backlinks AS backlinks FINAL
    PREWHERE backlinks.dst_domain = 'example.com') AS bl
  ON u.id = bl.user_id
