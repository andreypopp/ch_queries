#!/bin/bash
rg '(def|make_window) "([^"]+)"' -o -I --replace '$2' | sort -u |
clickhouse local --input-format "TSV" --multiquery --query "
with existing as (select c1 from table)
select name,description,syntax,arguments,parameters,returned_value
from system.functions
where
  not is_aggregate and origin = 'System'
  and alias_to = ''
  and not startsWith(name, '__')
  and description != ''
  and name not in existing
  and name not in ['divide']
order by categories, name limit 1
FORMAT Vertical
"
