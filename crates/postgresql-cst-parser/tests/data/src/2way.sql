
-- バインド変数のサンプル値欠如
select
	1
,	/*foo*/
;

-- 置換文字列のサンプル値欠如
select
	*
from
	/*#tbl*/
,	/*#tbl2*/
,	/*$tbl3*/
;

-- 先頭に余計なカンマ
select
,	*
from
	TBL
;

-- 先頭に余計なand
select
	*
from
	TBL
where
and 1=1
;
