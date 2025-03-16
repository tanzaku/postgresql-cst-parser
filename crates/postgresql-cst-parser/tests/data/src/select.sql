-- https://www.postgresql.jp/docs/9.2/sql-select.html
SELECT DISTINCT ON (location) location, time, report
    FROM weather_reports
    ORDER BY location, time DESC;
SELECT name FROM distributors ORDER BY code;
SELECT * FROM (SELECT * FROM mytable FOR UPDATE) ss WHERE col1 = 5;
SELECT * FROM (SELECT * FROM mytable FOR UPDATE) ss ORDER BY column1;
SELECT f.title, f.did, d.name, f.date_prod, f.kind
    FROM distributors d, films f
    WHERE f.did = d.did;
SELECT kind, sum(len) AS total FROM films GROUP BY kind;
SELECT kind, sum(len) AS total
    FROM films
    GROUP BY kind
    HAVING sum(len) < interval '5 hours';
SELECT * FROM distributors ORDER BY name;
SELECT * FROM distributors ORDER BY 2;
SELECT distributors.name
    FROM distributors
    WHERE distributors.name LIKE 'W%'
UNION
SELECT actors.name
    FROM actors
    WHERE actors.name LIKE 'W%';
CREATE FUNCTION distributors(int) RETURNS SETOF distributors AS $$
    SELECT * FROM distributors WHERE did = $1;
$$ LANGUAGE SQL;
SELECT * FROM distributors(111);
CREATE FUNCTION distributors_2(int) RETURNS SETOF record AS $$
    SELECT * FROM distributors WHERE did = $1;
$$ LANGUAGE SQL;

SELECT * FROM distributors_2(111) AS (f1 int, f2 text);
WITH t AS (
    SELECT random() as x FROM generate_series(1, 3)
  )
SELECT * FROM t
UNION ALL
SELECT * FROM t;
WITH RECURSIVE employee_recursive(distance, employee_name, manager_name) AS (
    SELECT 1, employee_name, manager_name
    FROM employee
    WHERE manager_name = 'Mary'
  UNION ALL
    SELECT er.distance + 1, e.employee_name, e.manager_name
    FROM employee_recursive er, employee e
    WHERE er.employee_name = e.manager_name
  )
SELECT distance, employee_name FROM employee_recursive;
SELECT 2+2;
SELECT distributors.* WHERE distributors.name = 'Westward';
WITH regional_sales AS (
    SELECT region, SUM(amount) AS total_sales
    FROM orders
    GROUP BY region
), top_regions AS (
    SELECT region
    FROM regional_sales
    WHERE total_sales > (SELECT SUM(total_sales)*1.1/10 FROM regional_sales)
)
SELECT region,
        product,
        SUM(quantity) AS product_units,
        SUM(amount) AS product_sales
FROM orders
WHERE region IN (SELECT region FROM top_regions)
GROUP BY region, product;
WITH regional_sales AS (
    SELECT region, SUM(amount) AS total_sales
    FROM orders
    GROUP BY region
), top_regions AS (
    SELECT region
    FROM regional_sales
    WHERE total_sales > (SELECT SUM(total_sales)/10 FROM regional_sales)
)
SELECT region,
        product,
        SUM(quantity) AS product_units,
        SUM(amount) AS product_sales,
        1*2-3/4 as X
FROM orders
WHERE region IN (SELECT region FROM top_regions)
GROUP BY region, product;
WITH regional_sales AS (
    SELECT region, SUM(amount) AS total_sales
    FROM orders
    GROUP BY region
), top_regions AS (
    SELECT region
    FROM regional_sales
    WHERE total_sales > (SELECT SUM(total_sales)/10 FROM regional_sales)
)
-- test
/* test */
SELECT region,
        product,
        SUM(quantity) AS product_units,
        SUM(amount) AS product_sales,
        1*2-3/4 as X
FROM orders
WHERE region IN (SELECT region FROM top_regions)
GROUP BY region, product;
-- a
/* b */
SELECT /*$c*/0 as a, -- d
-- e
b b -- f
,       c -- g
; -- h
select +-1;
select カラム;
select E'\\U0001F600';
SELECT e'\uD83D\uDC31'