-- https://www.postgresql.jp/docs/9.0/sql-delete.html
DELETE FROM films USING producers
  WHERE producer_id = producers.id AND producers.name = 'foo';
DELETE FROM films
  WHERE producer_id IN (SELECT id FROM producers WHERE name = 'foo');
DELETE FROM films WHERE kind <> 'Musical';
DELETE FROM films;
DELETE FROM tasks WHERE status = 'DONE' RETURNING *;
DELETE FROM tasks WHERE CURRENT OF c_tasks;
DELETE FROM employees
        WHERE position = 'Data Analyst' AND salary < 70000;