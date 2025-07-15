-- https://www.postgresql.jp/document/8.0/html/sql-update.html
UPDATE films SET kind = 'Dramatic' WHERE kind = 'Drama';
UPDATE weather SET temp_lo = temp_lo+1, temp_hi = temp_lo+15, prcp = DEFAULT
  WHERE city = 'San Francisco' AND date = '2003-07-03';
UPDATE employees SET sales_count = sales_count + 1 FROM accounts
  WHERE accounts.name = 'Acme Corporation'
  AND employees.id = accounts.sales_person;
UPDATE employees SET sales_count = sales_count + 1 WHERE id =
  (SELECT sales_person FROM accounts WHERE name = 'Acme Corporation');
UPDATE employees
  SET salary = salary * 1.05
  WHERE department_id = 3;