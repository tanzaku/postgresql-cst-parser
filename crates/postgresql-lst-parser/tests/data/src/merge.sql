MERGE INTO products p USING stock_movements s ON p.id = s.product_id
    WHEN MATCHED THEN
        UPDATE SET
        p.stock_quantity = p.stock_quantity + s.quantity
    WHEN NOT MATCHED THEN
        INSERT (p.id, p.name, p.price, p.stock_quantity)
        VALUES (s.product_id, s.product_name, s.price, s.quantity);
