-- Verify blotter:articles on pg

BEGIN;

SELECT * FROM articles LIMIT 1;

ROLLBACK;
