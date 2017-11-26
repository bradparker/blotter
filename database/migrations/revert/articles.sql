-- Revert blotter:articles from pg

BEGIN;

DROP TABLE IF EXISTS articles;

COMMIT;
