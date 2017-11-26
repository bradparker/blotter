-- Deploy blotter:articles to pg

BEGIN;

CREATE TABLE IF NOT EXISTS articles (
  _id uuid PRIMARY KEY,
  created_at timestamp NOT NULL,
  updated_at timestamp NOT NULL
);

COMMIT;
