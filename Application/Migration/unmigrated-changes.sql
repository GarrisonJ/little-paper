-- This file is created by the IHP Schema Designer.
-- When you generate a new migration, all changes in this file will be copied into your migration.
--
-- Use http://localhost:8001/NewMigration or `new-migration` to generate a new migration.
--
-- Learn more about migrations: https://ihp.digitallyinduced.com/Guide/database-migrations.html
ALTER TABLE posts ADD COLUMN big_post BOOLEAN DEFAULT false NOT NULL;
ALTER TABLE posts ADD COLUMN big_post_document JSONB NOT NULL;
ALTER TABLE posts ADD COLUMN big_post_title TEXT DEFAULT NULL;
