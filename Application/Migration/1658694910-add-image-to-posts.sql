-- Write your SQL migration code in here
-- This file is created by the IHP Schema Designer.
-- When you generate a new migration, all changes in this file will be copied into your migration.
--
-- Use http://localhost:8001/NewMigration or `new-migration` to generate a new migration.
--
-- Learn more about migrations: https://ihp.digitallyinduced.com/Guide/database-migrations.html
ALTER TABLE posts ADD COLUMN post_image_url TEXT DEFAULT NULL;
