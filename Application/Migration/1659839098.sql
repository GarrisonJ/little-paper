ALTER TABLE posts ADD COLUMN blurhash_image_placeholder TEXT DEFAULT null;
ALTER TABLE likes DROP CONSTRAINT likes_user_id_post_id_key;
ALTER TABLE posts DROP CONSTRAINT posts_user_id_created_on_day_key;
ALTER TABLE user_follows DROP CONSTRAINT user_follows_follower_id_followed_id_key;
CREATE FUNCTION ihp_user_id() RETURNS UUID AS $$
    SELECT NULLIF(current_setting('rls.ihp_user_id'), '')::uuid;
$$ language SQL;
