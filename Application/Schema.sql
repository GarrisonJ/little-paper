-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TABLE posts (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    body TEXT NOT NULL,
    created_on_day DATE NOT NULL,
    user_id UUID NOT NULL,
    created_on TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    user_timezone_snapshot TEXT NOT NULL,
    UNIQUE(user_id, created_on_day)
);
CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    email TEXT NOT NULL,
    password_hash TEXT NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    failed_login_attempts INT DEFAULT 0 NOT NULL,
    timezone TEXT NOT NULL,
    username TEXT NOT NULL UNIQUE,
    is_confirmed BOOLEAN DEFAULT false NOT NULL,
    picture_url TEXT DEFAULT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    confirmation_key TEXT,
    failed_email_confirm_attempts INT DEFAULT 0 NOT NULL
);
CREATE UNIQUE INDEX user_username_index ON users (LOWER(username));
CREATE INDEX posts_user_id_index ON posts (user_id);
CREATE TABLE user_follows (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    follower_id UUID NOT NULL,
    followed_id UUID NOT NULL,
    UNIQUE(follower_id, followed_id)
);
CREATE TABLE admins (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    email TEXT NOT NULL,
    password_hash TEXT NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    failed_login_attempts INT DEFAULT 0 NOT NULL
);
CREATE TABLE password_resets (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    reset_token TEXT NOT NULL UNIQUE,
    user_id UUID NOT NULL UNIQUE,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE INDEX password_resets_user_id_index ON password_resets (user_id);
ALTER TABLE password_resets ADD CONSTRAINT password_resets_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE NO ACTION;
ALTER TABLE posts ADD CONSTRAINT posts_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE NO ACTION;
ALTER TABLE user_follows ADD CONSTRAINT user_follows_ref_followed_id FOREIGN KEY (followed_id) REFERENCES users (id) ON DELETE NO ACTION;
ALTER TABLE user_follows ADD CONSTRAINT user_follows_ref_follower_id FOREIGN KEY (follower_id) REFERENCES users (id) ON DELETE NO ACTION;
