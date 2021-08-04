

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;


SET SESSION AUTHORIZATION DEFAULT;

ALTER TABLE public.admins DISABLE TRIGGER ALL;



ALTER TABLE public.admins ENABLE TRIGGER ALL;


ALTER TABLE public.users DISABLE TRIGGER ALL;

INSERT INTO public.users (id, email, password_hash, locked_at, failed_login_attempts, timezone, username, is_confirmed, picture_url, created_at, confirmation_key, failed_email_confirm_attempts) VALUES ('9f78b5e5-933f-4fd6-b29b-f1644ebb24c6', 'garrison.jensen@gmail.com', 'sha256|17|fZMCzFZusv7QmKkf/P1OpQ==|K30KRxQUOriVpu5PZHB0DpAz5IecCcNoe/Ff7wMbid8=', NULL, 0, 'America/Los_Angeles', 'garrison', true, NULL, '2021-08-02 18:55:18.345694-07', 736133, 0);


ALTER TABLE public.users ENABLE TRIGGER ALL;


ALTER TABLE public.password_resets DISABLE TRIGGER ALL;



ALTER TABLE public.password_resets ENABLE TRIGGER ALL;


ALTER TABLE public.posts DISABLE TRIGGER ALL;



ALTER TABLE public.posts ENABLE TRIGGER ALL;


ALTER TABLE public.user_follows DISABLE TRIGGER ALL;

INSERT INTO public.user_follows (id, follower_id, followed_id) VALUES ('54e1b2b2-e51e-4449-a551-9c9a09685588', '9f78b5e5-933f-4fd6-b29b-f1644ebb24c6', '9f78b5e5-933f-4fd6-b29b-f1644ebb24c6');


ALTER TABLE public.user_follows ENABLE TRIGGER ALL;


