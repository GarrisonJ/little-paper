

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

INSERT INTO public.users (id, email, password_hash, locked_at, failed_login_attempts, timezone, username, is_confirmed, picture_url, created_at, confirmation_key, failed_email_confirm_attempts) VALUES ('53e5d539-4c2e-4561-9564-6bf1ff656f2b', 'garrison.jensen@gmail.com', 'sha256|17|tMXBmOZemxAm2AurJvj3Fw==|EAeBs9UDRO4Wm6wW7Sk41sGBBKCEz52iTRPLjx0wJzk=', NULL, 0, 'America/Los_Angeles', 'garrison', true, NULL, '2021-08-03 18:01:33.02564-07', 'sha256|17|NNiBtpxVx06oYuCcTRZpIA==|pT0ALZjFredLyxyko5xm8Da+VgykRSaklxpwlTmhoh0=', 3);


ALTER TABLE public.users ENABLE TRIGGER ALL;


ALTER TABLE public.password_resets DISABLE TRIGGER ALL;



ALTER TABLE public.password_resets ENABLE TRIGGER ALL;


ALTER TABLE public.posts DISABLE TRIGGER ALL;



ALTER TABLE public.posts ENABLE TRIGGER ALL;


ALTER TABLE public.user_follows DISABLE TRIGGER ALL;

INSERT INTO public.user_follows (id, follower_id, followed_id) VALUES ('cf77b80a-5e8d-42b6-96b6-c2f3467d2f50', '53e5d539-4c2e-4561-9564-6bf1ff656f2b', '53e5d539-4c2e-4561-9564-6bf1ff656f2b');


ALTER TABLE public.user_follows ENABLE TRIGGER ALL;


