

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

ALTER TABLE public.users DISABLE TRIGGER ALL;

INSERT INTO public.users (id, email, password_hash, locked_at, failed_login_attempts, timezone, username) VALUES ('2e301cc2-d522-4952-a7bb-b7801272c2a8', 'garrison.jensen@gmail.com', 'sha256|17|BhRStZFF/rJBncExhgf/ww==|MuMFJ1G4+2O7Qfy8P3GU6lPwIv8WH4Z+fujHUGmZ/jk=', NULL, 0, 'America/Los_Angeles', 'garrison');


ALTER TABLE public.users ENABLE TRIGGER ALL;


ALTER TABLE public.posts DISABLE TRIGGER ALL;



ALTER TABLE public.posts ENABLE TRIGGER ALL;


