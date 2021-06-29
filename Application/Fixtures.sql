

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

INSERT INTO public.users (id, email, password_hash, locked_at, failed_login_attempts, timezone) VALUES ('1e5a2488-f4b4-4b63-9c30-c45a2033711c', 'garrison.jensen@gmail.com', 'sha256|17|onZZt4S8xnwq8rYgicelKw==|EYzHuGi8Lb4fqDSMoBfKjSr4HOpBM89eConI7Fb0BQI=', NULL, 0, '');


ALTER TABLE public.users ENABLE TRIGGER ALL;


ALTER TABLE public.posts DISABLE TRIGGER ALL;



ALTER TABLE public.posts ENABLE TRIGGER ALL;


