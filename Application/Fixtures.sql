

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



ALTER TABLE public.users ENABLE TRIGGER ALL;


ALTER TABLE public.posts DISABLE TRIGGER ALL;



ALTER TABLE public.posts ENABLE TRIGGER ALL;


ALTER TABLE public.comments DISABLE TRIGGER ALL;



ALTER TABLE public.comments ENABLE TRIGGER ALL;


ALTER TABLE public.likes DISABLE TRIGGER ALL;



ALTER TABLE public.likes ENABLE TRIGGER ALL;


ALTER TABLE public.login_state DISABLE TRIGGER ALL;



ALTER TABLE public.login_state ENABLE TRIGGER ALL;


ALTER TABLE public.notifications DISABLE TRIGGER ALL;



ALTER TABLE public.notifications ENABLE TRIGGER ALL;


ALTER TABLE public.password_resets DISABLE TRIGGER ALL;



ALTER TABLE public.password_resets ENABLE TRIGGER ALL;


ALTER TABLE public.schema_migrations DISABLE TRIGGER ALL;

INSERT INTO public.schema_migrations (revision) VALUES (1659839098);
INSERT INTO public.schema_migrations (revision) VALUES (1659843777);
INSERT INTO public.schema_migrations (revision) VALUES (1668894880);
INSERT INTO public.schema_migrations (revision) VALUES (1668895038);
INSERT INTO public.schema_migrations (revision) VALUES (1669408758);
INSERT INTO public.schema_migrations (revision) VALUES (1669409262);


ALTER TABLE public.schema_migrations ENABLE TRIGGER ALL;


ALTER TABLE public.user_follows DISABLE TRIGGER ALL;



ALTER TABLE public.user_follows ENABLE TRIGGER ALL;


