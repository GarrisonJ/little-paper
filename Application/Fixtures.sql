

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

INSERT INTO public.users (id, email, password_hash, locked_at, failed_login_attempts, timezone, username) VALUES ('13bd7e41-d784-4914-9746-2067fd43a88f', 'garrison.jensen@gmail.com', 'sha256|17|rjTB34+dsq7dHyCuFAv+nA==|ICie7eFFh+VyWT//TlSl5aurcBaa7zbD398/LFJmmGI=', NULL, 0, 'America/Los_Angeles', 'garrison');
INSERT INTO public.users (id, email, password_hash, locked_at, failed_login_attempts, timezone, username) VALUES ('feec9d78-4e93-4890-aa75-99454f58ed70', 'testingtesting@something.com', 'sha256|17|GOXFXt+b+ghkD4422913OA==|xXgmpisk8Yuv8DQETWtQxBto82GlwZkHvG9XZuGa6MI=', NULL, 0, 'America/Los_Angeles', 'testing');


ALTER TABLE public.users ENABLE TRIGGER ALL;


ALTER TABLE public.posts DISABLE TRIGGER ALL;

INSERT INTO public.posts (id, body, created_on, user_id) VALUES ('a6d0bf48-b4c6-45f0-b809-a8157ce02b47', 'asdf', '2021-07-10', '13bd7e41-d784-4914-9746-2067fd43a88f');
INSERT INTO public.posts (id, body, created_on, user_id) VALUES ('c5b8eb17-8851-4b7c-92b6-f8f2ce8c9f0b', 'asfasdfasdf', '2021-07-11', '13bd7e41-d784-4914-9746-2067fd43a88f');
INSERT INTO public.posts (id, body, created_on, user_id) VALUES ('b4e75784-f3ba-4dbf-9706-2718cfdd2eda', 'testing', '2021-07-14', '13bd7e41-d784-4914-9746-2067fd43a88f');
INSERT INTO public.posts (id, body, created_on, user_id) VALUES ('20bef516-da96-4bce-80fd-bd718eb84f17', 'testasdf', '2021-07-14', 'feec9d78-4e93-4890-aa75-99454f58ed70');


ALTER TABLE public.posts ENABLE TRIGGER ALL;


ALTER TABLE public.user_follows DISABLE TRIGGER ALL;



ALTER TABLE public.user_follows ENABLE TRIGGER ALL;


