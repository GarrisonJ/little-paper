

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
INSERT INTO public.users (id, email, password_hash, locked_at, failed_login_attempts, timezone, username) VALUES ('ac0bd45e-3173-4a94-bf7a-09b5fafe05db', 'afasdfa@asdf.com', 'sha256|17|8mf6ZQywPLdOfoMoSLvMtw==|zP+SkZob7OU3icIsE5A9tMnicisJHT2iAqyWFwR2ON8=', NULL, 0, 'America/Los_Angeles', 'tom');


ALTER TABLE public.users ENABLE TRIGGER ALL;


ALTER TABLE public.posts DISABLE TRIGGER ALL;



ALTER TABLE public.posts ENABLE TRIGGER ALL;


ALTER TABLE public.user_follows DISABLE TRIGGER ALL;

INSERT INTO public.user_follows (id, follower_id, followed_id) VALUES ('3ad95626-e71e-4151-a45d-57496fe0865e', '13bd7e41-d784-4914-9746-2067fd43a88f', 'feec9d78-4e93-4890-aa75-99454f58ed70');
INSERT INTO public.user_follows (id, follower_id, followed_id) VALUES ('c39a7a10-5fd2-4458-ab5d-32b3a9549a5b', '13bd7e41-d784-4914-9746-2067fd43a88f', '13bd7e41-d784-4914-9746-2067fd43a88f');
INSERT INTO public.user_follows (id, follower_id, followed_id) VALUES ('9a4c1786-67e6-4cb5-b85c-d43e7f380c57', 'feec9d78-4e93-4890-aa75-99454f58ed70', 'feec9d78-4e93-4890-aa75-99454f58ed70');
INSERT INTO public.user_follows (id, follower_id, followed_id) VALUES ('5c7c1f48-ba27-46a8-91b3-d43dea07b1d4', 'ac0bd45e-3173-4a94-bf7a-09b5fafe05db', 'ac0bd45e-3173-4a94-bf7a-09b5fafe05db');


ALTER TABLE public.user_follows ENABLE TRIGGER ALL;


