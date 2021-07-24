

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

INSERT INTO public.users (id, email, password_hash, locked_at, failed_login_attempts, timezone, username, isconfirmed) VALUES ('ac0bd45e-3173-4a94-bf7a-09b5fafe05db', 'afasdfa@asdf.com', 'sha256|17|8mf6ZQywPLdOfoMoSLvMtw==|zP+SkZob7OU3icIsE5A9tMnicisJHT2iAqyWFwR2ON8=', NULL, 0, 'America/Los_Angeles', 'tom', true);
INSERT INTO public.users (id, email, password_hash, locked_at, failed_login_attempts, timezone, username, isconfirmed) VALUES ('feec9d78-4e93-4890-aa75-99454f58ed70', 'testingtesting@something.com', 'sha256|17|GOXFXt+b+ghkD4422913OA==|xXgmpisk8Yuv8DQETWtQxBto82GlwZkHvG9XZuGa6MI=', NULL, 0, 'America/Los_Angeles', 'testing', true);
INSERT INTO public.users (id, email, password_hash, locked_at, failed_login_attempts, timezone, username, isconfirmed) VALUES ('13bd7e41-d784-4914-9746-2067fd43a88f', 'garrison.jensen@gmail.com', 'sha256|17|rjTB34+dsq7dHyCuFAv+nA==|ICie7eFFh+VyWT//TlSl5aurcBaa7zbD398/LFJmmGI=', NULL, 0, 'America/Los_Angeles', 'garrison', true);


ALTER TABLE public.users ENABLE TRIGGER ALL;


ALTER TABLE public.posts DISABLE TRIGGER ALL;

INSERT INTO public.posts (id, body, created_on_day, user_id, created_on, user_timezone_snapshot) VALUES ('077c59ff-8f75-48be-8b6f-1c4ed2431fe3', 'I''ve written web apps in Ruby, Python, Haskell, and C# sometimes professionally. 

I started a new project thinking that I would pick something I knew, but after a couple of false starts, I''ve decided on Rust. I guess learning the language/framework is half the fun for me.
', '2021-07-15', '13bd7e41-d784-4914-9746-2067fd43a88f', '2021-07-17 17:37:57.064396-07', 'America/Los_Angeles');
INSERT INTO public.posts (id, body, created_on_day, user_id, created_on, user_timezone_snapshot) VALUES ('65af81ed-cc14-43df-88cb-84e2124ceb74', '(If you don''t know, many places still only recognize adhd as a thing kids can have, meaning if you are looking for resources you aren''t looking for yourself, you''re looking for your problematic kid and trying to fix them)', '2021-07-13', '13bd7e41-d784-4914-9746-2067fd43a88f', '2021-07-17 18:31:09.295591-07', 'America/Los_Angeles');
INSERT INTO public.posts (id, body, created_on_day, user_id, created_on, user_timezone_snapshot) VALUES ('ed31c354-9be2-443d-bfd1-2aa1690dabea', 'Date: May 28, 1991. Location: Off Virginia Beach, VA, USA. Event: Battleship Wisconsin fires one of her 16"/50 Mark 7 guns. This might be the last time in history when a battleship fires its main caliber gun. Current status: Museum 36°50′54″N,76°17′43″W. Photo by Bruce M. Morris.', '2021-07-17', '13bd7e41-d784-4914-9746-2067fd43a88f', '2021-07-17 18:32:12.60363-07', 'America/Los_Angeles');
INSERT INTO public.posts (id, body, created_on_day, user_id, created_on, user_timezone_snapshot) VALUES ('a90b76e3-b602-404d-a097-89a9c5bd4758', 'It’s simple really: The Gold Cup is an international tournament featuring nations from North and Central America and also Qatar. Winning the Gold Cup means nothing when Mexico does it, but when the US wins it, the Gold Cup becomes the most coveted trophy in world football.', '2021-07-17', 'feec9d78-4e93-4890-aa75-99454f58ed70', '2021-07-17 18:33:37.810443-07', 'America/Los_Angeles');
INSERT INTO public.posts (id, body, created_on_day, user_id, created_on, user_timezone_snapshot) VALUES ('42213b56-ae65-4a4c-8fe4-5b66c2f66720', 'Well, this is my first post of the day. My only post because that''s what''s allowed. ', '2021-07-18', 'feec9d78-4e93-4890-aa75-99454f58ed70', '2021-07-18 10:07:27.450425-07', 'America/Los_Angeles');
INSERT INTO public.posts (id, body, created_on_day, user_id, created_on, user_timezone_snapshot) VALUES ('46d8ea5c-9598-4f8e-9384-5115cbf7541c', 'Another day another ... nickle. ', '2021-07-19', '13bd7e41-d784-4914-9746-2067fd43a88f', '2021-07-19 08:38:14.80671-07', 'America/Los_Angeles');
INSERT INTO public.posts (id, body, created_on_day, user_id, created_on, user_timezone_snapshot) VALUES ('ae699c55-e977-4d6d-b432-957c48419bc5', 'Just another day.', '2021-07-20', '13bd7e41-d784-4914-9746-2067fd43a88f', '2021-07-20 18:46:41.989474-07', 'America/Los_Angeles');
INSERT INTO public.posts (id, body, created_on_day, user_id, created_on, user_timezone_snapshot) VALUES ('05e91aaa-ebcb-4a9b-9727-b92bba08fd92', 'test', '2021-07-21', '13bd7e41-d784-4914-9746-2067fd43a88f', '2021-07-21 17:21:13.636858-07', 'America/Los_Angeles');
INSERT INTO public.posts (id, body, created_on_day, user_id, created_on, user_timezone_snapshot) VALUES ('71d633f4-8fe9-4142-866c-d0d8aa0ef9fa', 'The lights in the remote observing room have turned off randomly three times now this evening. Either some security person is keeping me on my toes or... [Fill in the blank]', '1991-01-01', '13bd7e41-d784-4914-9746-2067fd43a88f', '2021-07-21 18:06:54.194408-07', 'America/Los_Angeles');
INSERT INTO public.posts (id, body, created_on_day, user_id, created_on, user_timezone_snapshot) VALUES ('ee3e0543-b314-4e05-a746-6fe10a59a60e', 'Are you referring to one of the typed lambda calculi as languages for describing functions and pattern matching.', '1991-01-02', '13bd7e41-d784-4914-9746-2067fd43a88f', '2021-07-21 18:07:06.367898-07', 'America/Los_Angeles');
INSERT INTO public.posts (id, body, created_on_day, user_id, created_on, user_timezone_snapshot) VALUES ('631dfd61-ed76-4957-a003-f980784d74f6', 'Are you referring to one of the typed lambda calculi as languages for describing functions and pattern matching.', '1991-01-03', '13bd7e41-d784-4914-9746-2067fd43a88f', '2021-07-21 18:07:20.419251-07', 'America/Los_Angeles');
INSERT INTO public.posts (id, body, created_on_day, user_id, created_on, user_timezone_snapshot) VALUES ('134ebdd2-8e51-4b90-bb6c-8d4222091fef', 'Another test', '2021-07-22', '13bd7e41-d784-4914-9746-2067fd43a88f', '2021-07-22 21:56:40.181634-07', 'America/Los_Angeles');


ALTER TABLE public.posts ENABLE TRIGGER ALL;


ALTER TABLE public.user_follows DISABLE TRIGGER ALL;

INSERT INTO public.user_follows (id, follower_id, followed_id) VALUES ('c39a7a10-5fd2-4458-ab5d-32b3a9549a5b', '13bd7e41-d784-4914-9746-2067fd43a88f', '13bd7e41-d784-4914-9746-2067fd43a88f');
INSERT INTO public.user_follows (id, follower_id, followed_id) VALUES ('9a4c1786-67e6-4cb5-b85c-d43e7f380c57', 'feec9d78-4e93-4890-aa75-99454f58ed70', 'feec9d78-4e93-4890-aa75-99454f58ed70');
INSERT INTO public.user_follows (id, follower_id, followed_id) VALUES ('5c7c1f48-ba27-46a8-91b3-d43dea07b1d4', 'ac0bd45e-3173-4a94-bf7a-09b5fafe05db', 'ac0bd45e-3173-4a94-bf7a-09b5fafe05db');
INSERT INTO public.user_follows (id, follower_id, followed_id) VALUES ('15cb7224-4ef5-4d33-aefb-5ae13a6bb089', 'feec9d78-4e93-4890-aa75-99454f58ed70', '13bd7e41-d784-4914-9746-2067fd43a88f');
INSERT INTO public.user_follows (id, follower_id, followed_id) VALUES ('9c1f969d-75b9-41a2-bf06-2a3f0450435c', '13bd7e41-d784-4914-9746-2067fd43a88f', 'feec9d78-4e93-4890-aa75-99454f58ed70');


ALTER TABLE public.user_follows ENABLE TRIGGER ALL;


