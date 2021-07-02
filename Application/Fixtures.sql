

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

INSERT INTO public.users (id, email, password_hash, locked_at, failed_login_attempts, timezone, username) VALUES ('6f4c41c3-8902-44d6-895a-56f57e90c702', 'garrison.jensen@gmail.com', 'sha256|17|2Pqr2li5hy3rGsGT5HKyTw==|E5JgXiQ/0ZpNJF2xQkuO/D+8bt57RZzs6Wsl/7dnCbI=', NULL, 0, '', 'garrison');
INSERT INTO public.users (id, email, password_hash, locked_at, failed_login_attempts, timezone, username) VALUES ('1bd60e0e-78eb-4a3c-a06b-68b6ff7d2dc2', 'garrison.jensen@gmail.com', 'sha256|17|GKjp93xB7YuS/KKbtbnU3A==|gEygWY4bdakg53ydDuOKy1OxJbdNNFjjC6Yt0EATN38=', NULL, 0, '', '');


ALTER TABLE public.users ENABLE TRIGGER ALL;


ALTER TABLE public.posts DISABLE TRIGGER ALL;

INSERT INTO public.posts (id, body, created_on, user_id) VALUES ('8bb85cd5-8574-4c6b-91a6-407c8eb1eb21', 'asdf', '1858-11-17', '1e5a2488-f4b4-4b63-9c30-c45a2033711c');
INSERT INTO public.posts (id, body, created_on, user_id) VALUES ('a034c9ff-8a33-4891-88c5-9e5e35d67c6d', 'asf', '1858-11-17', '1e5a2488-f4b4-4b63-9c30-c45a2033711c');


ALTER TABLE public.posts ENABLE TRIGGER ALL;


