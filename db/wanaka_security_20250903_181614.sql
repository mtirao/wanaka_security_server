--
-- PostgreSQL database dump
--

-- Dumped from database version 15.3
-- Dumped by pg_dump version 17.2

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET transaction_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: activities; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.activities (
    date integer NOT NULL,
    user_id uuid NOT NULL,
    activity text NOT NULL,
    id uuid NOT NULL
);


ALTER TABLE public.activities OWNER TO postgres;

--
-- Name: messages; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.messages (
    type text NOT NULL,
    date integer NOT NULL,
    content text NOT NULL,
    id uuid NOT NULL
);


ALTER TABLE public.messages OWNER TO postgres;

--
-- Name: profiles; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.profiles (
    last_name text NOT NULL,
    email text NOT NULL,
    first_name text NOT NULL,
    phone text NOT NULL,
    cell_phone text NOT NULL,
    gender text,
    address text,
    city text,
    user_id text
);


ALTER TABLE public.profiles OWNER TO postgres;

--
-- Name: realms; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.realms (
    client_id text NOT NULL,
    client_secret text NOT NULL,
    grant_type text NOT NULL
);


ALTER TABLE public.realms OWNER TO postgres;

--
-- Name: statuses; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.statuses (
    status text NOT NULL,
    date text NOT NULL,
    user_id uuid NOT NULL,
    id uuid NOT NULL
);


ALTER TABLE public.statuses OWNER TO postgres;

--
-- Name: tenants; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.tenants (
    user_name text NOT NULL,
    user_password text NOT NULL,
    user_id text NOT NULL,
    status text,
    created_at bigint
);


ALTER TABLE public.tenants OWNER TO postgres;

--
-- Name: tokens; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.tokens (
    auth_token text NOT NULL,
    client_id text NOT NULL
);


ALTER TABLE public.tokens OWNER TO postgres;

--
-- Data for Name: activities; Type: TABLE DATA; Schema: public; Owner: postgres
--

INSERT INTO public.activities VALUES
	(1752324317, '677e8668-9292-4eb2-a41a-f98abee664e9', 'Armed', '662e7391-2bf7-4ce0-ba56-5f5a5aeb5603'),
	(1752324320, '677e8668-9292-4eb2-a41a-f98abee664e9', 'Disarmed', '662e7391-2bf7-4ce0-ba56-5f5a5aeb5603'),
	(1754139290, '3bd239d4-03d1-47ce-ae20-db15b2398011', 'ArmedAway', '10ff2887-df12-44a3-ad73-19d23b6a119d'),
	(1754139315, '3bd239d4-03d1-47ce-ae20-db15b2398011', 'Disarmed', '955e82b7-e0e0-4888-88fb-c1c089c6dd5a'),
	(1756744397, '3bd239d4-03d1-47ce-ae20-db15b2398011', 'ArmedStay', 'ddfb45d1-845c-469e-b13f-fcf026568750');


--
-- Data for Name: messages; Type: TABLE DATA; Schema: public; Owner: postgres
--

INSERT INTO public.messages VALUES
	('Alert', 1756744817, 'Bedroom Door: Opened', 'f03e7e8a-ce15-4c02-a47a-7681e4efb883'),
	('Info', 1756745042, 'Bedroom Door Sensor: Low battery', 'b7c64c03-7209-4d90-a4c5-dddeb3fc3dcf'),
	('Warn', 1756745182, 'ESP32 Bedroomm: health check failed', '68a3029a-7d6d-419e-a5d2-35bcf3337ec0'),
	('Fail', 1756745255, 'ESP32 Living room: no connection', 'b2eee7c0-6abb-4e38-b920-9195e863f82b'),
	('Fail', 1756746007, 'ESP32 Living room: connection lost', '5151c123-8c36-415f-a09a-39f3c2a6f86d');


--
-- Data for Name: profiles; Type: TABLE DATA; Schema: public; Owner: postgres
--

INSERT INTO public.profiles VALUES
	('TIRAO', 'paulo.tirao@gmail.com', 'PAULO ANDRÉS', '+5493516257849', '+5493516257849', 'male', 'Santina Norte', 'Cordoba', '3bd239d4-03d1-47ce-ae20-db15b2398011');


--
-- Data for Name: realms; Type: TABLE DATA; Schema: public; Owner: postgres
--

INSERT INTO public.realms VALUES
	('wanaka-security', 'ae7a79e3-c2bf-43c3-a339-c27b6ed0cd39', 'api-dev');


--
-- Data for Name: statuses; Type: TABLE DATA; Schema: public; Owner: postgres
--



--
-- Data for Name: tenants; Type: TABLE DATA; Schema: public; Owner: postgres
--

INSERT INTO public.tenants VALUES
	('marcos.tirao@icloud.com', '3172AppL', '3bd239d4-03d1-47ce-ae20-db15b2398011', 'enabled', 1),
	('marcos.tirao', '3172AppL', '06c8aecd-3c24-4da5-a2f9-1a823fa60edf', 'enabled', 1);


--
-- Data for Name: tokens; Type: TABLE DATA; Schema: public; Owner: postgres
--

INSERT INTO public.tokens VALUES
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTEzODMzNDIsInVzZXIiOiJhcGktZGV2In0.oqqWSYkEeiAGptydglKl5Kr-cLEPaCvv4Pl4FOKJOmU', 'wanaka-budget'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTEzOTQ2MzIsInVzZXIiOiJhcGktZGV2In0.8dmnOZRQ-Y4geuuVP0Y38mr2due464gKNzMm3jaGXjw', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTEzOTQ5MDMsInVzZXIiOiJhcGktZGV2In0.dMvw8YVqljXAyMywo5X56W2P4t_HOi3uBQT4hxGcdSA', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTEzOTUwMDUsInVzZXIiOiJhcGktZGV2In0.KX5KYIb0YqxEa-i2sKUXgyQGC--CQCqzDMYM3Oq2gb0', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTE0NzY4NjUsInVzZXIiOiJhcGktZGV2In0.Gsnjph5Zcf5tOm5wqSxtlHaIljvEduGx9mVqOitGSJk', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTE0NzczODQsInVzZXIiOiJhcGktZGV2In0.U3UJk7OG399DXnbnSoZ094MailADOipDjsMbIPFABaY', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTE0Nzc0NTQsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.1r-51Wqhe0NX-JCHx40az3bmAbdTUGL0OAjgWsrecto', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTE0NzgxMTAsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.XXKeUpBcREjcqApWE8-7mCaqyTsEFB4vrW14PdiJY3M', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTE0NzgxNzMsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.v0s00fsBF5_99NP-NHcrvcDSgLswJLcv--W8OeKov0U', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTE2NDcxMDMsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.aZSFsOT8DUtG0dcpkTrCfH1OAbRV1xjNEp8Zuh4_6r8', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTE2NTM1OTYsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.jM5TctAYeV4Lh9rkYz0gA2hDDaG_WBTH2ZaxQdAmawc', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTE2NTQ2NzksInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.B4r_V7XknJ874a2BkoKk13lOm1ijwSxnlPWa_qzvwro', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTE2NTUyMTgsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.TxPTDfiJ49uTz-i_yjlyBATijacaEtx70rDU4thRFYc', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTE2NTUzNDksInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.n2EwiGhtNyeFCLbeyk11PrkwMDz6v1Qk5r3cA_3iBiM', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTE2NTU0NjksInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.ybzITWR5f0NQ4a9piTOZjAbsFSwwcFRP1Lt3ANr0LKQ', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTE2NTYwNTYsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.hh9cw8yTRkaT_Sc805-SrHpIEHNqAHYuvrwnlaU-dIs', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTE2NTYxNTEsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.1bTORgHa0iQL5V_LVUAujpHBwrtDpCT9B7RreDYNWm8', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTE5OTUyNzYsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.4_SE-In9_M0T13y6eHBE631eqg02qjwtxIBoCJN50gA', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTE5OTYwMjQsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.dWnbfH2PfpcEtN5yK3-pRh9TQu7bO7hhGCoMvpE2C1s', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTE5OTg1MTQsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.jl3A8ACUrQzLY5s-k7gUHDuLjtLfXOLCnweIrTNH8A8', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTE5OTg2MjUsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.qXvqez53xcybdGVieFqn-tR4TMae5zUP-GBT8ZhjOQI', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTE5OTkwNjcsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.eHKoj-fwZ8Ou66zjRvAPKR_2tgIhfX-Gsm2AngEJBZ8', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTE5OTkyMTMsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.z-YKoqVEwhgcQdpeeZQOrvQ7ZsikP-PmM98lAxWC5ck', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTE5OTkzODQsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.S_DPOx1oZURxeposz5y-Oe6xSi5k6QMT0f5FlMowvd4', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTIwNTMwODQsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.sIYdQ2QVmp80YfyrJ3pmEy4-0eJ1ZwokRK7BEJATmvM', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTIwNTMyNTksInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.BlkfIOiIHHFjnZXaWMHzbtd7v7lJmUJID5M6WgOIQU4', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTIwNTYyNTksInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.aXk9lxVOUhiDK7FeSyA1IuChCoEGbckOhAMYCuUo1i4', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTIzNDMzNDYsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.llrE_JIxEjM7AYQbMJFQAuebu5iL4VxeCh_dK0h0vyo', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTIzNDU0NTEsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.nQ7tYIU5sN0eBI-kpYGB7btjHN7Sr3IVCq2mXsjJuTU', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTIzNDU2MTAsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.1m_xPfyppN-uWN_3U7Fo6htUZyHGPBnBK_QQsdmSz-w', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTIzNDYxMjIsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.xeHWyG9Kqq9PmEyTvImtTgG7yFQqE2FfX1CdfMEDXS8', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTIzNDYyMDEsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.sBVVng98hy3VuF3hfKZBcs7gKhgDtZi9XWZGs4fJXjM', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTIzNDYyNDksInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.qWzuAHpKBd-MCLgdtEtxWRVLeCWlPc_QBkfE8NeAm40', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTIzNDY2ODQsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.uxy-2tRP6kRXy9i0G2WpfB6ixt73YdXSdPEZwrJGy74', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTIzNDY3NjEsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.ZnJrCdQO9BWwZYtT2cFJjrorBXj3h_w2SGQIQoHjdKQ', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTIzNDY4NTksInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.zA8EzKYiOE33uHMTJv0NDXUyMwUa-32W6SC8xAjeWrg', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTI5NTU0MDgsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.uAZGDXa-tJjTKQhBxTwP1gk7B1gLxe_SD4L3FIEO10E', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTI5NTU1NDksInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.8jXh4NX1i6uIcR5u6VYNIYpmKPsqVFPsyJCyECRMpIc', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTI5NTU1ODksInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.qT5bExbpGHgflMMMVjGz3C5V2rpolRS_HNJ-f-JxyLk', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTMxODY2OTgsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.GEwpLOJuLkGgILZrUMhIs4Hyqt9pNBZFOMtfJCHMayI', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTMxODY5MTEsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.QpHJEL9hQzaivZvnVlnil4nmiysOMu17rGyNRO7yMzY', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTMxODgzMTAsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.6e6sQHLaeH1XA3_jG4rYabqcDfLIzHZyTc9RS_0OVc0', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTMyNzM1NDEsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ._V6_RqrpczeZc4AXZ9cv-nfSCWRoJUlhVh4ibXtVE7Q', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTM0NzA1MTIsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.cdQ8FX3P3pKYC3U_UHiyAeNCaUEqb99PmUOtJAJxNBI', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTUwMDIxNTIsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.TRM2HQj_sZLh23EFOmu0i1RPgTHvoxw3xajyTlh0Ns0', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTUwMDI2NTQsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.vuQClSlUw9Ooev9e173sgCbyzKYH4CQVBYJrx7oi0BE', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTUwMDI4ODgsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.6mXtcFeape6iBnyzaZi-LQOvpxXxvWj51UKZeXQjOhI', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTUwMDI5NDAsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.dM8gXQKSYngI59l49TZjkwADcze6tRMvZKKsHGeIHHI', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTUwMDMyNjUsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.WtSAsbwRdpo8BMqaJFbNxA5FT5yAKOM5xRgkWpINVm0', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTc1MTIzMzgsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.dKZcsBabvTuyMnZ5CF_KBvW1WKMv_UYWoOyEKklOrSk', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTc1MTI2NzcsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.DxszIbk-_nJel5qzkRIVF8d8xEZJRAb_sJJZxrPajzY', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTc1MTI3NjMsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.-6dL0EUijUKVlnVAqpO8GVnjNQi9cRN0cfvzgQohBpQ', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTc1MTM0NTIsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.q9xUgyca33wsF1ZqP9RkMxMgoUgv8y3HDJ55Fqruna0', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTc1MTM2NjYsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.HR7sTmfrAKNx7qvN-7G1o7xKsIVT0jsZ7IYl4d7bLnY', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTc1MTM4NjEsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.JEW6FpGTnWa1Q0wiD_4rzF8JpTYxw__iqKk_MyWS8zw', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTc1MTM5NjYsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.HA710Sx5L1SnoGJPmH4OPYMAO2iNtPrmGXHrdFlbZnM', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTc1MTQzMjksInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.ZvuT4BHiby-9Lp7UF52U4o-RpH6JmlmZySj6v82_nO4', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTc1MTQ0MDYsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.vP2EstxHtHeAg9IyJn9722uQq4cdbY1cn7rNG0Hhg5g', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTc1MTQ0OTAsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.5FhuwqTTzHcs5lXGs-uXSX_TEhay22iyzjtyqSxs-dk', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTc1MTQ3NzksInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.zdFUFuC-h7dUXJv4Rbqw74J6ElKULBj10pc3E-4yLro', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTc1MTQ5OTgsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.3FX8kVZOJcQb_3nFowzH7IozT7CuuTavfD7GkicBb7Q', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTc1MTUwNzMsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.kmu2R0zQvr8y9XoietCvkFHdB_2yjTTqCGlxGnc3jIA', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTc1MTU4NTksInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.Q3BMtIRfcEQcVf9SBi7IlYSRBPR3SeIggN_xssH18W4', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTc1MTg1MjcsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.0hvGRR3P7a0xuWaxQijQ2PT9W5x-Pq9URgjqX--nXtE', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTc1MTg2NzcsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.mWro7iO_-zc4KgpgyK3_aKcM-MypOdLYl3YnaA9quVc', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTc1MTg3NjcsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.ZbDhRXoO6GH_n9WpT-jPRLizyDVWxrdVxRHogSftVEY', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTc1MTg4MTMsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.nvjIgpsbGoa5GEgFDhimrA3k5EUgAzJYlq0w9XvXi6o', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTc1MTg5MTEsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.iQKqw1gYVLfgsjDaNGh0DLRWsw9rZFxEw7lqYduSqNc', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTc1MTkwMDIsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.VPKsp9lh9BxiTwDM_edIoNiwvK7hnLPYWRdOIkRqaF8', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTc1MTkxMDAsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ._KqtYgodlYnW3y2LbRLxQemFJv5Q3UjgE3ozasd3paQ', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTc1MTkyMTUsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.KKWzpkz60sidBac7KJ47rCL0EoWoF_Lk249-CWVmytc', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTc1MTkzNDEsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.xXg-eMLUmypGOcB8UfPqGTB0hkgh3r55_I95QWkAM8Q', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTc2MDYyMjAsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.NMx9rryCuxFzDKNxBymI4fVYNJKSwHiM3IEdcm4c2a4', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTc2MDgxNDAsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.cUbRe6V-S-5Ht1ymRvwErII6t4hiP-nLicwoO8cZ4uY', '3bd239d4-03d1-47ce-ae20-db15b2398011'),
	('eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTc2MDgzOTMsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.zvo4HtpK1Gu_ZZdYT31xjCG2YpekZ7s8CREHTKGMAbU', '3bd239d4-03d1-47ce-ae20-db15b2398011');


--
-- Name: index_id; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX index_id ON public.messages USING btree (id);


--
-- Name: index_id1; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX index_id1 ON public.activities USING btree (id);


--
-- Name: index_id2; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX index_id2 ON public.statuses USING btree (id);


--
-- PostgreSQL database dump complete
--

