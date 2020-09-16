CREATE USER gridfire_test;
CREATE DATABASE gridfire_test WITH OWNER gridfire_test;
GRANT ALL PRIVILEGES ON DATABASE gridfire_test TO gridfire_test;
\c gridfire_test;
CREATE EXTENSION postgis;
CREATE EXTENSION postgis_raster;
CREATE SCHEMA landfire;

-- create_gridifre_db.sql
