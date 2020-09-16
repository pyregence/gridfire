DROP DATABASE IF EXISTS gridfire_test;
DROP ROLE IF EXISTS gridfire_test;
CREATE ROLE gridfire_test WITH LOGIN CREATEDB PASSWORD 'gridfire_test';
CREATE DATABASE gridfire_test WITH OWNER gridfire_test;
\c gridfire_test
CREATE EXTENSION postgis;
CREATE EXTENSION postgis_raster;
CREATE SCHEMA landfire;
GRANT ALL PRIVILEGES ON SCHEMA landfire TO gridfire_test;
