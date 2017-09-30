#Merging tables and removing NULLS using SQLite3

ATTACH DATABASE 'NYT.db' AS 'NYT';

CREATE TABLE metadata(
	aid int,
	title varchar(255),
	primary_source varchar(255),
	original source varchar(255),
	pubdate date);
.separator ","
.import MetaDataNYT_1945-2005.csv nyt_meta

CREATE TABLE nyt_full (
	eid varchar(255),
	story_date date,
	year int,
	month int,
	day int,
	source varchar(255),
	source_root varchar(255),
	source_agent varchar(255),
	source_others varchar(255),
	target varchar(255),
	target_root varchar(255),
	target_agent varchar(255),
	target_others varchar(255),
	code int,
	root_code int,
	quad_class int,
	goldstein real,
	joined_issues varchar(255),
	lat real,
	long real,
	placename varchar(255),
	statename varchar(255),
	countryname varchar(255),
	aid int,
	process varchar(255));
.separator ","
.import PhoenixNYT_1945-2005.csv nyt_full

CREATE TABLE complete_nyt AS SELECT * FROM metadata INNER JOIN nyt_full ON metadata.aid = nyt_full.aid;

.headers on
.mode csv
.output complete_nyt.csv
SELECT * FROM complete_nyt;


