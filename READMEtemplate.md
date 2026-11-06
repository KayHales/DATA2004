This readme file was generated on [YYYY-MM-DD] by [NAME]

Adapted from Cornell Data Services' readme template. Replace everything in [square brackets], including the brackets.

# GENERAL INFORMATION

* Title of Dataset: MLB ballparks, 2000 vs. 2026

## Author Information
Name: [your name]
Institution: Transylvania University
Email: [your email]

* Date of data collection: [the date(s) you requested the data, YYYY-MM-DD]


# SHARING/ACCESS INFORMATION

* Licenses/restrictions placed on the data:
	* MLB Stats API: [what MLB's terms allow]
	* Wikipedia: [the license, and what you owe if you use it]
* Was data derived from another source? [yes or no]
	* If yes, list source(s): [both sources]


# DATA & FILE OVERVIEW

## File List

* `data/raw/mlb_teams_2000.json`: [one line: what it is]
* `data/raw/mlb_teams_2026.json`: [one line: what it is]
* `data/raw/mlb_stadiums.html`: [one line: what it is]
* `data/processed/mlb_parks_2026.csv`: [one line: what it is]

* Relationship between files: [how the raw files relate to the processed file]


# METHODOLOGICAL INFORMATION

## Description of methods used for collection/generation of data:

* MLB Stats API
	* URLs, exactly as sent: [both]
	* Requested on: [date and time, with time zone, for each]
	* Status: [for each]
* Wikipedia
	* URL: [the URL]
	* Requested on: [date and time, with time zone]
	* Page last edited on: [from the page footer]
	* Status: [the status]
	* robots.txt and user agent: [is this page allowed for your bot? what user agent did you send?]

## Methods for processing the data:

[numbered steps, in order: what you read in, how you cleaned it, how you joined it]

1. [step]
2. [step]
3. [step]

Decisions that change the data, and why:

* [decision, and why]
* [decision, and why]

## Instrument- or software-specific information needed to interpret the data:

* R version: [run R.version.string]
* httr2 version: [run packageVersion("httr2")]
* rvest version: [run packageVersion("rvest")]
* Code: [your script's file name]

## Quality-assurance procedures performed on the data:

[the checks you ran (for example, row counts, keys, anti_join) and what each one showed]

* [check: what it showed]
* [check: what it showed]


# DATA-SPECIFIC INFORMATION FOR: `mlb_parks_2026.csv`

* Number of variables: [number]
* Number of cases/rows: [number]
* Variable List:
	* [column name]: [what it means] ([units or values])
	* [column name]: [what it means] ([units or values])
* Missing data codes: [how missing values appear in this file, and which columns have them]
* Known problems: [anything someone using this file should know]
