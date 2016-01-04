{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module APVI.Docs where

import Data.String.Here (here)
import Data.ByteString.Lazy as BSL
import Data.Text (Text)

contCsvSample :: BSL.ByteString
contCsvSample = [here|
"Contribution (%)",State,"State name","Received at","Contribution over time"
3.66,1,NSW,"2015-12-23 23:55:00","<img src='http://services.aremi.nicta.com.au/apvi/v3/contribution/png/nsw'/>"
7.0,2,VIC,"2015-12-23 23:55:00","<img src='http://services.aremi.nicta.com.au/apvi/v3/contribution/png/vic'/>"
4.27,3,QLD,"2015-12-23 23:55:00","<img src='http://services.aremi.nicta.com.au/apvi/v3/contribution/png/qld'/>"
16.46,4,SA,"2015-12-23 23:55:00","<img src='http://services.aremi.nicta.com.au/apvi/v3/contribution/png/sa'/>"
5.29,5,WA,"2015-12-23 23:50:00","<img src='http://services.aremi.nicta.com.au/apvi/v3/contribution/png/wa'/>"
4.89,6,TAS,"2015-12-23 23:55:00","<img src='http://services.aremi.nicta.com.au/apvi/v3/contribution/png/tas'/>"
|]

perfCsvSample :: BSL.ByteString
perfCsvSample = [here|
"Performance (%)",State,"State name","Received at","Performance over time"
16.0,1,NSW,"2016-01-03 23:10:00","<img src='http://services.aremi.nicta.com.au/apvi/v3/performance/png/nsw'/>"
20.0,2,VIC,"2016-01-03 23:10:00","<img src='http://services.aremi.nicta.com.au/apvi/v3/performance/png/vic'/>"
28.0,3,QLD,"2016-01-03 23:10:00","<img src='http://services.aremi.nicta.com.au/apvi/v3/performance/png/qld'/>"
39.0,4,SA,"2016-01-03 23:10:00","<img src='http://services.aremi.nicta.com.au/apvi/v3/performance/png/sa'/>"
9.0,5,WA,"2016-01-03 23:10:00","<img src='http://services.aremi.nicta.com.au/apvi/v3/performance/png/wa'/>"
35.0,6,TAS,"2016-01-03 23:10:00","<img src='http://services.aremi.nicta.com.au/apvi/v3/performance/png/tas'/>"
|]

perfJsonSample :: Text
perfJsonSample = [here|
[["ts","2016-01-03T17:00:00Z","2016-01-03T17:05:00Z","2016-01-03T17:10:00Z","2016-01-03T17:15:00Z","2016-01-03T17:20:00Z","2016-01-03T17:25:00Z","2016-01-03T17:30:00Z","2016-01-03T17:35:00Z","2016-01-03T17:40:00Z","2016-01-03T17:45:00Z","2016-01-03T17:50:00Z","2016-01-03T17:55:00Z","2016-01-03T18:00:00Z","2016-01-03T18:05:00Z","2016-01-03T18:10:00Z","2016-01-03T18:15:00Z","2016-01-03T18:20:00Z","2016-01-03T18:25:00Z","2016-01-03T18:30:00Z","2016-01-03T18:35:00Z","2016-01-03T18:40:00Z","2016-01-03T18:45:00Z","2016-01-03T18:50:00Z","2016-01-03T18:55:00Z","2016-01-03T19:00:00Z","2016-01-03T19:05:00Z","2016-01-03T19:10:00Z","2016-01-03T19:15:00Z","2016-01-03T19:20:00Z","2016-01-03T19:25:00Z","2016-01-03T19:30:00Z","2016-01-03T19:35:00Z","2016-01-03T19:40:00Z","2016-01-03T19:45:00Z","2016-01-03T19:50:00Z","2016-01-03T19:55:00Z","2016-01-03T20:00:00Z","2016-01-03T20:05:00Z","2016-01-03T20:10:00Z","2016-01-03T20:15:00Z","2016-01-03T20:20:00Z","2016-01-03T20:25:00Z","2016-01-03T20:30:00Z","2016-01-03T20:35:00Z","2016-01-03T20:40:00Z","2016-01-03T20:45:00Z","2016-01-03T20:50:00Z","2016-01-03T20:55:00Z","2016-01-03T21:00:00Z","2016-01-03T21:05:00Z","2016-01-03T21:10:00Z","2016-01-03T21:15:00Z","2016-01-03T21:20:00Z","2016-01-03T21:25:00Z","2016-01-03T21:30:00Z","2016-01-03T21:35:00Z","2016-01-03T21:40:00Z","2016-01-03T21:45:00Z","2016-01-03T21:50:00Z","2016-01-03T21:55:00Z","2016-01-03T22:00:00Z","2016-01-03T22:05:00Z","2016-01-03T22:10:00Z","2016-01-03T22:15:00Z","2016-01-03T22:20:00Z","2016-01-03T22:25:00Z","2016-01-03T22:30:00Z","2016-01-03T22:35:00Z","2016-01-03T22:40:00Z","2016-01-03T22:45:00Z","2016-01-03T22:50:00Z","2016-01-03T22:55:00Z","2016-01-03T23:00:00Z","2016-01-03T23:05:00Z","2016-01-03T23:10:00Z","2016-01-03T23:15:00Z","2016-01-03T23:20:00Z","2016-01-03T23:25:00Z","2016-01-03T23:30:00Z","2016-01-03T23:35:00Z"],
["nsw",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,2,2,3,3,3,3,4,4,4,4,4,4,4,4,5,5,6,7,7,7,8,9,8,9,8,8,8,10,11,11,12,12,12,13,13,15,16,17,16,18,16,16,16,17],
["vic",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,2,3,3,5,5,6,5,5,5,7,6,6,8,9,9,10,11,11,12,13,13,14,14,15,15,16,16,17,17,18,18,19,19,19,20,21,21,22,23,24],
["qld",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,2,3,4,5,5,5,7,8,9,10,11,11,11,10,10,10,10,11,12,13,15,16,17,16,18,18,19,19,21,22,22,22,21,23,23,25,26,27,29,28,30,31,34,33,36],
["sa",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,2,2,3,3,4,4,5,6,7,8,8,10,11,11,12,14,14,16,17,18,20,21,22,24,25,26,27,30,29,32,33,35,34,38,37,39,38,41,40,44,43],
["wa",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,2,2,3,3,4,4,5,6,7,8,9,9,9,9,8,9,10,10,12]]
|]

contJsonSample :: Text
contJsonSample = [here|
[["ts","2016-01-03T17:00:00Z","2016-01-03T17:05:00Z","2016-01-03T17:10:00Z","2016-01-03T17:15:00Z","2016-01-03T17:20:00Z","2016-01-03T17:25:00Z","2016-01-03T17:30:00Z","2016-01-03T17:35:00Z","2016-01-03T17:40:00Z","2016-01-03T17:45:00Z","2016-01-03T17:50:00Z","2016-01-03T17:55:00Z","2016-01-03T18:00:00Z","2016-01-03T18:05:00Z","2016-01-03T18:10:00Z","2016-01-03T18:15:00Z","2016-01-03T18:20:00Z","2016-01-03T18:25:00Z","2016-01-03T18:30:00Z","2016-01-03T18:35:00Z","2016-01-03T18:40:00Z","2016-01-03T18:45:00Z","2016-01-03T18:50:00Z","2016-01-03T18:55:00Z","2016-01-03T19:00:00Z","2016-01-03T19:05:00Z","2016-01-03T19:10:00Z","2016-01-03T19:15:00Z","2016-01-03T19:20:00Z","2016-01-03T19:25:00Z","2016-01-03T19:30:00Z","2016-01-03T19:35:00Z","2016-01-03T19:40:00Z","2016-01-03T19:45:00Z","2016-01-03T19:50:00Z","2016-01-03T19:55:00Z","2016-01-03T20:00:00Z","2016-01-03T20:05:00Z","2016-01-03T20:10:00Z","2016-01-03T20:15:00Z","2016-01-03T20:20:00Z","2016-01-03T20:25:00Z","2016-01-03T20:30:00Z","2016-01-03T20:35:00Z","2016-01-03T20:40:00Z","2016-01-03T20:45:00Z","2016-01-03T20:50:00Z","2016-01-03T20:55:00Z","2016-01-03T21:00:00Z","2016-01-03T21:05:00Z","2016-01-03T21:10:00Z","2016-01-03T21:15:00Z","2016-01-03T21:20:00Z","2016-01-03T21:25:00Z","2016-01-03T21:30:00Z","2016-01-03T21:35:00Z","2016-01-03T21:40:00Z","2016-01-03T21:45:00Z","2016-01-03T21:50:00Z","2016-01-03T21:55:00Z","2016-01-03T22:00:00Z","2016-01-03T22:05:00Z","2016-01-03T22:10:00Z","2016-01-03T22:15:00Z","2016-01-03T22:20:00Z","2016-01-03T22:25:00Z","2016-01-03T22:30:00Z","2016-01-03T22:35:00Z","2016-01-03T22:40:00Z","2016-01-03T22:45:00Z","2016-01-03T22:50:00Z","2016-01-03T22:55:00Z","2016-01-03T23:00:00Z","2016-01-03T23:05:00Z","2016-01-03T23:10:00Z","2016-01-03T23:15:00Z","2016-01-03T23:20:00Z","2016-01-03T23:25:00Z","2016-01-03T23:30:00Z","2016-01-03T23:35:00Z","2016-01-03T23:40:00Z","2016-01-03T23:45:00Z","2016-01-03T23:50:00Z","2016-01-03T23:55:00Z","2016-01-04T00:00:00Z"],
["nsw","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.01","0.02","0.04","0.07","0.10","0.11","0.16","0.16","0.20","0.25","0.34","0.37","0.42","0.44","0.50","0.47","0.55","0.52","0.55","0.55","0.52","0.50","0.59","0.59","0.65","0.70","0.81","0.89","0.92","0.93","1.03","1.06","1.03","1.06","0.94","1.01","0.98","1.18","1.23","1.34","1.38","1.33","1.43","1.46","1.52","1.72","1.80","1.96","1.83","2.03","1.85","1.82","1.80","1.87","1.78","1.97","1.81","1.78","1.90"],
["vic","0.01","0.00","0.01","0.00","0.01","0.00","0.01","0.00","0.01","0.00","0.01","0.00","0.01","0.00","0.01","0.00","0.01","0.00","0.01","0.00","0.00","0.01","0.01","0.01","0.01","0.01","0.02","0.04","0.03","0.06","0.07","0.13","0.13","0.20","0.21","0.28","0.31","0.37","0.37","0.44","0.46","0.53","0.59","0.85","0.94","0.96","0.94","0.87","0.86","1.20","0.96","1.03","1.35","1.49","1.48","1.66","1.69","1.77","1.81","1.94","1.97","2.06","2.07","2.23","2.29","2.37","2.41","2.52","2.51","2.69","2.68","2.84","2.76","2.85","3.00","3.05","3.16","3.29","3.32","3.46","2.72","3.84","3.88","3.96",null],
["qld","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.01","0.03","0.07","0.14","0.22","0.28","0.34","0.39","0.43","0.48","0.58","0.77","1.01","1.21","1.28","1.38","1.66","1.91","2.25","2.48","2.53","2.66","2.57","2.39","2.23","2.28","2.25","2.47","2.61","2.80","3.18","3.35","3.56","3.49","3.79","3.77","3.97","4.06","4.26","4.44","4.52","4.53","4.42","4.63","4.69","5.02","5.17","5.33","5.69","5.67","5.85","6.11","6.71","6.45","7.08","7.06","7.20","7.34","7.68",null],
["sa","0.00","0.00","0.00","0.00","0.00","0.00","0.01","0.01","0.01","0.01","0.01","0.01","0.01","0.01","0.01","0.03","0.01","0.03","0.01","0.03","0.02","0.03","0.02","0.04","0.02","0.04","0.02","0.04","0.01","0.04","0.02","0.04","0.05","0.12","0.16","0.32","0.35","0.59","0.74","1.02","1.15","1.48","1.75","2.11","2.23","2.74","3.07","3.59","3.69","3.92","4.50","5.00","5.29","5.78","6.24","6.58","7.19","7.57","8.23","8.65","9.10","9.50","10.43","10.45","10.68","10.88","12.04","11.90","12.59","12.96","13.66","13.42","14.60","14.36","15.22","14.16","15.35","15.07","16.58","16.29","17.44","16.99","18.09","17.62",null],
["tas","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.00","0.01","0.03","0.04","0.06","0.08","0.13","0.13","0.19","0.21","0.29","0.31","0.39","0.45","0.55","0.58","0.63","0.63","0.65","0.77","0.83","0.90","1.00","1.11","1.13","1.28","1.26","1.43","1.35","1.47","1.59","1.66","1.69","1.91","1.74","1.99","2.00","2.10","1.97","2.22","2.35","2.43","2.41","2.91","2.63","2.60","2.87","2.76","2.90","2.86","2.93","2.94","3.05","2.62","2.62","2.74","3.06","2.82","2.68","3.05","3.09","3.16","3.12",null]]
|]