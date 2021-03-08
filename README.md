# TestSmellEmpirical_Data
### Description
TestSmellEmpirical_Data is a collection of test smell data collected through thirteen open-source projects.

### Summary of studied projects between 2016 to 2019

|Project|# Releases|# LOC in Source Code|# LOC in Test Code
|---|---|---|---|
|Kafka|9|95K-254K|18K-101K
|Groovy|9|338K-393K|8K-9K
|Camel|8|586K-1.0M|379K-484K
|Zookeeper|4|128K-119K|25K-36K
|Cxf|9|696K-753K|195K-218K
|Karaf|11|132K-168K|14K-17K
|Flink|8|338K-731K|100K-234K
|Accumulo|7|420K-577K|49K-47K
|Hive|11|3.5M-4.4M|162K-221K
|Bookkeeper|9|102K-200K|32K-85K
|Wicket|8|264K-257K|54K-57K
|Cassandra|6|315K-184K|43K-112K
|Hadoop|3|637K-1M|418K-658K
|Total|102|6.9M-9.1M|1.1M-1.6M

### Project structure

```
TestSmellEmpirical_Data
│
├── Exp:	RQ1 raw data on experience
│   ├── exp.csv:  Experience vs test smell addition/removal
│   ├── size_exp.csv:  Experience vs size of code change
├── Model: 
│   ├── within:  Within project modelling
│   ├── final.csv:  Raw data for modelling
├── snaphots: RQ1 raw data on evolution
│   ├── 2016_2019_norm.csv:  RQ1 change in magnitude of normalized data at 2016 vs 2019
│   ├── 2016_2019_raw.csv:   RQ1 change in magnitude of raw data at 2016 vs 2019
│   ├── 2016_2019_Raw_Individual.csv:  RQ1 time series evolution of test smells 
├── testSmellChange: Raw data for test smells
│   ├── Project Name: Name of systems(i.e., Hadoop)
│   ├── ├── Date: Duration mined (i.e., 2017-02-14_TO_2017-08-14)
│   ├── ├── ├── testSmellChange: Test smell process information in the studied duration 
│   ├── ├── ├── testSmellPresent: Test smell product information in the studied duration
├── model.R: R code for modelling
├── plot.R: R code for plotting (RQ1-3)
├── RQ2.csv: Analyzed instances for RQ2


```
