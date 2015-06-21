This script run_analysis.r reads data from UCI HAR dataset , processes it and creates a more tidy data set in the form of a table aggregating important values.

There is a single file which is divided into separate chunks performing the following operations.

1. Reading in raw training data
2. Reading in raw test data
3. Appropriately labeling training and test data based on available files
4. Combining training and test data into one large dataset
5. Calculating means and standard deviations of different attributes and aggregating these as columns for every observation chunk
6. Providing Columns and activity labels which are desriptive
7. Aggregating all data across different observations into a table grouped by subject and activity types

In a nutshell this script performs reading, processing, merging and aggregating of a complex data collection into an aggregated table.