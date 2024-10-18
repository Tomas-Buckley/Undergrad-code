# Statistical Consulting
The dataset I had was a list of over 600,000 motor insurance policies, with the Claim Amount and number of Claims separated into two datasets. \\
The first file cleans up the data (removing duplicates, removing clearly incorrect entries, etc.) \newline
The second file looks at outliers and the shape of the data. It had a heavy tail out to the right so I did not model the top 1% of Claim Amount values. \\
In the third file I lookat the importance of each variable by itself. \\
The fourth file caps some of the numerical variables. \\
The fifth file groups variables that have too many possible states. \\
The sixth file fits a gamma model and plots a lift chart for the results. 
(measures performance in each quantile of the dataset, the further above the line, the better) \\
The last file fits a random forest model to the data and compares it to the results of the gamma model. 
