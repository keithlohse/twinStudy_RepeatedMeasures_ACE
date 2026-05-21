# twinStudy_RepeatedMeasures_ACE
Implemention of a linear mixed model with repeated measures to estimate ACE components for accelerometry variables in a twin study. 
This repository contains none of raw or summary outputs, but does contain all of the data processing and analysis code. 

1. "heritability_utils" - contains R code for implementing a mixed effect model to estimate ACE parameters from repeated measures as per:  Ge T, Holmes AJ, Buckner RL, Smoller JW, Sabuncu MR. Heritability analysis with repeat measurements and its application to resting-state functional connectivity. Proceedings of the National Academy of Sciences. 2017 May 23;114(21):5521-6.

2. "script_00_mergingChunkData" - contains code for harmonizing "chunk level" accelerometry data from individual participants.

3. "script_01_dataCleaningAndDemographics" - contains code for integrating accelerometry data (from Box) with clinical and demographic data (from REDCap), exports summary data for demographic supplemental tables.

4. "script_02_lmerHeritabiltiy" - implements the functions from heritability_utils in a loop through the different outcome variables to estimate ACE components and their standard errors use leave-one-out jack-knife resampling.

5. "script_03_lmerReliabilityAndAggregation" - implements linear mixed models to estimate within-twin and within-family ICCs, then aggregates over chunks to obtain one value per participant for accelerometry data.

6. "script_04_dataReductionAndEFA" - used the aggregated accelerometry data in an exploratory factor analysis (EFA), and then tests associations between latent factors and 6-month clinical variables.