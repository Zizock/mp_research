# Repo of past projects

## Overview

This repo contains scripts from two earlier research projects. The scripts are not organized as a single, standalone pipeline and may require adaptation before reuse.

In the **DSGE/** project, I constructed, estimated, and simulated a New Keynesian DSGE model with endogenous labor force participation. This framework introduces an additional margin beyond the unemployment rate, allowing the model to capture richer labor market dynamics than standard DSGE models that focus solely on unemployment.

- **pre_dynare_processing/** collects short R scripts for data processing
- **dynare/** contains Dynare codes for model estimation and simulation experiments
- **graphing/** contains ggplot scripts for visualizing results
- **data/** contains processed data files used in the estimation, available upon request

In the **QE_loan/** project, I analyzed the heterogeneous sectoral effects of quantitative easing (QE) on corporate borrowing using Bayesian VAR and panel VAR models.

- **data_processing/** collects short R scripts for data processing
- **BEAR/** contains BEAR programs for Bayesian VAR estimation
- **data/** contains processed data files used in the estimation, available upon request

## An Estimated New Keynesian Model with Endogenous Labor Participation

***Abstract:***  In this project, I addresses the divergent behavior of labor force participation rates in the US and Japan during 2010s. We propose a New Keynesian model with endogenous labor force participation decisions and nonseparable preference to illustrate a potential role for monetary policy in this process. We show that in the model, the divergence in participation can be driven by either or both factors: the intratemporal substitution elasticity between market goods and home production, as well as the elasticity of labor supply. We then estimate the model for the US and Japan using Bayesian technique. The estimation results indicate that while the two countries have similar intratemporal substitution elasticities, they differ significantly in terms of labor supply elasticity, and consequently, labor participation shows different movements responding to similar policy shocks. A subsequent calibration experiment confirms the findings.

## Sectoral Heterogeneity in the Stimulative Effect of QE on Corporate Loans

***Abstract:***  I examine the effect of monetary easing (QE) on corporate loans in Japan using sector level data. Comparing an aggregated level VAR model with a sectoral panel VAR, we show that while QE stimulates overall lending, its effects are highly heterogeneous across sectors. Most of the responses are from construction, retail, wholesale, finance and real estate sectors. When we further split the loan data by firm size, we find that small and medium-sized firms (SMEs) across all non-manufacturing sectors tend to borrow more following a QE shock. In contrast, while large firms in the previously identified sectors also increase their borrowing, those in other sectors show a mild decline. Overall, the distribution of the stimulative effect across sectors appears to be related to the allocation of zombie firms. However, this paper does not establish a causal relationship between the two. A formal examination would require analysis based on firm-level data.