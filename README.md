# Nonparametric Bandwidth Selection in RDD ![R](https://img.shields.io/badge/R-4.3.0-blue)

## Table of Contents

* [Project Overview](#project-overview)
* [Repository Structure](#repository-structure)
* [Analyses](#analyses)
  * [Monte Carlo Simulation](#1-monte-carlo-simulation)
  * [Senate Election Data](#2-senate-election-data)
* [System Prerequisites](#system-prerequisites)
* [Getting Started](#getting-started)

---

## Project Overview

This repository contains an individual module of a broader Research Module project on Regression Discontinuity Designs (RDD). The full group project encompassing additional specification testing can be found [here](Link_to_Lennart_Repo).

This specific module focuses exclusively on the finite-sample performance of nonparametric bandwidth selectors in the **Sharp Regression Discontinuity Design (SRD)**. 

It consists of:
1. Custom-built algorithms for the **Imbens-Kalyanaraman (IK)** plug-in estimator and **Leave-One-Out Cross-Validation (LOOCV)**.
2. Monte Carlo simulations stress-testing these algorithms under 4 distinct Data-Generating Processes (DGPs) varying in structural curvature and asymmetry.
3. An application to **U.S. Senate election data** to observe algorithmic behavior in real-world environments.

The aim is to understand the bias-variance tradeoff inherent to bandwidth selection, analyzing how data-driven approaches navigate structural complexity versus local noise.

---

## Repository Structure

The Project is structured as follows:

* **`01_functions/`**: Contains the core algorithmic implementations.
  * `01_dgp.R`: Functions to generate synthetic SRD datasets for the four test cases.
  * `02_estimator.R`: Local Linear Regression utilizing a Triangular Kernel.
  * `03_bandwidth_ik.R`: The Imbens-Kalyanaraman optimal bandwidth selector.
  * `04_bandwidth_cv.R`: The Ludwig-Miller LOOCV bandwidth selector.

* **`03_output/`**: Contains all generated artifacts.
  * **plots**: Graphical diagnostics demonstrating the "Geometry of Bias Cancellation" and bandwidth estimation variances.
  * **tables**: LaTeX tables for Monte Carlo performance metrics (MSE, Variance, Bias).

* **Root Scripts**: Executable simulation runners.
  * `00_config.R`: Global configuration, sample sizes, and grid definitions.
  * `10_run_lennart_sims.R`: Simulation execution for the complex DGP (incorporating covariates for McCrary density tests).

---

## Analyses

### 1️⃣ Monte Carlo Simulation

* **Four DGPs:**
  * **Case 1 (Baseline):** Symmetric, gentle curvature.
  * **Case 2 (Moderate Asymmetry):** Differing functional forms on either side of the cutoff.
  * **Case 3 (Severe Curvature):** A rigorous stress test inducing sharp "S" curves to evaluate the *Penalty of Curvature*.
  * **Case 4 (Complex DGP):** Incorporates an underlying covariate ($Z$) to validate bandwidth robustness for downstream density and sensitivity checks.
* **Algorithms Compared:**
  * **IK Estimator:** An analytical plug-in approach prioritizing structural stability.
  * **Cross-Validation:** A data-driven approach scanning for global trends.

The simulations assess how varying degrees of curvature push Cross-Validation into an *Instability Trap* in finite samples, while analyzing the IK algorithm's rate of convergence.

### 2️⃣ Senate Election Data

* **Context:** Close U.S. Senate elections.
* **Analysis:**
  * Applies both bandwidth selectors to real-world incumbency advantage data.
  * Highlights the contrasting heuristics of the algorithms: IK acting as a "Microscope" (highly sensitive to local data clusters) and CV acting as a "Telescope" (smoothing over noise to capture global linearity).

---

## System Prerequisites

To run this project, you need:

* R ($\geq$ 4.3.0)
* RStudio (recommended)
* A modern LaTeX distribution (TeX Live) for table compilation
* Git

---

## Getting Started

**1. Clone the repository**
```bash
git clone [https://github.com/shashtg-git-some/RDD-Bandwidth-Selection.git](https://github.com/shashtg-git-some/RDD-Bandwidth-Selection.git)
