# healthgps-plots
A set of scripts for plotting and visualising data related to Health-GPS

## Getting Started

### Prerequisites

- [R](https://www.r-project.org/) installed
- [renv](https://rstudio.github.io/renv/) installed

### Cloning the Repository

```bash
git clone https://github.com/imperialCHEPI/healthgps-plots.git
cd healthgps-plots
```

### Setting Up the Project Environment with renv

1. Activate the renv environment:
```bash
Rscript -e 'renv::activate()'
```

2. Install project dependencies:
```bash
Rscript -e 'renv::restore()'
```

### Running the Script
You can now run the visualisation script using:
```bash
Rscript Visualisation.R path/to/HealthGPS_Result_S1.csv path/to/HealthGPS_Result_S2.csv path/to/HealthGPS_Result_S4.csv path/to/HealthGPS_Result_S5.csv
```

### Updating Dependencies
If you make changes to the project dependencies or want to ensure you have the latest versions, you can update the renv.lock file:
```bash
Rscript -e 'renv::snapshot()'
```

### Deactivating the renv Environment
After you are done using the project, you can deactivate the renv environment:
```bash
Rscript -e 'renv::deactivate()'
```