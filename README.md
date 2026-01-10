# Retail-Transaction-Analytics-using-R
## Dataset
My R program uses the following dataset:
```bash
https://github.com/luminati-io/eCommerce-dataset-samples
```
## Project Structure
- `/data`: Contains the dataset files used in the analysis.
- `/scripts`: Includes R scripts for data processing, analysis, and visualization.
- `/results`: Stores output files such as plots, reports, and generated insights.
- `README.md`: This file, providing project overview and documentation.
- `LICENSE`: Apache License 2.0 file.

## R Packages Used
- tidyverse - Data processing and visualization
- lubridate - Date and time handling
- ggplot2 - Data visualization
- gridExtra - Plot arrangement
- scales - Data scaling

## Project Outcomes
### Analysis Content
1. **Price Analysis**
   - Mean and median statistics for initial and final prices
   - Price range distribution (0-10, 10-20, 20-50, 50-100, 100-200, 200+)
   - Discount rate analysis (average discount, maximum discount, number of discounted items)

2. **Category Analysis**
   - Root category sales performance (number of products, average price, average discount)
   - TOP 20 sub-category ranking
   - Category price distribution (min, quartiles, median, max)

3. **Color/Size Analysis**
   - TOP 15 popular colors and their proportions
   - Analysis of color and price relationship
   - TOP 20 size distribution
   - Analysis of size and price relationship

4. **Brand Analysis**
   - TOP 20 brand ranking (number of products, average price, average discount)
   - Brand price positioning distribution (low-end, mid-range, mid-high, high-end)
   
## Authors and Acknowledgments
- Name: Junliang Li
- Email: 940747544@qq.com

## License
This project is licensed under the Apache License 2.0 - see the [LICENSE](LICENSE) file for details.
