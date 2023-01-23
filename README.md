# Divfolio

The primary objective of the software is to facilitate the creation and evaluation of portfolios that take into account decarbonization and ESG investing and divestment practices in line with climate change prevention trends emerging in wealth management. The software enables the creation of divestment plans and sustainable portfolios, as well as the comparison of the corresponding risk/return profiles, ESG scores, and bespoke portfolio variables, such as carbon intensity. The evaluation is based on simulations that collect and include pertinent historical market data before and after divestment, while also taking into account varying divestment planning rates. The software permits the user to implement these evaluations on publicly traded equities assets, exchange-traded funds (ETFs), exchange-traded notes (ETNs), and Depositary Receipts (DR). Alternatively, Divfolio can be used for general proposes, such as portfolio comparison, gathering ESG scores, and downloading historical return data.

![alt text](https://github.com/QuantFILab/Divfolio/blob/main/Figures/openpage.png?raw=true)

On the tab on the left, the <b> User Guide </b> provides detailed instructions on how to use this application. The application's functions are documented in <b> Divestment Plan </b>. Our contact information can be found in <b> Our Team </b>. We welcome the opportunity to collaborate with scholars and industry professionals interested in relevant issues and comparable Shiny applications. To report technical problem, please contact Pasin MarupanthoCancel changesrn at pm122@hw.ac.uk.

## Original Research Papers related to Divfolio

Divfolio is baed on the research results from two papers. Preprint versions of both papers are open access available on SSRN. First, the methodology paper explains all mathematical details in portfolio optimization, [Mechanisms to Incentivise Fossil Fuel Divestment and Implications on Portfolio Risk and Returns](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4131449). Secondly, the software paper explains how to use Divolio aloge with an example in FTSE100 divestment to improve ESG scores, [DivFolio: A Shiny Application for Portfolio Divestment in Green Finance Wealth Management](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4131449).
## Where to find the ticker?

If you are interested in downloading historical data from Divfolio, you will need the Yahoo! Finance ticker. You may find the tickers [here](https://finance.yahoo.com/screener/new). Note that the ESG scores are available for certain stocks. The ESG scores are not available for others assets, but the historical data may be available.


## Workflow

DivFolio App tool comprises two core components as will be detailed in the panels below. The first component involves a set of sequential STEPs that users undertake consisting of STEP 1 through STEP 5, which must be performed sequentially. The second component involves a collection of three independent optional STEPs that can be performed individually. For the optional steps, so long as the user has the relevant data prepared in CSV format already, for instance as output from STEPS 1 to 5 in Component 1, then they can upload this directly into any of the STEPS in Component 2 instead of generating them again from the tool. For more details please see the user guide on the application. 

![alt text](https://github.com/QuantFILab/Divfolio/blob/main/Figures/daig.jpg?raw=true)


## Functions in Divolio

**STEP 1:** Accessing the ESG scores and some profiles of the individual company.

**STEP 2:** Downloading ESG scores of the selected companies, and exporting to the local machine in CSV and png.

**STEP 3:** Downloading historical returns of the selected companies, comparing the performances, and exporting to the local machine in CSV and png.

**STEP 4:** Generating portfolio from the selected companies, rebalancing portfolio, summarising the performances, and exporting to the local machine in CSV and png.

**STEP 5:** Generating divestment schedule, divesting portfolio, rebalancing portfolio, summarising and comparing the performances, and exporting to the local machine in CSV and png.


**Option I:** For comparing more than two portfolios, generating divestment schedule, divesting portfolio, rebalancing portfolio, summarising and comparing the performances, and exporting to the local machine in CSV and png.

**Option II:** Clustering portfolio according to their risk profile, and exporting to local machine png.

**Option III:** Providing graph LASSO to portfolios over time to investigate covariance between assets during divestment, and exporting to local machine png.

You don't have to run all steps. For example, if you want only ESG scores, you can stop at STEP 2.


## Examples 1: Generate Portfolio on Application using Online Data

Let's set up the portfolio by divesting from big energy companies to the best ESG scoring companies in   
[Yahoo! Finance](https://www.insidermonkey.com/blog/top-5-esg-companies-in-2022-1082219/). We will invest in BAC, CRM, GOOGL,	INTC,	and MSFT which have a good environment score. Then, we divest from COP, CVM, and XOM which have a poor environmental scores.


### STEP 1 Asset Performance Investigation
To see the ESG score and past performance of the company (If you already have companies in mind, you can script this)
1. Chick to the Gear icon to open the input window
2. Type company's ticker in the box
3. Click on the "submit" button

![alt text](https://github.com/QuantFILab/Divfolio/blob/main/Figures/Step1_select.png?raw=true)

The results are shown below.

![alt text](https://github.com/QuantFILab/Divfolio/blob/main/Figures/Step1_page.png?raw=true)

To pick company stocks to a portfolio (Required)
1. Click on the Cart icon to open the input window.
2. Type the company's ticker.
3. Chick the "add" button.


![alt text](https://github.com/QuantFILab/Divfolio/blob/main/Figures/Step1_port.png?raw=true)

You need to add them one by one the ticker. Here, input  BAC, CRM, GOOGL,	INTC,	MSFT, COP, CVM, and XOM. The result will be shown in STEP 2.

### STEP 2 Selecting Divestable and Investable Assets

The tickers selected in STEP1 will appear in the box of "Potential Assets". We need to assign a divestment status to each asset by dragging and dropping them into the box of "Investable Assets" and "Divestable Assets" like in the figure below.

To get the ESG data and Company's sector
1. Drop tickers you want to invest in "Investable Assets" (If you want only ESG data, you can drop all assets to this box).
2. Drop tickers you want to divest in "Divestable Assets".
3. Click on the "summary" button and Click on "update summary" when you make a change to the divestment status or add more assets to the potential list.

![alt text](https://github.com/QuantFILab/Divfolio/blob/main/Figures/Step2_setup.png?raw=true)

The results are in the figure below. You can download the table to your local machine. The last table shows the percentage of the improvement in ESG score after divestment. The smaller score indicates lower ESG risk.

![alt text](https://github.com/QuantFILab/Divfolio/blob/main/Figures/Step2_page.png?raw=true)

### STEP 3 Batching Historical Data

To get the historical data of the assets passed from STEP2
1. Click on the Carlendar icon
2. Input start date
3. Input end date
4. Click the "submit" button

![alt text](https://github.com/QuantFILab/Divfolio/blob/main/Figures/Step3_setdate.png?raw=true)

You will get a table of the historical return calculated from the close price and the boxplots of the return that can be arranged by the company's historical risk profiles such as volatility and Sharpe ratio.

![alt text](https://github.com/QuantFILab/Divfolio/blob/main/Figures/Step3_page.png?raw=true)


### STEP 4 Constructing Investment Portfolio

To generate portfolio weights

1. Click on the List icon
2. Select the type of portfolio (Here we use GMV with leverage 1.3)
3. Switch on the option to limit the short position
4. Select the number of days each time rebalancing (Here we rebalance the portfolio monthly)
5. Select the window of covariance history (Usually equal to the number of days each time rebalancing)
6. Click on the "submit" button

![alt text](https://github.com/QuantFILab/Divfolio/blob/main/Figures/Step4_seteport.png?raw=true)

Te results will shown the ESG score and risk profiles of portfolio as in the figure below.

![alt text](https://github.com/QuantFILab/Divfolio/blob/main/Figures/Step4_page.png?raw=true)


### STEP 5 Divestment

To generate a divestment schedule and divested portfolio

1. Click on the Minus icon.
2. Select the divestment schedule (Here, we use linear (gradually) divestment).
3. Set the last date of divestment when the divestable assets will be divested completely.
4. Set the divestment schedule parameter
5. Click on the "preview" button to preview the divestment schedule 
6. If you like the current schedule click "submit" to process

![alt text](https://github.com/QuantFILab/Divfolio/blob/main/Figures/Step5_setdiv.png?raw=true)

The results show a comparison of the original portfolio in STEP4 and the divested portfolio in STEP5

![alt text](https://github.com/QuantFILab/Divfolio/blob/main/Figures/Step5_page.png?raw=true)

Looking at the last boxplots, they tell us that with this divestment schedule, the return dropped significantly whereas the environmental score improved a little. Therefore, this divestment strategy may not suit profit seekers. You can try a different types of portfolios or divestment schedules for comparison.

## Examples 2: Uploading CSV files

For STEP 2 to STEP 5, if you already have CSV files in the given form (see in application and examples in [Example Files](https://github.com/QuantFILab/Divfolio/tree/main/Example%20Files)), you can upload those files without retrieving data online by Clicking on the "Check" icon and upload a file like in the figure below. the files need to be uploaded sequentially. For example, F2 can not be uploaded unless F1 was updated. For matching of files and STEPs please look at the diagram in **Workflow**.

![alt text](https://github.com/QuantFILab/Divfolio/blob/main/Figures/Check.png?raw=true)


## Examples 3: Using Multiple Comparison in Option I to Option III

These options required CSV files, see example in [Example Files](https://github.com/QuantFILab/Divfolio/tree/main/Example%20Files). Here, we use a small portfolio in which the constituents selected from FTSE100 as an example. Here, we investigate the impact of the divestment schedule on the rsik profiles and stability of them. the comparison consists of the benmark portfolio (Passive Equal Weight), the linear (slow) divestment, the hyberbolic (Fast) divestment and the instant divestment.

### Option I  Multiple Portfolios Comparison

First, upload the required files F5, F1, and F2 and click on "submit".

![alt text](https://github.com/QuantFILab/Divfolio/blob/main/Figures/OptionI_setup.png?raw=true)

The results are the comparison of the ESG scores and risk profiles of the input portfolios.

![alt text](https://github.com/QuantFILab/Divfolio/blob/main/Figures/OptionI_page.png?raw=true)

Looking at the last panel in boxplots, the instant divestment yielded the most improvement (distribution) of the environmental score but it costs by dropping in return and other ESG scores both social and governance. If you mind the return and other scores rather than the E score, you shouldn't select this strategy. In contrast, if you mind the E score the hyperbolic divestment seems to be suited for you. The best divestment strategy depended on you final goal. 

### Option II Stability Analysis via Clustering

1. Upload the required files, F5 
2. Upload the required files, F2
3. Set the window of the smoothing risk profiles (Here we set 20 days)
4. Set the window of clustering (Here we set 20 days)
5. Click on "submit".

![alt text](https://github.com/QuantFILab/Divfolio/blob/main/Figures/OptionII_setup.png?raw=true)

The results are the smoothed rsik profiles and the clustering results.

![alt text](https://github.com/QuantFILab/Divfolio/blob/main/Figures/OptionII_page.png?raw=true)

Looking at the last plots of the heatmap, you will see the different colors on the time series of the cluster label of the divested portfolio and the benchmarks. That indicates the divestment changes the relative behavior of that risk profile. The linear divestment seems to be the best tracking on the benchmark as the labels of the clusters are quite similar to the benchmark while the other divested portfolio yields the series of diferent labels.

### Option III  Graph Structure Correlation Analysis

1. Upload file F5.
2. Upload file F6 as an option (can be replaced by F1).
3. Upload file F2.
4. Select the date of rebalancing you want to see the correlation structure.
5. Click on "submit" to process. The process is time-consuming as an optimization of the regularization parameter. Please be patient. 
![alt text](https://github.com/QuantFILab/Divfolio/blob/main/Figures/OptionIII_setup1.png?raw=true)
![alt text](https://github.com/QuantFILab/Divfolio/blob/main/Figures/OptionIII_setup2.png?raw=true)

The results are the robust correlation structure of the portfolio at the given time. We allow three cross-sectional graphs for comparison as the limit of visualization.  

![alt text](https://github.com/QuantFILab/Divfolio/blob/main/Figures/OptionIII_page.png?raw=true)

Here, the linear and hyperbolic divestments do not change the correlation structure of the portfolio in the early states. The correlation structure will be changed at the terminal states when some assets were divested completely. 

