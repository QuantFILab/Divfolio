# Divfolio

The primary objective of the software is to facilitate the creation and evaluation of portfolios that take into account decarbonization and ESG investing and divestment practices in line with climate change prevention trends emerging in wealth management. The software enables the creation of divestment plans and sustainable portfolios, as well as the comparison of the corresponding risk/return profiles, ESG scores, and bespoke portfolio variables, such as carbon intensity. The evaluation is based on simulations that collect and include pertinent historical market data before and after divestment, while also taking into account varying divestment planning rates. The software permits the user to implement these evaluations on publicly traded equities assets, exchange-traded funds (ETFs), exchange-traded notes (ETNs), and Depositary Receipts (DR). Alternatively, Divfolio can be used for general proposes, such as portfolio comparison, gathering ESG scores, and downloading historical return data.

![alt text](https://github.com/QuantFILab/Divfolio/blob/main/Figures/openpage.png?raw=true)

On the tab on the left, the <b> User Guide </b> provides detailed instructions on how to use this application. The application's functions are documented in <b> Divestment Plan </b>. Our contact information can be found in <b> Our Team </b>. We welcome the opportunity to collaborate with scholars and industry professionals interested in relevant issues and comparable Shiny applications. To report technical problem, please contact Pasin Marupanthorn at pm122@hw.ac.uk.

## Original Research Papers related to Divfolio

Divfolio is baed on the research results from two papers. Preprint versions of both papers are open access available on SSRN. First, the methodology paper explains all mathematical details in portfolio optimization, [Mechanisms to Incentivise Fossil Fuel Divestment and Implications on Portfolio Risk and Returns](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4131449). Secondly, the software paper explains how to use Divolio aloge with an example in FTSE100 divestment to improve ESG scores, [DivFolio: A Shiny Application for Portfolio Divestment in Green Finance Wealth Management](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4131449).


## Where to find ticker?

If you are interested in downloading historical data from Divfolio, you will need the Yahoo! Finance ticker. You may find the tickers [here](https://finance.yahoo.com/screener/new). Note that the ESG scores are available for certain stocks. The ESG scores are not available for others assets, but the historical data may be available.


## Workflow

DivFolio App tool comprises two core components as will be detailed in the panels below. The first component involves a set of sequential STEPs that users undertake consisting of STEP 1 through to STEP 5, which must be performed sequentially. The second component involves a collection of three independent optional STEPs that can be performed individually. For the optional steps, so long as the user has the relevant data prepared in CSV format already, for instance as output from STEPS 1 to 5 in Component 1, then they can upload this directly into any of the STEPS in Component 2 instead of generating them again from the tool. For more details please see user guide on the apllication. 

![alt text](https://github.com/QuantFILab/Divfolio/blob/main/Figures/daig.jpg?raw=true)


## Functions in Divolio

STEP 1: Accessing the ESG scores and some profiles of the individual company.

STEP 2: Downloading ESG scores of the selected companies, and exporting to local machine in csv and png.

STEP 3: Downloading historical returns of the selected companies, comparing the performances, and exporting to local machine in csv and png.

STEP 4: Generating portfolio from the selected companies, rebalancing portfolio, summarising the performances and exporting to local machine in csv and png.

STEP 5: Generating divestment schedule, divesting portfolio, rebalancing portfolio, summarising and comparing the performances and exporting to local machine in csv and png.


Option I: For comparing more than two portfolios, generating divestment schedule, divesting portfolio, rebalancing portfolio, summarising and comparing the performances, and exporting to local machine in csv and png.

Option II: Clustering portfolio according to their risk profile, and exporting to local machine png.

Option III: Proveing ghaph LASSO to portfolios over time to investigate covarience between assets during divestment, and exporting to local machine png.

## Examples 1: Generate Portfolio on Application 



### STEP 1 
![alt text](https://github.com/QuantFILab/Divfolio/blob/main/Figures/firststep.png?raw=true)


## Examples 2: Uploeading CSV. file
