# Horticultural-Case-Study
The profitability of horticultural peat industry
Economic Opportunity Costs of Peatland Conservation from Horticultural Peat Extraction in Alberta: NPV Analysis under Uncertainty
The profitability of horticultural peat extraction is quantified with the NPV model and Monte Carlo simulation for uncertainty modelling
•	Simulated peat prices (CAD/m3) 
o	Quantity shipped (kilotons) and Value of shipments (CAD000) from 1990 to 2024 were obtained from Statistics Canada: https://mmsd.nrcan-rncan.gc.ca/prod-prod/ann-ann-eng.aspx?FileT=2024&Lang=en 
o	Peat prices (CAD/t) in Alberta were calculated by dividing the value of shipments by quantity shipped. The prices were converted from CAD/t to CAD/m3 using a 0.168 t/m3 bulk density.
o	Peat prices (CAD/m3) were inflation-adjusted to 2024 CAD.
o	Fitted probability distribution (lognormal, Weibull, Normal, Log-logistics, Uniform and Gamma distribution) to inflation-adjusted peat prices
o	Lognormal is the best-fitting distribution 
o	Peat prices were simulated for 18 years with 1,000 iterations. 
•	Simulated peat harvested volume (m3/ha)
o	Based on Canadian industry data, the typical peat-harvest thickness averages 8.6 cm (range 5 - 14 cm/yr). 
o	Annual peat harvested volumes (m3/ha) are estimated based on harvest depth per year, using peat-harvesting depth 0.086 m (8.6 cm) multiplied by 10,000 m2/ha
o	Peat volumes were assumed to have a triangular distribution and simulated over an 18-year horizon using 1,000 Monte Carlo simulations 
•	Simulated peat harvesting operating costs in Alberta: Peat extraction cost data
https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=1610003101
o	Peat-harvesting operating costs include fuel & electricity, materials & supplies and employee salaries and wages are obtained from Natural Resource Canada and Statistics Canada for Alberta from 1998-2015, for Canada 1996-2023. 
o	To extend the Alberta cost series beyond their observed range (1998-2015), I employed an econometric calibration against the Canada cost series (1996-2023). The approach is designed to generate consistent Alberta estimates for 2016-2023 while keeping observed Alberta dynamics (1998-2015).
o	The operating costs (CAD/m3) were calculated for CAD/m3 by dividing the costs (CAD000) by the quantity shipped (kilotons) and converted to CAD/m3 using a bulk density of 0.168 t/m3. 
o	The costs are also inflation-adjusted to 2024 CAD. 
o	Aggregating components into a total operating cost and fitting a triangular distribution, and simulated over an 18-year horizon using 1,000 Monte Carlo simulations 
•	Machinery Costs
o	Basic equipment list for information for a 100-ha peat harvesting site is obtained from internal Premier Tech expert communication
o	Unit-level minimum and maximum of each item are obtained from the current market website. 
o	The total equipment price, from CAD2,051,500 to CAD2,653,000, was the initial cost for peat harvesting, and the cost was simulated using a Uniform distribution over an 18-year horizon using 1,000 Monte Carlo simulations 
•	The royalty fee for harvesting peat in Alberta
o	Alberta’s royalty fee is modelled as a production-based fee payable to the Crown, applied to each cubic metre of harvested peat at the statutory rate of CAD0.144 per cubic metre.
•	Restoration costs
o	The average restoration cost is CAD3,485/ha for the industry. 
•	Freight costs (leg 1: bog to plant)
o	Premier Tech harvesting sites in Alberta are found on their website. Locating the harvesting sites based on the GIS in the HFI of peat mining and using Google Maps to measure the distance from the harvesting sites to the Olds plant. 
o	CAD2/km from bog to plant, and 125 m3 truckload capacity.
o	Randomly pick the bog distance to plant and estimate the freight costs. 
•	Freight costs (leg 2: plant to customer)
o	California (USA) and British Columbia (Canada) are key markets for horticultural peat (internal Premier Tech expert communication). We assume all production is shipped to these two markets. 
o	Distributor locations in both regions were identified from the Premier Tech website in these two regions.
o	The distance from the plant to distributors is estimated by Google Maps: British Columbia (1023 km), California (1964, 2148, 2284 km). 
o	Using distributor location in BC (Surrey) and CA (Glenn, Willits, Gilroy) with CAD0.66/km, 3.8 ft3/bales, 30 bales/pallet at 22 pallets/truck (1 m3 = 35.3 ft3, 3.8 * 30 * 22/35.3 = 71 m3/TL). 
o	Randomly assign the customer market (California with probability 0.5; otherwise, BC). If California is chosen, draw one of the three CA distances with equal probability. 
Variance Decomposition of Horticultural NPV Uncertainty (hold-one-input-fixed) 
To attribute the dispersion of the per-hectare NPV to its major stochastic drivers - prices, harvest volume, and freight costs - we implement a hold-one-input-fixed experiment. The idea is to compare the variance of simulated NPV under the baseline model (all drivers stochastic) to counterfactual runs in which one driver is replaced by its deterministic per-year mean path while the other drivers remain stochastic. 
•	Discount rate: 7%
•	Depreciation rate for equipment: 8% per year (for salvage value calculation)
•	Initial costs:
o	Machinery cost: a random draw from a Uniform distribution: (CAD2,051,500, CAD2,653,000) for each simulation 
o	Environmental application cost: a random draw from a Uniform distribution (CAD300,000, CAD1,000,000) for each simulation
•	For each iteration, we calculate the net cash flow as follows:
o	Revenue = Prices (CAD/m3) * Yield (m3)
o	Variable costs = Operating costs (fuel & electricity, materials & supplies and salaries, CAD/m3) * Yield (m3)
o	Royalty fee = CAD 0.144/m3 * Yield (m3)
o	Net cash flow = Revenue - Variable costs - Restoration costs (in year 18 only) - Royalty fee - Freight costs 
