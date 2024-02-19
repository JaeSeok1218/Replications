# Main Model

* Gini coefficient: The measure of price dispersion

$Gini_{ijt} = \beta_{1}HHI_{jt} + \beta_{2}HHI_{jt}^{2} + \alpha X_{it} + \varepsilon_{ij} + \varepsilon_{t} + u_{ijt}$

* Variables
  - $HHI_{j}$: route's market concentration
  - $X_{it}$: time-varying carrier control variables
  - $\varepsilon_{ij}$: time-invariant (route-carrier)
  - $\varepsilon_{t}$: time-variant (year-quarter)

* Quadratic(Non-linear) effects of market concentration on price: $\beta_{1} > 0$ and $\beta_{2}<0$

* Fixed Effects model with time fixed effects
  - Individual fixed effects: route-carrier
  - Time fixed effects: year-quarter
  - By employing 'time-varying carrier control variables', this paper controls time-varying individual effects.

* Endogeneity Issue
  - e.g. Airlines might hesitate to enter the route when the prices are dispersed in low :arrow_right: Instrument Variables (see Appendix)

# Tables

<div id="Table 1 and 2">
  <a href="./Table1.R">
    <img src="./Results/Table1.png" title="Table 1" alt="Table 1" width="300.3" height="181"/>
  </a>
  <a href="./Table2.R">
    <img src="./Results/Table2.png" title="Table 2" alt="Table 2" width="300.3" height="201"/>
  </a>
</div>

<div id="Table 3 and 4">
  <a href="./Table3.R">
    <img src="./Results/Table3.png" title="Table 3" alt="Table 3" width="300.3" height="201"/>
  </a>
  <a href="./Table4.R">
    <img src="./Results/Table4.png" title="Table 4" alt="Table 4" width="300.3" height="251"/>
  </a>
</div>

<div id="Table 5 and 6">
  <a href="./Table5.R">
    <img src="./Results/Table5.png" title="Table 5" alt="Table 5" width="300.3" height="181"/>
  </a>
  <a href="./Table6.R">
    <img src="./Results/Table6.png" title="Table 6" alt="Table 6" width="300.3" height="180"/>
  </a>
</div>
