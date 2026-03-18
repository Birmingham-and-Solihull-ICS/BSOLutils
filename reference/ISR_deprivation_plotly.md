# Create an interactive Plotly bar chart for ISR deprivation output

This function creates an interactive bar chart from the output of
[`ISR_deprivation()`](https://birmingham-and-solihull-ics.github.io/BSOLutils/reference/ISR_deprivation.md),
showing indirectly standardised ratio estimates by IMD quintile with
confidence intervals. IMD quintile 1 is excluded from the chart because
it is the reference group used to calculate the ratios.

## Usage

``` r
ISR_deprivation_plotly(
  .dt,
  description = "Myocardial Infarction (Under 75 yrs) - 2024/25",
  measure_title = "Age-standardised Admission Ratio Ratios"
)
```

## Arguments

- .dt:

  A data frame produced by
  [`ISR_deprivation()`](https://birmingham-and-solihull-ics.github.io/BSOLutils/reference/ISR_deprivation.md)
  containing the following variables:

  imd_quintile

  :   IMD quintile identifier.

  ratio

  :   Indirectly standardised ratio for each IMD quintile relative to
      IMD 1.

  lowerCI

  :   Lower bound of the confidence interval for the ratio.

  upperCI

  :   Upper bound of the confidence interval for the ratio.

- description:

  A character string to include in the plot title, typically describing
  the indicator, age group, and reporting period. Default is Myocardial
  Infarction (Under 75 yrs) - 2024/25

- measure_title:

  A character string used as the main title prefix for the plot, for
  example `"Age-standardised Admission Ratio Ratios"` or
  `"Age-sex-standardised Admission Ratio Ratios"`.

## Value

A `plotly` htmlwidget object.

## Details

The plot includes:

- Bars for IMD quintiles 2, 3, 4, 5, and 999 (All Persons)

- Asymmetric error bars based on the lower and upper confidence
  intervals

- A horizontal reference line at ratio = 1

- Interactive hover text showing the ratio and confidence interval
  values

IMD quintile 1 is removed before plotting because it is the reference
category against which the other quintiles are compared. The horizontal
dashed line at ratio = 1 indicates equality with the rate in IMD
quintile 1.

IMD quintile 999 is displayed as *All Persons*.

## See also

[`ISR_deprivation`](https://birmingham-and-solihull-ics.github.io/BSOLutils/reference/ISR_deprivation.md)

## Examples

``` r
data(ISR_example)

standardised_dep <- ISR_deprivation(ISR_example)
#> Waiting for profiling to be done...

ISR_deprivation_plotly(
  .dt = standardised_dep,
  description = "Myocardial Infarction (Under 75 yrs) - 2024/25",
  measure_title = "Age-standardised Admission Ratio Ratios"
)

{"x":{"visdat":{"1ce5495d5d2a":["function () ","plotlyVisDat"]},"cur_data":"1ce5495d5d2a","attrs":{"1ce5495d5d2a":{"x":{},"y":{},"text":{},"hoverinfo":"text","textposition":"none","marker":{"color":["#66c2a5","#fc8d62","#8da0cb","#e78ac3","#a6d854"]},"error_y":{"type":"data","symmetric":false,"array":{},"arrayminus":{},"color":"black"},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":40,"l":60,"t":100,"r":10},"title":{"text":"Age-standardised Admission Ratio Ratios: Myocardial Infarction (Under 75 yrs) - 2024/25<br><span style='font-size:12px'><i>A ratio of 1 means the rate = the rate in Quintile 1<\/i><\/span>","font":{"size":14}},"showlegend":false,"xaxis":{"domain":[0,1],"automargin":true,"title":"IMD Quintile (999 = All Persons)","categoryorder":"array","categoryarray":["2","3","4","5","999"],"type":"category"},"yaxis":{"domain":[0,1],"automargin":true,"title":"Indirectly Standardised Ratio"},"shapes":[{"type":"line","xref":"paper","x0":0,"x1":1,"yref":"y","y0":1,"y1":1,"line":{"color":"red","width":2,"dash":"dash"}}],"hovermode":"closest"},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":["2","3","4","5","999"],"y":[1.0342908752964377,1.0805486697430986,1.0284716675159649,1.060090163456169,0.94371505524036625],"text":["IMD Quintile: 2<br>Ratio: 1.03<br>Lower CI: 0.92<br>Upper CI: 1.16","IMD Quintile: 3<br>Ratio: 1.08<br>Lower CI: 0.97<br>Upper CI: 1.21","IMD Quintile: 4<br>Ratio: 1.03<br>Lower CI: 0.92<br>Upper CI: 1.15","IMD Quintile: 5<br>Ratio: 1.06<br>Lower CI: 0.95<br>Upper CI: 1.19","IMD Quintile: 999<br>Ratio: 0.94<br>Lower CI: 0.84<br>Upper CI: 1.06"],"hoverinfo":["text","text","text","text","text"],"textposition":["none","none","none","none","none"],"marker":{"color":["#66c2a5","#fc8d62","#8da0cb","#e78ac3","#a6d854"],"line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"black","type":"data","symmetric":false,"array":[0.12694433033798735,0.12964614580503198,0.12273930929499555,0.12491619723979541,0.11137233062327556],"arrayminus":[0.11298935527797149,0.11554539436531908,0.10942249829067563,0.11144560962526795,0.099355056295908684]},"type":"bar","error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
