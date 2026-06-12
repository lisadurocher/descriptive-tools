# descriptive-tools
Several functions for descriptive analyses (tables, figures, etc.)


<ul>

<details>

<summary>Tableau descriptif</summary> 

# Fonction ‘TableDesc’

TableDesc : To create a comparative table with a treatment and tests.


### Description

This function calculates descriptive statistics for continuous and categorical variables.

### Usage

TableDesc(data, vars=NULL, names=NULL, trt=NULL, virg=0, virg.percent=0, nolevels=NULL, nonnormal=NULL, nrpval=4, boldpval=0.05, boldsmd=0.1, reference=TRUE, export=FALSE, pvalue=TRUE, smd=FALSE, missing=FALSE, paired=FALSE, legend=TRUE, binary01=FALSE)

### Arguments


<table style="border-collapse: collapse; width:100%; ">

<tr style="background:none;">
<td style="border:none;">data</td>
<td style="border:none;">An object of the class <i>data.frame</i> containing the variables cited in <i>vars</i>.</td>
</tr>

<tr style="background:none;">
<td style="border:none; ">vars</td>
<td style="border:none; ">A list of variable names to be described. If NULL, all the variable in <i>data</i> will be described except those with more than 20 levels (date, subject id …).</td>
</tr>

<tr style="background:none;">
<td style="border:none; ">names</td>
<td style="border:none; ">A list of labels for the variables. If NULL, <i>vars</i> will be used.</td>
</tr>

<tr style="background:none;">
<td style="border:none; ">trt</td>
<td style="border:none; ">A character string with the name of the comparative variable.</td>
</tr>

<tr style="background:none;">
<td style="border:none; ">virg</td>
<td style="border:none; ">Number of decimal for continuous variables. The default value is 0.</td>
</tr>

<tr style="background:none;">
<td style="border:none; ">virg.percent</td>
<td style="border:none; ">Number of decimal for percentages. The default value is 0.</td>
</tr>

<tr style="background:none;">
<td style="border:none; ">nolevels</td>
<td style="border:none; ">A list of variables with only 2 levels which will be described on a single line and the first level will be described, thus relevel needed.</td>
</tr>

<tr style="background:none;">
<td style="border:none; ">nonnormal</td>
<td style="border:none; ">A list of variables not normally distributed. Specify "none" if every continuous variable is normally distributing and "all" if every continuous variable is non-normally distributing. See details to know how variables will be described according to their distribution.</td>
</tr>

<tr style="background:none;">
<td style="border:none; ">nrpval</td>
<td style="border:none; ">Number of decimals to be used for the pvalue. The default value is 4.</td>
</tr>

<tr style="background:none;">
<td style="border:none; ">boldpval</td>
<td style="border:none; ">The statistical significance for p-value. Pvalues lower than set level will be in bold. The default value is 0.05.</td>
</tr>

<tr style="background:none;">
<td style="border:none; ">boldsmd</td>
<td style="border:none; ">The SMD threshold to considering an unbalance. SMD upper than set level will be in bold. The default value is 0.1 (10%).</td>
</tr>

<tr style="background:none;">
<td style="border:none; ">reference</td>
<td style="border:none; ">TRUE by default, any other value will remove the ", n (%)" after the label for categorical variables.</td>
</tr>

<tr style="background:none;">
<td style="border:none; ">export</td>
<td style="border:none; ">A logical value specifying if a word document with the descriptive table should be exporting. Default is FALSE.</td>
</tr>

<tr style="background:none;">
<td style="border:none; ">pvalue</td>
<td style="border:none; ">A logical value specifying if the pvalue should be add to compared groups. See details to know which test will be used. Default is TRUE.</td>
</tr>

<tr style="background:none;">
<td style="border:none; ">smd</td>
<td style="border:none; ">A logical value specifying if the standardized differences should be estimating. Default is FALSE.</td>
</tr>

<tr style="background:none;">
<td style="border:none; ">missing</td>
<td style="border:none; ">A logical value specifying if the missing values should be describing. Default is TRUE.</td>
</tr>

<tr style="background:none;">
<td style="border:none; ">paired</td>
<td style="border:none; ">A logical value indicating whether you want paired test. Default is FALSE.</td>
</tr>

<tr style="background:none;">
<td style="border:none; ">legend</td>
<td style="border:none; ">A logical value indicating whether a footnote should be printed to indicate which tests were used. Default is TRUE.</td>
</tr>

<tr style="background:none;">
<td style="border:none; ">binary01</td>
<td style="border:none; ">A logical value indicating if binary variables are expressed as 0/1. Default is FALSE.</td>
</tr>

</table>

### Details

This function returns a flextable object. If the continuous variable is normally distributed, mean and standard deviation will be presented and groups compared using a Student t-test. Else, median and 25th-75th percentile will be presented and groups compared using a Mann-Whitney test. If nonnormal = NULL, a Shapiro test will be performed to qualify the distribution. Categorical variable will be presented as effectives and percentage and compared according to the group using a Chi-2 test or Fisher exact tests if expected numbers of patients are less than 5.

### Examples

```r
source("https://raw.githubusercontent.com/lisadurocher/TableDesc/main/TableDesc.R")

data("mtcars")

mtcars$am <- factor(mtcars$am, levels=c(0,1), labels = c("Automatic","Manual"))

mtcars$cyl <- factor(mtcars$cyl, c(4,6,8))

mtcars$vs <- factor(mtcars$vs, levels = c(1,0))

vars <- c("mpg","cyl","hp","vs")

names <- c("Miles/(US) gallon", "Number of cylinders", "Gross horsepower", "V/S")

nolevels <- c("vs")

TableDesc(data=mtcars, vars=vars, names=names, trt="am", virg=1, nolevels=nolevels, nonnormal = "none", smd = TRUE, export = FALSE)
```
Simple version:
```r
TableDesc(data=mtcars, TRT="am")
```

</details>
</ul>
