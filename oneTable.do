program oneTable 
	syntax varlist(max=1 numeric) [if] [in], mean(varlist) footnote(string) title(string) name(string)
	
	*Check the sample and make sure it's not N=0
	*quietly {
	marksample touse
	markout `touse' `by', strok
	count if `touse'
	if r(N)==0 error 2000
		
	tokenize `varlist'
	local firstCat = "`1'"
	summ `firstCat'
	*If we're dealing with binary variables that could be zero, add one so that we have 1/2 variables
	tempvar tmp1
	if r(min)==0 gen `tmp1' = `firstCat'+1
	summ `tmp1'
	local numRows = r(max)
	
	di `secondCat'
	summ `secondCat'
	
	*if r(min)==0 local numCols = r(max) + 1
	*if r(min)>0 local numCols = r(max) 
	tempvar tmp2
	if r(min)==0 gen `tmp2' = `secondCat'+1
	summ `tmp2'
	local numCols = r(max)
	
	*display "a `N'"
	*di "b `M'"
	
	*group1 and group2 are variables that range from 1...number of categories of variables `firstCat' and `firstCat'
	tempvar group1 
	egen `group1' = group(`firstCat') if `touse' 
	tempvar group2 
	egen `group2' = group(`secondCat') if `touse'
	
	levelsof `group1', local(numG1)
	levelsof `firstCat', local(numG1Old)
	tokenize `numG1Old'
	
	levelsof `group2', local(numG2)
	levelsof `secondCat', local(numG2Old)
	tokenize `numG2Old'
	
	local cmd1 = "lab def first"
	local cmd2 = "lab def second"
	foreach i of local numG1 {
		local a : value label `firstCat'
		local lab1 : label `a' `=`i'-1'
		di "aaa`lab1'"
		local cmd1 = `"`cmd1' `i' "`lab1'""'
	}
	foreach i of local numG2 {
		local a : value label `secondCat'
		local lab2 : label `a' `=`i'-1'
		di "aaaa`lab2'"
		local cmd2 = `"`cmd2' `i' "`lab2'""'
	}
	capture lab drop first
	capture lab drop second
	`cmd1'
	`cmd2'
	lab val `group1' first
	lab val `group2' second
		
	local vertVar = "`group1'"
	*di "aaaaaa"
	di "`vertVar'"
	local horVar = "`group2'"
	
	*di "`mean'" 
	
	*numVariables is the number of variables to take the mean of
	local numVariables : word count `mean'
	*di `numVariables'
	
	tokenize `mean'
	*di "`firstCat'"
	
	*}	

	*Loop over all the variables we want the means of 
	forvalues i=1/`numVariables' {
		*need extra room for sample size and extra columns to be listed beyond M and N the number in each direction
		local bignumRows = `numRows'+1
		local bignumCols = `numCols'+1
		matrix mean`i' = J(`bignumRows',`bignumCols', 1)
		*freq is the frequency in that box
		matrix freq = J(`bignumRows', `bignumCols', 1)
		*CHANGE DIFFERENCE: the numCols Matrix will take sample size for the calculation of the LAST mean. see below
		matrix N = J(`bignumRows',`bignumCols', 1)
		*N is the number with the variable we're taking the mean off
		forvalues j=1/`numRows' {
			forvalues k=1/`numCols' {
		*sum `group'
		*local numGroups = r(max)
		*forvalues j=1/numGroups {
				local varLab`i' : variable label ``i''
				quietly summ ``i'' if `group1'==`j' & `group2'==`k'
				matrix mean`i'[`j', `k'] = r(mean)
				matrix N[`j',`k']=r(N)
				quietly count if `group1'==`j' & `group2'==`k'
				matrix freq[`j', `k'] = r(N)
				local vertValLabs : value label `vertVar'
				local horValLabs : value label `horVar'
				local vertCat`j' : label `vertValLabs' `j' 
				local horCat`k' : label `horValLabs' `k'
				local vertVarLab : variable label `vertVar'
				local horVarLab : variable label `horVar'
				quietly summ ``i'' if `group2'==`k' & `group2'!=.
				matrix mean`i'[`bignumRows',`k'] = r(mean)
				matrix N[`bignumRows',`k']=r(N)
				quietly count if `group2'==`k' & `group1'!=.
				matrix freq[`bignumRows', `k'] = r(N)
				local vertCat`bignumRows'="Total"
		*}
			}
			quietly summ ``i'' if `group1'==`j' & `group2'!=.
			matrix mean`i'[`j', `bignumCols'] = r(mean) 
			matrix N[`j',`bignumCols']=r(N)
			quietly count if `group1'==`j' & `group2'!=.
			matrix freq[`j', `bignumCols'] = r(N)			
			local horCat`bignumCols' = "Total"
		}
	quietly summ ``i'' if `group1'!=. & `group2'!=.
	matrix mean`i'[`bignumRows', `bignumCols'] = r(mean)
	matrix N[`bignumRows',`bignumRows']=r(N)
	quietly count if `group1'!=. & `group2'!=.
	matrix freq[`bignumCols', `bignumRows'] = r(N)
	matrix list mean`i'
	}

local ccc : display _dup(`bignumCols') "c"

local Cols = "&"
forvalues i=1/`bignumCols' {
	local Cols = "`Cols'" + "&" + "\textbf{`horCat`i''}"
}
local Cols = "`Cols'" + "&" + "\textbf{Difference}"
local tabBody = ""
forvalues i=1/`bignumRows' {
	local iminus = `i'-1
	local tabBody = "`tabBody'" + "\textbf{`vertCat`i''}" 
	forvalues j = 1/`numVariables' {
		local tabBody = "`tabBody'&" + "Mean `varLab`j''"
		forvalues k = 1/`bignumCols' {
			local tabBody = "`tabBody'&" + "`=round(mean`j'[`i', `k'], .001)'"
		}
	local tabBody = "`tabBody'"+ "&`=round(mean`j'[`i', 2] - mean`j'[`i', 1] , .001)'"
	local tabBody = "`tabBody'"+"\\"
	}
	local tabBody = "`tabBody'"+"&N"
	forvalues k = 1/`bignumCols' {
		local tabBody = "`tabBody'" + "&`=N[`i', `k']'"	
	}
	local tabBody = "`tabBody'"+"\\&Frequency"
	forvalues k = 1/`bignumCols' {
		local tabBody = "`tabBody'" + "&`=freq[`i', `k']'"	
	}
	local tabBody = "`tabBody'"+"\\\\"
}

local keyText = ""
forvalues i=1/`numVariables' {
	*di "f"
	local keyText = "`keyText'" + "Mean `varLab`i''" +  char(10)
}
local keyText = "`keyText'" + "Frequency"


#delimit ;
file open Output using "`name'.tex", write replace;
file write Output "\begin{table}[!htbp] \centering \caption{`title'} \medskip 
\begin{tabular}{ll`ccc'c} \hline \hline
`Cols' \\ \hline
`tabBody'
\hline
\end{tabular} 
\flushleft `footnote' \\
Table Key: \\
\begin{boxedverbatim}
`keyText'
\end{boxedverbatim} 
\vspace{.5cm}"  _n
"\end{table}";
file close Output;



/*\begin{table}[!htbp] 
\caption{Repeaters/Non-repeaters in Government/Private Schools: Test Scores} 

\centering \medskip \begin{tabular}{l*{5}{r}}

& \bf{Promoted} & \bf{Not Promoted} & \bf{Total} \\
\bf{Private} 		& 1.305   	& .830	&  1.292 \\
 				&  2566     	&   72    		&   2638 \\
				&97.27       	&2.73 		&100.00 \\
\hline
 \bf{Government}  	& .701 	&  -.282 	& .593 \\
				& 6744   		&     829   		&  7573 \\
				&89.05      	&10.95		&100.00 \\
\hline
\bf{Total} 			& .867  	& -.193 	& .774 \\
				&  9310  		&     901   		&   10211 \\
				&91.18       	&8.82		& 100.00 \\
\end{tabular}\\

\end{table}
*/
/*\hline Attock & `attockMean' & `attockSD' &`attockN'  \\
Faisalabad & `faisMean' & `faisSD' & `faisN'\\ Rahim Yar Khan & `rahimMean' & `rahimSD' &`rahimN'\\
Total & `totalMean' & `totalSD' &`totalN' \\ \hline */


end;

	
