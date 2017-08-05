*program define fixBinary, rclass
*	return numCols, numRows, firstCat, secondCat
*end;

program define fixBinary
	syntax varlist(max=2 min=2 numeric) [if] [in], num(real)
	
	local new = "`1'"
	local old = "`2'"

	capture lab drop label`num'
	
	levelsof `new', local(numNew)
	levelsof `old', local(numOld)
	tokenize `numOld'
	local cmd = "lab def label`num'"
	foreach i of local numNew {
		local a : value label `old'
		local lab : label `a' `=`i'-1'
		di "aaa`lab'"
		local cmd = `"`cmd' `i' "`lab'""'
	}
	`cmd'
	lab val `new' label`num'
	tab `new'
	tab `old'
end;

program define twoByTwo 
	syntax varlist(max=2 numeric) [if] [in], mean(varlist) footnote(string) title(string) name(string)
	
	*Check the sample and make sure it's not N=0
	*quietly {
	marksample touse
	markout `touse' `by', strok
	count if `touse'
	if r(N)==0 error 2000
	
	local numCat : word count `varlist'
	
	tokenize `varlist'
	
	local firstCat = "`1'"	
	summ `firstCat'
	*If we're dealing with binary variables that could be zero, add one so that we have 1/2 variables
	if r(min)==0 local numRows = r(max)+1
	if r(min)>0 local numRows = r(max)
	
	
	if `numCat'==2 {
		local secondCat = "`2'"
		summ `secondCat'
		if r(min)==0 local numCols = r(max) +1
		if r(min)>0 local numCols = r(max)
	}

	
	*group1 and group2 are variables that range from 1...number of categories of variables `firstCat' and `firstCat'
	tempvar group1 
	egen `group1' = group(`firstCat') if `touse' 
	tempvar group2 
	egen `group2' = group(`secondCat') if `touse'
	
	tab `group1'
	tab `group2'
	
	
	*Used to reassign the old labels to the new variables
	fixBinary `group1' `firstCat' , num(1)
	fixBinary `group2' `secondCat' , num(2)
	
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
				local vertValLabs : value label `group1'
				local horValLabs : value label `group2'
				local vertCat`j' : label `vertValLabs' `j' 
				local horCat`k' : label `horValLabs' `k'
				local vertVarLab : variable label `group1'
				local horVarLab : variable label `group2'
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

	
