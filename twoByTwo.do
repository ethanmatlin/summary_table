program define fixBinary
	syntax varlist(max=2 min=2 numeric) [if] [in], num(real)
	
	local new = "`1'"
	local old = "`2'"

	capture lab drop label`num'
	
	levelsof `new', local(numNew)
	local cmd = "lab def label`num'"
	foreach i of local numNew {
		local a : value label `old'
		if "`a'"!="" {
			local lab : label `a' `=`i'-1'
			local cmd = `"`cmd' `i' "`lab'""'
		}
		else {
			local cmd = ""
		}
	} 
	`cmd'
	lab val `new' label`num'
end;

program define twoByTwo 
	syntax varlist(max=2 numeric) [if] [in], mean(string) title(string) name(string) label(string) [sd freq firstTotal secondTotal]
	
	*Note: Freq gives the number in that box (i.e. at the intersection of two categories for example the number of girls in 10th grade) whereas N gives the sample size 
	*for computation of statistics (i.e. the intersection of the two categories with nonmissing data for the variable of interest for example the number of girls in 10th grade with 
	*nonmissing test scores).
	
	*=========================================
	*Setup====================================
	*=========================================
	
	*Ensure Sample size>0
	marksample touse
	markout `touse' `by', strok
	count if `touse'
	if r(N)==0 error 2000
	
	*Get dimension of tagble (1- or 2-D then tokenize varlist of categories/axes
	local numCat : word count `varlist'
	tokenize `varlist'
	*Get number of columns by checking the Max value of the first variable
	summ `1'
	*If we're dealing with binary variables that could be zero, add one so that we have 1/2 variables
	if r(min)==0 local numCols = r(max)+1
	if r(min)>0 local numCols = r(max)
	*If second variable, get number of rows by checking the Max value of the second variable. Otherwise, only one row
	if `numCat'==2 {
		summ `2'
		if r(min)==0 local numRows = r(max) +1
		if r(min)>0 local numRows = r(max)
	}
	else {
		local numRows=1
	}

	*group1 and group2 shift the category variables to start at one. Makes the rest of the program easier with indexing and labels.
	tempvar group1 
	egen `group1' = group(`1') if `touse' 
	tempvar group2 
	egen `group2' = group(`2') if `touse'
	*Reassign the old labels to group1 and group2
	
	fixBinary `group1' `1' , num(1)
	if `numCat'==2 {
		fixBinary `group2' `2' , num(2)
	}
	*Numver of variables we want statistics of in our table (mean, sd, etc.)
	local numVariables : word count `mean'
	
	tokenize `mean'
	
	*Need an additional 'total column'	
	if "`firstTotal'"!="" {
		local bignumCols = `numCols'+1
	}
	else {
		local bignumCols = `numCols'
	}
	
	*If there's a second variable, need an additional 'total' row.
	if `numRows' > 1 & "`secondTotal'"!="" {
		local bignumRows = `numRows'+1
	}
	else {
		local bignumRows = `numRows'
	}
	
	
	*=========================================
	*Preparing matrices of all relevant values
	*=========================================
	
	*Loop over all the variables we want statistics of
	forvalues i=1/`numVariables' {
		*If not a separator
		if "``i''"!="\hline" & "``i''"!="indent" {
			*Create matrices for each variable of interest
			matrix mean`i' = J(`bignumRows',`bignumCols', 1)
			matrix sd`i' = J(`bignumRows', `bignumCols',1)
			matrix freq = J(`bignumRows', `bignumCols', 1)
			matrix N = J(`bignumRows',`=`bignumCols'+1', 1)
			matrix p`i' = J(`numRows',1, 1)
			forvalues j=1/`numRows' {
				local varLab`i' : variable label ``i''
				if "`varLab`i''"=="" {
					local varLab`i' = subinstr("``i''", "_", "\_", 10)
				}
				if `numCols'==2 {
					quietly ttest ``i'' if `group2'==`j', by (`group1')
					matrix mean`i'[`j', 1] = r(mu_1)
					matrix mean`i'[`j', 2] = r(mu_2)
					matrix sd`i'[`j',1]=r(sd_1)
					matrix sd`i'[`j',2]=r(sd_2)
					matrix N[`j',1]=r(N_1)
					matrix N[`j',2]=r(N_2)
					matrix p`i'[`j',1]=r(p)
					if `numRows'>1 {
						quietly ttest ``i'' if `group2'==`j', by (`group1')
						matrix mean`i'[`j', 1] = r(mu_1)
						matrix mean`i'[`j', 2] = r(mu_2)
						matrix sd`i'[`j',1]=r(sd_1)
						matrix sd`i'[`j',2]=r(sd_2)
						matrix N[`j',1]=r(N_1)
						matrix N[`j',2]=r(N_2)
						matrix p`i'[`j',1]=r(p)
						if `bignumRows'!=`numRows' {
							local vertCat`bignumRows'="Total"
						}
					}
				}
				else {
					forvalues k=1/`numCols' {
						quietly summ ``i'' if `group2'==`j' & `group1'==`k'
						matrix mean`i'[`j', `k'] = r(mean)
						matrix sd`i'[`j',`k']=r(sd)
						matrix N[`j',`k']=r(N)
					}
				}
				forvalues k=1/`numCols' {
					quietly count if `group2'==`j' & `group1'==`k'
					matrix freq[`j', `k'] = r(N)
						if `numRows'>1 {
							local vertValLabs : value label `group2'
							local vertCat`j' : label `vertValLabs' `j'
							local vertVarLab : variable label `group1'
						}
						local horValLabs : value label `group1'
						local horCat`k' : label `horValLabs' `k'
						local horVarLab : variable label `group1'
						if `numRows'>1 & "`secondTotal'"!="" {
							quietly summ ``i'' if `group1'==`k' & `group2'!=.
							matrix mean`i'[`bignumRows',`k'] = r(mean)
							matrix sd`i'[`bignumRows',`k']=r(sd)
							matrix N[`bignumRows',`k']=r(N)
							quietly count if `group1'==`k' & `group2'!=.
							matrix freq[`bignumRows', `k'] = r(N)
							local vertCat`bignumRows'="Total"
						}
				}
				if "`firstTotal'"!="" {
					*Last column (total column)
					quietly summ ``i'' if `group2'==`j' & `group1'!=.
					matrix mean`i'[`j', `bignumCols'] = r(mean) 
					matrix sd`i'[`j',`bignumCols']=r(sd)
					matrix N[`j',`bignumCols']=r(N)
					quietly count if `group2'==`j' & `group1'!=.
					matrix freq[`j', `bignumCols'] = r(N)			
					local horCat`bignumCols' = "Total"
				}
				else {
					quietly summ ``i'' if `group2'==`j' & `group1'!=.
					matrix N[`j',`=`numCols'+1']=r(N)
				}
			}	
			*Last row (total row)
			if `numRows'>1 & "`secondTotal'"!="" {	
				quietly summ ``i'' if `group2'!=. & `group1'!=.
				matrix mean`i'[`bignumRows', `bignumCols'] = r(mean)
				matrix sd`i'[`bignumRows',`bignumCols']=r(sd)
				matrix N[`bignumRows',`bignumCols']=r(N)
				quietly count if `group2'!=. & `group1'!=.
				matrix freq[`bignumRows', `bignumCols'] = r(N)
			}	
			matrix list mean`i'
		}
	}
	


	*===============================
	*Preparing table body for output
	*===============================
		
	local ccc : display _dup(`bignumCols') "c"

	*Set up labels across top of table
	local Cols = "&"
	forvalues i=1/`bignumCols' {
		local Cols = "`Cols'" + "&" + "\textbf{`horCat`i''}"
	}
	local Cols = "`Cols'" + "&" + "\textbf{Difference}"
	local tabBody = ""
	local count = 0 
	forvalues i=1/`bignumRows' {
		local count = `count'+1
		*Set up category labels along left side of table
		local tabBody = "`tabBody'" + "\textbf{`vertCat`i''}" 
		
		*Choose which stats to include in table given options passed in
		local stats = "mean"
		if ("`sd'" != "" & "`freq'" != "") { 
			local stats = "mean sd freq"
		}
		else if "`sd'" != "" {
			local stats = "mean sd" 
		}
		else if "`freq'" != "" {
			local stats = "mean freq" 
		}

		forvalues j = 1/`numVariables' {			
			if "``j''"=="\hline" {
				*Otherwise, just write the separator
				local tabBody = "`tabBody' \hline"
			}
			else if "``j''"=="indent" {
			}
			*If not a separator, write everything to appropriate location
			else {
				foreach stat in `stats' {
					if "`stat'"=="mean" {
						if `numRows'>1 {
							local tabBody = "`tabBody'&" + "`varLab`j''"
						}
						else {
							if `j'>1 {
								if "```=`j'-1'''"!="indent" {
									local tabBody = "`tabBody'&" + "\hspace{1em}\textit{`varLab`j''}"
								}
								else {
									local tabBody = "`tabBody'&" + "\textbf{`varLab`j''}"
								}
							}
							else {
								local tabBody = "`tabBody'&" + "\textbf{`varLab`j''}"
							}
						}
					}	
					else if "`stat'"=="sd" {
						local tabBody = "`tabBody'&"
					}
					else if "`stat'"=="freq" {
						local tabBody = "`tabBody'&" 
					}
					forvalues k = 1/`bignumCols' {
						if "`stat'"=="mean" {
							*Deals with the repeating 0 and repeating 9 problem
							local entry = "`=round(`stat'`j'[`i', `k'], .01)'"
							local tabBody = "`tabBody'&" + substr("`entry'",1,strpos("`entry'", "."))+substr("`entry'", strpos("`entry'",".")+1,3)
						}
						else if "`stat'"=="sd" {
							*Deals with the repeating 0 and repeating 9 problem
							local entry = "`=round(`stat'`j'[`i', `k'], .01)'"
							local tabBody = "`tabBody'&" + "\footnotesize{("+ substr("`entry'",1,strpos("`entry'", "."))+substr("`entry'", strpos("`entry'",".")+1,3) +")}"
						}
						else {
							local tabBody = "`tabBody'&" + "`=round(`stat'[`i', `k'], 1)'"
						}

					}
					if "`stat'"=="mean" {
						if (p`j'[`i',1]<.01) {
							local stars = "***"
						}
						else if (p`j'[`i',1]<.05) {
							local stars = "**"
						}
						local entry = "`=round(mean`j'[`i', 2] - mean`j'[`i', 1] , .01)'"
						local tabBody = "`tabBody'&"+ substr("`entry'",1,strpos("`entry'", "."))+substr("`entry'", strpos("`entry'",".")+1,3) + "`stars'"
					}
					local tabBody = "`tabBody'"+"\\"
				}
			}
		}
		*Write sample size (outside of variables loop
		if `numRows'==1 {
			local tabBody = "`tabBody'&" + "\textbf{N}"
		}
		else {
			local tabBody = "`tabBody'&" + "N"
		}
		forvalues k = 1/`bignumCols' {
			local tabBody = "`tabBody'&" + "`=round(N[`i', `k'], 1)'"
		}
		local tabBody = "`tabBody'"+"&`=round(N[`i', `=`bignumCols'+1'], 1)'\\"
	}

	/*local keyText = ""
	forvalues i=1/`numVariables' {
		local keyText = "`keyText'" + "Mean `varLab`i''" +  char(10)
	}
	local keyText = "`keyText'" + "Frequency"*/

	
	*===================
	*Write to Latex file
	*===================
	#delimit ;
	file open Output using "`name'.tex", write replace;
	file write Output "\begin{table}[!htbp] \centering \caption{`title'} \medskip 
	\begin{tabular}{ll`ccc'c} \hline \hline
	`Cols' \\ \hline
	`tabBody'
	\hline
	\end{tabular}  \\"  _n
	"\label{`label'} \end{table}";
	file close Output;
end;

	
