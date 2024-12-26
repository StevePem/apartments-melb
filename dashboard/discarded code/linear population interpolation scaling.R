# matching MB and SA2 populations - but this does't seem to be right, as it 
# assumes a linear population progression


# calculate population for each year, by scaling the SA2 populations so that
# they align at census years with the census year meshblock populations
# before 2006: end.mesh + ((year.est - end.est) * (end.mesh / end.est))
# 2006 - 2021: start.mesh + ((year.est - start.est) * ((end.mesh – start.mesh) / (end.est - start.est))) 
# after 2021: start.mesh + ((year.est - start.est) * (start.mesh / start.est))
pop.2003 <- MB.pop.2006 + ((SA2.pop.2003 - SA2.pop.2006) * (MB.pop.2006 / SA2.pop.2006))
pop.2004 <- MB.pop.2006 + ((SA2.pop.2004 - SA2.pop.2006) * (MB.pop.2006 / SA2.pop.2006))
pop.2005 <- MB.pop.2006 + ((SA2.pop.2005 - SA2.pop.2006) * (MB.pop.2006 / SA2.pop.2006))
pop.2006 <- MB.pop.2006
pop.2007 <- MB.pop.2006 + ((SA2.pop.2007 - SA2.pop.2006) * ((MB.pop.2011 - MB.pop.2006) / (SA2.pop.2011 - SA2.pop.2006))) 
pop.2008 <- MB.pop.2006 + ((SA2.pop.2008 - SA2.pop.2006) * ((MB.pop.2011 - MB.pop.2006) / (SA2.pop.2011 - SA2.pop.2006)))
pop.2009 <- MB.pop.2006 + ((SA2.pop.2009 - SA2.pop.2006) * ((MB.pop.2011 - MB.pop.2006) / (SA2.pop.2011 - SA2.pop.2006)))
pop.2010 <- MB.pop.2006 + ((SA2.pop.2010 - SA2.pop.2006) * ((MB.pop.2011 - MB.pop.2006) / (SA2.pop.2011 - SA2.pop.2006)))
pop.2011 <- MB.pop.2011
pop.2012 <- MB.pop.2011 + ((SA2.pop.2012 - SA2.pop.2011) * ((MB.pop.2016 - MB.pop.2011) / (SA2.pop.2016 - SA2.pop.2011))) 
pop.2013 <- MB.pop.2011 + ((SA2.pop.2013 - SA2.pop.2011) * ((MB.pop.2016 - MB.pop.2011) / (SA2.pop.2016 - SA2.pop.2011)))
pop.2014 <- MB.pop.2011 + ((SA2.pop.2014 - SA2.pop.2011) * ((MB.pop.2016 - MB.pop.2011) / (SA2.pop.2016 - SA2.pop.2011)))
pop.2015 <- MB.pop.2011 + ((SA2.pop.2015 - SA2.pop.2011) * ((MB.pop.2016 - MB.pop.2011) / (SA2.pop.2016 - SA2.pop.2011)))
pop.2016 <- MB.pop.2016
pop.2017 <- MB.pop.2016 + ((SA2.pop.2017 - SA2.pop.2016) * ((MB.pop.2021 - MB.pop.2016) / (SA2.pop.2021 - SA2.pop.2016)))
pop.2018 <- MB.pop.2016 + ((SA2.pop.2018 - SA2.pop.2016) * ((MB.pop.2021 - MB.pop.2016) / (SA2.pop.2021 - SA2.pop.2016)))
pop.2019 <- MB.pop.2016 + ((SA2.pop.2019 - SA2.pop.2016) * ((MB.pop.2021 - MB.pop.2016) / (SA2.pop.2021 - SA2.pop.2016)))
pop.2020 <- MB.pop.2016 + ((SA2.pop.2020 - SA2.pop.2016) * ((MB.pop.2021 - MB.pop.2016) / (SA2.pop.2021 - SA2.pop.2016)))
pop.2021 <- MB.pop.2021
pop.2022 <- MB.pop.2021 + ((SA2.pop.2022 - SA2.pop.2021) * (MB.pop.2021 / SA2.pop.2021))

