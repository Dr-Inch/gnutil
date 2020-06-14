#File         : x.cmd
#Created by   : GNUTIL 2.01
#Sourcefile   : test.dat
#Fit kind     : polynom, degree 2
#Fit function : y=p0+p1*x+p2*x**2+...
#Fit range    : whole file
#
#
#
#
#
#Parameters   :
p0= 7.694456987825316E-15
p1= 0.000000000000000E+00
p2= 9.999999999999999E-01
#Fit function:
fit(x)=p0+p1*x+p2*x**2
#
#
#
#plot it:
plot 'test.dat', fit(x)
