#X:\courses\2019CR\data\Res2019\HousesSoldNE.spc was created on 8/9/2019 11:51:14 AM
#Created using X-13A-S version 1.1 build 48

series{ 
    file = "HousesSoldNE.dat"
    format = Datevalue
    period = 12
}
spectrum{ 
    savelog = peaks
}
transform{ 
    function = log
}
regression{ 
    variables = ( AO2012.Jun )
    #aictest = ( td easter )
    #savelog = aictest
}
outlier{ 
    types = ( AO LS TC )
}
arima{ 
    model =  (3 1 0)(0 0 1)
}
forecast{ 
    maxlead = 36
    print = none
}
estimate{ 
    print = (roots regcmatrix acm)
    savelog = (aicc aic bic hq afc)
}
check{ 
    print = all
    savelog = (lbq nrm)
}
x11{ 
    seasonalma = s3x9
    savelog = all
}
slidingspans{ 
    savelog = percent
    additivesa = percent
}
history{ 
    estimates = (fcst aic sadj sadjchng trend trendchng)
    savelog = (asa ach atr atc)
}
